#!/usr/bin/env -S deno run -A

import {
  callCompilerWasmRaw,
} from "./wasm-compiler-abi.mjs";
import {
  decodeInt,
  instantiateWithRuntime,
} from "./wasm-runtime.mjs";

function fail(message) {
  console.error(`full-compiler-verify: FAIL (${message})`);
  Deno.exit(1);
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "").trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/strict-native/seed.wasm";
}

function decodeTaggedInt(raw) {
  if (typeof raw !== "number") {
    throw new Error(`expected numeric wasm result, got ${typeof raw}`);
  }
  if ((raw & 1) !== 1) {
    throw new Error(`expected tagged int result, got raw=${raw >>> 0}`);
  }
  return decodeInt(raw);
}

async function runExport(wasmBytes, exportName, args) {
  const { instance } = await instantiateWithRuntime(wasmBytes);
  const exported = instance.exports[exportName];
  assert(typeof exported === "function",
    `missing export ${exportName}`);
  const raw = exported(...args);
  return decodeTaggedInt(raw);
}

function deepEqual(a, b) {
  return JSON.stringify(a) === JSON.stringify(b);
}

function buildCompileRequest(testCase) {
  return {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: testCase.inputPath,
    input_source: testCase.source,
    plugin_wasm_paths: [],
    entrypoint_exports: testCase.entrypointExports,
  };
}

async function compileCase(wasmPath, testCase) {
  const response = await callCompilerWasmRaw(
    wasmPath,
    buildCompileRequest(testCase),
    {
      validateCompileContract: true,
      withContractMetadata: true,
    },
  );
  assert(response && typeof response === "object",
    `${testCase.label}: response must be an object`);
  assert(response.ok === true,
    `${testCase.label}: compile failed (${String(response.error_code ?? response.error ?? "unknown")})`);
  assert(typeof response.compile_strategy === "string" && response.compile_strategy.length > 0,
    `${testCase.label}: missing compile_strategy`);
  assert(response.compatibility_used !== true,
    `${testCase.label}: used compatibility path (${String(response.compile_strategy)})`);
  assert(response.compile_strategy !== "phase1_compatibility_stub",
    `${testCase.label}: used compatibility stub strategy`);
  assert(typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `${testCase.label}: missing wasm_base64`);
  assert(Array.isArray(response.public_exports),
    `${testCase.label}: missing public_exports`);
  assert(Array.isArray(response.abi_exports),
    `${testCase.label}: missing abi_exports`);
  assert(response.abi_exports.length === 0,
    `${testCase.label}: expected user-program abi_exports to be empty`);
  assert(
    deepEqual(response.public_exports, testCase.expectedPublicExports),
    `${testCase.label}: expected public_exports ${JSON.stringify(testCase.expectedPublicExports)}, got ${JSON.stringify(response.public_exports)}`,
  );
  const wasmBytes = Uint8Array.from(atob(response.wasm_base64), (char) =>
    char.charCodeAt(0));
  const value = await runExport(wasmBytes, testCase.runtimeExport, testCase.runtimeArgs ?? []);
  assert(value === testCase.expectedValue,
    `${testCase.label}: expected ${testCase.expectedValue}, got ${value}`);
}

const CASES = [
  {
    label: "const-main",
    inputPath: "full-compiler-verify/const-main.clapse",
    source: [
      "export { main }",
      "",
      "main = add 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "let-if-main",
    inputPath: "full-compiler-verify/let-if-main.clapse",
    source: [
      "export { main }",
      "",
      "main = let b = lt 1 2 in if b then 7 else 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 7,
  },
  {
    label: "explicit-nullary-root",
    inputPath: "full-compiler-verify/explicit-nullary-root.clapse",
    source: [
      "answer = add 20 22",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 42,
  },
  {
    label: "explicit-non-nullary-root",
    inputPath: "full-compiler-verify/explicit-non-nullary-root.clapse",
    source: [
      "answer x = add x 1",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 1 }],
    runtimeExport: "answer",
    runtimeArgs: [9],
    expectedValue: 10,
  },
  {
    label: "lambda-closure",
    inputPath: "full-compiler-verify/lambda-closure.clapse",
    source: [
      "export { main }",
      "",
      "inc = \\x -> add x 1",
      "main = inc 2",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "constructor-pattern-case",
    inputPath: "full-compiler-verify/constructor-pattern-case.clapse",
    source: [
      "export { main }",
      "",
      "xs = Cons 1 Nil",
      "",
      "main = case xs of",
      "  Cons x _ -> x",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "partial-application",
    inputPath: "full-compiler-verify/partial-application.clapse",
    source: [
      "export { main }",
      "",
      "add2 = add 2",
      "main = add2 3",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 5,
  },
  {
    label: "captured-closure",
    inputPath: "full-compiler-verify/captured-closure.clapse",
    source: [
      "export { main }",
      "",
      "make_adder x = \\y -> add x y",
      "main = (make_adder 2) 3",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 5,
  },
  {
    label: "bool-case",
    inputPath: "full-compiler-verify/bool-case.clapse",
    source: [
      "export { main }",
      "",
      "main = case True of",
      "  True -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "recursive-fib-like",
    inputPath: "full-compiler-verify/recursive-fib-like.clapse",
    source: [
      "export { main }",
      "",
      "fib n = case eq n 0 of",
      "  True -> 0",
      "  _ -> case eq n 1 of",
      "    True -> 1",
      "    _ -> add (fib (sub n 1)) (fib (sub n 2))",
      "main = fib 7",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 13,
  },
  {
    label: "list-map-foldl",
    inputPath: "full-compiler-verify/list-map-foldl.clapse",
    source: [
      "export { main }",
      "",
      "xs = Cons 1 (Cons 2 (Cons 3 Nil))",
      "main = foldl (+) 0 (fmap (\\x -> add x 1) xs)",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 9,
  },
  {
    label: "higher-order-user-function",
    inputPath: "full-compiler-verify/higher-order-user-function.clapse",
    source: [
      "export { main }",
      "",
      "twice f x = f (f x)",
      "add2 = add 2",
      "main = twice add2 3",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 7,
  },
  {
    label: "nested-constructor-pattern-case",
    inputPath: "full-compiler-verify/nested-constructor-pattern-case.clapse",
    source: [
      "export { main }",
      "",
      "xs = Cons (Cons 1 Nil) Nil",
      "main = case xs of",
      "  Cons (Cons x _) _ -> x",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "constructor-alternative-case",
    inputPath: "full-compiler-verify/constructor-alternative-case.clapse",
    source: [
      "export { main }",
      "",
      "xs = Nil",
      "",
      "main = case xs of",
      "  Cons x _ -> x",
      "  Nil -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 0,
  },
  {
    label: "custom-constructor-case",
    inputPath: "full-compiler-verify/custom-constructor-case.clapse",
    source: [
      "export { main }",
      "",
      "data Maybe a = Nothing | Just a",
      "",
      "main = case Just 1 of",
      "  Just x -> x",
      "  Nothing -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "constructor-value-ref",
    inputPath: "full-compiler-verify/constructor-value-ref.clapse",
    source: [
      "export { main }",
      "",
      "data Maybe a = Nothing | Just a",
      "",
      "make = Just",
      "main = case make 1 of",
      "  Just x -> x",
      "  Nothing -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "custom-constructor-arity-two",
    inputPath: "full-compiler-verify/custom-constructor-arity-two.clapse",
    source: [
      "export { main }",
      "",
      "data Pair a b = Pair a b",
      "",
      "main = case Pair 1 2 of",
      "  Pair x y -> add x y",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "constructor-through-fmap",
    inputPath: "full-compiler-verify/constructor-through-fmap.clapse",
    source: [
      "export { main }",
      "",
      "data Maybe a = Nothing | Just a",
      "",
      "xs = Cons 1 Nil",
      "ys = fmap Just xs",
      "main = case ys of",
      "  Cons (Just x) _ -> x",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "list-literal-fold",
    inputPath: "full-compiler-verify/list-literal-fold.clapse",
    source: [
      "export { main }",
      "",
      "main = foldl (+) 0 [1, 2, 3]",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 6,
  },
  {
    label: "qualified-import-alias-call",
    inputPath: "full-compiler-verify/qualified-import-alias-call.clapse",
    source: [
      "import \"math\" as m",
      "export { main }",
      "",
      "main = m.add 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "record-literal-projection",
    inputPath: "full-compiler-verify/record-literal-projection.clapse",
    source: [
      "export { main }",
      "",
      "options = { allow = true, include = Nothing }",
      "main = case options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
];

async function main() {
  const wasmPath = resolveCompilerWasmPath();
  for (const testCase of CASES) {
    await compileCase(wasmPath, testCase);
    console.log(`full-compiler-verify: PASS (${testCase.label})`);
  }
  console.log(`full-compiler-verify: PASS (${CASES.length} cases)`);
}

await main();
