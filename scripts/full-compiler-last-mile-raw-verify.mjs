#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw } from "./wasm-compiler-abi.mjs";
import { decodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

function fail(message) {
  console.error(`full-compiler-last-mile-raw-verify: FAIL (${message})`);
  Deno.exit(1);
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAP_COMPILER_WASM_PATH") ?? "").trim();
  return fromEnv.length > 0 ? fromEnv : "artifacts/latest/clap_compiler.wasm";
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
  assert(typeof exported === "function", `missing export ${exportName}`);
  const raw = exported(...args);
  return decodeTaggedInt(raw);
}

function deepEqual(a, b) {
  return JSON.stringify(a) === JSON.stringify(b);
}

async function compileCase(wasmPath, testCase) {
  const response = await callCompilerWasmRaw(
    wasmPath,
    {
      command: "compile",
      compile_mode: "kernel-native",
      input_path: testCase.inputPath,
      input_source: testCase.source,
      plugin_wasm_paths: [],
      entrypoint_exports: testCase.entrypointExports,
    },
    {
      validateCompileContract: true,
      withContractMetadata: true,
    },
  );
  assert(response?.ok === true,
    `${testCase.label}: compile failed (${String(response?.error_code ?? response?.error ?? "unknown")})`);
  assert(response.compile_strategy === "compiler_raw",
    `${testCase.label}: expected compiler_raw, got ${JSON.stringify(response.compile_strategy)}`);
  assert(response.compatibility_used !== true,
    `${testCase.label}: unexpected compatibility path`);
  assert(
    deepEqual(response.public_exports, testCase.expectedPublicExports),
    `${testCase.label}: expected public_exports ${JSON.stringify(testCase.expectedPublicExports)}, got ${JSON.stringify(response.public_exports)}`,
  );
  assert(typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `${testCase.label}: missing wasm_base64`);
  const wasmBytes = Uint8Array.from(atob(response.wasm_base64), (char) =>
    char.charCodeAt(0));
  const value = await runExport(
    wasmBytes,
    testCase.runtimeExport,
    testCase.runtimeArgs ?? [],
  );
  assert(value === testCase.expectedValue,
    `${testCase.label}: expected ${testCase.expectedValue}, got ${value}`);
}

const CASES = [
  {
    label: "last-mile-same-line-ctor-let-pattern",
    inputPath: "full-compiler-last-mile-raw-verify/same-line-ctor-let-pattern.clap",
    source: [
      "export { main }",
      "",
      "data HttpRequest method path version = HttpRequest method path version",
      "main x = let HttpRequest method path version = HttpRequest x 2 3 in method",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [15],
    expectedValue: 15,
  },
  {
    label: "last-mile-helper-returned-simple-constructor-field",
    inputPath: "full-compiler-last-mile-raw-verify/helper-returned-simple-constructor-field.clap",
    source: [
      "export { main }",
      "",
      "data HttpRequest method path version = HttpRequest method path version",
      "build x = HttpRequest x 2 3",
      "main x = let HttpRequest method path version = build x in method",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [15],
    expectedValue: 15,
  },
  {
    label: "last-mile-helper-returned-multiline-let-constructor-field",
    inputPath: "full-compiler-last-mile-raw-verify/helper-returned-multiline-let-constructor-field.clap",
    source: [
      "export { main }",
      "",
      "data HttpRequest method path version = HttpRequest method path version",
      "build x = let",
      "  method = x",
      "  path = 2",
      "  version = 3",
      "  in HttpRequest method path version",
      "main x = let HttpRequest method path version = build x in method",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [15],
    expectedValue: 15,
  },
  {
    label: "last-mile-nested-constructor-helper-chain",
    inputPath: "full-compiler-last-mile-raw-verify/nested-constructor-helper-chain.clap",
    source: [
      "export { main }",
      "",
      "data List a = Nil | Cons a (List a)",
      "sum_three xs = case xs of",
      "  Nil -> 0",
      "  Cons a rest -> add_tail a rest",
      "add_tail a ys = case ys of",
      "  Nil -> a",
      "  Cons b tail -> add_tail2 a b tail",
      "add_tail2 a b zs = case zs of",
      "  Nil -> a + b",
      "  Cons c _ -> a + b + c",
      "mk_three a b c = Cons a (Cons b (Cons c Nil))",
      "main x = sum_three (mk_three x 2 3)",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [15],
    expectedValue: 20,
  },
  {
    label: "last-mile-http-parser-reduced",
    inputPath: "full-compiler-last-mile-raw-verify/http-parser-reduced.clap",
    source: [
      "export { main }",
      "",
      "data HttpRequest method path version = HttpRequest method path version",
      "method_code packed = packed / 10000",
      "without_method packed = packed - method_code packed * 10000",
      "path_code packed = without_method packed / 100",
      "version_code packed = without_method packed - path_code packed * 100",
      "parse_http_request packed = let",
      "  method = method_code packed;",
      "  path = path_code packed;",
      "  version = version_code packed",
      "  in HttpRequest method path version",
      "request_method req = let HttpRequest method path version = req in method",
      "main packed = request_method (parse_http_request packed)",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [10203],
    expectedValue: 1,
  },
  {
    label: "last-mile-maybe-bind-reduced",
    inputPath: "full-compiler-last-mile-raw-verify/maybe-bind-reduced.clap",
    source: [
      "export { main }",
      "",
      "data Maybe a = Nothing : Maybe a | Just : a -> Maybe a",
      "maybe_pure x = Just x",
      "maybe_bind m f = case m of",
      "  Nothing -> m",
      "  Just x -> f x",
      "inc x = x + 1",
      "maybe_demo x = case maybe_bind (maybe_pure x) (\\n -> maybe_pure (inc n)) of",
      "  Nothing -> 0",
      "  Just y -> y",
      "main x = maybe_demo x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [15],
    expectedValue: 16,
  },
  {
    label: "last-mile-either-bind-reduced",
    inputPath: "full-compiler-last-mile-raw-verify/either-bind-reduced.clap",
    source: [
      "export { main }",
      "",
      "data Either e a = Left : e -> Either e a | Right : a -> Either e a",
      "either_pure x = Right x",
      "either_bind m f = case m of",
      "  Left err -> m",
      "  Right x -> f x",
      "double x = x * 2",
      "either_demo x = case either_bind (either_pure x) (\\n -> either_pure (double n)) of",
      "  Left _ -> 0",
      "  Right y -> y",
      "main x = either_demo x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [15],
    expectedValue: 30,
  },
];

async function main() {
  const wasmPath = resolveCompilerWasmPath();
  for (const testCase of CASES) {
    await compileCase(wasmPath, testCase);
  }
  console.log(
    `full-compiler-last-mile-raw-verify: PASS (${CASES.length} cases)`,
  );
}

await main();
