#!/usr/bin/env -S deno run -A

import {
  callCompilerWasmRaw,
} from "./wasm-compiler-abi.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";
import { runWithArgs } from "./run-clapse-compiler-wasm.mjs";
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
  return "artifacts/latest/clapse_compiler.wasm";
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
  const request = {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: testCase.inputPath,
    input_source: testCase.source,
    plugin_wasm_paths: [],
  };
  if (Array.isArray(testCase.entrypointExports)) {
    request.entrypoint_exports = testCase.entrypointExports;
  }
  return request;
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

async function compileFailureCase(wasmPath, testCase) {
  const response = await callCompilerWasmRaw(
    wasmPath,
    buildCompileRequest(testCase),
    {
      validateCompileContract: false,
      withContractMetadata: true,
    },
  );
  assert(response && typeof response === "object",
    `${testCase.label}: response must be an object`);
  assert(response.ok === false,
    `${testCase.label}: expected compile failure`);
  assert(typeof response.error === "string" && response.error.length > 0,
    `${testCase.label}: missing error text`);
  assert(
    response.error.includes(testCase.expectedErrorSubstring),
    `${testCase.label}: expected error containing ${JSON.stringify(testCase.expectedErrorSubstring)}, got ${JSON.stringify(response.error)}`,
  );
  assert(
    typeof response.wasm_base64 !== "string" || response.wasm_base64.length === 0,
    `${testCase.label}: unexpected wasm_base64 on failure`,
  );
}

async function runCli(args, {
  cwd = Deno.cwd(),
  extraEnv = {},
} = {}) {
  const cliScriptPath = new URL("./clapse.mjs", import.meta.url).pathname;
  const normalizedEnv = { ...extraEnv };
  if (typeof normalizedEnv.CLAPSE_COMPILER_WASM_PATH === "string") {
    normalizedEnv.CLAPSE_COMPILER_WASM_PATH = new URL(
      normalizedEnv.CLAPSE_COMPILER_WASM_PATH,
      `file://${Deno.cwd()}/`,
    ).pathname;
  }
  const command = new Deno.Command(Deno.execPath(), {
    args: ["run", "-A", cliScriptPath, ...args],
    cwd,
    env: {
      ...Object.fromEntries(Deno.env.toObject ? Object.entries(Deno.env.toObject()) : []),
      ...normalizedEnv,
    },
    stdout: "piped",
    stderr: "piped",
  });
  const result = await command.output();
  if (!result.success) {
    const stderr = new TextDecoder().decode(result.stderr).trim();
    const stdout = new TextDecoder().decode(result.stdout).trim();
    fail(`cli ${args[0]} failed (${stderr || stdout || "unknown error"})`);
  }
}

async function compileDebugCliCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-debug-cli-" });
  const inputPath = `${tmpDir}/case.clapse`;
  const outputPath = `${tmpDir}/case.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "export { main }",
      "",
      "main = add 1 2",
      "",
    ].join("\n"),
  );
  await runCli(["compile-debug", inputPath, outputPath, artifactsDir], {
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-debug-artifacts: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-artifacts",
    requiredDefs: ["main"],
  });
}

async function compileDebugCliExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-debug-cli-root-" });
  const inputPath = `${tmpDir}/case.clapse`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "answer = add 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-artifacts-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-artifacts-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliAliasCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-native-cli-" });
  const inputPath = `${tmpDir}/case.clapse`;
  const outputPath = `${tmpDir}/case_native.wasm`;
  const debugOutputPath = `${tmpDir}/case_native_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "export { main }",
      "",
      "main = add 1 2",
      "",
    ].join("\n"),
  );
  await runCli(["compile_native", inputPath, outputPath], {
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-native-alias: expected 3, got ${value}`);
  await runCli(["compile_native_debug", inputPath, debugOutputPath, artifactsDir], {
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "main", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-alias: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-alias",
    requiredDefs: ["main"],
  });
}

async function compileNativeCliAliasExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-native-cli-root-" });
  const inputPath = `${tmpDir}/case.clapse`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "answer = add 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile_native",
    inputPath,
    outputPath,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-alias-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-alias-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-alias-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphAliasExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-native-cli-module-root-" });
  const projectDir = `${tmpDir}/alias-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clapse.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clapse`,
    [
      "export { addOne }",
      "addOne x = add x 1",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clapse`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.addOne 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile_native",
    inputPath,
    outputPath,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-alias-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAPSE_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-alias-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-alias-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileModuleGraphImportListCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-verify-" });
  const projectDir = `${tmpDir}/import-list-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clapse.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clapse`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clapse`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/pair" { mkPair, type Pair }',
      "export { main }",
      "main = case mkPair 1 2 of",
      "  Pair x y -> add x y",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/import-list.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAPSE_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAPSE_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAPSE_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAPSE_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(
    value === 3,
    `module-graph-import-list: expected 3, got ${value}`,
  );
}

async function compileModuleGraphImportListExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-verify-root-" });
  const projectDir = `${tmpDir}/import-list-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clapse.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clapse`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clapse`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/pair" { mkPair, type Pair }',
      "answer = case mkPair 1 2 of",
      "  Pair x y -> add x y",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/import-list-root.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAPSE_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAPSE_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs([
      "compile-debug",
      entryPath,
      outputPath,
      artifactsDir,
      "--entrypoint-exports",
      "answer",
    ]);
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAPSE_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAPSE_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `module-graph-import-list-explicit-root: expected 3, got ${value}`,
  );
}

async function compileModuleGraphAliasCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-alias-" });
  const projectDir = `${tmpDir}/alias-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clapse.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clapse`,
    [
      "export { addOne }",
      "addOne x = add x 1",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clapse`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/math" as math',
      "export { main }",
      "main = math.addOne 2",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/alias.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAPSE_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAPSE_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAPSE_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAPSE_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(
    value === 3,
    `module-graph-alias-call: expected 3, got ${value}`,
  );
}

async function compileModuleGraphAliasExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-alias-root-" });
  const projectDir = `${tmpDir}/alias-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clapse.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clapse`,
    [
      "export { addOne }",
      "addOne x = add x 1",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clapse`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/math" as math',
      "answer = math.addOne 2",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/alias-root.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAPSE_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAPSE_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs([
      "compile-debug",
      entryPath,
      outputPath,
      artifactsDir,
      "--entrypoint-exports",
      "answer",
    ]);
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAPSE_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAPSE_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `module-graph-alias-explicit-root: expected 3, got ${value}`,
  );
}

async function compileModuleGraphFailureCase(wasmPath, options) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clapse-full-compiler-fail-" });
  const projectDir = `${tmpDir}/project`;
  const srcDir = `${projectDir}/src`;
  await Deno.mkdir(srcDir, { recursive: true });
  if (options.projectConfig !== null) {
    await Deno.writeTextFile(
      `${projectDir}/clapse.json`,
      JSON.stringify(options.projectConfig, null, 2),
    );
  }
  const entryPath = `${srcDir}/entry.clapse`;
  await Deno.writeTextFile(entryPath, options.source);
  const outputPath = `${tmpDir}/out.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAPSE_COMPILER_WASM_PATH");
  let thrown = null;
  try {
    Deno.env.set("CLAPSE_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
  } catch (error) {
    thrown = error;
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAPSE_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAPSE_COMPILER_WASM_PATH");
    }
  }
  assert(thrown instanceof Error,
    `${options.label}: expected compile-debug to fail`);
  const message = String(thrown.message ?? thrown);
  assert(
    message.includes(options.expectedErrorSubstring),
    `${options.label}: expected error containing ${JSON.stringify(options.expectedErrorSubstring)}, got ${JSON.stringify(message)}`,
  );
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
    label: "explicit-main-root-without-export",
    inputPath: "full-compiler-verify/explicit-main-root-without-export.clapse",
    source: [
      "main = add 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
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
    label: "valid-newtype-case",
    inputPath: "full-compiler-verify/valid-newtype-case.clapse",
    source: [
      "export { main }",
      "",
      "newtype Box a = Box a",
      "",
      "main = case Box 7 of",
      "  Box x -> x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 7,
  },
  {
    label: "newtype-constructor-value-ref",
    inputPath: "full-compiler-verify/newtype-constructor-value-ref.clapse",
    source: [
      "export { main }",
      "",
      "newtype Box a = Box a",
      "",
      "box = Box",
      "main = case box 7 of",
      "  Box x -> x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 7,
  },
  {
    label: "newtype-let-pattern-deconstruction",
    inputPath: "full-compiler-verify/newtype-let-pattern-deconstruction.clapse",
    source: [
      "export { main }",
      "",
      "newtype Box a = Box a",
      "",
      "main = let Box x = Box 7 in x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 7,
  },
  {
    label: "newtype-through-fmap",
    inputPath: "full-compiler-verify/newtype-through-fmap.clapse",
    source: [
      "export { main }",
      "",
      "newtype Box a = Box a",
      "",
      "boxes = fmap Box [7]",
      "main = case boxes of",
      "  Cons first _ -> case first of",
      "    Box x -> x",
      "    _ -> 0",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 7,
  },
  {
    label: "newtype-explicit-non-main-root",
    inputPath: "full-compiler-verify/newtype-explicit-non-main-root.clapse",
    source: [
      "newtype Box a = Box a",
      "",
      "unbox x = case x of",
      "  Box y -> y",
      "",
    ].join("\n"),
    entrypointExports: ["unbox"],
    expectedPublicExports: [{ name: "unbox", arity: 1 }],
    runtimeExport: "unbox",
    runtimeArgs: [7],
    expectedValue: 7,
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
    label: "qualified-prelude-final-segment",
    inputPath: "full-compiler-verify/qualified-prelude-final-segment.clapse",
    source: [
      "export { main }",
      "",
      "main = prelude.add 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "legacy-list-constructors",
    inputPath: "full-compiler-verify/legacy-list-constructors.clapse",
    source: [
      "export { main }",
      "",
      "main =",
      "  let xs = ListCons 1 (ListCons 2 ListNil)",
      "  in case xs of",
      "       ListCons x _ -> x",
      "       _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "collection-literal-custom-target",
    inputPath: "full-compiler-verify/collection-literal-custom-target.clapse",
    source: [
      "export { main }",
      "",
      "data Vec a = VecNil | VecCons a (Vec a)",
      "",
      "instance CollectionLiteral Vec where",
      "  collection_empty _ = VecNil",
      "  collection_extend xs x = VecCons x xs",
      "",
      "as_vec : Vec i64",
      "as_vec = [1, 2, 3]",
      "",
      "main = case as_vec of",
      "  VecCons x _ -> x",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
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
  {
    label: "record-update-projection",
    inputPath: "full-compiler-verify/record-update-projection.clapse",
    source: [
      "export { main }",
      "",
      "options = { allow = true, include = Nothing }",
      "updated = options { allow = false }",
      "main = case updated.allow of",
      "  false -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "parameterized-type-alias-record",
    inputPath: "full-compiler-verify/parameterized-type-alias-record.clapse",
    source: [
      "export { main }",
      "",
      "type Options a = { allow: bool, include: Maybe a }",
      "default_options = { allow = true, include = Nothing }",
      "main = case default_options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "parameterized-type-alias-record-update",
    inputPath: "full-compiler-verify/parameterized-type-alias-record-update.clapse",
    source: [
      "export { main }",
      "",
      "type Options a = { allow: bool, include: Maybe a }",
      "default_options = { allow = true, include = Nothing }",
      "updated = default_options { allow = false }",
      "main = case updated.allow of",
      "  false -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "record-explicit-non-main-root",
    inputPath: "full-compiler-verify/record-explicit-non-main-root.clapse",
    source: [
      "options = { allow = true, include = Nothing }",
      "",
      "allow_flag = case options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["allow_flag"],
    expectedPublicExports: [{ name: "allow_flag", arity: 0 }],
    runtimeExport: "allow_flag",
    expectedValue: 1,
  },
  {
    label: "parameterized-type-alias-explicit-non-main-root",
    inputPath: "full-compiler-verify/parameterized-type-alias-explicit-non-main-root.clapse",
    source: [
      "type Options a = { allow: bool, include: Maybe a }",
      "default_options = { allow = true, include = Nothing }",
      "",
      "allow_flag = case default_options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["allow_flag"],
    expectedPublicExports: [{ name: "allow_flag", arity: 0 }],
    runtimeExport: "allow_flag",
    expectedValue: 1,
  },
  {
    label: "parameterized-type-alias-record-update-explicit-non-main-root",
    inputPath: "full-compiler-verify/parameterized-type-alias-record-update-explicit-non-main-root.clapse",
    source: [
      "type Options a = { allow: bool, include: Maybe a }",
      "default_options = { allow = true, include = Nothing }",
      "updated = default_options { allow = false }",
      "",
      "allow_flag = case updated.allow of",
      "  false -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["allow_flag"],
    expectedPublicExports: [{ name: "allow_flag", arity: 0 }],
    runtimeExport: "allow_flag",
    expectedValue: 1,
  },
  {
    label: "boolean-operator-chain",
    inputPath: "full-compiler-verify/boolean-operator-chain.clapse",
    source: [
      "export { main }",
      "",
      "main = case lt 1 2 && not false || false of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "list-filter-any",
    inputPath: "full-compiler-verify/list-filter-any.clapse",
    source: [
      "export { main }",
      "",
      "xs = [1, 2, 3, 4]",
      "evens = list_filter (\\x -> eq (mod x 2) 0) xs",
      "main = case list_any (\\x -> eq x 4) evens of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "constructor-partial-application",
    inputPath: "full-compiler-verify/constructor-partial-application.clapse",
    source: [
      "export { main }",
      "",
      "data Pair a b = Pair a b",
      "",
      "mk = Pair 1",
      "main = case mk 2 of",
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
    label: "guarded-case-of",
    inputPath: "full-compiler-verify/guarded-case-of.clapse",
    source: [
      "export { main }",
      "",
      "x = 0",
      "main = case of",
      "  | eq x 0 -> 1",
      "  | otherwise -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "guarded-case-of-multi",
    inputPath: "full-compiler-verify/guarded-case-of-multi.clapse",
    source: [
      "export { main }",
      "",
      "x = 1",
      "main = case of",
      "  | eq x 0 -> 0",
      "  | eq x 1 -> 1",
      "  | otherwise -> 2",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "guarded-let-binding",
    inputPath: "full-compiler-verify/guarded-let-binding.clapse",
    source: [
      "export { main }",
      "",
      "main x =",
      "  let selected",
      "        | eq x 0 = 0",
      "        | otherwise = x",
      "  in selected",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [7],
    expectedValue: 7,
  },
  {
    label: "guarded-function-clause",
    inputPath: "full-compiler-verify/guarded-function-clause.clapse",
    source: [
      "export { main }",
      "",
      "add_or_zero x y | eq x 0 = 0",
      "  | otherwise = add x y",
      "main = add_or_zero 2 3",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 5,
  },
  {
    label: "let-pattern-deconstruction",
    inputPath: "full-compiler-verify/let-pattern-deconstruction.clapse",
    source: [
      "export { main }",
      "",
      "p = Pair 2 3",
      "main = let Pair left right = p in add left right",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 5,
  },
  {
    label: "multi-scrutinee-case",
    inputPath: "full-compiler-verify/multi-scrutinee-case.clapse",
    source: [
      "export { main }",
      "",
      "a = 2",
      "b = 3",
      "main = case a b of",
      "  0 0 -> 0",
      "  x y -> add x y",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 5,
  },
  {
    label: "literal-pattern-case",
    inputPath: "full-compiler-verify/literal-pattern-case.clapse",
    source: [
      "export { main }",
      "",
      "n = 2",
      "main = case n of",
      "  0 -> 0",
      "  x -> add x 1",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "char-literal-codepoint",
    inputPath: "full-compiler-verify/char-literal-codepoint.clapse",
    source: [
      "export { main }",
      "",
      "main = add 'a' 1",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 98,
  },
  {
    label: "char-literal-escape",
    inputPath: "full-compiler-verify/char-literal-escape.clapse",
    source: [
      "export { main }",
      "",
      "main = '\\n'",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 10,
  },
  {
    label: "custom-symbolic-infix-operator",
    inputPath: "full-compiler-verify/custom-symbolic-infix-operator.clapse",
    source: [
      "export { main }",
      "",
      "infixl 6 +.",
      "+. x y = add x y",
      "main = 1 +. 2 +. 3",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 6,
  },
  {
    label: "multi-root-symbolic-export",
    inputPath: "full-compiler-verify/multi-root-symbolic-export.clapse",
    source: [
      "infixl 6 +.",
      "+. x y = add x y",
      "other = add 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["+.", "other"],
    expectedPublicExports: [
      { name: "+.", arity: 2 },
      { name: "other", arity: 0 },
    ],
    runtimeExport: "+.",
    runtimeArgs: [1, 2],
    expectedValue: 3,
  },
  {
    label: "multi-root-record-export",
    inputPath: "full-compiler-verify/multi-root-record-export.clapse",
    source: [
      "type Options a = { allow: bool, include: Maybe a }",
      "default_options = { allow = true, include = Nothing }",
      "updated = default_options { allow = false }",
      "",
      "allow_true = case default_options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "allow_false = case updated.allow of",
      "  false -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["allow_true", "allow_false"],
    expectedPublicExports: [
      { name: "allow_true", arity: 0 },
      { name: "allow_false", arity: 0 },
    ],
    runtimeExport: "allow_true",
    expectedValue: 1,
  },
  {
    label: "multi-root-mixed-record-symbolic-export",
    inputPath: "full-compiler-verify/multi-root-mixed-record-symbolic-export.clapse",
    source: [
      "type Options a = { allow: bool, include: Maybe a }",
      "default_options = { allow = true, include = Nothing }",
      "",
      "allow_flag = case default_options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
      "infixl 6 +.",
      "+. x y = add x y",
      "",
    ].join("\n"),
    entrypointExports: ["allow_flag", "+."],
    expectedPublicExports: [
      { name: "allow_flag", arity: 0 },
      { name: "+.", arity: 2 },
    ],
    runtimeExport: "+.",
    runtimeArgs: [1, 2],
    expectedValue: 3,
  },
];

const FAILURE_CASES = [
  {
    label: "unknown-entrypoint-root-fail",
    inputPath: "full-compiler-verify/unknown-entrypoint-root-fail.clapse",
    source: [
      "export { main }",
      "",
      "main = add 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["missing_root"],
    expectedErrorSubstring: "unknown entrypoint root",
  },
  {
    label: "invalid-newtype-shape-fail",
    inputPath: "full-compiler-verify/invalid-newtype-shape-fail.clapse",
    source: [
      "newtype Pair a = Pair a | Maybe a",
      "",
    ].join("\n"),
    entrypointExports: ["Pair"],
    expectedErrorSubstring: "newtype accepts exactly one constructor + one field",
  },
  {
    label: "case-arm-arity-mismatch-fail",
    inputPath: "full-compiler-verify/case-arm-arity-mismatch-fail.clapse",
    source: [
      "data Maybe a = Just a | Nothing",
      "",
      "main = case Just 1 of",
      "  Just x y -> x",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedErrorSubstring: "scrutinee/arm arity mismatch",
  },
  {
    label: "missing-main-root-fail",
    inputPath: "full-compiler-verify/missing-main-root-fail.clapse",
    source: [
      "answer = add 1 2",
      "",
    ].join("\n"),
    expectedErrorSubstring: "main",
  },
];

async function main() {
  const wasmPath = resolveCompilerWasmPath();
  for (const testCase of CASES) {
    await compileCase(wasmPath, testCase);
    console.log(`full-compiler-verify: PASS (${testCase.label})`);
  }
  for (const testCase of FAILURE_CASES) {
    await compileFailureCase(wasmPath, testCase);
    console.log(`full-compiler-verify: PASS (${testCase.label})`);
  }
  await compileModuleGraphImportListCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-import-list)");
  await compileModuleGraphImportListExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-import-list-explicit-root)");
  await compileModuleGraphAliasCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-alias-call)");
  await compileModuleGraphAliasExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-alias-explicit-root)");
  await compileModuleGraphFailureCase(wasmPath, {
    label: "unresolved-bare-import-with-include",
    projectConfig: { include: ["src"] },
    source: [
      'import "pkg/does-not-exist" { missing }',
      "export { main }",
      "",
      "main = missing",
      "",
    ].join("\n"),
    expectedErrorSubstring: "unresolved import",
  });
  console.log("full-compiler-verify: PASS (unresolved-bare-import-with-include)");
  await compileModuleGraphFailureCase(wasmPath, {
    label: "unresolved-relative-quoted-import-fail",
    projectConfig: null,
    source: [
      'import "./does-not-exist" { missing }',
      "export { main }",
      "",
      "main = missing",
      "",
    ].join("\n"),
    expectedErrorSubstring: "unresolved import",
  });
  console.log("full-compiler-verify: PASS (unresolved-relative-quoted-import-fail)");
  await compileDebugCliCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-artifacts)");
  await compileDebugCliExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-artifacts-explicit-root)");
  await compileNativeCliAliasCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-aliases)");
  await compileNativeCliAliasExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-aliases-explicit-root)");
  await compileNativeCliModuleGraphAliasExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-aliases-explicit-root)");
  console.log(`full-compiler-verify: PASS (${CASES.length + FAILURE_CASES.length + 9} cases)`);
}

await main();
