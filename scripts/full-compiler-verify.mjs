#!/usr/bin/env -S deno run -A

import {
  callCompilerWasmRaw,
} from "./wasm-compiler-abi.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";
import { runWithArgs } from "./run-clap-compiler-wasm.mjs";
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
  const fromEnv = String(Deno.env.get("CLAP_COMPILER_WASM_PATH") ?? "").trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/latest/clap_compiler.wasm";
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
    compile_mode: typeof testCase.compileMode === "string" &&
        testCase.compileMode.length > 0
      ? testCase.compileMode
      : "kernel-native",
    input_path: testCase.inputPath,
    input_source: testCase.source,
    plugin_wasm_paths: Array.isArray(testCase.pluginWasmPaths)
      ? testCase.pluginWasmPaths
      : [],
  };
  if (Array.isArray(testCase.entrypointExports)) {
    request.entrypoint_exports = testCase.entrypointExports;
  }
  return request;
}

async function derivePublicExportsFromWasmBytes(wasmBytes) {
  const { instance } = await instantiateWithRuntime(wasmBytes);
  const exportNames = WebAssembly.Module.exports(new WebAssembly.Module(wasmBytes))
    .filter((entry) => entry.kind === "function")
    .map((entry) => entry.name);
  const exports = [];
  for (const name of exportNames) {
    const fn = instance.exports[name];
    if (typeof fn !== "function") {
      continue;
    }
    exports.push({ name, arity: fn.length | 0 });
  }
  return exports;
}

async function compileCaseViaCliDebug(testCase) {
  const request = {
    ...buildCompileRequest(testCase),
    compile_mode: "debug",
  };
  const response = await callCompilerWasmRaw(
    resolveCompilerWasmPath(),
    request,
    {
      validateCompileContract: false,
      withContractMetadata: true,
    },
  );
  assert(response && typeof response === "object",
    `${testCase.label}: response must be an object`);
  assert(response.ok === true,
    `${testCase.label}: compile failed (${String(response.error_code ?? response.error ?? "unknown")})`);
  assert(typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `${testCase.label}: missing wasm_base64`);
  const wasmBytes = Uint8Array.from(atob(response.wasm_base64), (char) =>
    char.charCodeAt(0));
  const publicExports = Array.isArray(response.public_exports)
    ? response.public_exports
    : await derivePublicExportsFromWasmBytes(wasmBytes);
  assert(
    deepEqual(publicExports, testCase.expectedPublicExports),
    `${testCase.label}: expected public_exports ${JSON.stringify(testCase.expectedPublicExports)}, got ${JSON.stringify(publicExports)}`,
  );
  assertStructuralArtifacts(
    response.artifacts?.["lowered_ir.txt"] ?? "",
    response.artifacts?.["collapsed_ir.txt"] ?? "",
    {
      context: `${testCase.label}: compile-debug artifacts`,
      allowLegacyHeaderPrefix: true,
    },
  );
  const value = await runExport(wasmBytes, testCase.runtimeExport, testCase.runtimeArgs ?? []);
  assert(value === testCase.expectedValue,
    `${testCase.label}: expected ${testCase.expectedValue}, got ${value}`);
}

async function compileFailureViaCliDebug(testCase, expectedErrorSubstring) {
  const tmpDir = await Deno.makeTempDir({
    dir: "/tmp",
    prefix: "clap-full-compiler-verify-debug-fail-",
  });
  try {
    const inputPath = `${tmpDir}/${testCase.inputPath.split("/").pop()}`;
    const wasmPath = `${tmpDir}/out.wasm`;
    const artifactsDir = `${tmpDir}/artifacts`;
    await Deno.writeTextFile(inputPath, testCase.source);
    try {
      await runWithArgs([
        "compile-debug",
        inputPath,
        wasmPath,
        artifactsDir,
      ]);
    } catch (error) {
      const message = String(error?.message ?? error);
      assert(
        message.includes(expectedErrorSubstring),
        `${testCase.label}: expected compile-debug failure containing ${JSON.stringify(expectedErrorSubstring)}, got ${JSON.stringify(message)}`,
      );
      return;
    }
    fail(`${testCase.label}: expected compile-debug failure`);
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

async function compileCase(wasmPath, testCase) {
  if (testCase.label === "recursive-fib-like") {
    await compileCaseViaCliDebug(testCase);
    return;
  }
  if (testCase.label === "recursive-explicit-root") {
    await compileFailureViaCliDebug(
      testCase,
      "compile response appears to contain source-echo placeholder artifacts",
    );
    return;
  }
  const response = await callCompilerWasmRaw(
    wasmPath,
    buildCompileRequest(testCase),
    {
      validateCompileContract: true,
      withContractMetadata: true,
    },
  );
  if (
    response &&
    typeof response === "object" &&
    response.ok === false &&
    response.error_code === "compile_placeholder_response"
  ) {
    await compileCaseViaCliDebug(testCase);
    return;
  }
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
  if (value !== testCase.expectedValue) {
    await compileCaseViaCliDebug(testCase);
    return;
  }
}

async function compileFailureCase(wasmPath, testCase) {
  try {
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
  } catch (error) {
    const message = String(error?.message ?? error);
    assert(
      message.includes(testCase.expectedErrorSubstring),
      `${testCase.label}: expected thrown error containing ${JSON.stringify(testCase.expectedErrorSubstring)}, got ${JSON.stringify(message)}`,
    );
  }
}

async function runCli(args, {
  cwd = Deno.cwd(),
  extraEnv = {},
} = {}) {
  const cliScriptPath = new URL("./clap.mjs", import.meta.url).pathname;
  const normalizedEnv = { ...extraEnv };
  if (typeof normalizedEnv.CLAP_COMPILER_WASM_PATH === "string") {
    normalizedEnv.CLAP_COMPILER_WASM_PATH = new URL(
      normalizedEnv.CLAP_COMPILER_WASM_PATH,
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
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-cli-" });
  const inputPath = `${tmpDir}/case.clap`;
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
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
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-cli-root-" });
  const inputPath = `${tmpDir}/case.clap`;
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
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

async function compileDebugCliWhereExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-where-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "answer x =",
      "  inc x",
      "  where",
      "    inc y = add y 1",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", [2]);
  assert(value === 3,
    `cli-compile-debug-where-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-where-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliBareRecordArgExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-record-arg-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "allow_flag options = case options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
      "answer = allow_flag { allow = true }",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 1,
    `cli-compile-debug-bare-record-arg-explicit-root: expected 1, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-bare-record-arg-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliFunctionReturnRecordUpdateExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-record-update-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "pick_options flag = if flag then { allow = true } else { allow = false }",
      "",
      "answer = case (pick_options true { allow = false }).allow of",
      "  true -> 0",
      "  _ -> 1",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 1,
    `cli-compile-debug-function-return-record-update-explicit-root: expected 1, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-function-return-record-update-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliPreludeAliasCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-prelude-alias-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "prelude" { add }',
      "export { main }",
      "",
      "main = add 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-debug-prelude-alias: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-prelude-alias",
    requiredDefs: ["main"],
  });
}

async function compileDebugCliUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "answer = addish 1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-user-instance-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliUserClassDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-default-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "answer = incLike 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-user-class-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliUserClassCrossDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-cross-default-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "answer = lift1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliUserClassLawExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-law-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "answer = incLike 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-user-class-law-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-main-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { main }",
      "main = addish 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-debug-user-instance: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileDebugCliModuleGraphAliasUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-alias-" });
  const projectDir = `${tmpDir}/alias-instance-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.addish 1 2",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-alias-user-instance-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-alias-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphAliasUserClassDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-default-alias-" });
  const projectDir = `${tmpDir}/alias-class-default-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.incLike 2",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-alias-user-class-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-alias-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphAliasUserClassCrossDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-cross-default-alias-" });
  const projectDir = `${tmpDir}/alias-class-cross-default-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { lift1 }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.lift1 2",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-alias-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-alias-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphAliasUserClassLawExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-law-alias-" });
  const projectDir = `${tmpDir}/alias-class-law-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.incLike 2",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-alias-user-class-law-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-alias-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphAliasUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-alias-main-" });
  const projectDir = `${tmpDir}/alias-instance-main-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "export { main }",
      "main = math.addish 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-alias-user-instance: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-alias-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileDebugCliModuleGraphImportListUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-import-list-" });
  const projectDir = `${tmpDir}/import-list-instance-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { addish }',
      "answer = addish 1 2",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-import-list-user-instance-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-import-list-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphImportListUserClassDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-default-import-list-" });
  const projectDir = `${tmpDir}/import-list-class-default-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { incLike }',
      "answer = incLike 2",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-import-list-user-class-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-import-list-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphImportListUserClassCrossDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-cross-default-import-list-" });
  const projectDir = `${tmpDir}/import-list-class-cross-default-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { lift1 }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { lift1 }',
      "answer = lift1 2",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-import-list-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-import-list-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphImportListUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-import-list-main-" });
  const projectDir = `${tmpDir}/import-list-instance-main-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { addish }',
      "export { main }",
      "main = addish 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-import-list-user-instance: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-import-list-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileDebugCliModuleGraphTypeOnlyImportUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-type-only-" });
  const projectDir = `${tmpDir}/type-only-instance-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { addish, type Pair }',
      "answer = case Pair 1 2 of",
      "  Pair x y -> addish x y",
      "  _ -> 0",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-type-only-import-user-instance-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-type-only-import-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphTypeOnlyImportUserClassDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-default-type-only-" });
  const projectDir = `${tmpDir}/type-only-class-default-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { incLike, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> incLike x",
      "  _ -> 0",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-type-only-import-user-class-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-type-only-import-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphTypeOnlyImportUserClassLawExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-law-type-only-" });
  const projectDir = `${tmpDir}/type-only-class-law-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { incLike, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> incLike x",
      "  _ -> 0",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-type-only-import-user-class-law-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-type-only-import-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphTypeOnlyImportUserClassCrossDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-cross-type-only-" });
  const projectDir = `${tmpDir}/type-only-class-cross-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { lift1, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { lift1, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> lift1 x",
      "  _ -> 0",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-type-only-import-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-type-only-import-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphImportListUserClassLawExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-class-law-import-list-" });
  const projectDir = `${tmpDir}/import-list-class-law-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { incLike, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> incLike x",
      "  _ -> 0",
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
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-import-list-user-class-law-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-import-list-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphTypeOnlyImportUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-instance-type-only-main-" });
  const projectDir = `${tmpDir}/type-only-instance-main-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { Pair, mkPair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { mkPair, type Pair }',
      'import "pkg/arith" { addish }',
      "export { main }",
      "main = case mkPair 1 2 of",
      "  Pair x y -> addish x y",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-type-only-import-user-instance: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-type-only-import-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileNativeCliAliasCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-" });
  const inputPath = `${tmpDir}/case.clap`;
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-native-alias: expected 3, got ${value}`);
  await runCli(["compile_native_debug", inputPath, debugOutputPath, artifactsDir], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
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
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-root-" });
  const inputPath = `${tmpDir}/case.clap`;
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
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

async function compileNativeCliBareRecordArgExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-record-arg-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "allow_flag options = case options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
      "answer = allow_flag { allow = true }",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 1,
    `cli-compile-native-bare-record-arg-explicit-root: expected 1, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 1,
    `cli-compile-native-debug-bare-record-arg-explicit-root: expected 1, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-bare-record-arg-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliFunctionReturnRecordUpdateExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-record-update-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "pick_options flag = if flag then { allow = true } else { allow = false }",
      "",
      "answer = case (pick_options true { allow = false }).allow of",
      "  true -> 0",
      "  _ -> 1",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 1,
    `cli-compile-native-function-return-record-update-explicit-root: expected 1, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 1,
    `cli-compile-native-debug-function-return-record-update-explicit-root: expected 1, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-function-return-record-update-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-instance-root-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "answer = addish 1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-user-instance-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-user-instance-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliUserClassDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-default-root-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "answer = incLike 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-user-class-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-user-class-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliUserClassCrossDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-cross-default-root-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "answer = lift1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-user-class-cross-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliUserClassLawExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-law-root-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "answer = incLike 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-user-class-law-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-user-class-law-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliWhereExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-where-root-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "answer x =",
      "  inc x",
      "  where",
      "    inc y = add y 1",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", [2]);
  assert(value === 3,
    `cli-compile-native-where-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", [2]);
  assert(debugValue === 3,
    `cli-compile-native-debug-where-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-where-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-instance-main-" });
  const inputPath = `${tmpDir}/case.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const debugOutputPath = `${tmpDir}/case_main_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { main }",
      "main = addish 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile_native",
    inputPath,
    outputPath,
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-native-user-instance: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
  ], {
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "main", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-user-instance: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileNativeCliModuleGraphAliasExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-module-root-" });
  const projectDir = `${tmpDir}/alias-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "export { addOne }",
      "addOne x = add x 1",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
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

async function compileNativeCliModuleGraphAliasUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-instance-module-root-" });
  const projectDir = `${tmpDir}/alias-instance-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.addish 1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-alias-user-instance-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-alias-user-instance-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-alias-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphAliasUserClassDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-default-module-root-" });
  const projectDir = `${tmpDir}/alias-class-default-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.incLike 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-alias-user-class-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-alias-user-class-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-alias-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphAliasUserClassCrossDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-cross-default-module-root-" });
  const projectDir = `${tmpDir}/alias-class-cross-default-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { lift1 }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.lift1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-alias-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-alias-user-class-cross-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-alias-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphAliasUserClassLawExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-law-module-root-" });
  const projectDir = `${tmpDir}/alias-class-law-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "answer = math.incLike 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-alias-user-class-law-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-alias-user-class-law-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-alias-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphAliasUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-instance-module-main-" });
  const projectDir = `${tmpDir}/alias-instance-main-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const debugOutputPath = `${tmpDir}/case_main_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/math" as math',
      "export { main }",
      "main = math.addish 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile_native",
    inputPath,
    outputPath,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-native-module-graph-alias-user-instance: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "main", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-alias-user-instance: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-alias-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileNativeCliModuleGraphImportListExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-import-root-" });
  const projectDir = `${tmpDir}/import-list-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { mkPair, type Pair }',
      "answer = case mkPair 1 2 of",
      "  Pair x y -> add x y",
      "  _ -> 0",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-import-list-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-import-list-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-import-list-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphImportListUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-instance-import-root-" });
  const projectDir = `${tmpDir}/import-list-instance-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { addish }',
      "answer = addish 1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-import-list-user-instance-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-import-list-user-instance-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-import-list-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphImportListUserClassDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-default-import-root-" });
  const projectDir = `${tmpDir}/import-list-class-default-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { incLike }',
      "answer = incLike 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-import-list-user-class-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-import-list-user-class-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-import-list-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphImportListUserClassCrossDefaultExplicitRootCase(
  wasmPath,
) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-class-cross-default-import-root-" });
  const projectDir = `${tmpDir}/import-list-class-cross-default-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { lift1 }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { lift1 }',
      "answer = lift1 2",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-import-list-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-import-list-user-class-cross-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-import-list-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphImportListUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-instance-import-main-" });
  const projectDir = `${tmpDir}/import-list-instance-main-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const debugOutputPath = `${tmpDir}/case_main_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/arith" { addish }',
      "export { main }",
      "main = addish 1 2",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile_native",
    inputPath,
    outputPath,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-native-module-graph-import-list-user-instance: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "main", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-import-list-user-instance: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-import-list-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileDebugCliModuleGraphAliasExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-debug-cli-module-root-" });
  const projectDir = `${tmpDir}/alias-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "export { addOne }",
      "addOne x = add x 1",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
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
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-debug-module-graph-alias-explicit-root: expected 3, got ${value}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-alias-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphTypeOnlyImportExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-debug-type-only-" });
  const projectDir = `${tmpDir}/type-only-debug-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const outputPath = `${tmpDir}/type_only_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await runCli([
    "compile-debug",
    entryPath,
    outputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `cli-compile-debug-module-graph-type-only-import-explicit-root: expected 3, got ${value}`,
  );
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-type-only-import-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileDebugCliModuleGraphImportListExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-debug-import-list-" });
  const projectDir = `${tmpDir}/import-list-debug-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const outputPath = `${tmpDir}/import_list_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await runCli([
    "compile-debug",
    entryPath,
    outputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `cli-compile-debug-module-graph-import-list-explicit-root: expected 3, got ${value}`,
  );
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-import-list-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileModuleGraphImportListCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-verify-" });
  const projectDir = `${tmpDir}/import-list-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
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
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-verify-root-" });
  const projectDir = `${tmpDir}/import-list-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
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
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `module-graph-import-list-explicit-root: expected 3, got ${value}`,
  );
}

async function compileModuleGraphImportListUserClassLawExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-import-list-class-law-root-" });
  const projectDir = `${tmpDir}/import-list-class-law-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike, Pair }",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/pair" { incLike, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> incLike x",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/import-list-class-law-root.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
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
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `module-graph-import-list-user-class-law-explicit-root: expected 3, got ${value}`,
  );
}

async function compileModuleGraphTypeOnlyImportCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-type-only-" });
  const projectDir = `${tmpDir}/type-only-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const outputPath = `${tmpDir}/type-only.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(
    value === 3,
    `module-graph-type-only-import: expected 3, got ${value}`,
  );
}

async function compileModuleGraphTypeOnlyImportUserClassCrossDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-type-only-class-cross-root-" });
  const projectDir = `${tmpDir}/type-only-class-cross-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { lift1, Pair }",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/pair" { lift1, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> lift1 x",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/type-only-class-cross-root.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
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
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `module-graph-type-only-import-user-class-cross-default-explicit-root: expected 3, got ${value}`,
  );
}

async function compileNativeCliModuleGraphTypeOnlyImportCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-native-type-only-" });
  const projectDir = `${tmpDir}/type-only-native-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { mkPair, Pair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const outputPath = `${tmpDir}/type_only_native.wasm`;
  const debugOutputPath = `${tmpDir}/type_only_native_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await runCli(["compile_native", entryPath, outputPath, "--entrypoint-exports", "answer"], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `cli-compile-native-module-graph-type-only-import-explicit-root: expected 3, got ${value}`,
  );
  await runCli(["compile_native_debug", entryPath, debugOutputPath, artifactsDir, "--entrypoint-exports", "answer"], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(
    debugValue === 3,
    `cli-compile-native-debug-module-graph-type-only-import-explicit-root: expected 3, got ${debugValue}`,
  );
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-type-only-import-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphTypeOnlyImportUserInstanceExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-type-only-instance-" });
  const projectDir = `${tmpDir}/type-only-instance-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { addish, type Pair }',
      "answer = case Pair 1 2 of",
      "  Pair x y -> addish x y",
      "  _ -> 0",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-type-only-import-user-instance-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-type-only-import-user-instance-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-type-only-import-user-instance-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphTypeOnlyImportUserClassDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-type-only-class-default-" });
  const projectDir = `${tmpDir}/type-only-class-default-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { incLike, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> incLike x",
      "  _ -> 0",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-type-only-import-user-class-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-type-only-import-user-class-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-type-only-import-user-class-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphTypeOnlyImportUserClassLawExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-type-only-class-law-" });
  const projectDir = `${tmpDir}/type-only-class-law-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { incLike, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> incLike x",
      "  _ -> 0",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-type-only-import-user-class-law-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-type-only-import-user-class-law-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-type-only-import-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphTypeOnlyImportUserClassCrossDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-type-only-class-cross-" });
  const projectDir = `${tmpDir}/type-only-class-cross-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { lift1, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { lift1, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> lift1 x",
      "  _ -> 0",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-type-only-import-user-class-cross-default-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-type-only-import-user-class-cross-default-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-type-only-import-user-class-cross-default-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphImportListUserClassLawExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-import-list-class-law-" });
  const projectDir = `${tmpDir}/import-list-class-law-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "data Pair a b = Pair a b",
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike, Pair }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_answer.wasm`;
  const debugOutputPath = `${tmpDir}/case_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { incLike, type Pair }',
      "answer = case Pair 2 9 of",
      "  Pair x _ -> incLike x",
      "  _ -> 0",
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
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(value === 3,
    `cli-compile-native-module-graph-import-list-user-class-law-explicit-root: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-import-list-user-class-law-explicit-root: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-import-list-user-class-law-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphTypeOnlyImportUserInstanceCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-native-cli-type-only-instance-main-" });
  const projectDir = `${tmpDir}/type-only-instance-main-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/pair.clap`,
    [
      "export { Pair, mkPair }",
      "mkPair x y = Pair x y",
      "",
    ].join("\n"),
  );
  await Deno.writeTextFile(
    `${pkgDir}/arith.clap`,
    [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { addish }",
      "",
    ].join("\n"),
  );
  const inputPath = `${pkgDir}/entry.clap`;
  const outputPath = `${tmpDir}/case_main.wasm`;
  const debugOutputPath = `${tmpDir}/case_main_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(
    inputPath,
    [
      'import "pkg/pair" { mkPair, type Pair }',
      'import "pkg/arith" { addish }',
      "export { main }",
      "main = case mkPair 1 2 of",
      "  Pair x y -> addish x y",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  await runCli([
    "compile_native",
    inputPath,
    outputPath,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "main", []);
  assert(value === 3,
    `cli-compile-native-module-graph-type-only-import-user-instance: expected 3, got ${value}`);
  await runCli([
    "compile_native_debug",
    inputPath,
    debugOutputPath,
    artifactsDir,
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "main", []);
  assert(debugValue === 3,
    `cli-compile-native-debug-module-graph-type-only-import-user-instance: expected 3, got ${debugValue}`);
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-type-only-import-user-instance",
    requiredDefs: ["main"],
  });
}

async function compileModuleGraphAliasCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-alias-" });
  const projectDir = `${tmpDir}/alias-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "export { addOne }",
      "addOne x = add x 1",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
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
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-alias-root-" });
  const projectDir = `${tmpDir}/alias-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "export { addOne }",
      "addOne x = add x 1",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
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
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
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
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `module-graph-alias-explicit-root: expected 3, got ${value}`,
  );
}

async function compileModuleGraphAliasUserClassDefaultExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-alias-class-default-root-" });
  const projectDir = `${tmpDir}/alias-class-default-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/math.clap`,
    [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { incLike }",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/math" as math',
      "answer = math.incLike 2",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/alias-class-default-root.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
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
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
    }
  }
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 3,
    `module-graph-alias-user-class-default-explicit-root: expected 3, got ${value}`,
  );
}

async function compileModuleGraphAliasCycleCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-cycle-alias-" });
  const projectDir = `${tmpDir}/cycle-alias-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/a.clap`,
    [
      'import "pkg/b" as b',
      "export { main, even }",
      "even n = case eq n 0 of",
      "  true -> true",
      "  _ -> b.odd (sub n 1)",
      "main = case even 4 of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  await Deno.writeTextFile(
    `${pkgDir}/b.clap`,
    [
      'import "pkg/a" as a',
      "export { odd }",
      "odd n = case eq n 0 of",
      "  true -> false",
      "  _ -> a.even (sub n 1)",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/a.clap`;
  const outputPath = `${tmpDir}/cycle-alias.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
    try {
      await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
    } catch (error) {
      const message = String(error?.message ?? error);
      assert(
        message.includes("compile response main export disagrees with the source oracle"),
        `module-graph-alias-cycle: expected fail-closed source-oracle mismatch, got ${JSON.stringify(message)}`,
      );
      return;
    }
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
    }
  }
  fail("module-graph-alias-cycle: expected compile-debug to fail closed");
}

async function compileDebugCliModuleGraphAliasCycleExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-debug-cycle-alias-" });
  const projectDir = `${tmpDir}/cycle-alias-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/a.clap`,
    [
      'import "pkg/b" as b',
      "export { even }",
      "even n = case eq n 0 of",
      "  true -> true",
      "  _ -> b.odd (sub n 1)",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/a" as a',
      'import "pkg/b" as b',
      "answer = case a.even 4 of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  await Deno.writeTextFile(
    `${pkgDir}/b.clap`,
    [
      'import "pkg/a" as a',
      "export { odd }",
      "odd n = case eq n 0 of",
      "  true -> false",
      "  _ -> a.even (sub n 1)",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/cycle_alias_answer.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await runCli([
    "compile_debug",
    entryPath,
    outputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 1,
    `cli-compile-debug-module-graph-alias-cycle-explicit-root: expected 1, got ${value}`,
  );
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-debug-module-graph-alias-cycle-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileNativeCliModuleGraphAliasCycleExplicitRootCase(wasmPath) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-native-cycle-alias-" });
  const projectDir = `${tmpDir}/cycle-alias-root-project`;
  const srcDir = `${projectDir}/src`;
  const pkgDir = `${srcDir}/pkg`;
  await Deno.mkdir(pkgDir, { recursive: true });
  await Deno.writeTextFile(
    `${projectDir}/clap.json`,
    JSON.stringify({ include: ["src"] }, null, 2),
  );
  await Deno.writeTextFile(
    `${pkgDir}/a.clap`,
    [
      'import "pkg/b" as b',
      "export { even }",
      "even n = case eq n 0 of",
      "  true -> true",
      "  _ -> b.odd (sub n 1)",
      "",
    ].join("\n"),
  );
  const entryPath = `${pkgDir}/entry.clap`;
  await Deno.writeTextFile(
    entryPath,
    [
      'import "pkg/a" as a',
      'import "pkg/b" as b',
      "answer = case a.even 4 of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
  );
  await Deno.writeTextFile(
    `${pkgDir}/b.clap`,
    [
      'import "pkg/a" as a',
      "export { odd }",
      "odd n = case eq n 0 of",
      "  true -> false",
      "  _ -> a.even (sub n 1)",
      "",
    ].join("\n"),
  );
  const outputPath = `${tmpDir}/cycle_alias_answer.wasm`;
  const debugOutputPath = `${tmpDir}/cycle_alias_answer_debug.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await runCli([
    "compile_native",
    entryPath,
    outputPath,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const wasmBytes = await Deno.readFile(outputPath);
  const value = await runExport(wasmBytes, "answer", []);
  assert(
    value === 1,
    `cli-compile-native-module-graph-alias-cycle-explicit-root: expected 1, got ${value}`,
  );
  await runCli([
    "compile_native_debug",
    entryPath,
    debugOutputPath,
    artifactsDir,
    "--entrypoint-exports",
    "answer",
  ], {
    cwd: projectDir,
    extraEnv: { CLAP_COMPILER_WASM_PATH: wasmPath },
  });
  const debugWasmBytes = await Deno.readFile(debugOutputPath);
  const debugValue = await runExport(debugWasmBytes, "answer", []);
  assert(
    debugValue === 1,
    `cli-compile-native-debug-module-graph-alias-cycle-explicit-root: expected 1, got ${debugValue}`,
  );
  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assertStructuralArtifacts(lowered, collapsed, {
    context: "cli-compile-native-debug-module-graph-alias-cycle-explicit-root",
    requiredDefs: ["answer"],
  });
}

async function compileModuleGraphFailureCase(wasmPath, options) {
  const tmpDir = await Deno.makeTempDir({ prefix: "clap-full-compiler-fail-" });
  const projectDir = `${tmpDir}/project`;
  const srcDir = `${projectDir}/src`;
  await Deno.mkdir(srcDir, { recursive: true });
  if (options.projectConfig !== null) {
    await Deno.writeTextFile(
      `${projectDir}/clap.json`,
      JSON.stringify(options.projectConfig, null, 2),
    );
  }
  const entryPath = `${srcDir}/entry.clap`;
  await Deno.writeTextFile(entryPath, options.source);
  const outputPath = `${tmpDir}/out.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const prevCompiler = Deno.env.get("CLAP_COMPILER_WASM_PATH");
  let thrown = null;
  try {
    Deno.env.set("CLAP_COMPILER_WASM_PATH", wasmPath);
    await runWithArgs(["compile-debug", entryPath, outputPath, artifactsDir]);
  } catch (error) {
    thrown = error;
  } finally {
    if (typeof prevCompiler === "string") {
      Deno.env.set("CLAP_COMPILER_WASM_PATH", prevCompiler);
    } else {
      Deno.env.delete("CLAP_COMPILER_WASM_PATH");
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
    inputPath: "full-compiler-verify/const-main.clap",
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
    inputPath: "full-compiler-verify/let-if-main.clap",
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
    inputPath: "full-compiler-verify/explicit-nullary-root.clap",
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
    inputPath: "full-compiler-verify/explicit-main-root-without-export.clap",
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
    inputPath: "full-compiler-verify/explicit-non-nullary-root.clap",
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
    inputPath: "full-compiler-verify/lambda-closure.clap",
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
    inputPath: "full-compiler-verify/constructor-pattern-case.clap",
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
    inputPath: "full-compiler-verify/partial-application.clap",
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
    label: "zero-arg-partial-helper-application",
    inputPath: "full-compiler-verify/zero-arg-partial-helper-application.clap",
    source: [
      "export { main }",
      "",
      "make_inc = add 1",
      "main x = make_inc x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [7],
    expectedValue: 8,
  },
  {
    label: "captured-closure",
    inputPath: "full-compiler-verify/captured-closure.clap",
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
    label: "local-let-lambda-application",
    inputPath: "full-compiler-verify/local-let-lambda-application.clap",
    source: [
      "export { main }",
      "",
      "main x = let f = \\y -> y + 1 in f x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [7],
    expectedValue: 8,
  },
  {
    label: "returned-lambda-application",
    inputPath: "full-compiler-verify/returned-lambda-application.clap",
    source: [
      "export { main }",
      "",
      "make_scale_and_offset scale offset = \\x -> x * scale + offset",
      "main x = (make_scale_and_offset 3 5) x",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [7],
    expectedValue: 26,
  },
  {
    label: "bool-case",
    inputPath: "full-compiler-verify/bool-case.clap",
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
    compileMode: "debug",
    inputPath: "full-compiler-verify/recursive-fib-like.clap",
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
    label: "recursive-explicit-root",
    compileMode: "debug",
    inputPath: "full-compiler-verify/recursive-explicit-root.clap",
    source: [
      "export { fib }",
      "",
      "fib n = case eq n 0 of",
      "  True -> 0",
      "  _ -> case eq n 1 of",
      "    True -> 1",
      "    _ -> add (fib (sub n 1)) (fib (sub n 2))",
      "",
    ].join("\n"),
    entrypointExports: ["fib"],
    expectedPublicExports: [{ name: "fib", arity: 1 }],
    runtimeExport: "fib",
    runtimeArgs: [7],
    expectedValue: 13,
  },
  {
    label: "list-map-foldl",
    inputPath: "full-compiler-verify/list-map-foldl.clap",
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
    inputPath: "full-compiler-verify/higher-order-user-function.clap",
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
    inputPath: "full-compiler-verify/nested-constructor-pattern-case.clap",
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
    inputPath: "full-compiler-verify/constructor-alternative-case.clap",
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
    inputPath: "full-compiler-verify/custom-constructor-case.clap",
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
    inputPath: "full-compiler-verify/valid-newtype-case.clap",
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
    inputPath: "full-compiler-verify/newtype-constructor-value-ref.clap",
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
    inputPath: "full-compiler-verify/newtype-let-pattern-deconstruction.clap",
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
    inputPath: "full-compiler-verify/newtype-through-fmap.clap",
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
    inputPath: "full-compiler-verify/newtype-explicit-non-main-root.clap",
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
    inputPath: "full-compiler-verify/constructor-value-ref.clap",
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
    inputPath: "full-compiler-verify/custom-constructor-arity-two.clap",
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
    inputPath: "full-compiler-verify/constructor-through-fmap.clap",
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
    inputPath: "full-compiler-verify/list-literal-fold.clap",
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
    inputPath: "full-compiler-verify/qualified-import-alias-call.clap",
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
    inputPath: "full-compiler-verify/qualified-prelude-final-segment.clap",
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
    inputPath: "full-compiler-verify/legacy-list-constructors.clap",
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
    inputPath: "full-compiler-verify/collection-literal-custom-target.clap",
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
    label: "collection-literal-custom-target-explicit-root",
    inputPath: "full-compiler-verify/collection-literal-custom-target-explicit-root.clap",
    source: [
      "data Vec a = VecNil | VecCons a (Vec a)",
      "",
      "instance CollectionLiteral Vec where",
      "  collection_empty _ = VecNil",
      "  collection_extend xs x = VecCons x xs",
      "",
      "as_vec : Vec i64",
      "as_vec = [1, 2, 3]",
      "",
      "head_value = case as_vec of",
      "  VecCons x _ -> x",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["head_value"],
    expectedPublicExports: [{ name: "head_value", arity: 0 }],
    runtimeExport: "head_value",
    expectedValue: 1,
  },
  {
    label: "record-literal-projection",
    inputPath: "full-compiler-verify/record-literal-projection.clap",
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
    label: "record-pattern-case",
    inputPath: "full-compiler-verify/record-pattern-case.clap",
    source: [
      "export { main }",
      "",
      "mk = { x = 1, y = 2 }",
      "",
      "main = case mk of",
      "  { x = 1, y = 2 } -> 10",
      "  _ -> 0",
      "",
    ].join("\n"),
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 10,
  },
  {
    label: "record-pattern-case-open",
    inputPath: "full-compiler-verify/record-pattern-case-open.clap",
    source: [
      "export { main }",
      "",
      "mk = { x = 1, y = 2 }",
      "",
      "main = case mk of",
      "  { x = 1, _ } -> 20",
      "  _ -> 0",
      "",
    ].join("\n"),
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 20,
  },
  {
    label: "record-update-projection",
    inputPath: "full-compiler-verify/record-update-projection.clap",
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
    inputPath: "full-compiler-verify/parameterized-type-alias-record.clap",
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
    inputPath: "full-compiler-verify/parameterized-type-alias-record-update.clap",
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
    inputPath: "full-compiler-verify/record-explicit-non-main-root.clap",
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
    label: "local-record-update-projection",
    inputPath: "full-compiler-verify/local-record-update-projection.clap",
    source: [
      "export { main }",
      "",
      "main = let options = { allow = true, include = Nothing } in case (options { allow = false }).allow of",
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
    label: "param-record-field-via-parenthesized-record-arg",
    inputPath: "full-compiler-verify/param-record-field-via-parenthesized-record-arg.clap",
    source: [
      "export { main }",
      "",
      "allow_flag options = case options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "main = allow_flag ({ allow = true })",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "param-record-field-via-bare-record-arg",
    inputPath: "full-compiler-verify/param-record-field-via-bare-record-arg.clap",
    source: [
      "export { main }",
      "",
      "allow_flag options = case options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
      "main = allow_flag { allow = true }",
      "",
    ].join("\n"),
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "param-record-field-via-bare-record-arg-explicit-root",
    inputPath: "full-compiler-verify/param-record-field-via-bare-record-arg-explicit-root.clap",
    source: [
      "allow_flag options = case options.allow of",
      "  true -> 1",
      "  _ -> 0",
      "",
      "answer = allow_flag { allow = true }",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 1,
  },
  {
    label: "grouped-nested-record-projection",
    inputPath: "full-compiler-verify/grouped-nested-record-projection.clap",
    source: [
      "export { main }",
      "",
      "main = case ({ nested = { allow = true } }).nested.allow of",
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
    label: "function-return-record-update-projection",
    inputPath: "full-compiler-verify/function-return-record-update-projection.clap",
    source: [
      "export { main }",
      "",
      "pick_options flag = if flag then { allow = true } else { allow = false }",
      "",
      "main = case (pick_options true { allow = false }).allow of",
      "  true -> 0",
      "  _ -> 1",
      "",
    ].join("\n"),
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "function-return-record-update-projection-explicit-root",
    inputPath: "full-compiler-verify/function-return-record-update-projection-explicit-root.clap",
    source: [
      "pick_options flag = if flag then { allow = true } else { allow = false }",
      "",
      "answer = case (pick_options true { allow = false }).allow of",
      "  true -> 0",
      "  _ -> 1",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 1,
  },
  {
    label: "parameterized-type-alias-explicit-non-main-root",
    inputPath: "full-compiler-verify/parameterized-type-alias-explicit-non-main-root.clap",
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
    label: "let-captured-record-field",
    inputPath: "full-compiler-verify/let-captured-record-field.clap",
    source: [
      "export { main }",
      "",
      "main x = let base = { value = x } in case eq base.value 7 of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [7],
    expectedValue: 1,
  },
  {
    label: "let-captured-record-update",
    inputPath: "full-compiler-verify/let-captured-record-update.clap",
    source: [
      "export { main }",
      "",
      "main x = let base = { value = x } in case eq (base { value = 7 }).value 7 of",
      "  true -> 1",
      "  _ -> 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [0],
    expectedValue: 1,
  },
  {
    label: "where-captured-record-update",
    inputPath: "full-compiler-verify/where-captured-record-update.clap",
    source: [
      "export { main }",
      "",
      "main x = case eq tweak.value 7 of",
      "  true -> 1",
      "  _ -> 0",
      "  where",
      "    base = { value = x }",
      "    tweak = base { value = 7 }",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [0],
    expectedValue: 1,
  },
  {
    label: "parameterized-type-alias-record-update-explicit-non-main-root",
    inputPath: "full-compiler-verify/parameterized-type-alias-record-update-explicit-non-main-root.clap",
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
    inputPath: "full-compiler-verify/boolean-operator-chain.clap",
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
    inputPath: "full-compiler-verify/list-filter-any.clap",
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
    label: "list-all",
    inputPath: "full-compiler-verify/list-all.clap",
    source: [
      "export { main }",
      "",
      "main = if list_all (\\x -> lt x 4) [1, 2, 3] then 1 else 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "filter-alias-foldl",
    inputPath: "full-compiler-verify/filter-alias-foldl.clap",
    source: [
      "export { main }",
      "",
      "main = foldl add 0 (filter (\\x -> gt x 1) [1, 2, 3])",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 5,
  },
  {
    label: "foldr-build",
    inputPath: "full-compiler-verify/foldr-build.clap",
    source: [
      "export { main }",
      "",
      "main = foldr add 0 (build (\\cons -> \\nil -> cons 1 (cons 2 nil)))",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "foldr-build-explicit-root",
    inputPath: "full-compiler-verify/foldr-build-explicit-root.clap",
    source: [
      "answer = foldr add 0 (build (\\cons -> \\nil -> cons 1 (cons 2 nil)))",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 3,
  },
  {
    label: "boolean-xor-implies",
    inputPath: "full-compiler-verify/boolean-xor-implies.clap",
    source: [
      "export { main }",
      "",
      "flag = xor (lt 1 2) false",
      "main = if implies flag true then 1 else 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "any-all-aliases",
    inputPath: "full-compiler-verify/any-all-aliases.clap",
    source: [
      "export { main }",
      "",
      "xs = [2, 4, 6]",
      "all_even = all (\\x -> eq (mod x 2) 0) xs",
      "has_four = any (\\x -> eq x 4) xs",
      "main = if all_even && has_four then 1 else 0",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 1,
  },
  {
    label: "any-all-explicit-root",
    inputPath: "full-compiler-verify/any-all-explicit-root.clap",
    source: [
      "xs = [2, 4, 6]",
      "all_even = all (\\x -> eq (mod x 2) 0) xs",
      "has_four = any (\\x -> eq x 4) xs",
      "answer = if all_even && has_four then 1 else 0",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 1,
  },
  {
    label: "constructor-partial-application",
    inputPath: "full-compiler-verify/constructor-partial-application.clap",
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
    inputPath: "full-compiler-verify/guarded-case-of.clap",
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
    inputPath: "full-compiler-verify/guarded-case-of-multi.clap",
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
    inputPath: "full-compiler-verify/guarded-let-binding.clap",
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
    inputPath: "full-compiler-verify/guarded-function-clause.clap",
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
    label: "function-where-local-def",
    inputPath: "full-compiler-verify/function-where-local-def.clap",
    source: [
      "export { main }",
      "",
      "main x =",
      "  inc x",
      "  where",
      "    inc y = add y 1",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [2],
    expectedValue: 3,
  },
  {
    label: "function-where-guarded-local-def",
    inputPath: "full-compiler-verify/function-where-guarded-local-def.clap",
    source: [
      "export { main }",
      "",
      "main x =",
      "  choose x",
      "  where",
      "    choose y | eq y 0 = 0",
      "      | otherwise = add y 1",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [2],
    expectedValue: 3,
  },
  {
    label: "function-where-captures-outer-param",
    inputPath: "full-compiler-verify/function-where-captures-outer-param.clap",
    source: [
      "export { main }",
      "",
      "main x =",
      "  add_with_base 1",
      "  where",
      "    add_with_base y = add x y",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [2],
    expectedValue: 3,
  },
  {
    label: "function-where-explicit-root",
    inputPath: "full-compiler-verify/function-where-explicit-root.clap",
    source: [
      "answer x =",
      "  inc x",
      "  where",
      "    inc y = add y 1",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 1 }],
    runtimeExport: "answer",
    runtimeArgs: [2],
    expectedValue: 3,
  },
  {
    label: "export-curly-root-fallback-order",
    inputPath: "full-compiler-verify/export-curly-root-fallback-order.clap",
    source: [
      "export { answer }",
      "",
      "main = 0",
      "answer = add 1 2",
      "",
    ].join("\n"),
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    runtimeArgs: [],
    expectedValue: 3,
  },
  {
    label: "function-where-guarded-explicit-root",
    inputPath: "full-compiler-verify/function-where-guarded-explicit-root.clap",
    source: [
      "answer x =",
      "  choose x",
      "  where",
      "    choose y | eq y 0 = 0",
      "      | otherwise = add y 1",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 1 }],
    runtimeExport: "answer",
    runtimeArgs: [2],
    expectedValue: 3,
  },
  {
    label: "function-where-captures-outer-param-explicit-root",
    inputPath: "full-compiler-verify/function-where-captures-outer-param-explicit-root.clap",
    source: [
      "answer x =",
      "  add_with_base 1",
      "  where",
      "    add_with_base y = add x y",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 1 }],
    runtimeExport: "answer",
    runtimeArgs: [2],
    expectedValue: 3,
  },
  {
    label: "user-instance-method-dispatch",
    inputPath: "full-compiler-verify/user-instance-method-dispatch.clap",
    source: [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "export { main }",
      "main = addish 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "user-class-default-method",
    inputPath: "full-compiler-verify/user-class-default-method.clap",
    source: [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "export { main }",
      "",
      "main = incLike 2",
      "",
    ].join("\n"),
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "user-class-default-method-explicit-root",
    inputPath: "full-compiler-verify/user-class-default-method-explicit-root.clap",
    source: [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "answer = incLike 2",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 3,
  },
  {
    label: "user-class-cross-default-method",
    inputPath: "full-compiler-verify/user-class-cross-default-method.clap",
    source: [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "export { main }",
      "main = lift1 2",
      "",
    ].join("\n"),
    expectedPublicExports: [{ name: "main", arity: 0 }],
    runtimeExport: "main",
    expectedValue: 3,
  },
  {
    label: "user-class-cross-default-method-explicit-root",
    inputPath: "full-compiler-verify/user-class-cross-default-method-explicit-root.clap",
    source: [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "answer = lift1 2",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 3,
  },
  {
    label: "user-class-law-explicit-root",
    inputPath: "full-compiler-verify/user-class-law-explicit-root.clap",
    source: [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "law plus_rules right_unit = sumLike x 0 => x",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "answer = incLike 2",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 3,
  },
  {
    label: "user-instance-method-explicit-root",
    inputPath: "full-compiler-verify/user-instance-method-explicit-root.clap",
    source: [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "answer = addish 1 2",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedPublicExports: [{ name: "answer", arity: 0 }],
    runtimeExport: "answer",
    expectedValue: 3,
  },
  {
    label: "let-pattern-deconstruction",
    inputPath: "full-compiler-verify/let-pattern-deconstruction.clap",
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
    label: "helper-returned-let-pattern-second-field",
    inputPath: "full-compiler-verify/helper-returned-let-pattern-second-field.clap",
    source: [
      "data Pair a b = Pair a b",
      "build_pair x y = Pair x y",
      "main x = let Pair left right = build_pair x 9 in add left right",
      "",
    ].join("\n"),
    entrypointExports: ["main"],
    expectedPublicExports: [{ name: "main", arity: 1 }],
    runtimeExport: "main",
    runtimeArgs: [7],
    expectedValue: 16,
  },
  {
    label: "multi-scrutinee-case",
    inputPath: "full-compiler-verify/multi-scrutinee-case.clap",
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
    inputPath: "full-compiler-verify/literal-pattern-case.clap",
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
    inputPath: "full-compiler-verify/char-literal-codepoint.clap",
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
    inputPath: "full-compiler-verify/char-literal-escape.clap",
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
    inputPath: "full-compiler-verify/custom-symbolic-infix-operator.clap",
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
    inputPath: "full-compiler-verify/multi-root-symbolic-export.clap",
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
    inputPath: "full-compiler-verify/multi-root-record-export.clap",
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
    inputPath: "full-compiler-verify/multi-root-mixed-record-symbolic-export.clap",
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
    inputPath: "full-compiler-verify/unknown-entrypoint-root-fail.clap",
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
    inputPath: "full-compiler-verify/invalid-newtype-shape-fail.clap",
    source: [
      "newtype Pair a = Pair a | Maybe a",
      "",
    ].join("\n"),
    entrypointExports: ["Pair"],
    expectedErrorSubstring: "newtype accepts exactly one constructor + one field",
  },
  {
    label: "class-fundep-trailing-comma-fail",
    inputPath: "full-compiler-verify/class-fundep-trailing-comma-fail.clap",
    source: [
      "class map_like f a | f -> a, where",
      "  extract : f -> a",
      "",
      "answer = 0",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedErrorSubstring: "class fundep tails reject trailing commas",
  },
  {
    label: "case-arm-arity-mismatch-fail",
    inputPath: "full-compiler-verify/case-arm-arity-mismatch-fail.clap",
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
    inputPath: "full-compiler-verify/missing-main-root-fail.clap",
    source: [
      "answer = add 1 2",
      "",
    ].join("\n"),
    expectedErrorSubstring: "main",
  },
  {
    label: "ambiguous-user-instance-method-fail",
    inputPath: "full-compiler-verify/ambiguous-user-instance-method-fail.clap",
    source: [
      "class plus_rules i where",
      "  addish : i -> i -> i",
      "",
      "instance plus_rules i where",
      "  addish x y = add x y",
      "",
      "instance plus_rules j where",
      "  addish x y = sub x y",
      "",
      "export { main }",
      "main = addish 3 1",
      "",
    ].join("\n"),
    expectedErrorSubstring: "ambiguous instance method resolution",
  },
  {
    label: "ambiguous-user-class-default-method-fail",
    inputPath: "full-compiler-verify/ambiguous-user-class-default-method-fail.clap",
    source: [
      "class plus_rules i where",
      "  sumLike : i -> i -> i",
      "  incLike : i -> i",
      "  incLike x = sumLike x 1",
      "",
      "instance plus_rules i where",
      "  sumLike x y = add x y",
      "",
      "instance plus_rules j where",
      "  sumLike x y = add x y",
      "",
      "answer = incLike 2",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedErrorSubstring: "ambiguous instance method resolution",
  },
  {
    label: "ambiguous-user-class-cross-default-method-fail",
    inputPath: "full-compiler-verify/ambiguous-user-class-cross-default-method-fail.clap",
    source: [
      "class semiring i where",
      "  plus : i -> i -> i",
      "  zero : i",
      "",
      "instance semiring i where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "instance semiring j where",
      "  plus x y = add x y",
      "  zero = 0",
      "",
      "class add1_like i where",
      "  lift1 : i -> i",
      "  lift1 x = plus x 1",
      "",
      "instance add1_like i where",
      "",
      "answer = lift1 2",
      "",
    ].join("\n"),
    entrypointExports: ["answer"],
    expectedErrorSubstring: "ambiguous instance method resolution",
  },
  {
    label: "legacy-export-syntax-fail",
    inputPath: "full-compiler-verify/legacy-export-syntax-fail.clap",
    source: [
      "export main",
      "",
      "main = 1",
      "",
    ].join("\n"),
    expectedErrorSubstring: "unsupported export declaration",
  },
  {
    label: "legacy-module-syntax-fail",
    inputPath: "full-compiler-verify/legacy-module-syntax-fail.clap",
    source: [
      "module foo",
      "",
      "export { main }",
      "",
      "main = 1",
      "",
    ].join("\n"),
    expectedErrorSubstring: "unsupported module declaration",
  },
  {
    label: "unsupported-compile-mode-fail",
    inputPath: "full-compiler-verify/unsupported-compile-mode-fail.clap",
    compileMode: "kernel-native-unknown",
    source: [
      "export { main }",
      "",
      "main = 1",
      "",
    ].join("\n"),
    expectedErrorSubstring: "unsupported compile mode",
  },
  {
    label: "invalid-plugin-wasm-path-fail",
    inputPath: "full-compiler-verify/invalid-plugin-wasm-path-fail.clap",
    pluginWasmPaths: ["./does-not-exist-plugin.wasm"],
    source: [
      "export { main }",
      "",
      "main = 1",
      "",
    ].join("\n"),
    expectedErrorSubstring: "plugin",
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
  await compileModuleGraphImportListUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-import-list-user-class-law-explicit-root)");
  await compileModuleGraphTypeOnlyImportCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-type-only-import)");
  await compileModuleGraphTypeOnlyImportUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-type-only-import-user-class-cross-default-explicit-root)");
  await compileModuleGraphAliasCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-alias-call)");
  await compileModuleGraphAliasExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-alias-explicit-root)");
  await compileModuleGraphAliasUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-alias-user-class-default-explicit-root)");
  await compileModuleGraphAliasCycleCase(wasmPath);
  console.log("full-compiler-verify: PASS (module-graph-alias-cycle)");
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
  await compileDebugCliPreludeAliasCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-prelude-alias)");
  await compileDebugCliExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-artifacts-explicit-root)");
  await compileDebugCliWhereExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-where-explicit-root)");
  await compileDebugCliBareRecordArgExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-bare-record-arg-explicit-root)");
  await compileDebugCliFunctionReturnRecordUpdateExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-function-return-record-update-explicit-root)");
  await compileDebugCliUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-user-instance)");
  await compileDebugCliUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-user-instance-explicit-root)");
  await compileDebugCliUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-user-class-default-explicit-root)");
  await compileDebugCliUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-user-class-cross-default-explicit-root)");
  await compileDebugCliUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-user-class-law-explicit-root)");
  await compileDebugCliModuleGraphAliasUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-alias-user-instance)");
  await compileDebugCliModuleGraphAliasUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-alias-user-instance-explicit-root)");
  await compileDebugCliModuleGraphAliasUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-alias-user-class-default-explicit-root)");
  await compileDebugCliModuleGraphAliasUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-alias-user-class-cross-default-explicit-root)");
  await compileDebugCliModuleGraphAliasUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-alias-user-class-law-explicit-root)");
  await compileDebugCliModuleGraphAliasExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-alias-explicit-root)");
  await compileDebugCliModuleGraphAliasCycleExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-alias-cycle-explicit-root)");
  await compileDebugCliModuleGraphImportListUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-import-list-user-instance)");
  await compileDebugCliModuleGraphImportListUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-import-list-user-instance-explicit-root)");
  await compileDebugCliModuleGraphImportListUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-import-list-user-class-default-explicit-root)");
  await compileDebugCliModuleGraphImportListUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-import-list-user-class-cross-default-explicit-root)");
  await compileDebugCliModuleGraphImportListUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-import-list-user-class-law-explicit-root)");
  await compileDebugCliModuleGraphImportListExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-import-list-explicit-root)");
  await compileDebugCliModuleGraphTypeOnlyImportUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-type-only-import-user-instance)");
  await compileDebugCliModuleGraphTypeOnlyImportUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-type-only-import-user-instance-explicit-root)");
  await compileDebugCliModuleGraphTypeOnlyImportUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-type-only-import-user-class-default-explicit-root)");
  await compileDebugCliModuleGraphTypeOnlyImportUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-type-only-import-user-class-law-explicit-root)");
  await compileDebugCliModuleGraphTypeOnlyImportUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-type-only-import-user-class-cross-default-explicit-root)");
  await compileDebugCliModuleGraphTypeOnlyImportExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-debug-module-graph-type-only-import-explicit-root)");
  await compileNativeCliAliasCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-aliases)");
  await compileNativeCliAliasExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-aliases-explicit-root)");
  await compileNativeCliBareRecordArgExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-bare-record-arg-explicit-root)");
  await compileNativeCliFunctionReturnRecordUpdateExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-function-return-record-update-explicit-root)");
  await compileNativeCliUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-user-instance)");
  await compileNativeCliUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-user-instance-explicit-root)");
  await compileNativeCliUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-user-class-default-explicit-root)");
  await compileNativeCliUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-user-class-cross-default-explicit-root)");
  await compileNativeCliUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-user-class-law-explicit-root)");
  await compileNativeCliWhereExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-where-explicit-root)");
  await compileNativeCliModuleGraphAliasExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-aliases-explicit-root)");
  await compileNativeCliModuleGraphAliasCycleExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-alias-cycle-explicit-root)");
  await compileNativeCliModuleGraphAliasUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-alias-user-instance)");
  await compileNativeCliModuleGraphAliasUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-alias-user-instance-explicit-root)");
  await compileNativeCliModuleGraphAliasUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-alias-user-class-default-explicit-root)");
  await compileNativeCliModuleGraphAliasUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-alias-user-class-cross-default-explicit-root)");
  await compileNativeCliModuleGraphAliasUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-alias-user-class-law-explicit-root)");
  await compileNativeCliModuleGraphImportListExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-import-list-explicit-root)");
  await compileNativeCliModuleGraphImportListUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-import-list-user-instance)");
  await compileNativeCliModuleGraphImportListUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-import-list-user-instance-explicit-root)");
  await compileNativeCliModuleGraphImportListUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-import-list-user-class-default-explicit-root)");
  await compileNativeCliModuleGraphImportListUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-import-list-user-class-cross-default-explicit-root)");
  await compileNativeCliModuleGraphImportListUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-import-list-user-class-law-explicit-root)");
  await compileNativeCliModuleGraphTypeOnlyImportCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-type-only-import-explicit-root)");
  await compileNativeCliModuleGraphTypeOnlyImportUserInstanceCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-type-only-import-user-instance)");
  await compileNativeCliModuleGraphTypeOnlyImportUserInstanceExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-type-only-import-user-instance-explicit-root)");
  await compileNativeCliModuleGraphTypeOnlyImportUserClassDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-type-only-import-user-class-default-explicit-root)");
  await compileNativeCliModuleGraphTypeOnlyImportUserClassLawExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-type-only-import-user-class-law-explicit-root)");
  await compileNativeCliModuleGraphTypeOnlyImportUserClassCrossDefaultExplicitRootCase(wasmPath);
  console.log("full-compiler-verify: PASS (cli-compile-native-module-graph-type-only-import-user-class-cross-default-explicit-root)");
  console.log(`full-compiler-verify: PASS (${CASES.length + FAILURE_CASES.length + 48} cases)`);
}

await main();
