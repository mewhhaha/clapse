#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw } from "./wasm-compiler-abi.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "")
    .trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/latest/clapse_compiler.wasm";
}

function isObject(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function readCollapsedArtifact(response, label) {
  assert(
    isObject(response),
    `native-tail-recursion-gate: ${label} response must be an object`,
  );
  assert(
    response.ok === true,
    `native-tail-recursion-gate: ${label} response must be ok=true`,
  );
  const artifacts = response.artifacts;
  assert(
    isObject(artifacts),
    `native-tail-recursion-gate: ${label} response missing artifacts`,
  );
  const collapsed = artifacts["collapsed_ir.txt"];
  assert(
    typeof collapsed === "string" && collapsed.length > 0,
    `native-tail-recursion-gate: ${label} collapsed_ir.txt missing`,
  );
  return collapsed;
}

function buildCompileRequest(inputPath, source) {
  return {
    command: "compile",
    compile_mode: "debug",
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
  };
}

async function run() {
  const wasmPath = resolveCompilerWasmPath();
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-tail-recursion-gate-",
  });
  try {
    const selfPath = `${tmpDir}/self_tail.clapse`;
    const selfSource = [
      "export main, loop, non_tail",
      "loop n = loop n",
      "non_tail n = add 1 (non_tail n)",
      "main x = loop x",
      "",
    ].join("\n");
    await Deno.writeTextFile(selfPath, selfSource);
    const selfResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(selfPath, selfSource),
    );
    const selfCollapsed = readCollapsedArtifact(selfResponse, "self");
    assert(
      selfCollapsed.includes("VSelfTailCall loop"),
      "native-tail-recursion-gate: expected VSelfTailCall loop marker in collapsed_ir.txt",
    );
    assert(
      !selfCollapsed.includes("VSelfTailCall non_tail"),
      "native-tail-recursion-gate: non_tail should not be marked as self tail call",
    );

    const mutualPath = `${tmpDir}/mutual_tail.clapse`;
    const mutualSource = [
      "export main, even, odd",
      "even n = odd n",
      "odd n = even n",
      "main x = even x",
      "",
    ].join("\n");
    await Deno.writeTextFile(mutualPath, mutualSource);
    const mutualResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(mutualPath, mutualSource),
    );
    const mutualCollapsed = readCollapsedArtifact(mutualResponse, "mutual");
    assert(
      mutualCollapsed.includes("VMutualTailCall even -> odd"),
      "native-tail-recursion-gate: expected VMutualTailCall even -> odd marker",
    );
    assert(
      mutualCollapsed.includes("VMutualTailCall odd -> even"),
      "native-tail-recursion-gate: expected VMutualTailCall odd -> even marker",
    );

    console.log("native-tail-recursion-gate: PASS");
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
