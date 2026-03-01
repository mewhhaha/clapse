#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";

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

function readCompileArtifactsOrThrow(response, label) {
  assert(
    isObject(response),
    `native-entrypoint-exports-dce-gate: ${label} response must be an object`,
  );
  assert(
    response.ok === true,
    `native-entrypoint-exports-dce-gate: ${label} response must be ok=true`,
  );
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `native-entrypoint-exports-dce-gate: ${label} response missing wasm_base64`,
  );
  const artifacts = response.artifacts;
  assert(
    isObject(artifacts),
    `native-entrypoint-exports-dce-gate: ${label} response missing artifacts`,
  );
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  assert(
    typeof lowered === "string" && lowered.length > 0,
    `native-entrypoint-exports-dce-gate: ${label} lowered_ir.txt missing`,
  );
  assert(
    typeof collapsed === "string" && collapsed.length > 0,
    `native-entrypoint-exports-dce-gate: ${label} collapsed_ir.txt missing`,
  );
  return {
    wasmBytes: decodeWasmBase64(response.wasm_base64),
    lowered,
    collapsed,
  };
}

function buildCompileRequest(inputPath, source, entrypointExports = null) {
  const request = {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
  };
  if (Array.isArray(entrypointExports) && entrypointExports.length > 0) {
    request.entrypoint_exports = [...entrypointExports];
  }
  return request;
}

async function run() {
  const wasmPath = resolveCompilerWasmPath();
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-entrypoint-exports-dce-gate-",
  });
  try {
    const inputPath = `${tmpDir}/entrypoint_exports_gate.clapse`;
    const deadMarker = `native-entrypoint-exports-dead-${crypto.randomUUID()}`;
    const source = [
      "export main, helper",
      "main x = keep x",
      "keep x = x",
      `helper x = dead_fn x -- ${deadMarker}`,
      "dead_fn x = x",
      "",
    ].join("\n");
    await Deno.writeTextFile(inputPath, source);

    const baselineResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source),
    );
    const baseline = readCompileArtifactsOrThrow(baselineResponse, "baseline");
    const baselineHasDead = baseline.lowered.includes(deadMarker) ||
      baseline.collapsed.includes(deadMarker);
    assert(
      baselineHasDead,
      "native-entrypoint-exports-dce-gate: baseline artifacts should contain helper/dead marker",
    );

    const subsetResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source, ["main"]),
    );
    const subset = readCompileArtifactsOrThrow(subsetResponse, "subset");
    const subsetHasDead = subset.lowered.includes(deadMarker) ||
      subset.collapsed.includes(deadMarker);
    assert(
      !subsetHasDead,
      "native-entrypoint-exports-dce-gate: subset-root artifacts should prune helper/dead marker",
    );
    assert(
      subset.wasmBytes.length <= baseline.wasmBytes.length,
      `native-entrypoint-exports-dce-gate: subset-root wasm bytes regressed (${subset.wasmBytes.length} > ${baseline.wasmBytes.length})`,
    );

    console.log(
      `native-entrypoint-exports-dce-gate: PASS (baseline=${baseline.wasmBytes.length}; subset=${subset.wasmBytes.length}; roots=main)`,
    );
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
