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

function readCompileArtifacts(response, label) {
  assert(
    isObject(response),
    `native-temp-pruning-gate: ${label} response must be an object`,
  );
  assert(
    response.ok === true,
    `native-temp-pruning-gate: ${label} response must be ok=true`,
  );
  const artifacts = response.artifacts;
  assert(
    isObject(artifacts),
    `native-temp-pruning-gate: ${label} response missing artifacts`,
  );
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  assert(
    typeof lowered === "string" && lowered.length > 0,
    `native-temp-pruning-gate: ${label} lowered_ir.txt missing`,
  );
  assert(
    typeof collapsed === "string" && collapsed.length > 0,
    `native-temp-pruning-gate: ${label} collapsed_ir.txt missing`,
  );
  return { lowered, collapsed };
}

function buildCompileRequest(inputPath, source, entrypointExports = undefined) {
  const request = {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
  };
  if (entrypointExports) {
    request.entrypoint_exports = entrypointExports;
  }
  return request;
}

function extractTempIndexes(text) {
  const indexes = [];
  const regex = /\blet\s+(t\d+)\b/g;
  for (const match of text.matchAll(regex)) {
    const raw = match[1];
    indexes.push(Number(raw.slice(1)));
  }
  return indexes;
}

function assertRenumbering(prunedCollapsed, baselineCollapsed) {
  const baselineTemps = extractTempIndexes(baselineCollapsed);
  const prunedTemps = extractTempIndexes(prunedCollapsed);

  // Ensure dead-temp removal is exercised with an artificial gap.
  assert(
    baselineTemps.includes(10),
    "native-temp-pruning-gate: baseline should include t10 (to validate multi-digit renumbering target)",
  );
  assert(
    baselineTemps.includes(11),
    "native-temp-pruning-gate: baseline should include t11 for renumbering gap test",
  );

  // Confirm compacted numbering for remaining temps: expected {0,1}.
  assert(
    prunedTemps.length === 2,
    `native-temp-pruning-gate: expected 2 temps after dead-temp pruning, got ${prunedTemps.length}`,
  );
  assert(
    prunedTemps.includes(0) && prunedTemps.includes(1),
    "native-temp-pruning-gate: expected surviving temps to be renumbered to t0/t1",
  );
}

async function run() {
  const wasmPath = resolveCompilerWasmPath();
  const deadMarker = `native-temp-pruning-dead-${crypto.randomUUID()}`;
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-temp-pruning-gate-",
  });
  try {
    const inputPath = `${tmpDir}/gate.clapse`;
    const source = [
      "export { main }",
      "main x =",
      "  let t0 = add x 1",
      `  let t1 = dead_fn x -- ${deadMarker}`,
      "  let t2 = add x 2",
      "  let t3 = add x 3",
      "  let t4 = add x 4",
      "  let t5 = add x 5",
      "  let t6 = add x 6",
      "  let t7 = add x 7",
      "  let t8 = add x 8",
      "  let t9 = add x 9",
      "  let t10 = add x 10",
      "  let t11 = add x 11",
      `  let t12 = dead_fn x -- ${deadMarker}`,
      "  in add t10 t11",
      "",
    ].join("\n");
    await Deno.writeTextFile(inputPath, source);

    const baselineResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source),
    );
    const baseline = readCompileArtifacts(baselineResponse, "baseline");

    const prunedResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source, ["main"]),
    );
    const pruned = readCompileArtifacts(prunedResponse, "pruned");

    const deadPresentInBaseline = baseline.lowered.includes(deadMarker) ||
      baseline.collapsed.includes(deadMarker);
    assert(
      deadPresentInBaseline,
      "native-temp-pruning-gate: baseline artifacts should contain the dead temp marker",
    );
    assert(
      !pruned.lowered.includes(deadMarker),
      "native-temp-pruning-gate: pruned lowered_ir.txt still contains dead temp marker",
    );
    assert(
      !pruned.collapsed.includes(deadMarker),
      "native-temp-pruning-gate: pruned collapsed_ir.txt still contains dead temp marker",
    );

    assertRenumbering(pruned.collapsed, baseline.collapsed);
    assert(
      pruned.collapsed.includes("in add"),
      "native-temp-pruning-gate: pruned collapsed_ir.txt should keep expected output shape",
    );
    console.log(
      `native-temp-pruning-gate: PASS (baseline_len=${baseline.collapsed.length}, pruned_len=${pruned.collapsed.length})`,
    );
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
