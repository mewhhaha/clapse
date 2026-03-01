#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";
import {
  applyCompileReachabilityToCompileRequest,
} from "./compile-entrypoint-reachability.mjs";

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
    `native-ir-liveness-size-gate: ${label} response must be an object`,
  );
  assert(
    response.ok === true,
    `native-ir-liveness-size-gate: ${label} response must be ok=true`,
  );
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `native-ir-liveness-size-gate: ${label} response missing wasm_base64`,
  );
  const artifacts = response.artifacts;
  assert(
    isObject(artifacts),
    `native-ir-liveness-size-gate: ${label} response missing artifacts`,
  );
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  assert(
    typeof lowered === "string" && lowered.length > 0,
    `native-ir-liveness-size-gate: ${label} lowered_ir.txt missing`,
  );
  assert(
    typeof collapsed === "string" && collapsed.length > 0,
    `native-ir-liveness-size-gate: ${label} collapsed_ir.txt missing`,
  );
  return {
    wasmBytes: decodeWasmBase64(response.wasm_base64),
    lowered,
    collapsed,
  };
}

function buildCompileRequest(inputPath, source, extra = {}) {
  return {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
    ...extra,
  };
}

async function run() {
  const wasmPath = resolveCompilerWasmPath();
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-ir-liveness-size-gate-",
  });
  try {
    const inputPath = `${tmpDir}/gate.clapse`;
    const deadMarker = `native-ir-liveness-dead-${crypto.randomUUID()}`;
    const source = [
      "export main",
      "main x = keep x",
      "keep x = x",
      `dead_fn x = x -- ${deadMarker}`,
      "",
    ].join("\n");
    await Deno.writeTextFile(inputPath, source);

    const baselineRequest = buildCompileRequest(inputPath, source, {
      // Mark reachability metadata to bypass ABI-side request pruning.
      __clapse_host_reachability: { mode: "baseline-no-prune" },
    });
    const baselineResponse = await callCompilerWasmRaw(
      wasmPath,
      baselineRequest,
    );
    const baseline = readCompileArtifactsOrThrow(baselineResponse, "baseline");

    const dceSeedRequest = buildCompileRequest(inputPath, source);
    const dceApplied = await applyCompileReachabilityToCompileRequest(
      dceSeedRequest,
      {
        forceEnable: true,
        skipCompilerInternalPathGuard: true,
        skipIfReachabilityPresent: false,
      },
    );
    assert(
      dceApplied.reachabilityPlan !== null,
      "native-ir-liveness-size-gate: expected reachability plan for pruned request",
    );
    const prunedResponse = await callCompilerWasmRaw(
      wasmPath,
      dceApplied.requestObject,
    );
    const pruned = readCompileArtifactsOrThrow(prunedResponse, "pruned");

    const prunedHasDead = pruned.lowered.includes(deadMarker) ||
      pruned.collapsed.includes(deadMarker);
    assert(
      !prunedHasDead,
      "native-ir-liveness-size-gate: pruned artifacts still contain dead marker",
    );

    assert(
      pruned.wasmBytes.length <= baseline.wasmBytes.length,
      `native-ir-liveness-size-gate: pruned wasm bytes regressed (${pruned.wasmBytes.length} > ${baseline.wasmBytes.length})`,
    );

    const baselineHasDead = baseline.lowered.includes(deadMarker) ||
      baseline.collapsed.includes(deadMarker);
    const equalBytes = pruned.wasmBytes.length === baseline.wasmBytes.length;
    if (equalBytes && baselineHasDead) {
      console.log(
        `native-ir-liveness-size-gate: PASS (baseline=${baseline.wasmBytes.length}; pruned=${pruned.wasmBytes.length}; dead marker removed; static wasm size unchanged)`,
      );
      return;
    }
    console.log(
      `native-ir-liveness-size-gate: PASS (baseline=${baseline.wasmBytes.length}; pruned=${pruned.wasmBytes.length}; baseline_has_dead=${baselineHasDead})`,
    );
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
