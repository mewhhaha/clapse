#!/usr/bin/env -S deno run -A

import { callCompilerWasm } from "./wasm-compiler-abi.mjs";

const PRODUCER_CONTRACT_KEYS = new Set([
  "source_version",
  "compile_contract_version",
]);

function contractMeta(response) {
  const raw = response?.__clapse_contract;
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    return {};
  }
  return raw;
}

function boundaryFallbackContractKeys(response) {
  const contract = contractMeta(response);
  const keys = [];
  for (const [key, value] of Object.entries(contract)) {
    if (PRODUCER_CONTRACT_KEYS.has(key)) {
      continue;
    }
    if (
      value === false || value === null || value === 0 ||
      (typeof value === "string" && value.length === 0)
    ) {
      continue;
    }
    keys.push(key);
  }
  return keys;
}

function resolveWasmPath() {
  const candidates = [
    Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "",
    "artifacts/latest/clapse_compiler.wasm",
    "out/clapse_compiler.wasm",
  ];
  for (const candidate of candidates) {
    if (candidate.length === 0) {
      continue;
    }
    try {
      const stat = Deno.statSync(candidate);
      if (stat.isFile && stat.size > 0) {
        return candidate;
      }
    } catch {
      // try next candidate
    }
  }
  throw new Error(
    "native-boundary-strict-smoke: missing compiler wasm (set CLAPSE_COMPILER_WASM_PATH or provide artifacts/latest|out compiler wasm)",
  );
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function hasSyntheticArtifactMarkers(value) {
  if (typeof value !== "string") {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

async function runCompileSmoke(wasmPath) {
  const probeToken = `native-boundary-strict-smoke-${crypto.randomUUID()}`;
  const inputSource = [
    "main x = x",
    `-- ${probeToken}`,
    "",
  ].join("\n");
  const response = await callCompilerWasm(wasmPath, {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: "examples/native_boundary_strict_smoke.clapse",
    input_source: inputSource,
    plugin_wasm_paths: [],
  }, {
    withContractMetadata: true,
  });
  assert(
    response && typeof response === "object",
    "native-boundary-strict-smoke: compile response must be an object",
  );
  assert(
    response.ok === true,
    `native-boundary-strict-smoke: compile failed: ${
      String(response?.error ?? "unknown")
    }`,
  );
  assert(
    response.backend === "kernel-native",
    `native-boundary-strict-smoke: expected compile backend=kernel-native, got ${
      String(response?.backend ?? "<missing>")
    }`,
  );
  const artifacts = response.artifacts;
  assert(
    artifacts && typeof artifacts === "object",
    "native-boundary-strict-smoke: compile response missing artifacts object",
  );
  assert(
    typeof artifacts["lowered_ir.txt"] === "string" &&
      artifacts["lowered_ir.txt"].length > 0,
    "native-boundary-strict-smoke: compile response missing non-empty artifacts.lowered_ir.txt",
  );
  assert(
    typeof artifacts["collapsed_ir.txt"] === "string" &&
      artifacts["collapsed_ir.txt"].length > 0,
    "native-boundary-strict-smoke: compile response missing non-empty artifacts.collapsed_ir.txt",
  );
  assert(
    !hasSyntheticArtifactMarkers(artifacts["lowered_ir.txt"]),
    "native-boundary-strict-smoke: compile response lowered_ir.txt should not contain synthetic markers",
  );
  assert(
    !hasSyntheticArtifactMarkers(artifacts["collapsed_ir.txt"]),
    "native-boundary-strict-smoke: compile response collapsed_ir.txt should not contain synthetic markers",
  );
  assert(
    artifacts["lowered_ir.txt"].includes("main x = x"),
    "native-boundary-strict-smoke: compile response lowered_ir.txt should include request source content",
  );
  assert(
    artifacts["collapsed_ir.txt"].includes("main x = x"),
    "native-boundary-strict-smoke: compile response collapsed_ir.txt should include request source content",
  );
  assert(
    artifacts["lowered_ir.txt"].includes(probeToken),
    "native-boundary-strict-smoke: compile response lowered_ir.txt should include request source probe token",
  );
  assert(
    artifacts["collapsed_ir.txt"].includes(probeToken),
    "native-boundary-strict-smoke: compile response collapsed_ir.txt should include request source probe token",
  );
  const fallbackKeys = boundaryFallbackContractKeys(response);
  assert(
    fallbackKeys.length === 0,
    `native-boundary-strict-smoke: compile response includes fallback contract marker(s): ${
      fallbackKeys.join(",")
    }`,
  );
}

async function runEmitWatSmoke(wasmPath) {
  const marker = "native_boundary_emit_wat_marker";
  const response = await callCompilerWasm(wasmPath, {
    command: "emit-wat",
    emit_wat_mode: "source-data",
    input_path: "examples/native_boundary_emit_wat.clapse",
    input_source: `${marker} = 42\n`,
  });
  assert(
    response && typeof response === "object",
    "native-boundary-strict-smoke: emit-wat response must be an object",
  );
  assert(
    response.ok === true,
    `native-boundary-strict-smoke: emit-wat failed: ${
      String(response?.error ?? "unknown")
    }`,
  );
  assert(
    typeof response.wat === "string" && response.wat.includes(marker),
    "native-boundary-strict-smoke: emit-wat response missing source-data payload marker",
  );
}

async function run() {
  const wasmPath = resolveWasmPath();
  await runCompileSmoke(wasmPath);
  await runEmitWatSmoke(wasmPath);
  console.log(`native-boundary-strict-smoke: PASS (${wasmPath})`);
}

await run();
