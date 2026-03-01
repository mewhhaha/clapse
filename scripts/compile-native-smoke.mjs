#!/usr/bin/env -S deno run -A

import { callCompilerWasm } from "./wasm-compiler-abi.mjs";

const PRODUCER_CONTRACT_KEYS = new Set([
  "source_version",
  "compile_contract_version",
]);

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

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

function fileExists(path) {
  try {
    const stat = Deno.statSync(path);
    return stat.isFile && stat.size > 0;
  } catch {
    return false;
  }
}

function resolveWasmPath() {
  const envPath = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "")
    .trim();
  if (envPath.length > 0 && fileExists(envPath)) {
    return envPath;
  }
  if (fileExists("artifacts/latest/clapse_compiler.wasm")) {
    return "artifacts/latest/clapse_compiler.wasm";
  }
  if (fileExists("out/clapse_compiler.wasm")) {
    return "out/clapse_compiler.wasm";
  }
  throw new Error(
    "compile-native-smoke: missing compiler wasm (set CLAPSE_COMPILER_WASM_PATH or provide artifacts/latest|out compiler wasm)",
  );
}

function decodeBase64(raw) {
  const binary = atob(raw);
  const out = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    out[i] = binary.charCodeAt(i);
  }
  return out;
}

function hasSyntheticArtifactMarkers(value) {
  if (typeof value !== "string") {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

async function run() {
  const wasmPath = resolveWasmPath();
  const probeToken = `native-compile-smoke-${crypto.randomUUID()}`;
  const inputSource = [
    "main x = x",
    `-- ${probeToken}`,
    "",
  ].join("\n");
  const defaultResponse = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: "examples/native_compile_default_smoke.clapse",
    input_source: inputSource,
    plugin_wasm_paths: [],
  }, {
    withContractMetadata: true,
  });
  assert(
    defaultResponse && typeof defaultResponse === "object",
    "compile-native-smoke: default response must be an object",
  );
  assert(
    defaultResponse.ok === true,
    `compile-native-smoke: default compile failed: ${
      String(defaultResponse?.error ?? "unknown")
    }`,
  );
  assert(
    defaultResponse.backend === "kernel-native",
    `compile-native-smoke: default compile should be kernel-native backend, got ${
      String(defaultResponse?.backend ?? "<missing>")
    }`,
  );
  const response = await callCompilerWasm(wasmPath, {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: "examples/native_compile_smoke.clapse",
    input_source: inputSource,
    plugin_wasm_paths: [],
  }, {
    withContractMetadata: true,
  });
  assert(
    response && typeof response === "object",
    "compile-native-smoke: response must be an object",
  );
  assert(
    response.ok === true,
    `compile-native-smoke: compile failed: ${
      String(response?.error ?? "unknown")
    }`,
  );
  assert(
    typeof response.backend === "string",
    "compile-native-smoke: missing backend marker",
  );
  assert(
    response.backend === "kernel-native",
    `compile-native-smoke: expected backend=kernel-native, got ${
      String(response.backend)
    }`,
  );
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    "compile-native-smoke: missing wasm_base64",
  );
  const wasmBytes = decodeBase64(response.wasm_base64);
  assert(wasmBytes.length >= 8, "compile-native-smoke: wasm payload too small");
  assert(
    wasmBytes[0] === 0x00 && wasmBytes[1] === 0x61 &&
      wasmBytes[2] === 0x73 && wasmBytes[3] === 0x6d,
    "compile-native-smoke: wasm magic header mismatch",
  );
  const artifacts = response.artifacts;
  assert(
    artifacts && typeof artifacts === "object",
    "compile-native-smoke: missing artifacts object",
  );
  const fallbackKeys = boundaryFallbackContractKeys(response);
  assert(
    fallbackKeys.length === 0,
    `compile-native-smoke: compile response includes fallback contract marker(s): ${
      fallbackKeys.join(",")
    }`,
  );
  assert(
    typeof artifacts["lowered_ir.txt"] === "string" &&
      artifacts["lowered_ir.txt"].length > 0,
    "compile-native-smoke: lowered_ir.txt should be non-empty",
  );
  assert(
    typeof artifacts["collapsed_ir.txt"] === "string" &&
      artifacts["collapsed_ir.txt"].length > 0,
    "compile-native-smoke: collapsed_ir.txt should be non-empty",
  );
  assert(
    !hasSyntheticArtifactMarkers(artifacts["lowered_ir.txt"]),
    "compile-native-smoke: lowered_ir.txt should not contain synthetic placeholder markers",
  );
  assert(
    !hasSyntheticArtifactMarkers(artifacts["collapsed_ir.txt"]),
    "compile-native-smoke: collapsed_ir.txt should not contain synthetic placeholder markers",
  );
  assert(
    artifacts["lowered_ir.txt"].includes("main x = x"),
    "compile-native-smoke: lowered_ir.txt should include request source content",
  );
  assert(
    artifacts["collapsed_ir.txt"].includes("main x = x"),
    "compile-native-smoke: collapsed_ir.txt should include request source content",
  );
  assert(
    artifacts["lowered_ir.txt"].includes(probeToken),
    "compile-native-smoke: lowered_ir.txt should include request source probe token",
  );
  assert(
    artifacts["collapsed_ir.txt"].includes(probeToken),
    "compile-native-smoke: collapsed_ir.txt should include request source probe token",
  );
  console.log("compile-native-smoke: PASS");
}

await run();
