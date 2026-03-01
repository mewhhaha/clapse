#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";

const DEFAULT_COMPILER_WASM_PATH = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_SAMPLES = 200;
const REQUIRED_SOURCE_VERSION_ENV = "CLAPSE_NATIVE_SOURCE_VERSION_REQUIRED";
const EXPECTED_COMPILE_CONTRACT_VERSION = "native-v1";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/native-producer-payload-scan.mjs [--wasm <path>] [--samples <n>] [--require-source-version <token>]",
    "",
    "Scans compile payload variants by issuing randomized compile requests and",
    "grouping outputs by marker/size/export shape and compile contract metadata.",
    "",
    `Env fallback: ${REQUIRED_SOURCE_VERSION_ENV}=<token>`,
  ].join("\n");
}

function fail(msg) {
  console.error(`native-producer-payload-scan: FAIL (${msg})`);
  Deno.exit(1);
}

function parseArgs(argv) {
  let wasmPath = DEFAULT_COMPILER_WASM_PATH;
  let samples = DEFAULT_SAMPLES;
  let requireSourceVersion = String(
    Deno.env.get(REQUIRED_SOURCE_VERSION_ENV) ?? "",
  ).trim();
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--help" || arg === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (arg === "--wasm") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        fail("missing value for --wasm");
      }
      wasmPath = value;
      i += 1;
      continue;
    }
    if (arg === "--samples") {
      const raw = argv[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        fail(`invalid --samples value: ${raw}`);
      }
      samples = parsed;
      i += 1;
      continue;
    }
    if (arg === "--require-source-version") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        fail("missing value for --require-source-version");
      }
      requireSourceVersion = value;
      i += 1;
      continue;
    }
    fail(`unknown argument: ${arg}`);
  }
  return { wasmPath, samples, requireSourceVersion };
}

function nonEmptyString(value) {
  return typeof value === "string" && value.length > 0;
}

function stageMarker(value) {
  if (typeof value !== "string" || value.length === 0) {
    return "no-marker";
  }
  const match = value.match(/seed-stage[0-9]+:[^)\s"]+/u);
  if (match && typeof match[0] === "string" && match[0].length > 0) {
    return match[0];
  }
  if (value.includes("kernel:compile:")) {
    return "kernel:compile:*";
  }
  return "no-marker";
}

function randomProbeSource() {
  return [
    "main x = x",
    `-- native-producer-payload-scan ${crypto.randomUUID()}`,
    "",
  ].join("\n");
}

async function wasmShapeFromBase64(raw) {
  if (typeof raw !== "string" || raw.length === 0) {
    return {
      wasm_size: 0,
      wasm_exports: [],
      wasm_valid: false,
    };
  }
  try {
    const bytes = decodeWasmBase64(raw);
    const module = await WebAssembly.compile(bytes);
    return {
      wasm_size: bytes.length,
      wasm_exports: WebAssembly.Module.exports(module).map((entry) =>
        entry.name
      ),
      wasm_valid: true,
    };
  } catch {
    return {
      wasm_size: 0,
      wasm_exports: [],
      wasm_valid: false,
    };
  }
}

async function runSample(wasmPath, index) {
  const source = randomProbeSource();
  const response = await callCompilerWasmRaw(wasmPath, {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: `examples/native_producer_payload_scan_${index}.clapse`,
    input_source: source,
    plugin_wasm_paths: [],
  });
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return { ok: false, reason: "response-not-object" };
  }
  if (response.ok !== true) {
    return {
      ok: false,
      reason: `response-not-ok:${String(response.error ?? "unknown")}`,
    };
  }
  const artifacts = response.artifacts;
  const lowered = artifacts && typeof artifacts === "object" &&
      !Array.isArray(artifacts)
    ? artifacts["lowered_ir.txt"]
    : "";
  const shape = await wasmShapeFromBase64(response.wasm_base64);
  const rawContract = response.__clapse_contract;
  const contract = rawContract && typeof rawContract === "object" &&
      !Array.isArray(rawContract)
    ? rawContract
    : null;
  const sourceVersion = nonEmptyString(contract?.source_version)
    ? contract.source_version
    : "missing";
  const compileContractVersion =
    nonEmptyString(contract?.compile_contract_version)
      ? contract.compile_contract_version
      : "missing";
  const contractValid = sourceVersion !== "missing" &&
    compileContractVersion === EXPECTED_COMPILE_CONTRACT_VERSION;
  return {
    ok: true,
    marker: stageMarker(lowered),
    wasm_size: shape.wasm_size,
    wasm_exports: shape.wasm_exports,
    wasm_valid: shape.wasm_valid,
    source_version: sourceVersion,
    compile_contract_version: compileContractVersion,
    contract_valid: contractValid,
  };
}

async function main() {
  const { wasmPath, samples, requireSourceVersion } = parseArgs(Deno.args);
  const groups = new Map();
  let failures = 0;
  for (let i = 0; i < samples; i += 1) {
    const sample = await runSample(wasmPath, i);
    if (!sample.ok) {
      failures += 1;
      const key = `fail:${sample.reason}`;
      const count = groups.get(key) ?? 0;
      groups.set(key, count + 1);
      continue;
    }
    if (
      nonEmptyString(requireSourceVersion) &&
      sample.source_version !== requireSourceVersion
    ) {
      failures += 1;
      const mismatchKey =
        `fail:source-version-mismatch:${sample.source_version}`;
      const mismatchCount = groups.get(mismatchKey) ?? 0;
      groups.set(mismatchKey, mismatchCount + 1);
      continue;
    }
    const exportsKey = sample.wasm_exports.join(",");
    const key =
      `${sample.marker}|size=${sample.wasm_size}|exports=${exportsKey}|valid=${sample.wasm_valid}|source_version=${sample.source_version}|compile_contract_version=${sample.compile_contract_version}|contract_valid=${sample.contract_valid}`;
    const count = groups.get(key) ?? 0;
    groups.set(key, count + 1);
  }

  console.log(
    JSON.stringify(
      {
        tool: "native-producer-payload-scan",
        wasm: wasmPath,
        samples,
        failures,
        required_source_version: nonEmptyString(requireSourceVersion)
          ? requireSourceVersion
          : null,
        expected_compile_contract_version: EXPECTED_COMPILE_CONTRACT_VERSION,
        variants: [...groups.entries()].map(([variant, count]) => ({
          variant,
          count,
        })),
      },
      null,
      2,
    ),
  );
}

await main();
