#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";

const DEFAULT_COMPILER_WASM_PATH = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_INPUT_PATH = "lib/compiler/kernel.clapse";
const DEFAULT_HOPS = 1;
const MIN_COMPILER_BYTES = 4096;
const REQUIRED_SOURCE_VERSION_ENV = "CLAPSE_NATIVE_SOURCE_VERSION_REQUIRED";
const EXPECTED_COMPILE_CONTRACT_VERSION = "native-v1";
const EMIT_WAT_TEMPLATE_MEMORY_NEEDLE = '(memory (export "__memory") 1)';

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/native-producer-raw-probe.mjs [--wasm <path>] [--input <path>] [--hops <n>] [--require-source-version <token>]",
    "",
    "Checks (producer-only; no ABI normalization):",
    "  1) compile artifacts are non-synthetic and include request source",
    "  2) kernel compile output emits compiler ABI (memory + clapse_run)",
    "  3) kernel compile output remains compiler-like across N hops",
    "  4) emit-wat source mode echoes request token",
    `  5) emit-wat template mode includes '${EMIT_WAT_TEMPLATE_MEMORY_NEEDLE}'`,
    `  6) compile response contract metadata includes source_version + compile_contract_version=${EXPECTED_COMPILE_CONTRACT_VERSION}`,
    "",
    `Env fallback: ${REQUIRED_SOURCE_VERSION_ENV}=<token>`,
  ].join("\n");
}

function fail(msg) {
  console.error(`native-producer-raw-probe: FAIL (${msg})`);
  Deno.exit(1);
}

function nonEmptyString(value) {
  return typeof value === "string" && value.length > 0;
}

function hasSyntheticArtifactMarkers(value) {
  if (!nonEmptyString(value)) {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

function parseArgs(argv) {
  let wasmPath = DEFAULT_COMPILER_WASM_PATH;
  let inputPath = DEFAULT_INPUT_PATH;
  let hops = DEFAULT_HOPS;
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
    if (arg === "--input") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        fail("missing value for --input");
      }
      inputPath = value;
      i += 1;
      continue;
    }
    if (arg === "--hops") {
      const raw = argv[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        fail(`invalid --hops value: ${raw}`);
      }
      hops = parsed;
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
  return { wasmPath, inputPath, hops, requireSourceVersion };
}

function parseCompileContract(response, context, requireSourceVersion) {
  const raw = response?.__clapse_contract;
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    fail(`${context}: compile response missing __clapse_contract object`);
  }
  const sourceVersion = raw.source_version;
  if (!nonEmptyString(sourceVersion)) {
    fail(
      `${context}: compile response missing __clapse_contract.source_version`,
    );
  }
  const compileContractVersion = raw.compile_contract_version;
  if (compileContractVersion !== EXPECTED_COMPILE_CONTRACT_VERSION) {
    fail(
      `${context}: compile response compile_contract_version must be ${EXPECTED_COMPILE_CONTRACT_VERSION} (got ${
        String(compileContractVersion ?? "<missing>")
      })`,
    );
  }
  if (
    nonEmptyString(requireSourceVersion) &&
    sourceVersion !== requireSourceVersion
  ) {
    fail(
      `${context}: compile response source_version mismatch (expected ${requireSourceVersion}, got ${sourceVersion})`,
    );
  }
  return { sourceVersion, compileContractVersion };
}

function assertCompileArtifactsFromSource(
  response,
  sourceText,
  context,
  requireSourceVersion,
) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    fail(`${context}: compile response was not an object`);
  }
  if (response.ok !== true) {
    fail(
      `${context}: compile response returned ok=false (${
        String(response.error ?? "unknown")
      })`,
    );
  }
  if (response.backend !== "kernel-native") {
    fail(
      `${context}: compile response backend must be kernel-native (got ${
        String(response.backend ?? "<missing>")
      })`,
    );
  }
  const artifacts = response.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    fail(`${context}: compile response missing artifacts object`);
  }
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  if (!nonEmptyString(lowered) || !nonEmptyString(collapsed)) {
    fail(`${context}: compile response missing lowered/collapsed artifacts`);
  }
  if (hasSyntheticArtifactMarkers(lowered)) {
    fail(`${context}: lowered_ir.txt contains synthetic markers`);
  }
  if (hasSyntheticArtifactMarkers(collapsed)) {
    fail(`${context}: collapsed_ir.txt contains synthetic markers`);
  }
  if (!lowered.includes(sourceText)) {
    fail(`${context}: lowered_ir.txt missing request source content`);
  }
  if (!collapsed.includes(sourceText)) {
    fail(`${context}: collapsed_ir.txt missing request source content`);
  }
  return parseCompileContract(response, context, requireSourceVersion);
}

function compileArtifactsProbeSource() {
  const probeToken = `native_producer_raw_probe_${
    crypto.randomUUID().replaceAll("-", "_")
  }`;
  const sourceText = [
    `${probeToken} x = x`,
    `main x = ${probeToken} x`,
    "",
  ].join("\n");
  return {
    sourceText,
    sourceNeedle: probeToken,
  };
}

function compilerAbiFromBytes(bytes, context) {
  let module;
  try {
    module = new WebAssembly.Module(bytes);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`${context}: emitted wasm is invalid (${msg})`);
  }
  const exportNames = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  if (!hasMemory) {
    fail(
      `${context}: emitted wasm missing memory export (${
        exportNames.join(", ")
      })`,
    );
  }
  if (!exportNames.includes("clapse_run")) {
    fail(
      `${context}: emitted wasm missing clapse_run export (${
        exportNames.join(", ")
      })`,
    );
  }
  if (bytes.length < MIN_COMPILER_BYTES) {
    fail(
      `${context}: emitted wasm too small (${bytes.length} < ${MIN_COMPILER_BYTES})`,
    );
  }
  return exportNames;
}

async function runCompileArtifactsProbe(wasmPath, requireSourceVersion) {
  const { sourceText, sourceNeedle } = compileArtifactsProbeSource();
  const response = await callCompilerWasmRaw(wasmPath, {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: "examples/native_producer_raw_probe.clapse",
    input_source: sourceText,
    plugin_wasm_paths: [],
  });
  const contract = assertCompileArtifactsFromSource(
    response,
    sourceNeedle,
    "producer-compile-artifacts",
    requireSourceVersion,
  );
  return contract.sourceVersion;
}

function assertEmitWatResponse(response, context) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    fail(`${context}: emit-wat response was not an object`);
  }
  if (response.ok !== true) {
    fail(
      `${context}: emit-wat returned ok=false (${
        String(response.error ?? "unknown")
      })`,
    );
  }
  if (!nonEmptyString(response.wat)) {
    fail(`${context}: emit-wat response missing non-empty wat`);
  }
  return String(response.wat);
}

async function runEmitWatProbe(wasmPath) {
  const sourceToken = `native-producer-emit-wat-probe-${crypto.randomUUID()}`;
  const sourceResponse = await callCompilerWasmRaw(wasmPath, {
    command: "emit-wat",
    input_path: "examples/native_producer_emit_wat_probe.clapse",
    input_source: `${sourceToken} = 42\n`,
  });
  const sourceWat = assertEmitWatResponse(
    sourceResponse,
    "emit-wat-source-mode",
  );
  if (!sourceWat.includes(sourceToken)) {
    fail("emit-wat-source-mode: wat missing request source token");
  }

  const templateResponse = await callCompilerWasmRaw(wasmPath, {
    command: "emit-wat",
    emit_wat_mode: "template",
    input_path: "examples/native_producer_emit_wat_template_probe.clapse",
    input_source: `${sourceToken} = 42\n`,
  });
  const templateWat = assertEmitWatResponse(
    templateResponse,
    "emit-wat-template-mode",
  );
  if (!templateWat.includes(EMIT_WAT_TEMPLATE_MEMORY_NEEDLE)) {
    fail(
      `emit-wat-template-mode: wat missing template memory shape '${EMIT_WAT_TEMPLATE_MEMORY_NEEDLE}'`,
    );
  }
}

async function compileKernelHop(
  wasmPath,
  inputPath,
  inputSource,
  hopIndex,
  requireSourceVersion,
) {
  let response;
  try {
    response = await callCompilerWasmRaw(wasmPath, {
      command: "compile",
      compile_mode: "kernel-native",
      input_path: inputPath,
      input_source: inputSource,
      plugin_wasm_paths: [],
    });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`hop ${hopIndex}: compile request failed (${msg})`);
  }
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    fail(`hop ${hopIndex}: compile response was not an object`);
  }
  if (response.ok !== true) {
    fail(
      `hop ${hopIndex}: compile response returned ok=false (${
        String(response.error ?? "unknown")
      })`,
    );
  }
  if (response.backend !== "kernel-native") {
    fail(
      `hop ${hopIndex}: compile response backend must be kernel-native (got ${
        String(response.backend ?? "<missing>")
      })`,
    );
  }
  if (!nonEmptyString(response.wasm_base64)) {
    fail(`hop ${hopIndex}: compile response missing non-empty wasm_base64`);
  }
  const contract = parseCompileContract(
    response,
    `hop ${hopIndex}`,
    requireSourceVersion,
  );
  let bytes;
  try {
    bytes = decodeWasmBase64(response.wasm_base64);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`hop ${hopIndex}: failed to decode wasm_base64 (${msg})`);
  }
  const exportNames = compilerAbiFromBytes(bytes, `hop ${hopIndex}`);
  return { bytes, exportNames, sourceVersion: contract.sourceVersion };
}

async function runKernelHopProbe(
  wasmPath,
  inputPath,
  hops,
  requireSourceVersion,
) {
  let inputSource = "";
  try {
    inputSource = await Deno.readTextFile(inputPath);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`failed to read input source '${inputPath}' (${msg})`);
  }

  let compilerPath = wasmPath;
  const tempCompilers = [];
  let finalBytes = new Uint8Array();
  let finalExports = [];
  let firstSourceVersion = "";
  try {
    for (let hop = 1; hop <= hops; hop += 1) {
      const hopResult = await compileKernelHop(
        compilerPath,
        inputPath,
        inputSource,
        hop,
        requireSourceVersion,
      );
      finalBytes = hopResult.bytes;
      finalExports = hopResult.exportNames;
      if (firstSourceVersion.length === 0) {
        firstSourceVersion = hopResult.sourceVersion;
      } else if (hopResult.sourceVersion !== firstSourceVersion) {
        fail(
          `hop ${hop}: __clapse_contract.source_version changed across hops (${firstSourceVersion} -> ${hopResult.sourceVersion})`,
        );
      }
      if (hop < hops) {
        const nextPath = await Deno.makeTempFile({
          prefix: "clapse-native-producer-hop-",
          suffix: ".wasm",
        });
        await Deno.writeFile(nextPath, finalBytes);
        tempCompilers.push(nextPath);
        compilerPath = nextPath;
      }
    }
  } finally {
    for (const tempPath of tempCompilers) {
      try {
        await Deno.remove(tempPath);
      } catch {
        // best-effort cleanup
      }
    }
  }
  return { finalBytes, finalExports, sourceVersion: firstSourceVersion };
}

async function main() {
  const { wasmPath, inputPath, hops, requireSourceVersion } = parseArgs(
    Deno.args,
  );
  const artifactsSourceVersion = await runCompileArtifactsProbe(
    wasmPath,
    requireSourceVersion,
  );
  await runEmitWatProbe(wasmPath);
  const { finalBytes, finalExports, sourceVersion } = await runKernelHopProbe(
    wasmPath,
    inputPath,
    hops,
    requireSourceVersion,
  );
  if (artifactsSourceVersion !== sourceVersion) {
    fail(
      `compile response source_version changed between artifact probe and kernel hop probe (${artifactsSourceVersion} -> ${sourceVersion})`,
    );
  }
  console.log(
    `native-producer-raw-probe: PASS (${wasmPath}; hops=${hops}; source_version=${sourceVersion}; compile_contract_version=${EXPECTED_COMPILE_CONTRACT_VERSION}; output_bytes=${finalBytes.length}; output_exports=${
      finalExports.join(",")
    })`,
  );
}

await main();
