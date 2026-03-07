#!/usr/bin/env -S deno run -A

import { callCompilerWasm, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";

const DEFAULT_WASM = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_SOURCE_VERSION = "";
const TARGET_DIR = "lib/compiler";
const TARGET_PREFIX = "native_compile";
const TARGET_SUFFIX = ".clapse";
const JSON_COMPILE_SUFFIX_PREFIX = 'json_compile_suffix = str_to_slice "';
const JSON_COMPILE_WASM_B64_PREFIXES = [
  'json_compile_wasm_b64 = str_to_slice "',
  'json_compile_wasm_b64_default = str_to_slice "',
];
const SOURCE_VERSION_MARKER = '\\"source_version\\":\\"';
const SOURCE_VERSION_END_MARKER =
  '\\",\\"compile_contract_version\\":\\"native-v1\\"}}';
const EXPECTED_COMPILE_CONTRACT_VERSION = "native-v1";
const KNOWN_STUB_WASM_BYTES = 122;
const MIN_VALID_WASM_BYTES = 4096;
const PRODUCER_CONTRACT_KEYS = new Set([
  "source_version",
  "compile_contract_version",
]);

function usage() {
  console.log(
    [
      "usage:",
      "  deno run -A scripts/refresh-native-compile-payload.mjs [--wasm <path>] [--source-version <token>]",
      "",
      "updates lib/compiler/native_compile*.clapse files with wasm_base64 payload and __clapse_contract.source_version",
      "if --source-version is omitted, it is read from a live kernel-native compile probe",
    ].join("\n"),
  );
}

function fail(message) {
  console.error(`refresh-native-compile-payload: FAIL (${message})`);
  Deno.exit(1);
}

function parseArgs(argv) {
  let wasmPath = DEFAULT_WASM;
  let sourceVersion = DEFAULT_SOURCE_VERSION;
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--help" || arg === "-h") {
      usage();
      Deno.exit(0);
    }
    if (arg === "--wasm") {
      wasmPath = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    if (arg === "--source-version") {
      sourceVersion = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    fail(`unknown argument: ${arg}`);
  }
  if (!wasmPath) fail("missing --wasm <path>");
  return { wasmPath, sourceVersion };
}

function nonEmptyString(value) {
  return typeof value === "string" && value.length > 0;
}

function contractMeta(response) {
  const raw = response?.__clapse_contract;
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    return {};
  }
  return raw;
}

function preferredPublicExports(response) {
  const publicExports = Array.isArray(response?.public_exports)
    ? response.public_exports
    : null;
  return publicExports !== null ? publicExports : [];
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

function hasSyntheticArtifactMarkers(value) {
  if (typeof value !== "string") {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

function isKnownStubCompileArtifact(wasmBytes, response) {
  if (!(wasmBytes instanceof Uint8Array) || wasmBytes.length !== KNOWN_STUB_WASM_BYTES) {
    return false;
  }
  const exportsList = preferredPublicExports(response);
  const dts = typeof response?.dts === "string" ? response.dts.trim() : "";
  return exportsList.length === 0 && (dts.length === 0 || dts === "export {}");
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

async function probeCompilerContract(compilerWasmPath, sourceVersion) {
  const targetFiles = resolveTargetFiles();
  if (targetFiles.length === 0) {
    fail(
      `no files matched ${TARGET_PREFIX}*${TARGET_SUFFIX} under ${TARGET_DIR}`,
    );
  }
  const probePath = targetFiles[0];
  const sourceText = await Deno.readTextFile(probePath);
  const sourceProbe = [
    sourceText,
    "",
    `-- native-compile-payload-probe-${crypto.randomUUID()}`,
  ].join("\n");
  let response;
  try {
    response = await callCompilerWasm(compilerWasmPath, {
      command: "compile",
      compile_mode: "kernel-native",
      input_path: probePath,
      input_source: sourceProbe,
      plugin_wasm_paths: [],
    }, {
      withContractMetadata: true,
    });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`kernel-native compile probe failed (${msg})`);
  }
  assert(
    response && typeof response === "object" && !Array.isArray(response),
    "compile probe did not return an object",
  );
  assert(response.ok === true, `compile probe failed: ${String(response.error ?? "unknown")}`);
  assert(
    response.backend === "kernel-native",
    `compile probe backend must be kernel-native, got ${String(response.backend ?? "<missing>")}`,
  );
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    "compile probe missing non-empty wasm_base64",
  );
  let wasmBytes;
  try {
    wasmBytes = decodeWasmBase64(response.wasm_base64);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`compile probe wasm_base64 decode failed (${msg})`);
  }
  assert(
    wasmBytes.length >= MIN_VALID_WASM_BYTES,
    `compile probe emitted wasm payload is suspiciously small (${wasmBytes.length} bytes)`,
  );
  assert(
    !isKnownStubCompileArtifact(wasmBytes, response),
    "compile probe returned known stub payload; refusing to refresh native payload",
  );
  const artifacts = response.artifacts;
  assert(artifacts && typeof artifacts === "object", "compile probe missing artifacts");
  const fallbackKeys = boundaryFallbackContractKeys(response);
  assert(
    fallbackKeys.length === 0,
    `compile probe includes fallback contract marker(s): ${fallbackKeys.join(",")}`,
  );
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  assert(
    typeof lowered === "string" && lowered.length > 0,
    "compile probe missing lowered_ir.txt",
  );
  assert(
    typeof collapsed === "string" && collapsed.length > 0,
    "compile probe missing collapsed_ir.txt",
  );
  assertStructuralArtifacts(lowered, collapsed, {
    context: "compile probe",
    requiredDefs: [],
  });
  const contract = contractMeta(response);
  assert(
    typeof contract.source_version === "string" && contract.source_version.length > 0,
    "compile probe missing __clapse_contract.source_version",
  );
  assert(
    contract.compile_contract_version === EXPECTED_COMPILE_CONTRACT_VERSION,
    `compile probe compile_contract_version must be ${EXPECTED_COMPILE_CONTRACT_VERSION} (got ${
      String(contract.compile_contract_version ?? "<missing>")
    })`,
  );
  if (sourceVersion.length > 0) {
    assert(
      sourceVersion === contract.source_version,
      `compile probe source_version mismatch: requested ${sourceVersion}, observed ${contract.source_version}`,
    );
  }
  return contract.source_version;
}

function toBase64(bytes) {
  const chunkSize = 0x8000;
  let binary = "";
  for (let i = 0; i < bytes.length; i += chunkSize) {
    const chunk = bytes.subarray(i, i + chunkSize);
    binary += String.fromCharCode(...chunk);
  }
  return btoa(binary);
}

function resolveTargetFiles() {
  const files = [];
  try {
    for (const entry of Deno.readDirSync(TARGET_DIR)) {
      if (
        entry.isFile &&
        entry.name.startsWith(TARGET_PREFIX) &&
        entry.name.endsWith(TARGET_SUFFIX)
      ) {
        files.push(`${TARGET_DIR}/${entry.name}`);
      }
    }
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`failed reading ${TARGET_DIR}: ${msg}`);
  }
  if (files.length === 0) {
    fail(
      `no files matched ${TARGET_PREFIX}*${TARGET_SUFFIX} under ${TARGET_DIR}`,
    );
  }
  files.sort();
  return files;
}

function findLinePrefixIndex(source, prefix) {
  if (source.startsWith(prefix)) {
    return 0;
  }
  const withNewline = `\n${prefix}`;
  const at = source.indexOf(withNewline);
  if (at < 0) {
    return -1;
  }
  return at + 1;
}

function findLiteralEnd(source, startIndex) {
  let escaped = false;
  for (let index = startIndex; index < source.length; index += 1) {
    const ch = source[index];
    if (escaped) {
      escaped = false;
      continue;
    }
    if (ch === "\\") {
      escaped = true;
      continue;
    }
    if (ch === '"') {
      return index;
    }
  }
  return -1;
}

function rewriteLiteralValue(source, prefix, rewrite, path, label) {
  const prefixIndex = findLinePrefixIndex(source, prefix);
  if (prefixIndex < 0) {
    return { source, updated: false };
  }
  const valueStart = prefixIndex + prefix.length;
  const valueEnd = findLiteralEnd(source, valueStart);
  if (valueEnd < 0) {
    fail(`could not find ${path}: unterminated ${label} literal`);
  }
  const literalValue = source.slice(valueStart, valueEnd);
  const rewrittenValue = rewrite(literalValue);
  return {
    source: source.slice(0, valueStart) + rewrittenValue +
      source.slice(valueEnd),
    updated: true,
  };
}

async function updateFile(path, wasmBase64, sourceVersion) {
  let source = await Deno.readTextFile(path);
  const suffixRewrite = rewriteLiteralValue(
    source,
    JSON_COMPILE_SUFFIX_PREFIX,
    (literalValue) => {
      const markerAt = literalValue.indexOf(SOURCE_VERSION_MARKER);
      if (markerAt < 0) {
        return literalValue;
      }
      const valueStart = markerAt + SOURCE_VERSION_MARKER.length;
      const markerEnd = literalValue.indexOf(
        SOURCE_VERSION_END_MARKER,
        valueStart,
      );
      if (markerEnd < 0) {
        fail(
          `could not find ${path}: json_compile_suffix compile_contract_version marker`,
        );
      }
      return literalValue.slice(0, valueStart) +
        sourceVersion +
        literalValue.slice(markerEnd);
    },
    path,
    "json_compile_suffix",
  );
  source = suffixRewrite.source;
  let sourceWithWasm = source;
  let updatedWasm = false;
  for (const prefix of JSON_COMPILE_WASM_B64_PREFIXES) {
    const rewrite = rewriteLiteralValue(
      sourceWithWasm,
      prefix,
      () => wasmBase64,
      path,
      "json_compile_wasm_b64",
    );
    sourceWithWasm = rewrite.source;
    if (rewrite.updated) {
      updatedWasm = true;
      break;
    }
  }
  source = sourceWithWasm;
  const updatedSuffix = suffixRewrite.updated;
  if (!updatedSuffix || !updatedWasm) {
    console.log(
      `refresh-native-compile-payload: SKIP ${path} (missing payload markers)`,
    );
    return false;
  }
  await Deno.writeTextFile(path, source);
  console.log(`refresh-native-compile-payload: UPDATED ${path}`);
  return true;
}

async function main() {
  const opts = parseArgs(Deno.args);
  const wasmBytes = await Deno.readFile(opts.wasmPath);
  if (wasmBytes.length === 0) {
    fail(`wasm file is empty: ${opts.wasmPath}`);
  }
  let sourceVersion = opts.sourceVersion;
  if (!nonEmptyString(sourceVersion)) {
    sourceVersion = await probeCompilerContract(opts.wasmPath, "");
  } else {
    await probeCompilerContract(opts.wasmPath, sourceVersion);
  }
  const wasmBase64 = toBase64(wasmBytes);

  let updatedCount = 0;
  for (const file of resolveTargetFiles()) {
    if (await updateFile(file, wasmBase64, sourceVersion)) {
      updatedCount += 1;
    }
  }
  if (updatedCount === 0) {
    fail(
      `no native payload markers found in ${TARGET_PREFIX}*${TARGET_SUFFIX} under ${TARGET_DIR}`,
    );
  }

  console.log(
    `refresh-native-compile-payload: PASS (wasm=${opts.wasmPath}; bytes=${wasmBytes.length}; source_version=${sourceVersion}; files=${updatedCount})`,
  );
}

await main();
