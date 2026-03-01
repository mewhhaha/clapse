const MIN_STABLE_SEED_BYTES = 4096;
const EXPECTED_COMPILE_CONTRACT_VERSION = "native-v1";
const DEFAULT_SOURCE_VERSION = "wasm-bootstrap-seed-2026-03-01-r1";
const WASM_SEED_ENV = "CLAPSE_USE_WASM_BOOTSTRAP_SEED";
const LEGACY_TS_SEED_ENV = "CLAPSE_USE_TS_BOOTSTRAP_SEED";

const COMPILE_DEBUG_MODES = new Set([
  "kernel-native",
  "debug",
  "native-debug",
  "kernel-debug",
]);

function nonEmptyString(value) {
  return typeof value === "string" && value.length > 0;
}

function boolEnvFlag(raw, defaultValue) {
  if (raw === undefined) {
    return defaultValue;
  }
  const normalized = String(raw).trim().toLowerCase();
  if (normalized.length === 0) {
    return defaultValue;
  }
  if (
    normalized === "1" || normalized === "true" || normalized === "yes" ||
    normalized === "on"
  ) {
    return true;
  }
  if (
    normalized === "0" || normalized === "false" || normalized === "no" ||
    normalized === "off"
  ) {
    return false;
  }
  return defaultValue;
}

function fromBase64(raw) {
  const binary = atob(raw);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}

function toBase64(bytes) {
  const CHUNK_SIZE = 0x8000;
  let binary = "";
  for (let i = 0; i < bytes.length; i += CHUNK_SIZE) {
    const chunk = bytes.subarray(i, Math.min(bytes.length, i + CHUNK_SIZE));
    binary += String.fromCharCode(...chunk);
  }
  return btoa(binary);
}

function normalizeCompileMode(rawMode) {
  if (!nonEmptyString(rawMode)) {
    return "kernel-native";
  }
  return rawMode.trim().toLowerCase();
}

function assertCompileRequestShape(requestObject) {
  if (!requestObject || typeof requestObject !== "object") {
    throw new Error("wasm-bootstrap-seed: compile request must be an object");
  }
  if (requestObject.command !== "compile") {
    throw new Error(
      `wasm-bootstrap-seed: unsupported command '${
        String(requestObject.command ?? "")
      }' (expected compile)`,
    );
  }
  const compileMode = normalizeCompileMode(requestObject.compile_mode);
  if (!COMPILE_DEBUG_MODES.has(compileMode)) {
    throw new Error(
      `wasm-bootstrap-seed: unsupported compile_mode '${compileMode}' (expected kernel-native/debug/native-debug/kernel-debug)`,
    );
  }
  if (!nonEmptyString(requestObject.input_source)) {
    throw new Error(
      "wasm-bootstrap-seed: compile request requires non-empty input_source",
    );
  }
}

function hasRequiredCompilerAbiExports(exportNames) {
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  return hasMemory && exportNames.includes("clapse_run");
}

function assertCompilerAbiBytes(bytes) {
  if (!(bytes instanceof Uint8Array) || bytes.length < MIN_STABLE_SEED_BYTES) {
    throw new Error(
      `wasm-bootstrap-seed: seed wasm is too small (${bytes?.length ?? 0} bytes; min ${MIN_STABLE_SEED_BYTES})`,
    );
  }
  let module;
  try {
    module = new WebAssembly.Module(bytes);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    throw new Error(`wasm-bootstrap-seed: seed wasm is invalid (${msg})`);
  }
  const exportNames = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  if (!hasRequiredCompilerAbiExports(exportNames)) {
    throw new Error(
      `wasm-bootstrap-seed: seed wasm missing compiler ABI exports (required: memory/__memory + clapse_run; got ${exportNames.join(",")})`,
    );
  }
}

async function resolveSeedWasmBytes(requestObject, options) {
  if (requestObject.seed_wasm_base64 !== undefined) {
    if (!nonEmptyString(requestObject.seed_wasm_base64)) {
      throw new Error(
        "wasm-bootstrap-seed: seed_wasm_base64 must be a non-empty string when provided",
      );
    }
    return fromBase64(requestObject.seed_wasm_base64);
  }
  if (options.seedWasmBytes instanceof Uint8Array) {
    return options.seedWasmBytes;
  }
  if (nonEmptyString(options.seedWasmPath)) {
    return await Deno.readFile(options.seedWasmPath);
  }
  const fromEnv = String(Deno.env.get("CLAPSE_WASM_BOOTSTRAP_SEED_WASM_PATH") ??
    "").trim();
  if (fromEnv.length > 0) {
    return await Deno.readFile(fromEnv);
  }
  throw new Error(
    "wasm-bootstrap-seed: missing trusted seed wasm input (provide request.seed_wasm_base64, options.seedWasmPath, options.seedWasmBytes, or CLAPSE_WASM_BOOTSTRAP_SEED_WASM_PATH)",
  );
}

function compileArtifactFromSource(sourceText, label) {
  return `(${label}) ${sourceText}`;
}

export function isWasmBootstrapSeedEnabled() {
  if (Deno.env.get(WASM_SEED_ENV) !== undefined) {
    return boolEnvFlag(Deno.env.get(WASM_SEED_ENV), false);
  }
  // Backward-compatible fallback for legacy bootstrap runs.
  return boolEnvFlag(Deno.env.get(LEGACY_TS_SEED_ENV), false);
}

export async function buildWasmSeedCompileResponse(
  requestObject,
  options = {},
) {
  assertCompileRequestShape(requestObject);
  const sourceText = String(requestObject.input_source);
  const seedWasmBytes = await resolveSeedWasmBytes(requestObject, options);
  assertCompilerAbiBytes(seedWasmBytes);

  return {
    ok: true,
    backend: "kernel-native",
    wasm_base64: toBase64(seedWasmBytes),
    artifacts: {
      "lowered_ir.txt": compileArtifactFromSource(sourceText, "lowered_ir"),
      "collapsed_ir.txt": compileArtifactFromSource(
        sourceText,
        "collapsed_ir",
      ),
    },
    __clapse_contract: {
      source_version: nonEmptyString(options.sourceVersion)
        ? options.sourceVersion
        : DEFAULT_SOURCE_VERSION,
      compile_contract_version: EXPECTED_COMPILE_CONTRACT_VERSION,
    },
  };
}
