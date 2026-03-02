#!/usr/bin/env -S deno run -A

const DEFAULT_WASM = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_SOURCE_VERSION = "";
const TARGET_DIR = "lib/compiler";
const TARGET_PREFIX = "native_compile";
const TARGET_SUFFIX = ".clapse";
const JSON_COMPILE_SUFFIX_PREFIX = 'json_compile_suffix = str_to_slice "';
const JSON_COMPILE_WASM_B64_PREFIX = 'json_compile_wasm_b64 = str_to_slice "';
const SOURCE_VERSION_MARKER = '\\"source_version\\":\\"';
const SOURCE_VERSION_END_MARKER =
  '\\",\\"compile_contract_version\\":\\"native-v1\\"}}';

function usage() {
  console.log(
    [
      "usage:",
      "  deno run -A scripts/refresh-native-compile-payload.mjs [--wasm <path>] [--source-version <token>]",
      "",
      "updates lib/compiler/native_compile*.clapse files with wasm_base64 payload and __clapse_contract.source_version",
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

function extractSourceVersionFromText(sourceText) {
  const escapedMatch = sourceText.match(
    /source_version\\":\\"([^"\\]+)\\",\\"compile_contract_version/u,
  );
  if (
    Array.isArray(escapedMatch) &&
    escapedMatch.length > 1 &&
    escapedMatch[1].length > 0
  ) {
    return escapedMatch[1];
  }
  const plainMatch = sourceText.match(
    /"source_version":"([^"]+)","compile_contract_version"/u,
  );
  if (
    Array.isArray(plainMatch) &&
    plainMatch.length > 1 &&
    plainMatch[1].length > 0
  ) {
    return plainMatch[1];
  }
  return "";
}

async function resolveDefaultSourceVersion() {
  let sourceVersion = "";
  for (const path of resolveTargetFiles()) {
    if (!path.includes(TARGET_SUFFIX)) {
      continue;
    }
    const sourceText = await Deno.readTextFile(path);
    const fromSource = extractSourceVersionFromText(sourceText);
    if (fromSource.length === 0) {
      continue;
    }
    if (sourceVersion.length > 0 && sourceVersion !== fromSource) {
      fail("mismatched source_version markers across native_compile*.clapse");
    }
    sourceVersion = fromSource;
  }
  return sourceVersion;
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
  const wasmRewrite = rewriteLiteralValue(
    source,
    JSON_COMPILE_WASM_B64_PREFIX,
    () => wasmBase64,
    path,
    "json_compile_wasm_b64",
  );
  source = wasmRewrite.source;
  const updatedSuffix = suffixRewrite.updated;
  const updatedWasm = wasmRewrite.updated;
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
    sourceVersion = await resolveDefaultSourceVersion();
    if (!sourceVersion) {
      fail(
        "missing --source-version and no resolvable marker in target source files",
      );
    }
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
