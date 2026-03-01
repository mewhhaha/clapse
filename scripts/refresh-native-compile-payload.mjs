#!/usr/bin/env -S deno run -A

const DEFAULT_WASM = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_SOURCE_VERSION = "native-source-2026-03-01-r2";
const TARGET_FILE = "lib/compiler/native_compile.clapse";

function usage() {
  console.log(
    [
      "usage:",
      "  deno run -A scripts/refresh-native-compile-payload.mjs [--wasm <path>] [--source-version <token>]",
      "",
      "updates lib/compiler/native_compile.clapse with wasm_base64 payload and __clapse_contract.source_version",
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
  if (!sourceVersion) fail("missing --source-version <token>");
  return { wasmPath, sourceVersion };
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

function replaceBetweenMarkers(
  source,
  startMarker,
  endMarker,
  replacement,
  label,
) {
  const start = source.indexOf(startMarker);
  if (start < 0) {
    fail(`could not find ${label} start marker`);
  }
  const valueStart = start + startMarker.length;
  const end = source.indexOf(endMarker, valueStart);
  if (end < 0) {
    fail(`could not find ${label} end marker`);
  }
  return source.slice(0, valueStart) + replacement + source.slice(end);
}

async function main() {
  const opts = parseArgs(Deno.args);
  const wasmBytes = await Deno.readFile(opts.wasmPath);
  if (wasmBytes.length === 0) {
    fail(`wasm file is empty: ${opts.wasmPath}`);
  }
  const wasmBase64 = toBase64(wasmBytes);

  let source = await Deno.readTextFile(TARGET_FILE);

  source = replaceBetweenMarkers(
    source,
    'json_compile_suffix = str_to_slice "\\",\\"__clapse_contract\\":{\\"source_version\\":\\"',
    '\\",\\"compile_contract_version\\":\\"native-v1\\"}}"',
    opts.sourceVersion,
    "json_compile_suffix source_version",
  );

  source = replaceBetweenMarkers(
    source,
    'json_compile_wasm_b64 = str_to_slice "',
    '"\n\ncompile_missing_input_path_response',
    wasmBase64,
    "json_compile_wasm_b64 payload",
  );

  await Deno.writeTextFile(TARGET_FILE, source);

  console.log(
    `refresh-native-compile-payload: PASS (wasm=${opts.wasmPath}; bytes=${wasmBytes.length}; source_version=${opts.sourceVersion})`,
  );
}

await main();
