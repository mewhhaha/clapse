#!/usr/bin/env -S deno run -A

import { callCompilerWasm, decodeWasmBase64, inspectCompilerWasmAbi } from "./wasm-compiler-abi.mjs";

const DEFAULT_WASM_PATH = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_MIN_BYTES = 4096;
const SMOKE_INPUT_PATH = "_compiler_smoke.clapse";
const SMOKE_INPUT_SOURCE = "main x = x";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/check-browser-compiler-wasm.mjs [--wasm <path>] [--min-bytes <n>]",
    "",
    "Checks:",
    "  - wasm exists and is not tiny",
    "  - wasm exports browser runtime ABI (memory + clapse_run)",
    "  - compile smoke request succeeds",
    "  - emitted module is valid wasm and exports main",
  ].join("\n");
}

function fail(msg) {
  console.error(`browser-compiler-wasm-check: FAIL (${msg})`);
  Deno.exit(1);
}

function parseArgs(argv) {
  let wasmPath = DEFAULT_WASM_PATH;
  let minBytes = DEFAULT_MIN_BYTES;
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
    if (arg === "--min-bytes") {
      const raw = argv[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        fail(`invalid --min-bytes value: ${raw}`);
      }
      minBytes = parsed;
      i += 1;
      continue;
    }
    fail(`unknown argument: ${arg}`);
  }
  return { wasmPath, minBytes };
}

async function ensureCompilerAbi(wasmPath, minBytes) {
  let stat;
  try {
    stat = await Deno.stat(wasmPath);
  } catch {
    fail(`missing wasm file: ${wasmPath}`);
  }
  if (!stat.isFile) {
    fail(`not a file: ${wasmPath}`);
  }
  if (stat.size < minBytes) {
    fail(`wasm too small (${stat.size} bytes < ${minBytes}) at ${wasmPath}`);
  }
  let abi;
  try {
    abi = await inspectCompilerWasmAbi(wasmPath);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`invalid compiler ABI at ${wasmPath}: ${msg}`);
  }
  if (!abi || abi.ok !== true) {
    fail(`compiler ABI probe failed at ${wasmPath}`);
  }
  if (abi.mode !== "native") {
    fail(`compiler wasm must be native, got mode=${abi.mode}`);
  }
}

async function runCompileSmoke(wasmPath) {
  let response;
  try {
    response = await callCompilerWasm(wasmPath, {
      command: "compile",
      input_path: SMOKE_INPUT_PATH,
      input_source: SMOKE_INPUT_SOURCE,
      plugin_wasm_paths: [],
    });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`compile smoke request failed: ${msg}`);
  }
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    fail("compile smoke response was not an object");
  }
  if (response.ok !== true) {
    const msg = typeof response.error === "string"
      ? response.error
      : "compile smoke response returned ok=false";
    fail(msg);
  }
  if (typeof response.wasm_base64 !== "string" || response.wasm_base64.length === 0) {
    fail("compile smoke response missing non-empty wasm_base64");
  }

  let bytes;
  try {
    bytes = decodeWasmBase64(response.wasm_base64);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`compile smoke wasm_base64 decode failed: ${msg}`);
  }
  if (bytes.length < 64) {
    fail(`compile smoke output wasm too small (${bytes.length} bytes)`);
  }
  let module;
  try {
    module = await WebAssembly.compile(bytes);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`compile smoke output is not valid wasm: ${msg}`);
  }
  const exportNames = WebAssembly.Module.exports(module).map((entry) => entry.name);
  if (!exportNames.includes("main")) {
    fail("compile smoke output missing expected export: main");
  }
}

const { wasmPath, minBytes } = parseArgs(Deno.args);
await ensureCompilerAbi(wasmPath, minBytes);
await runCompileSmoke(wasmPath);
console.log(`browser-compiler-wasm-check: PASS (${wasmPath})`);
