#!/usr/bin/env -S deno run -A

import { callCompilerWasm, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";

const DEFAULT_COMPILER_WASM_PATH = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_INPUT_PATH = "lib/compiler/kernel.clapse";
const MIN_OUTPUT_BYTES = 4096;
const DEFAULT_HOPS = 1;

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/native-selfhost-probe.mjs [--wasm <path>] [--input <path>] [--hops <n>]",
    "",
    "Checks:",
    "  - compiler wasm can compile kernel source in kernel-native mode",
    "  - compile response is ok with backend=kernel-native",
    "  - emitted wasm is non-trivial and exports compiler ABI (memory + clapse_run)",
    "  - repeated for N hops when --hops is set (default 1)",
  ].join("\n");
}

function fail(msg) {
  console.error(`native-selfhost-probe: FAIL (${msg})`);
  Deno.exit(1);
}

function stageHintFromResponse(response) {
  const artifacts = response?.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    return "";
  }
  for (const key of ["lowered_ir.txt", "collapsed_ir.txt"]) {
    const value = artifacts[key];
    if (typeof value !== "string" || value.length === 0) {
      continue;
    }
    const match = value.match(/seed-stage[0-9]+:[^)\s"]+/u);
    if (match && typeof match[0] === "string" && match[0].length > 0) {
      return match[0];
    }
  }
  return "";
}

function formatWithStage(msg, stageHint) {
  if (typeof stageHint !== "string" || stageHint.length === 0) {
    return msg;
  }
  return `${msg} [${stageHint}]`;
}

function parseArgs(argv) {
  let wasmPath = DEFAULT_COMPILER_WASM_PATH;
  let inputPath = DEFAULT_INPUT_PATH;
  let hops = DEFAULT_HOPS;
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
    fail(`unknown argument: ${arg}`);
  }
  return { wasmPath, inputPath, hops };
}

function assertCompilerLikeOutput(bytes, hopIndex, stageHint) {
  if (bytes.length < MIN_OUTPUT_BYTES) {
    fail(
      formatWithStage(
        `hop ${hopIndex}: compiled kernel artifact is too small (${bytes.length} < ${MIN_OUTPUT_BYTES})`,
        stageHint,
      ),
    );
  }
  let module;
  try {
    module = new WebAssembly.Module(bytes);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(
      formatWithStage(
        `hop ${hopIndex}: compiled kernel artifact is not valid wasm: ${msg}`,
        stageHint,
      ),
    );
  }
  const exportNames = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  if (!hasMemory) {
    fail(
      formatWithStage(
        `hop ${hopIndex}: compiled kernel artifact missing memory export (exports: ${
          exportNames.join(", ")
        })`,
        stageHint,
      ),
    );
  }
  if (!exportNames.includes("clapse_run")) {
    fail(
      formatWithStage(
        `hop ${hopIndex}: compiled kernel artifact missing clapse_run export (exports: ${
          exportNames.join(", ")
        })`,
        stageHint,
      ),
    );
  }
}

async function compileKernel(wasmPath, inputPath, inputSource, hopIndex) {
  let response;
  try {
    response = await callCompilerWasm(wasmPath, {
      command: "compile",
      compile_mode: "kernel-native",
      input_path: inputPath,
      input_source: inputSource,
      plugin_wasm_paths: [],
    });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`hop ${hopIndex}: compile request failed: ${msg}`);
  }

  if (!response || typeof response !== "object" || Array.isArray(response)) {
    fail(`hop ${hopIndex}: compile response was not an object`);
  }
  if (response.ok !== true) {
    const msg = typeof response.error === "string"
      ? response.error
      : "compile response returned ok=false";
    fail(`hop ${hopIndex}: ${msg}`);
  }
  if (response.backend !== "kernel-native") {
    fail(
      `hop ${hopIndex}: compile response backend must be kernel-native, got ${
        String(response.backend ?? "<missing>")
      }`,
    );
  }
  if (
    typeof response.wasm_base64 !== "string" ||
    response.wasm_base64.length === 0
  ) {
    fail(`hop ${hopIndex}: compile response missing non-empty wasm_base64`);
  }
  const stageHint = stageHintFromResponse(response);

  let bytes;
  try {
    bytes = decodeWasmBase64(response.wasm_base64);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(
      formatWithStage(
        `hop ${hopIndex}: failed to decode compile response wasm_base64: ${msg}`,
        stageHint,
      ),
    );
  }
  assertCompilerLikeOutput(bytes, hopIndex, stageHint);
  return { bytes, stageHint };
}

async function runProbe(wasmPath, inputPath, hops) {
  let inputSource = "";
  try {
    inputSource = await Deno.readTextFile(inputPath);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`failed to read input source '${inputPath}': ${msg}`);
  }

  const tempCompilers = [];
  let compilerPath = wasmPath;
  let finalBytes = new Uint8Array();
  let finalStageHint = "";
  for (let hop = 1; hop <= hops; hop += 1) {
    const hopResult = await compileKernel(
      compilerPath,
      inputPath,
      inputSource,
      hop,
    );
    finalBytes = hopResult.bytes;
    finalStageHint = hopResult.stageHint;
    if (hop < hops) {
      const nextPath = await Deno.makeTempFile({
        prefix: "clapse-native-selfhost-hop-",
        suffix: ".wasm",
      });
      await Deno.writeFile(nextPath, finalBytes);
      tempCompilers.push(nextPath);
      compilerPath = nextPath;
    }
  }
  for (const tempPath of tempCompilers) {
    try {
      await Deno.remove(tempPath);
    } catch {
      // best-effort cleanup
    }
  }
  console.log(
    `native-selfhost-probe: PASS (${wasmPath} -> ${inputPath}; hops=${hops}; output_bytes=${finalBytes.length}; final_stage=${
      finalStageHint || "n/a"
    })`,
  );
}

const { wasmPath, inputPath, hops } = parseArgs(Deno.args);
await runProbe(wasmPath, inputPath, hops);
