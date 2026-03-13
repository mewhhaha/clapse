#!/usr/bin/env -S deno run -A

import {
  callCompilerWasm,
  callCompilerWasmRaw,
  decodeWasmBase64,
} from "./wasm-compiler-abi.mjs";
import { getCompilerRunExport } from "./compiler-abi-compat.mjs";
import { decodeInt, instantiateWithRuntime, makeRuntime } from "./wasm-runtime.mjs";

const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAP_COMPILER_WASM_PATH") ?? "").trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/latest/clap_compiler.wasm";
}

function buildRequest() {
  return {
    command: "compile",
    compile_mode: "debug",
    input_path: "repl/input.clap",
    input_source: "identity x = x\nmain = identity 7\n",
    plugin_wasm_paths: [],
    entrypoint_exports: ["main"],
  };
}

async function callCompilerWasmDirect(path, requestObject) {
  const wasmBytes = await Deno.readFile(path);
  const module = await WebAssembly.compile(wasmBytes);
  const imports = WebAssembly.Module.imports(module);
  const hostImports = imports.filter((imp) => imp.module === "host");
  if (hostImports.length > 0) {
    throw new Error(
      `bridge compiler wasm detected (${hostImports.map((imp) => imp.name).join(", ")})`,
    );
  }
  const runtime = makeRuntime();
  const instance = await WebAssembly.instantiate(module, {});
  const memoryExport = instance.exports.__memory ?? instance.exports.memory;
  assert(memoryExport instanceof WebAssembly.Memory, "compiler wasm missing memory export");
  const run = getCompilerRunExport(instance);
  runtime.state.memory = memoryExport;
  const heapGlobal = instance.exports.__heap_ptr;
  if (heapGlobal instanceof WebAssembly.Global) {
    runtime.state.heapGlobal = heapGlobal;
  }
  const requestBytes = UTF8_ENCODER.encode(JSON.stringify(requestObject));
  const requestHandle = runtime.alloc_slice_u8(requestBytes);
  const responseHandle = run(requestHandle);
  assert(
    Number.isInteger(responseHandle) && (responseHandle & 1) === 0,
    `compiler wasm returned invalid response handle: ${responseHandle}`,
  );
  const responseBytes = runtime.read_slice_u8_copy(responseHandle);
  return JSON.parse(UTF8_DECODER.decode(responseBytes));
}

function assertPublicExportsMainOnly(response, label) {
  const publicExports = Array.isArray(response.public_exports)
    ? response.public_exports.map((entry) => ({
      name: entry?.name,
      arity: entry?.arity,
    }))
    : [];
  assert(
    publicExports.length === 1 &&
      publicExports[0]?.name === "main" &&
      publicExports[0]?.arity === 0,
    `${label}: expected public_exports=[{name:\"main\",arity:0}], got ${JSON.stringify(publicExports)}`,
  );
}

function assertAbiExportsEmpty(response, label) {
  const abiExports = Array.isArray(response?.abi_exports) ? response.abi_exports : [];
  assert(
    abiExports.length === 0,
    `${label}: expected abi_exports=[], got ${JSON.stringify(abiExports)}`,
  );
}

async function assertMainEvaluatesTo(bytes, expected, label) {
  const { instance } = await instantiateWithRuntime(bytes);
  const mainFn = instance.exports.main;
  assert(typeof mainFn === "function", `${label}: missing main export`);
  const result = mainFn();
  assert(
    decodeInt(result | 0) === expected,
    `${label}: expected tagged-int:${expected}, got raw ${String(result)}`,
  );
}

async function run() {
  const wasmPath = resolveCompilerWasmPath();
  const request = buildRequest();

  const direct = await callCompilerWasmDirect(wasmPath, request);
  assert(direct?.ok === true, "direct raw compile should succeed");
  assert(
    typeof direct?.wasm_base64 === "string" && direct.wasm_base64.length > 0,
    `direct raw compile missing wasm_base64: ${JSON.stringify(direct)}`,
  );
  const directBytes = decodeWasmBase64(direct.wasm_base64);
  assert(directBytes.length > 0, "direct raw compile produced empty wasm");
  await assertMainEvaluatesTo(directBytes, 7, "direct raw compile");
  assertPublicExportsMainOnly(direct, "direct raw compile");
  assertAbiExportsEmpty(direct, "direct raw compile");

  const validated = await callCompilerWasm(wasmPath, request, {
    withContractMetadata: true,
  });
  assert(validated?.ok === true, "validated compile should succeed");
  const validatedBytes = decodeWasmBase64(validated.wasm_base64);
  assert(validatedBytes.length > 0, "validated compile produced empty wasm");
  await assertMainEvaluatesTo(validatedBytes, 7, "validated compile");
  assertPublicExportsMainOnly(validated, "validated compile");
  assertAbiExportsEmpty(validated, "validated compile");
  assert(
    validated?.compile_strategy === "compiler_raw",
    `validated compile expected compiler_raw, got ${String(validated?.compile_strategy)}`,
  );
  assert(
    validated?.compatibility_used === false,
    `validated compile expected compatibility_used=false, got ${JSON.stringify(validated?.compatibility_used)}`,
  );

  const rawWrapped = await callCompilerWasmRaw(wasmPath, request, {
    validateCompileContract: true,
    withContractMetadata: true,
  });
  assert(rawWrapped?.ok === true, "validated raw compile should succeed");
  const rawWrappedBytes = decodeWasmBase64(rawWrapped.wasm_base64);
  assert(rawWrappedBytes.length > 0, "validated raw compile produced empty wasm");
  await assertMainEvaluatesTo(rawWrappedBytes, 7, "validated raw compile");
  assertPublicExportsMainOnly(rawWrapped, "validated raw compile");
  assertAbiExportsEmpty(rawWrapped, "validated raw compile");
  assert(
    rawWrapped?.compile_strategy === "compiler_raw",
    `validated raw compile expected compiler_raw, got ${String(rawWrapped?.compile_strategy)}`,
  );
  assert(
    rawWrapped?.compatibility_used === false,
    `validated raw compile expected compatibility_used=false, got ${JSON.stringify(rawWrapped?.compatibility_used)}`,
  );

  console.log(
    `native-raw-boundary-synthesis-gate: PASS (${wasmPath}; direct_bytes=${directBytes.length}; strategy=compiler_raw)`,
  );
}

await run();
