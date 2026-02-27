#!/usr/bin/env -S deno run -A

import { callCompilerWasm } from "./wasm-compiler-abi.mjs";

function resolveWasmPath() {
  const candidates = [
    Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "",
    "artifacts/latest/clapse_compiler.wasm",
    "out/clapse_compiler.wasm",
  ];
  for (const candidate of candidates) {
    if (candidate.length === 0) {
      continue;
    }
    try {
      const stat = Deno.statSync(candidate);
      if (stat.isFile && stat.size > 0) {
        return candidate;
      }
    } catch {
      // try next
    }
  }
  throw new Error(
    "record-kernel-smoke: missing compiler wasm (set CLAPSE_COMPILER_WASM_PATH or provide artifacts/latest/out compiler wasm)",
  );
}

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

async function compileSource(wasmPath, inputPath, inputSource) {
  return await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: inputPath,
    input_source: inputSource,
    plugin_wasm_paths: [],
  });
}

async function run() {
  const wasmPath = resolveWasmPath();
  const source = [
    "type Options a = { allow: bool, include: Maybe a }",
    "default_options = { allow = true, include = Nothing }",
    "flip_allow options = options { allow = not options.allow }",
    "main x = default_options.allow",
    "",
  ].join("\n");
  const response = await compileSource(wasmPath, "examples/record_kernel_smoke.clapse", source);
  assert(response && typeof response === "object", "record-kernel-smoke: invalid compile response");
  assert(response.ok === true, `record-kernel-smoke: compile failed: ${String(response?.error ?? "unknown")}`);
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    "record-kernel-smoke: compile returned empty wasm_base64",
  );
  console.log("record-kernel-smoke: PASS");
}

await run();
