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

function boolEnvFlag(name, defaultValue = false) {
  const raw = String(Deno.env.get(name) ?? "").trim().toLowerCase();
  if (raw.length === 0) {
    return defaultValue;
  }
  return raw === "1" || raw === "true" || raw === "yes" || raw === "on";
}

async function compileSource(wasmPath, inputPath, inputSource, compileMode = "") {
  const request = {
    command: "compile",
    input_path: inputPath,
    input_source: inputSource,
    plugin_wasm_paths: [],
  };
  if (compileMode.length > 0) {
    request.compile_mode = compileMode;
  }
  return await callCompilerWasm(wasmPath, request);
}

async function emitWatSource(wasmPath, inputSource, emitMode) {
  const request = {
    command: "emit-wat",
    input_path: "examples/emit_wat_source_probe.clapse",
    input_source: inputSource,
  };
  if (emitMode.length > 0) {
    request.emit_wat_mode = emitMode;
  }
  const response = await callCompilerWasm(wasmPath, request);
  assert(
    response && typeof response === "object",
    "record-kernel-smoke: invalid emit-wat response",
  );
  assert(
    response.ok === true,
    `record-kernel-smoke: emit-wat failed: ${String(response?.error ?? "unknown")}`,
  );
  assert(
    typeof response.wat === "string" && response.wat.length > 0,
    "record-kernel-smoke: emit-wat returned empty wat",
  );
  return response.wat;
}

async function run() {
  const allowTemplateFallback = boolEnvFlag(
    "CLAPSE_RECORD_KERNEL_SMOKE_ALLOW_TEMPLATE_FALLBACK",
    false,
  );
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
  const nativeResponse = await compileSource(
    wasmPath,
    "examples/record_kernel_smoke_native.clapse",
    source,
    "kernel-native",
  );
  assert(nativeResponse && typeof nativeResponse === "object", "record-kernel-smoke: invalid native compile response");
  assert(nativeResponse.ok === true, `record-kernel-smoke: native compile failed: ${String(nativeResponse?.error ?? "unknown")}`);
  assert(
    nativeResponse.backend === "kernel-native",
    `record-kernel-smoke: expected backend=kernel-native, got ${String(nativeResponse?.backend ?? "<missing>")}`,
  );
  assert(
    nativeResponse.artifacts &&
      typeof nativeResponse.artifacts === "object" &&
      typeof nativeResponse.artifacts["lowered_ir.txt"] === "string" &&
      nativeResponse.artifacts["lowered_ir.txt"].includes("main"),
    "record-kernel-smoke: native compile should provide lowered_ir.txt with source context",
  );
  const emitSource = "emit_wat_marker = 42";
  const defaultEmitWat = await emitWatSource(wasmPath, emitSource, "");
  assert(
    defaultEmitWat.includes(emitSource),
    "record-kernel-smoke: emit-wat default mode should include source payload",
  );
  const templateEmitWat = await emitWatSource(wasmPath, emitSource, "template");
  const hasTemplateShape = templateEmitWat.includes(
    "(memory (export \"__memory\") 1)",
  );
  const hasSourceFallback = templateEmitWat.includes(emitSource);
  assert(
    hasTemplateShape || (allowTemplateFallback && hasSourceFallback),
    "record-kernel-smoke: emit-wat template mode should keep base template shape",
  );
  console.log("record-kernel-smoke: PASS");
}

await run();
