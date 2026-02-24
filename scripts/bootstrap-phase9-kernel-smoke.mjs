#!/usr/bin/env -S deno run -A

import { callCompilerWasm } from "./wasm-compiler-abi.mjs";
import { makeRuntime } from "./wasm-runtime.mjs";

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

async function callCompilerWasmRawRequest(wasmPath, requestText) {
  const wasmBytes = await Deno.readFile(wasmPath);
  const wasm = await WebAssembly.instantiate(wasmBytes);
  const instance = wasm.instance;
  const run = instance.exports.clapse_run;
  const memory = instance.exports.__memory ?? instance.exports.memory;
  assert(typeof run === "function", "wasm must export clapse_run");
  assert(memory instanceof WebAssembly.Memory, "wasm must export memory");

  const runtime = makeRuntime();
  runtime.state.memory = memory;
  const requestBytes = new TextEncoder().encode(requestText);
  const requestHandle = runtime.alloc_slice_u8(requestBytes);
  const responseHandle = run(requestHandle);
  const responseBytes = runtime.read_slice_u8_copy(responseHandle);
  const responseText = new TextDecoder().decode(responseBytes);
  return JSON.parse(responseText);
}

async function main() {
  const wasmPath = Deno.args[0] ?? "out/clapse_compiler.wasm";

  const compileResp = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: "examples/wasm_main.clapse",
    input_source: "main x = x",
  });
  assert(compileResp && compileResp.ok === false, "compile response must fail");
  assert(
    typeof compileResp.error === "string" &&
      compileResp.error.includes("not implemented yet"),
    "compile response must report not implemented yet",
  );

  const compileMissingPathResp = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_source: "main x = x",
  });
  assert(
    compileMissingPathResp && compileMissingPathResp.ok === false,
    "compile response without input_path must fail",
  );
  assert(
    typeof compileMissingPathResp.error === "string" &&
      compileMissingPathResp.error.includes("missing input_path"),
    "compile response without input_path must report missing input_path",
  );

  const compileEmptyPathResp = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: "",
    input_source: "main x = x",
  });
  assert(
    compileEmptyPathResp && compileEmptyPathResp.ok === false,
    "compile response with empty input_path must fail",
  );
  assert(
    typeof compileEmptyPathResp.error === "string" &&
      compileEmptyPathResp.error.includes("missing input_path"),
    "compile response with empty input_path must report missing input_path",
  );

  const compileMissingSourceResp = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: "examples/wasm_main.clapse",
  });
  assert(
    compileMissingSourceResp && compileMissingSourceResp.ok === false,
    "compile response without input_source must fail",
  );
  assert(
    typeof compileMissingSourceResp.error === "string" &&
      compileMissingSourceResp.error.includes("missing input_source"),
    "compile response without input_source must report missing input_source",
  );

  const compileSourceFallbackResp = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: "examples/wasm_main.clapse",
    source: "main x = x",
  });
  assert(
    compileSourceFallbackResp && compileSourceFallbackResp.ok === false,
    "compile response with source fallback must fail (not implemented yet)",
  );
  assert(
    typeof compileSourceFallbackResp.error === "string" &&
      compileSourceFallbackResp.error.includes("not implemented yet"),
    "compile response with source fallback must route to compile-not-implemented path",
  );

  const compileStubSuccessResp = await callCompilerWasm(wasmPath, {
    command: "compile",
    compile_mode: "stub-success",
    input_path: "examples/wasm_main.clapse",
    input_source: "main x = x",
  });
  assert(
    compileStubSuccessResp && compileStubSuccessResp.ok === true,
    "compile stub-success response must succeed",
  );
  assert(
    typeof compileStubSuccessResp.wasm_base64 === "string" &&
      compileStubSuccessResp.wasm_base64.length > 0,
    "compile stub-success response must include non-empty wasm_base64",
  );
  {
    const wasmBytes = Uint8Array.from(atob(compileStubSuccessResp.wasm_base64), (c) =>
      c.charCodeAt(0)
    );
    assert(
      wasmBytes.length >= 4 &&
        wasmBytes[0] === 0x00 &&
        wasmBytes[1] === 0x61 &&
        wasmBytes[2] === 0x73 &&
        wasmBytes[3] === 0x6d,
      "compile stub-success wasm_base64 must decode to wasm magic bytes",
    );
  }
  assert(
    Array.isArray(compileStubSuccessResp.exports),
    "compile stub-success response must include exports array",
  );
  for (const [idx, ex] of compileStubSuccessResp.exports.entries()) {
    assert(
      ex && typeof ex === "object" && !Array.isArray(ex),
      `compile stub-success exports[${idx}] must be object`,
    );
    assert(
      typeof ex.name === "string" && ex.name.length > 0,
      `compile stub-success exports[${idx}].name must be non-empty string`,
    );
    assert(
      Number.isInteger(ex.arity) && ex.arity >= 0,
      `compile stub-success exports[${idx}].arity must be non-negative integer`,
    );
  }
  if ("dts" in compileStubSuccessResp) {
    assert(
      typeof compileStubSuccessResp.dts === "string",
      "compile stub-success response dts must be string when present",
    );
  }

  const formatResp = await callCompilerWasm(wasmPath, {
    command: "format",
    mode: "stdout",
    input_path: "examples/wasm_main.clapse",
    source: "main x = x\n",
  });
  assert(formatResp && formatResp.ok === true, "format response must succeed");
  assert(
    typeof formatResp.formatted === "string",
    "format response must include formatted string",
  );
  assert(
    formatResp.formatted === "main x = x\n",
    "format response must preserve source payload",
  );

  // Regression guard: command routing must not depend on command key byte offset.
  const formatRespReordered = await callCompilerWasm(wasmPath, {
    mode: "stdout",
    input_path: "examples/wasm_main.clapse",
    source: "main x = x\n",
    command: "format",
  });
  assert(
    formatRespReordered && formatRespReordered.ok === true,
    "format response must succeed with reordered request keys",
  );
  assert(
    formatRespReordered.formatted === "main x = x\n",
    "format response must preserve source payload with reordered request keys",
  );

  const escapedSource = "main x = x\nlabel x = \"a\\\\\\\"b\"\n";
  const formatRespEscaped = await callCompilerWasm(wasmPath, {
    command: "format",
    mode: "stdout",
    input_path: "examples/wasm_main.clapse",
    source: escapedSource,
  });
  assert(
    formatRespEscaped && formatRespEscaped.ok === true,
    "format response must succeed for escaped source payload",
  );
  assert(
    formatRespEscaped.formatted === escapedSource,
    "format response must preserve escaped source payload",
  );

  const formatRespPretty = await callCompilerWasmRawRequest(
    wasmPath,
    '{\n  "source" : "main x = x\\n",\n  "mode" : "stdout",\n  "command" : "format"\n}\n',
  );
  assert(
    formatRespPretty && formatRespPretty.ok === true,
    "format response must succeed with spaced pretty JSON request",
  );
  assert(
    formatRespPretty.formatted === "main x = x\n",
    "format response must preserve payload with spaced pretty JSON request",
  );

  const compileMalformedSourceResp = await callCompilerWasmRawRequest(
    wasmPath,
    '{"command":"compile","input_path":"examples/wasm_main.clapse","input_source":"main x = x',
  );
  assert(
    compileMalformedSourceResp && compileMalformedSourceResp.ok === false,
    "compile response with malformed unterminated input_source must fail",
  );
  assert(
    typeof compileMalformedSourceResp.error === "string" &&
      compileMalformedSourceResp.error.includes("missing input_source"),
    "compile malformed input_source must report missing input_source",
  );

  const compileMalformedPathResp = await callCompilerWasmRawRequest(
    wasmPath,
    '{"command":"compile","input_path":"examples/wasm_main.clapse,"input_source":"main x = x"}',
  );
  assert(
    compileMalformedPathResp && compileMalformedPathResp.ok === false,
    "compile response with malformed unterminated input_path must fail",
  );
  assert(
    typeof compileMalformedPathResp.error === "string" &&
      (compileMalformedPathResp.error.includes("missing input_path") ||
        compileMalformedPathResp.error.includes("not implemented yet")),
    "compile malformed input_path must currently either report missing input_path or route to compile-not-implemented (scanner limitation)",
  );

  const unknownSamePrefixResp = await callCompilerWasm(wasmPath, {
    command: "compress",
    input_path: "examples/wasm_main.clapse",
    input_source: "main x = x",
  });
  assert(
    unknownSamePrefixResp &&
      unknownSamePrefixResp.ok === false &&
      typeof unknownSamePrefixResp.error === "string" &&
      unknownSamePrefixResp.error.includes("unsupported command"),
    "commands sharing compile prefix must not misroute to compile handler",
  );

  const selfhostResp = await callCompilerWasm(wasmPath, {
    command: "selfhost-artifacts",
    input_path: "examples/wasm_main.clapse",
    input_source: "main x = x",
  });
  assert(
    selfhostResp && selfhostResp.ok === true,
    "selfhost-artifacts response must succeed",
  );
  assert(
    selfhostResp.artifacts && typeof selfhostResp.artifacts === "object",
    "selfhost-artifacts response must include artifacts object",
  );
  for (
    const key of [
      "merged_module.txt",
      "type_info.txt",
      "type_info_error.txt",
      "lowered_ir.txt",
      "collapsed_ir.txt",
      "exports.txt",
      "wasm_stats.txt",
    ]
  ) {
    assert(
      typeof selfhostResp.artifacts[key] === "string",
      `selfhost-artifacts must include string key: ${key}`,
    );
  }
  assert(
    selfhostResp.artifacts["merged_module.txt"] === "main x = x",
    "selfhost-artifacts merged_module.txt must mirror input_source payload",
  );

  console.log("bootstrap phase9 kernel smoke: PASS");
}

await main();
