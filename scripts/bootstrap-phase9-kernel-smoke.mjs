#!/usr/bin/env -S deno run -A

import { callCompilerWasm } from "./wasm-compiler-abi.mjs";

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

async function main() {
  const wasmPath = Deno.args[0] ?? "out/clapse_compiler.wasm";

  const compileResp = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: "examples/wasm_main.clapse",
    input_source: "main x = x",
  });
  assert(compileResp && compileResp.ok === true, "inline compile response must succeed");
  assert(
    typeof compileResp.wasm_base64 === "string" &&
      compileResp.wasm_base64.length > 0,
    "inline compile response must include non-empty wasm_base64",
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
    "compile response with legacy source field must fail",
  );
  assert(
    typeof compileSourceFallbackResp.error === "string" &&
      compileSourceFallbackResp.error.includes("missing input_source"),
    "compile response with legacy source field must report missing input_source",
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
  console.log("bootstrap phase9 kernel smoke: PASS");
}

await main();
