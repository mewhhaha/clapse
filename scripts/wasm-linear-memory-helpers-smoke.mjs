#!/usr/bin/env node

import { cliArgs, failWithError, readBinaryFile } from "./runtime-env.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

function assertEqual(label, expected, actual) {
  if (expected !== actual) {
    throw new Error(`${label}: expected ${expected}, got ${actual}`);
  }
}

async function main() {
  const [wasmPathArg] = cliArgs();
  const wasmPath = wasmPathArg ?? "out/wasm_linear_memory_helpers.wasm";
  const wasmBytes = await readBinaryFile(wasmPath);
  const { instance, runtime } = await instantiateWithRuntime(wasmBytes);
  const fn = instance.exports.main;
  if (typeof fn !== "function") {
    throw new Error("expected exported function: main");
  }

  const input = runtime.alloc_slice_u8(Uint8Array.from([7, 9, 11]));
  const out0 = decodeInt(fn(input, encodeInt(0)));
  const out1 = decodeInt(fn(input, encodeInt(1)));

  assertEqual("idx 0", 0, out0);
  assertEqual("idx 1", 9, out1);
  console.log("wasm linear memory helpers smoke: PASS");
}

main().catch(failWithError);
