#!/usr/bin/env node

import { cliArgs, failWithError, readBinaryFile } from "./runtime-env.mjs";
import { decodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function callMainTaggedInt(instance, bytes) {
  const fn = instance.exports.main;
  if (typeof fn !== "function") {
    throw new Error("main export missing in wasm");
  }
  return decodeInt(fn(bytes));
}

async function main() {
  const [wasmPathArg] = cliArgs();
  const wasmPath = wasmPathArg ?? "out/bootstrap_phase4_parser_pilot.wasm";
  const wasmBytes = await readBinaryFile(wasmPath);
  const { instance, runtime } = await instantiateWithRuntime(wasmBytes);

  const good = runtime.alloc_slice_u8(Uint8Array.from([120, 61, 55, 59]));
  const bad = runtime.alloc_slice_u8(Uint8Array.from([120, 61, 200, 59]));

  const goodOut = callMainTaggedInt(instance, good);
  const badOut = callMainTaggedInt(instance, bad);

  assert(goodOut === 1, `expected parse success, got ${goodOut}`);
  assert(badOut === 0, `expected parse fail, got ${badOut}`);
  console.log("bootstrap phase4 parser pilot smoke: PASS");
}

main().catch(failWithError);
