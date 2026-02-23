#!/usr/bin/env node

import { cliArgs, failWithError, readBinaryFile } from "./runtime-env.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

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
  return decodeInt(fn(encodeInt(bytes[0]), encodeInt(bytes[1])));
}

async function main() {
  const [wasmPathArg] = cliArgs();
  const wasmPath = wasmPathArg ?? "out/bootstrap_phase5_dispatch_pilot.wasm";
  const wasmBytes = await readBinaryFile(wasmPath);
  const { instance } = await instantiateWithRuntime(wasmBytes);

  assert(callMainTaggedInt(instance, [3, 1]) === 0, "expected unknown method dispatch 0");
  assert(callMainTaggedInt(instance, [1, 1]) === 1, "expected GET/HOME dispatch 1");
  assert(callMainTaggedInt(instance, [1, 2]) === 2, "expected GET/STATUS dispatch 2");
  assert(callMainTaggedInt(instance, [2, 1]) === 3, "expected POST/HOME dispatch 3");
  assert(callMainTaggedInt(instance, [2, 2]) === 4, "expected POST/STATUS dispatch 4");
  assert(callMainTaggedInt(instance, [1, 3]) === 0, "expected unknown route dispatch 0");

  console.log("bootstrap phase5 dispatch pilot smoke: PASS");
}

main().catch(failWithError);
