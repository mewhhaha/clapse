#!/usr/bin/env node

import { cliArgs, failWithError, readBinaryFile } from "./runtime-env.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

async function callMainTaggedInt(path, n) {
  const wasmBytes = await readBinaryFile(path);
  const { instance } = await instantiateWithRuntime(wasmBytes);
  const fn = instance.exports.main;
  if (typeof fn !== "function") {
    throw new Error(`main export missing in ${path}`);
  }
  const out = fn(encodeInt(n));
  return decodeInt(out | 0);
}

async function callMainExpectTrap(path, n) {
  const wasmBytes = await readBinaryFile(path);
  const { instance } = await instantiateWithRuntime(wasmBytes);
  const fn = instance.exports.main;
  if (typeof fn !== "function") {
    throw new Error(`main export missing in ${path}`);
  }
  let trapped = false;
  try {
    fn(encodeInt(n));
  } catch (_err) {
    trapped = true;
  }
  assert(trapped, `expected wasm trap in ${path}`);
}

async function main() {
  const [hasTagTruePath, hasTagFalsePath, getOkPath, mismatchPath] = cliArgs();
  if (!hasTagTruePath || !hasTagFalsePath || !getOkPath || !mismatchPath) {
    throw new Error(
      "usage: deno run -A scripts/wasm-struct-helpers-smoke.mjs <has_tag_true.wasm> <has_tag_false.wasm> <get_ok.wasm> <get_mismatch.wasm>"
    );
  }

  const hasTagTrueOut = await callMainTaggedInt(hasTagTruePath, 7);
  assert(hasTagTrueOut === 1, `expected __is_pair result 1, got ${hasTagTrueOut}`);

  const hasTagFalseOut = await callMainTaggedInt(hasTagFalsePath, 7);
  assert(hasTagFalseOut === 0, `expected __is_other result 0, got ${hasTagFalseOut}`);

  const getOkOut = await callMainTaggedInt(getOkPath, 7);
  assert(getOkOut === 7, `expected __get_pair_0 result 7, got ${getOkOut}`);

  await callMainExpectTrap(mismatchPath, 7);
  console.log("wasm struct helpers smoke: PASS");
}

main().catch(failWithError);
