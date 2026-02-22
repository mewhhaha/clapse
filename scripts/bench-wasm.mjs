#!/usr/bin/env node

import fs from "node:fs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

function parsePositiveInt(raw, label) {
  const n = Number(raw);
  if (!Number.isInteger(n) || n <= 0) {
    throw new Error(`${label} must be a positive integer, got: ${raw}`);
  }
  return n;
}

function parseNonNegativeInt(raw, label) {
  const n = Number(raw);
  if (!Number.isInteger(n) || n < 0) {
    throw new Error(`${label} must be a non-negative integer, got: ${raw}`);
  }
  return n;
}

async function main() {
  const [, , wasmPath, exportNameArg, iterationsArg, warmupArg] = process.argv;
  if (!wasmPath) {
    console.error("Usage: node scripts/bench-wasm.mjs <module.wasm> [export_name] [iterations] [warmup]");
    process.exit(1);
  }

  const exportName = exportNameArg ?? "main";
  const iterations = iterationsArg === undefined ? 2_000_000 : parsePositiveInt(iterationsArg, "iterations");
  const warmup = warmupArg === undefined ? 20_000 : parseNonNegativeInt(warmupArg, "warmup");

  const wasmBytes = fs.readFileSync(wasmPath);
  const { instance } = await instantiateWithRuntime(wasmBytes);
  const fn = instance.exports[exportName];
  if (typeof fn !== "function") {
    const exportsList = Object.keys(instance.exports).join(", ");
    throw new Error(`export '${exportName}' not found. available exports: ${exportsList}`);
  }

  const arity = fn.length | 0;
  const taggedArgPools = makeTaggedArgPools(arity);

  for (let i = 0; i < warmup; i += 1) {
    fn(...argsForIteration(taggedArgPools, i));
  }

  let checksum = 0;
  const start = process.hrtime.bigint();
  for (let i = 0; i < iterations; i += 1) {
    const out = fn(...argsForIteration(taggedArgPools, i));
    checksum = (checksum + (decodeInt(out | 0) | 0)) | 0;
  }
  const end = process.hrtime.bigint();

  const elapsedNs = Number(end - start);
  const elapsedMs = elapsedNs / 1_000_000;
  const nsPerCall = elapsedNs / iterations;
  const opsPerSec = (iterations * 1_000_000_000) / elapsedNs;

  console.log(`module: ${wasmPath}`);
  console.log(`export: ${exportName}`);
  console.log(`arity: ${arity}`);
  console.log(`iterations: ${iterations}`);
  console.log(`warmup: ${warmup}`);
  console.log(`elapsed_ms: ${elapsedMs.toFixed(3)}`);
  console.log(`ns_per_call: ${nsPerCall.toFixed(2)}`);
  console.log(`ops_per_sec: ${opsPerSec.toFixed(2)}`);
  console.log(`checksum: ${checksum | 0}`);
}

function makeTaggedArgPools(arity) {
  const poolSize = 1024;
  const pools = [];
  for (let argIx = 0; argIx < arity; argIx += 1) {
    const pool = new Array(poolSize);
    for (let i = 0; i < poolSize; i += 1) {
      const n = (i + argIx * 31) & 1023;
      pool[i] = encodeInt(n);
    }
    pools.push(pool);
  }
  return pools;
}

function argsForIteration(pools, i) {
  if (pools.length === 0) {
    return [];
  }
  const idx = i & 1023;
  const args = new Array(pools.length);
  for (let argIx = 0; argIx < pools.length; argIx += 1) {
    args[argIx] = pools[argIx][idx];
  }
  return args;
}

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});
