#!/usr/bin/env node

import fs from "node:fs";
import { encodeInt, instantiateWithRuntime, renderResult } from "./wasm-runtime.mjs";

async function main() {
  const [, , wasmPath, exportNameArg, ...argStrings] = process.argv;

  if (!wasmPath) {
    console.error("Usage: node scripts/run-wasm.mjs <module.wasm> [export_name] [args...]");
    process.exit(1);
  }

  const exportName = exportNameArg ?? "main";
  const args = argStrings.map((s) => {
    const n = Number(s);
    if (!Number.isInteger(n)) {
      throw new Error(`non-integer argument: ${s}`);
    }
    return n;
  });

  const wasmBytes = fs.readFileSync(wasmPath);
  const { instance, runtime } = await instantiateWithRuntime(wasmBytes);
  const fn = instance.exports[exportName];
  if (typeof fn !== "function") {
    const exportsList = Object.keys(instance.exports).join(", ");
    throw new Error(`export '${exportName}' not found. available exports: ${exportsList}`);
  }

  const taggedArgs = args.map(encodeInt);
  const result = fn(...taggedArgs);
  if (result !== undefined) {
    console.log(renderResult(result, runtime.state));
  }
}

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});
