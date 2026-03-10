#!/usr/bin/env node

import { cliArgs, failWithError, readBinaryFile } from "./runtime-env.mjs";
import { encodeInt, instantiateWithRuntime, renderResult } from "./wasm-runtime.mjs";

async function main() {
  const [wasmPath, exportNameArg, ...argStrings] = cliArgs();

  if (!wasmPath) {
    throw new Error("Usage: deno run -A scripts/run-wasm.mjs <module.wasm> [export_name] [args...]");
  }

  const exportName = exportNameArg ?? "main";
  const args = argStrings.map((s) => {
    const n = Number(s);
    if (!Number.isInteger(n)) {
      throw new Error(`non-integer argument: ${s}`);
    }
    return n;
  });

  const wasmBytes = await readBinaryFile(wasmPath);
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

main().catch(failWithError);
