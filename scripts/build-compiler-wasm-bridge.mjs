#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";

function bridgeModuleBytes() {
  // wasm module:
  // (module
  //   (import "host" "clapse_run" (func (param i32) (result i32)))
  //   (memory (export "memory") 1)
  //   (export "clapse_run" (func 0))
  // )
  return new Uint8Array([
    0x00,
    0x61,
    0x73,
    0x6d, // magic
    0x01,
    0x00,
    0x00,
    0x00, // version
    0x01,
    0x06,
    0x01,
    0x60,
    0x01,
    0x7f,
    0x01,
    0x7f, // type section
    0x02,
    0x13,
    0x01,
    0x04,
    0x68,
    0x6f,
    0x73,
    0x74,
    0x0a,
    0x63,
    0x6c,
    0x61,
    0x70,
    0x73,
    0x65,
    0x5f,
    0x72,
    0x75,
    0x6e,
    0x00,
    0x00, // import section
    0x05,
    0x03,
    0x01,
    0x00,
    0x01, // memory section
    0x07,
    0x17,
    0x02,
    0x0a,
    0x63,
    0x6c,
    0x61,
    0x70,
    0x73,
    0x65,
    0x5f,
    0x72,
    0x75,
    0x6e,
    0x00,
    0x00,
    0x06,
    0x6d,
    0x65,
    0x6d,
    0x6f,
    0x72,
    0x79,
    0x02,
    0x00, // export section
  ]);
}

async function main() {
  const [outPathArg] = cliArgs();
  const outPath = outPathArg ?? "out/clapse_compiler_bridge.wasm";
  const outDir = outPath.includes("/")
    ? outPath.slice(0, outPath.lastIndexOf("/"))
    : ".";
  if (outDir.length > 0 && outDir !== ".") {
    await Deno.mkdir(outDir, { recursive: true });
  }
  await Deno.writeFile(outPath, bridgeModuleBytes());
  console.log(`wrote compiler wasm bridge artifact: ${outPath}`);
}

await main().catch(failWithError);
