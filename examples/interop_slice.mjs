#!/usr/bin/env node

import fs from "node:fs";
import { instantiateWithRuntime, renderResult } from "../scripts/wasm-runtime.mjs";

const text_encoder = new TextEncoder();

async function main() {
  const wasm_path = process.argv[2] ?? "out/interop_slice.wasm";
  const wasm_bytes = fs.readFileSync(wasm_path);
  const { instance, runtime } = await instantiateWithRuntime(wasm_bytes);
  const fn = instance.exports.main;
  if (typeof fn !== "function") {
    throw new Error("expected exported function: main");
  }

  const get_req = runtime.alloc_slice_u8(text_encoder.encode("GET / HTTP/1.1\r\n"));
  const post_req = runtime.alloc_slice_u8(text_encoder.encode("POST / HTTP/1.1\r\n"));

  console.log(`GET ${renderResult(fn(get_req), runtime.state)}`);
  console.log(`POST ${renderResult(fn(post_req), runtime.state)}`);
}

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});
