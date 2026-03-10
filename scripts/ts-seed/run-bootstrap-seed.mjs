#!/usr/bin/env -S deno run -A

import { buildWasmSeedCompileResponse } from "../wasm-bootstrap-seed.mjs";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/ts-seed/run-bootstrap-seed.mjs --request '<json>' --seed-wasm <path> [--out <path>]",
    "  deno run -A scripts/ts-seed/run-bootstrap-seed.mjs --request-file <path> --seed-wasm <path> [--out <path>]",
  ].join("\n");
}

function parseArgs(argv) {
  let requestJson = "";
  let requestFile = "";
  let seedWasmPath = "";
  let outPath = "";
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--help" || arg === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (arg === "--request") {
      requestJson = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    if (arg === "--request-file") {
      requestFile = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    if (arg === "--seed-wasm") {
      seedWasmPath = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    if (arg === "--out") {
      outPath = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    throw new Error(`run-bootstrap-seed: unknown argument '${arg}'`);
  }
  if (seedWasmPath.length === 0) {
    throw new Error("run-bootstrap-seed: missing --seed-wasm <path>");
  }
  if (requestJson.length === 0 && requestFile.length === 0) {
    throw new Error("run-bootstrap-seed: provide --request <json> or --request-file <path>");
  }
  return { requestJson, requestFile, seedWasmPath, outPath };
}

async function parseRequestObject(requestJson, requestFile) {
  let raw = requestJson;
  if (raw.length === 0) {
    raw = await Deno.readTextFile(requestFile);
  }
  let parsed;
  try {
    parsed = JSON.parse(raw);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    throw new Error(`run-bootstrap-seed: request JSON parse failed (${msg})`);
  }
  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error("run-bootstrap-seed: request payload must be a JSON object");
  }
  return parsed;
}

async function run() {
  const { requestJson, requestFile, seedWasmPath, outPath } = parseArgs(
    Deno.args,
  );
  const requestObject = await parseRequestObject(requestJson, requestFile);
  const response = await buildWasmSeedCompileResponse(requestObject, {
    seedWasmPath,
  });
  const encoded = `${JSON.stringify(response, null, 2)}\n`;
  if (outPath.length > 0) {
    await Deno.writeTextFile(outPath, encoded);
    return;
  }
  await Deno.stdout.write(new TextEncoder().encode(encoded));
}

await run();
