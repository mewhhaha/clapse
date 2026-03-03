#!/usr/bin/env -S deno run -A

import { runWithArgs } from "./run-clapse-compiler-wasm.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

async function run() {
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-entrypoint-dce-strict-gate-",
  });
  try {
    const deadFnMarker = `native-entrypoint-dce-strict-gate-${crypto.randomUUID()}`;
    const deadOperatorMarker =
      `native-entrypoint-dce-operator-gate-${crypto.randomUUID()}`;
    const inputPath = `${tmpDir}/gate.clapse`;
    const outputPath = `${tmpDir}/gate.wasm`;
    const artifactsDir = `${tmpDir}/artifacts`;
    const source = [
      "export main",
      "main x = keep x",
      "keep x = x",
      `dead_fn x = x -- ${deadFnMarker}`,
      `+. x y = x -- ${deadOperatorMarker}`,
      "",
    ].join("\n");
    await Deno.writeTextFile(inputPath, source);
    await runWithArgs(["compile-debug", inputPath, outputPath, artifactsDir]);

    const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
    const collapsed = await Deno.readTextFile(
      `${artifactsDir}/collapsed_ir.txt`,
    );
    assert(
      !lowered.includes(deadFnMarker),
      "native-entrypoint-dce-strict-gate: lowered_ir.txt still contains dead_fn marker with native request-shape pruning",
    );
    assert(
      !collapsed.includes(deadFnMarker),
      "native-entrypoint-dce-strict-gate: collapsed_ir.txt still contains dead_fn marker with native request-shape pruning",
    );
    assert(
      !lowered.includes(deadOperatorMarker),
      "native-entrypoint-dce-strict-gate: lowered_ir.txt still contains dead operator marker with native request-shape pruning",
    );
    assert(
      !collapsed.includes(deadOperatorMarker),
      "native-entrypoint-dce-strict-gate: collapsed_ir.txt still contains dead operator marker with native request-shape pruning",
    );
    console.log("native-entrypoint-dce-strict-gate: PASS");
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
