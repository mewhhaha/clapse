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
  const hostDceBefore = Deno.env.get("CLAPSE_ENTRYPOINT_DCE");
  const internalDceBefore = Deno.env.get("CLAPSE_INTERNAL_ENTRYPOINT_DCE");
  try {
    Deno.env.set("CLAPSE_ENTRYPOINT_DCE", "0");
    Deno.env.set("CLAPSE_INTERNAL_ENTRYPOINT_DCE", "0");

    const marker = `native-entrypoint-dce-strict-gate-${crypto.randomUUID()}`;
    const inputPath = `${tmpDir}/gate.clapse`;
    const outputPath = `${tmpDir}/gate.wasm`;
    const artifactsDir = `${tmpDir}/artifacts`;
    const source = [
      "export main",
      "main x = keep x",
      "keep x = x",
      `dead_fn x = x -- ${marker}`,
      "",
    ].join("\n");
    await Deno.writeTextFile(inputPath, source);
    await runWithArgs(["compile-debug", inputPath, outputPath, artifactsDir]);

    const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
    const collapsed = await Deno.readTextFile(
      `${artifactsDir}/collapsed_ir.txt`,
    );
    assert(
      !lowered.includes(marker),
      "native-entrypoint-dce-strict-gate: lowered_ir.txt still contains dead_fn marker with legacy DCE env toggles disabled",
    );
    assert(
      !collapsed.includes(marker),
      "native-entrypoint-dce-strict-gate: collapsed_ir.txt still contains dead_fn marker with legacy DCE env toggles disabled",
    );
    console.log("native-entrypoint-dce-strict-gate: PASS");
  } finally {
    if (hostDceBefore === undefined) {
      Deno.env.delete("CLAPSE_ENTRYPOINT_DCE");
    } else {
      Deno.env.set("CLAPSE_ENTRYPOINT_DCE", hostDceBefore);
    }
    if (internalDceBefore === undefined) {
      Deno.env.delete("CLAPSE_INTERNAL_ENTRYPOINT_DCE");
    } else {
      Deno.env.set("CLAPSE_INTERNAL_ENTRYPOINT_DCE", internalDceBefore);
    }
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
