#!/usr/bin/env -S deno run -A

import { runWithArgs } from "./run-clapse-compiler-wasm.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function hasSyntheticArtifactMarkers(value) {
  if (typeof value !== "string") {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

async function run() {
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-fold-laws-gate-",
  });
  try {
    const inputPath = `${tmpDir}/fold_law_fixture.clapse`;
    const outputPath = `${tmpDir}/fold_law_fixture.wasm`;
    const artifactsDir = `${tmpDir}/fold-law-artifacts`;
    const source = [
      "class Foldable t where",
      "  foldr : (a -> b -> b) -> b -> t a -> b",
      "law Foldable foldr_fmap_fusion = foldr f z (fmap g xs) => foldr (compose f g) z xs",
      "class Buildable t where",
      "  build : ((a -> t a -> t a) -> t a -> t a) -> t a",
      "law Buildable foldr_build = foldr f z (build g) => g f z",
      "export main",
      "main = 0",
      "",
    ].join("\n");
    await Deno.writeTextFile(inputPath, source);

    await runWithArgs([
      "compile-debug",
      inputPath,
      outputPath,
      artifactsDir,
      "--entrypoint-export",
      "main",
    ]);

    const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
    const collapsed = await Deno.readTextFile(
      `${artifactsDir}/collapsed_ir.txt`,
    );
    assert(
      !hasSyntheticArtifactMarkers(lowered),
      "native-fold-laws-gate: lowered_ir.txt contains synthetic markers",
    );
    assert(
      !hasSyntheticArtifactMarkers(collapsed),
      "native-fold-laws-gate: collapsed_ir.txt contains synthetic markers",
    );
    assert(
      collapsed.includes(
        "law Foldable foldr_fmap_fusion = foldr f z (fmap g xs) => foldr (compose f g) z xs",
      ),
      "native-fold-laws-gate: missing Foldable foldr/map law in collapsed_ir.txt",
    );
    assert(
      collapsed.includes(
        "law Buildable foldr_build = foldr f z (build g) => g f z",
      ),
      "native-fold-laws-gate: missing Buildable foldr/build law in collapsed_ir.txt",
    );

    const wasm = await Deno.readFile(outputPath);
    assert(
      wasm.length >= 8 && wasm[0] === 0x00 && wasm[1] === 0x61 &&
        wasm[2] === 0x73 && wasm[3] === 0x6d,
      `native-fold-laws-gate: invalid wasm output: ${outputPath}`,
    );

    console.log("native-fold-laws-gate: PASS");
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
