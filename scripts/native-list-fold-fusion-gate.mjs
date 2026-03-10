#!/usr/bin/env -S deno run -A

import { runWithArgs } from "./run-clapse-compiler-wasm.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function normalizeLine(line) {
  return line.replace(/\s+/gu, " ").trim();
}

function extractMainLine(collapsedIr) {
  const match = collapsedIr.match(
    /(?:^|\n)\s*(?:\(collapsed_ir\)\s*)?(?:[A-Za-z0-9_/'-]+\.)*main\s*=\s*[^\n]*/u,
  );
  assert(match !== null && match.length > 0, "native-list-fold-fusion-gate: collapsed_ir.txt is missing main line");
  return normalizeLine(match[0]);
}

async function run() {
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-list-fold-fusion-gate-",
  });
  try {
    const sourceText = [
      "export { main }",
      "main = foldl f z (fmap g xs)",
      "",
    ].join("\n");
    const inputPath = `${tmpDir}/fold-fusion.clapse`;
    await Deno.writeTextFile(inputPath, sourceText);

    const baselineArtifacts = `${tmpDir}/baseline-artifacts`;
    const baselineOutput = `${baselineArtifacts}/module.wasm`;
    await runWithArgs(["compile-native-debug", inputPath, baselineOutput, baselineArtifacts]);
    const baselineCollapsed = await Deno.readTextFile(`${baselineArtifacts}/collapsed_ir.txt`);
    const baselineMainLine = extractMainLine(baselineCollapsed);
    assert(
      /main\s*=\s*foldl\s+f\s+z\s+\(fmap\s+g\s+xs\)/u.test(baselineMainLine),
      "native-list-fold-fusion-gate: baseline collapsed main should contain foldl + fmap shape",
    );

    const prunedArtifacts = `${tmpDir}/entrypoint-pruned-artifacts`;
    const prunedOutput = `${prunedArtifacts}/module.wasm`;
    await runWithArgs([
      "compile-native-debug",
      inputPath,
      prunedOutput,
      prunedArtifacts,
      "--entrypoint-export",
      "main",
    ]);
    const prunedCollapsed = await Deno.readTextFile(
      `${prunedArtifacts}/collapsed_ir.txt`,
    );
    const prunedMainLine = extractMainLine(prunedCollapsed);
    assert(
      prunedMainLine.includes("foldl"),
      "native-list-fold-fusion-gate: pruned collapsed main should contain foldl",
    );
    assert(
      !prunedMainLine.includes("fmap"),
      "native-list-fold-fusion-gate: pruned collapsed main should not contain fmap",
    );

    console.log("native-list-fold-fusion-gate: PASS");
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
