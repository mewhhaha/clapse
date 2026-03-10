#!/usr/bin/env -S deno run -A

import { runWithArgs } from "./run-clapse-compiler-wasm.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

async function readText(path) {
  return await Deno.readTextFile(path);
}

async function statFile(path) {
  return await Deno.stat(path);
}

async function assertWasmFile(path, label) {
  const stat = await statFile(path);
  assert(
    stat.isFile && stat.size >= 8,
    `compile-debug-smoke: ${label} output wasm missing or too small: ${path}`,
  );
  const bytes = await Deno.readFile(path);
  assert(
    bytes[0] === 0x00 && bytes[1] === 0x61 && bytes[2] === 0x73 &&
      bytes[3] === 0x6d,
    `compile-debug-smoke: ${label} output wasm has invalid magic header: ${path}`,
  );
}

async function assertArtifacts(artifactsDir, label) {
  const loweredPath = `${artifactsDir}/lowered_ir.txt`;
  const collapsedPath = `${artifactsDir}/collapsed_ir.txt`;
  const lowered = await readText(loweredPath);
  const collapsed = await readText(collapsedPath);
  assertStructuralArtifacts(lowered, collapsed, {
    context: `compile-debug-smoke: ${label}`,
    requiredDefs: ["main"],
  });
}

async function runCase(tmpDir, inputPath, command) {
  const stem = command.replaceAll("_", "-");
  const outputPath = `${tmpDir}/${stem}.wasm`;
  const artifactsDir = `${tmpDir}/${stem}-artifacts`;
  await runWithArgs([command, inputPath, outputPath, artifactsDir]);
  await assertWasmFile(outputPath, command);
  await assertArtifacts(artifactsDir, command);
}

async function run() {
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-compile-debug-smoke-",
  });
  try {
    const probeToken = `compile-debug-smoke-${crypto.randomUUID()}`;
    const sourceText = [
      "main x = x",
      `-- ${probeToken}`,
      "",
    ].join("\n");
    const inputPath = `${tmpDir}/smoke.clapse`;
    await Deno.writeTextFile(inputPath, sourceText);
    const commands = [
      "compile-debug",
      "compile_debug",
      "compile-native-debug",
      "compile_native_debug",
    ];
    for (const command of commands) {
      await runCase(tmpDir, inputPath, command);
    }
    const dceMarker = `compile-debug-smoke-dead-${crypto.randomUUID()}`;
    const dceInputPath = `${tmpDir}/entrypoint_dce.clapse`;
    const dceOutputPath = `${tmpDir}/entrypoint_dce.wasm`;
    const dceArtifactsDir = `${tmpDir}/entrypoint-dce-artifacts`;
    const dceSource = [
      "export { main }",
      "main x = helper x",
      "helper x = x",
      `dead_fn x = x -- ${dceMarker}`,
      "",
    ].join("\n");
    await Deno.writeTextFile(dceInputPath, dceSource);
    await runWithArgs([
      "compile-debug",
      dceInputPath,
      dceOutputPath,
      dceArtifactsDir,
    ]);
    await assertWasmFile(dceOutputPath, "compile-debug dce");
    const dceLowered = await readText(`${dceArtifactsDir}/lowered_ir.txt`);
    const dceCollapsed = await readText(`${dceArtifactsDir}/collapsed_ir.txt`);
    assertStructuralArtifacts(dceLowered, dceCollapsed, {
      context: "compile-debug-smoke: compile-debug dce",
      requiredDefs: ["main", "helper"],
      forbiddenDefs: ["dead_fn"],
    });
    const projectDir = `${tmpDir}/entrypoint-dce-project`;
    const srcDir = `${projectDir}/src`;
    const moduleDir = `${srcDir}/smoke`;
    await Deno.mkdir(moduleDir, { recursive: true });
    await Deno.writeTextFile(
      `${projectDir}/clapse.json`,
      JSON.stringify({ include: ["src"] }, null, 2),
    );
    const importDeadMarker =
      `compile-debug-smoke-import-dead-${crypto.randomUUID()}`;
    const entryDeadMarker =
      `compile-debug-smoke-entry-dead-${crypto.randomUUID()}`;
    const unusedMarker = `compile-debug-smoke-unused-${crypto.randomUUID()}`;
    const entryModulePath = `${moduleDir}/entry.clapse`;
    const utilModulePath = `${moduleDir}/util.clapse`;
    const unusedModulePath = `${moduleDir}/unused.clapse`;
    await Deno.writeTextFile(
      entryModulePath,
      [
        "import \"smoke/util\" as util",
        "export { main }",
        "main x = util.live x",
        `entry_dead x = util.dead_helper x -- ${entryDeadMarker}`,
        "",
      ].join("\n"),
    );
    await Deno.writeTextFile(
      utilModulePath,
      [
        "export { live }",
        "live x = x",
        `dead_helper x = x -- ${importDeadMarker}`,
        "dead_chain x = dead_helper x",
        "",
      ].join("\n"),
    );
    await Deno.writeTextFile(
      unusedModulePath,
      [
        `unused x = x -- ${unusedMarker}`,
        "",
      ].join("\n"),
    );
    const moduleGraphArtifactsDir =
      `${tmpDir}/entrypoint-dce-module-graph-artifacts`;
    await runWithArgs([
      "compile-debug",
      entryModulePath,
      `${tmpDir}/entrypoint_dce_module_graph.wasm`,
      moduleGraphArtifactsDir,
    ]);
    const moduleGraphLowered = await readText(
      `${moduleGraphArtifactsDir}/lowered_ir.txt`,
    );
    const moduleGraphCollapsed = await readText(
      `${moduleGraphArtifactsDir}/collapsed_ir.txt`,
    );
    assertStructuralArtifacts(moduleGraphLowered, moduleGraphCollapsed, {
      context: "compile-debug-smoke: module-graph dce",
      requiredDefs: ["main"],
      forbiddenDefs: ["entry_dead", "dead_helper", "dead_chain", "unused"],
    });
    const internalOnlyMarker =
      `compile-debug-smoke-internal-dce-${crypto.randomUUID()}`;
    const internalOnlyInputPath = `${tmpDir}/internal_only_dce.clapse`;
    const internalOnlyOutputPath = `${tmpDir}/internal_only_dce.wasm`;
    const internalOnlyArtifactsDir = `${tmpDir}/internal-only-dce-artifacts`;
    await Deno.writeTextFile(
      internalOnlyInputPath,
      [
        "export { main }",
        "main x = keep x",
        "keep x = x",
        `dead_internal x = x -- ${internalOnlyMarker}`,
        "",
      ].join("\n"),
    );
    await runWithArgs([
      "compile-debug",
      internalOnlyInputPath,
      internalOnlyOutputPath,
      internalOnlyArtifactsDir,
    ]);
    await assertWasmFile(
      internalOnlyOutputPath,
      "compile-debug internal dce",
    );
    const internalLowered = await readText(
      `${internalOnlyArtifactsDir}/lowered_ir.txt`,
    );
    const internalCollapsed = await readText(
      `${internalOnlyArtifactsDir}/collapsed_ir.txt`,
    );
    assertStructuralArtifacts(internalLowered, internalCollapsed, {
      context: "compile-debug-smoke: internal dce",
      requiredDefs: ["main", "keep"],
      forbiddenDefs: ["dead_internal"],
    });
    console.log("compile-debug-smoke: PASS (4 command forms + entrypoint dce)");
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
