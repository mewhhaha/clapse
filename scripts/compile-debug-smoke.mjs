#!/usr/bin/env -S deno run -A

import {
  buildCompileReachabilityPlanForTest,
  runWithArgs,
} from "./run-clapse-compiler-wasm.mjs";

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

async function assertArtifacts(artifactsDir, sourceText, probeToken, label) {
  const loweredPath = `${artifactsDir}/lowered_ir.txt`;
  const collapsedPath = `${artifactsDir}/collapsed_ir.txt`;
  const lowered = await readText(loweredPath);
  const collapsed = await readText(collapsedPath);
  assert(
    lowered.length > 0,
    `compile-debug-smoke: ${label} lowered_ir.txt should be non-empty`,
  );
  assert(
    collapsed.length > 0,
    `compile-debug-smoke: ${label} collapsed_ir.txt should be non-empty`,
  );
  assert(
    !hasSyntheticArtifactMarkers(lowered),
    `compile-debug-smoke: ${label} lowered_ir.txt should not contain synthetic markers`,
  );
  assert(
    !hasSyntheticArtifactMarkers(collapsed),
    `compile-debug-smoke: ${label} collapsed_ir.txt should not contain synthetic markers`,
  );
  assert(
    lowered.includes(sourceText),
    `compile-debug-smoke: ${label} lowered_ir.txt should include request source`,
  );
  assert(
    collapsed.includes(sourceText),
    `compile-debug-smoke: ${label} collapsed_ir.txt should include request source`,
  );
  assert(
    lowered.includes(probeToken),
    `compile-debug-smoke: ${label} lowered_ir.txt should include probe token`,
  );
  assert(
    collapsed.includes(probeToken),
    `compile-debug-smoke: ${label} collapsed_ir.txt should include probe token`,
  );
}

async function runCase(tmpDir, inputPath, sourceText, probeToken, command) {
  const stem = command.replaceAll("_", "-");
  const outputPath = `${tmpDir}/${stem}.wasm`;
  const artifactsDir = `${tmpDir}/${stem}-artifacts`;
  await runWithArgs([command, inputPath, outputPath, artifactsDir]);
  await assertWasmFile(outputPath, command);
  await assertArtifacts(artifactsDir, sourceText, probeToken, command);
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
      await runCase(tmpDir, inputPath, sourceText, probeToken, command);
    }
    const dceMarker = `compile-debug-smoke-dead-${crypto.randomUUID()}`;
    const dceInputPath = `${tmpDir}/entrypoint_dce.clapse`;
    const dceOutputPath = `${tmpDir}/entrypoint_dce.wasm`;
    const dceArtifactsDir = `${tmpDir}/entrypoint-dce-artifacts`;
    const dceSource = [
      "export main",
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
    assert(
      !dceLowered.includes(dceMarker),
      "compile-debug-smoke: lowered_ir.txt should not include dead_fn marker after entrypoint reachability pruning",
    );
    assert(
      !dceCollapsed.includes(dceMarker),
      "compile-debug-smoke: collapsed_ir.txt should not include dead_fn marker after entrypoint reachability pruning",
    );
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
        "module smoke.entry",
        "import smoke.util",
        "export main",
        "main x = smoke.util.live x",
        `entry_dead x = smoke.util.dead_helper x -- ${entryDeadMarker}`,
        "",
      ].join("\n"),
    );
    await Deno.writeTextFile(
      utilModulePath,
      [
        "module smoke.util",
        "export live",
        "live x = x",
        `dead_helper x = x -- ${importDeadMarker}`,
        "dead_chain x = dead_helper x",
        "",
      ].join("\n"),
    );
    await Deno.writeTextFile(
      unusedModulePath,
      [
        "module smoke.unused",
        `unused x = x -- ${unusedMarker}`,
        "",
      ].join("\n"),
    );
    const modulePlan = await buildCompileReachabilityPlanForTest(
      entryModulePath,
    );
    assert(
      modulePlan !== null,
      "compile-debug-smoke: expected reachability plan for module graph case",
    );
    const entryPrunedSource =
      modulePlan.sourceByPath.get(modulePlan.entryPath) ??
        "";
    assert(
      !entryPrunedSource.includes(entryDeadMarker),
      "compile-debug-smoke: entrypoint plan should prune unreachable entrypoint functions",
    );
    const utilModule = modulePlan.moduleSources.find((item) =>
      item.module_name === "smoke.util"
    );
    assert(
      Boolean(utilModule),
      "compile-debug-smoke: expected reachable imported module source override for smoke.util",
    );
    assert(
      utilModule.reachable_functions.length === 1 &&
        utilModule.reachable_functions[0] === "live",
      "compile-debug-smoke: expected only live to be reachable in imported util module",
    );
    assert(
      !utilModule.input_source.includes(importDeadMarker),
      "compile-debug-smoke: imported module dead function should be pruned from source override",
    );
    assert(
      !modulePlan.moduleSources.some((item) =>
        item.module_name === "smoke.unused"
      ),
      "compile-debug-smoke: unused modules should not be included in module source overrides",
    );
    const subsetPlan = await buildCompileReachabilityPlanForTest(
      entryModulePath,
      { entrypointExports: ["main"] },
    );
    assert(
      subsetPlan !== null,
      "compile-debug-smoke: expected subset reachability plan",
    );
    assert(
      Array.isArray(subsetPlan.rootExports) &&
        subsetPlan.rootExports.length === 1 &&
        subsetPlan.rootExports[0] === "main",
      "compile-debug-smoke: subset reachability plan should pin root exports to requested entrypoint_exports",
    );
    const subsetEntrySource =
      subsetPlan.sourceByPath.get(subsetPlan.entryPath) ?? "";
    assert(
      !subsetEntrySource.includes(entryDeadMarker),
      "compile-debug-smoke: subset roots should prune extra exported entrypoint functions",
    );
    await runWithArgs([
      "compile-debug",
      entryModulePath,
      `${tmpDir}/entrypoint_dce_module_graph.wasm`,
      `${tmpDir}/entrypoint-dce-module-graph-artifacts`,
    ]);
    const internalOnlyMarker =
      `compile-debug-smoke-internal-dce-${crypto.randomUUID()}`;
    const internalOnlyInputPath = `${tmpDir}/internal_only_dce.clapse`;
    const internalOnlyOutputPath = `${tmpDir}/internal_only_dce.wasm`;
    const internalOnlyArtifactsDir = `${tmpDir}/internal-only-dce-artifacts`;
    const hostDceBefore = Deno.env.get("CLAPSE_ENTRYPOINT_DCE");
    const internalDceBefore = Deno.env.get("CLAPSE_INTERNAL_ENTRYPOINT_DCE");
    try {
      Deno.env.set("CLAPSE_ENTRYPOINT_DCE", "0");
      Deno.env.set("CLAPSE_INTERNAL_ENTRYPOINT_DCE", "0");
      await Deno.writeTextFile(
        internalOnlyInputPath,
        [
          "export main",
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
      assert(
        !internalLowered.includes(internalOnlyMarker),
        "compile-debug-smoke: lowered_ir.txt should not include dead marker when host/internal env DCE toggles are both disabled",
      );
      assert(
        !internalCollapsed.includes(internalOnlyMarker),
        "compile-debug-smoke: collapsed_ir.txt should not include dead marker when host/internal env DCE toggles are both disabled",
      );
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
    }
    console.log("compile-debug-smoke: PASS (4 command forms + entrypoint dce)");
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
