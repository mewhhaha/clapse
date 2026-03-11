#!/usr/bin/env -S deno run -A

import { runWithArgs } from "./run-clapse-compiler-wasm.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";

const UTF8_DECODER = new TextDecoder();

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

async function assertSupportedCompileDebugCase(tmpDir, name, sourceText) {
  const inputPath = `${tmpDir}/${name}.clapse`;
  const outputPath = `${tmpDir}/${name}.wasm`;
  const artifactsDir = `${tmpDir}/${name}-artifacts`;
  await Deno.writeTextFile(inputPath, sourceText);
  await runWithArgs([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ]);
  await assertWasmFile(outputPath, name);
  await assertArtifacts(artifactsDir, name);
}

async function decodeNullarySliceString(path, exportName = "main") {
  const bytes = await Deno.readFile(path);
  const module = await WebAssembly.compile(bytes);
  const instance = await WebAssembly.instantiate(module, {});
  const exported = instance.exports?.[exportName];
  const memory = instance.exports?.memory;
  assert(
    typeof exported === "function" && exported.length === 0,
    `compile-debug-smoke: expected nullary export ${exportName} in ${path}`,
  );
  assert(
    memory instanceof WebAssembly.Memory,
    `compile-debug-smoke: expected memory export in ${path}`,
  );
  const handle = exported();
  assert(
    Number.isInteger(handle) && handle >= 0,
    `compile-debug-smoke: expected non-negative slice handle from ${path}, got ${handle}`,
  );
  const view = new DataView(memory.buffer);
  const dataPtr = view.getUint32(handle, true);
  const length = view.getUint32(handle + 4, true);
  const slice = new Uint8Array(memory.buffer, dataPtr, length);
  return UTF8_DECODER.decode(slice);
}

async function decodeNullaryTaggedInt(path, exportName = "main") {
  const bytes = await Deno.readFile(path);
  const module = await WebAssembly.compile(bytes);
  const instance = await WebAssembly.instantiate(module, {});
  const exported = instance.exports?.[exportName];
  assert(
    typeof exported === "function" && exported.length === 0,
    `compile-debug-smoke: expected nullary export ${exportName} in ${path}`,
  );
  const raw = exported();
  assert(
    Number.isInteger(raw),
    `compile-debug-smoke: expected integer return from ${path}, got ${raw}`,
  );
  assert(
    (raw & 1) === 1,
    `compile-debug-smoke: expected tagged integer from ${path}, got raw=${raw}`,
  );
  return raw >> 1;
}

async function assertSupportedCompileDebugStringCase(
  tmpDir,
  name,
  sourceText,
  expectedString,
) {
  const inputPath = `${tmpDir}/${name}.clapse`;
  const outputPath = `${tmpDir}/${name}.wasm`;
  const artifactsDir = `${tmpDir}/${name}-artifacts`;
  await Deno.writeTextFile(inputPath, sourceText);
  await runWithArgs([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ]);
  await assertWasmFile(outputPath, name);
  await assertArtifacts(artifactsDir, name);
  const decoded = await decodeNullarySliceString(outputPath);
  assert(
    decoded === expectedString,
    `compile-debug-smoke: expected ${name} to decode to ${JSON.stringify(expectedString)}, got ${JSON.stringify(decoded)}`,
  );
}

async function assertSupportedCompileDebugTaggedIntCase(
  tmpDir,
  name,
  sourceText,
  expectedValue,
) {
  const inputPath = `${tmpDir}/${name}.clapse`;
  const outputPath = `${tmpDir}/${name}.wasm`;
  const artifactsDir = `${tmpDir}/${name}-artifacts`;
  await Deno.writeTextFile(inputPath, sourceText);
  await runWithArgs([
    "compile-debug",
    inputPath,
    outputPath,
    artifactsDir,
  ]);
  await assertWasmFile(outputPath, name);
  await assertArtifacts(artifactsDir, name);
  const decoded = await decodeNullaryTaggedInt(outputPath);
  assert(
    decoded === expectedValue,
    `compile-debug-smoke: expected ${name} to decode to tagged int ${expectedValue}, got ${decoded}`,
  );
}

async function assertUnsupportedCompileDebugCase(tmpDir, name, sourceText) {
  const inputPath = `${tmpDir}/${name}.clapse`;
  await Deno.writeTextFile(inputPath, sourceText);
  let message = "";
  try {
    await runWithArgs([
      "compile-debug",
      inputPath,
      `${tmpDir}/${name}.wasm`,
      `${tmpDir}/${name}-artifacts`,
    ]);
    throw new Error(
      `compile-debug-smoke: expected ${name} to fail closed`,
    );
  } catch (err) {
    message = err instanceof Error ? err.message : String(err);
  }
  assert(
    message.includes("compile_phase1_unsupported"),
    `compile-debug-smoke: expected compile_phase1_unsupported for ${name}, got ${message}`,
  );
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
    const supportedCases = [
      {
        name: "supported_maybe_bind",
        source: [
          'import "prelude" { Just, maybe_bind }',
          "",
          "export { main }",
          "",
          "main =",
          "  case maybe_bind (Just 201) (\\x -> Just x) of",
          "    Just y -> y",
          "    _ -> 0",
          "",
        ].join("\n"),
      },
      {
        name: "supported_recursive_constructor_helper",
        source: [
          "data List a = Nil | Cons a (List a)",
          "",
          "export { main }",
          "",
          "insert x xs =",
          "  case xs of",
          "    Nil -> Cons x Nil",
          "    Cons y ys -> Cons y (insert x ys)",
          "",
          "head1 xs =",
          "  case xs of",
          "    Cons y _ -> y",
          "    _ -> 0",
          "",
          "main = head1 (insert 201 Nil)",
          "",
        ].join("\n"),
      },
      {
        name: "supported_state_pure",
        source: [
          'import "prelude" { eval_state, state_pure }',
          "",
          "export { main }",
          "",
          "main = eval_state (state_pure 201) 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_state_bind_eval",
        source: [
          'import "prelude" { get_state, state_bind, state_pure, eval_state }',
          "",
          "export { main }",
          "",
          "main = eval_state (state_bind get_state (\\x -> state_pure x)) 201",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_map_lookup_by_chain",
        source: [
          'import "prelude" { Pair, eq, map_from_list_by, map_lookup_by, maybe_with_default }',
          "",
          "export { main }",
          "",
          "status_codes =",
          "  map_from_list_by eq",
          '    [ Pair "GET" 200',
          '    , Pair "POST" 201',
          '    , Pair "DELETE" 204',
          "    ]",
          "",
          "lookup_code method =",
          "  maybe_with_default 500 (map_lookup_by eq method status_codes)",
          "",
          'main = lookup_code "POST"',
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_maybe_with_default_root",
        source: [
          'import "prelude" { maybe_with_default, Just }',
          "",
          "export { main }",
          "",
          "main = maybe_with_default 500 (Just 201)",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_eval_reader_bind",
        source: [
          'import "prelude" { reader_bind, reader_pure, run_reader }',
          "",
          "export { main }",
          "",
          "main = run_reader (reader_bind (reader_pure 201) (\\x -> reader_pure x)) 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_exec_state_root",
        source: [
          'import "prelude" { exec_state, state_pure }',
          "",
          "export { main }",
          "",
          "main = exec_state (state_pure 201) 7",
          "",
        ].join("\n"),
        expectedTaggedInt: 7,
      },
      {
        name: "supported_set_member_by_root",
        source: [
          'import "prelude" { eq, set_from_list_by, set_member_by }',
          "",
          "export { main }",
          "",
          "s = set_from_list_by eq [1, 2, 2]",
          "main = set_member_by eq 2 s",
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_map_from_list_by_root",
        source: [
          'import "prelude" { Pair, eq, map_from_list_by }',
          "",
          "export { main }",
          "",
          'main = map_from_list_by eq [ Pair "POST" 201 ]',
          "",
        ].join("\n"),
        expectedString: 'Map [Pair "POST" 201]',
      },
      {
        name: "supported_map_lookup_by_root",
        source: [
          'import "prelude" { Pair, eq, map_from_list_by, map_lookup_by }',
          "",
          "export { main }",
          "",
          'status_codes = map_from_list_by eq [ Pair "POST" 201 ]',
          'main = map_lookup_by eq "POST" status_codes',
          "",
        ].join("\n"),
        expectedString: "Just 201",
      },
      {
        name: "supported_state_bind_root",
        source: [
          'import "prelude" { get_state, state_bind, state_pure }',
          "",
          "export { main }",
          "",
          "main = state_bind get_state (\\x -> state_pure x)",
          "",
        ].join("\n"),
        expectedString:
          "State (\\s0 -> case run_state (State (\\s -> Pair s s)) s0 of Pair value s1 -> run_state (state_pure value) s1)",
      },
    ];
    for (const testCase of supportedCases) {
      if (Number.isInteger(testCase.expectedTaggedInt)) {
        await assertSupportedCompileDebugTaggedIntCase(
          tmpDir,
          testCase.name,
          testCase.source,
          testCase.expectedTaggedInt,
        );
      } else if (typeof testCase.expectedString === "string") {
        await assertSupportedCompileDebugStringCase(
          tmpDir,
          testCase.name,
          testCase.source,
          testCase.expectedString,
        );
      } else {
        await assertSupportedCompileDebugCase(
          tmpDir,
          testCase.name,
          testCase.source,
        );
      }
    }
    const unsupportedCases = [
      {
        name: "unsupported_keep_left_default_nested_lambda",
        source: [
          'import "prelude" { Just, keep_left_default }',
          "",
          "export { main }",
          "",
          "main =",
          "  case keep_left_default (Just 201) (Just 1) of",
          "    Just x -> x",
          "    _ -> 0",
          "",
        ].join("\n"),
      },
      {
        name: "unsupported_keep_right_default_nested_lambda",
        source: [
          'import "prelude" { Just, keep_right_default }',
          "",
          "export { main }",
          "",
          "main =",
          "  case keep_right_default (Just 201) (Just 1) of",
          "    Just x -> x",
          "    _ -> 0",
          "",
        ].join("\n"),
      },
      {
        name: "unsupported_reader_ap_eval",
        source: [
          'import "prelude" { add, reader_ap, reader_pure, run_reader }',
          "",
          "export { main }",
          "",
          "main = run_reader (reader_ap (reader_pure (\\x -> add x 1)) (reader_pure 200)) 0",
          "",
        ].join("\n"),
      },
      {
        name: "unsupported_reader_ap_root",
        source: [
          'import "prelude" { add, reader_ap, reader_pure }',
          "",
          "export { main }",
          "",
          "main = reader_ap (reader_pure (\\x -> add x 1)) (reader_pure 200)",
          "",
        ].join("\n"),
      },
    ];
    for (const testCase of unsupportedCases) {
      await assertUnsupportedCompileDebugCase(
        tmpDir,
        testCase.name,
        testCase.source,
      );
    }
    console.log(
      "compile-debug-smoke: PASS (4 command forms + entrypoint dce + helper boundary matrix)",
    );
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
