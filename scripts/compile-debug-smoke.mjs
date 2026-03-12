#!/usr/bin/env -S deno run -A

import {
  buildDemandDrivenCompileInput,
  runWithArgs,
} from "./run-clapse-compiler-wasm.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";
import { callCompilerWasm } from "./wasm-compiler-abi.mjs";

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

async function assertUnsupportedCompileDebugCase(
  tmpDir,
  name,
  sourceText,
  expectedSubstring = "compile_phase1_unsupported",
) {
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
    message.includes(expectedSubstring),
    `compile-debug-smoke: expected ${expectedSubstring} for ${name}, got ${message}`,
  );
}

async function assertDirectBoundaryFailsClosedOnSourceEcho(
  tmpDir,
  name,
  sourceText,
) {
  const inputPath = `${tmpDir}/${name}.clapse`;
  await Deno.writeTextFile(inputPath, sourceText);
  const shaped = await buildDemandDrivenCompileInput(inputPath, []);
  const compilerPath =
    `${Deno.cwd()}/artifacts/latest/clapse_compiler.wasm`;
  const response = await callCompilerWasm(compilerPath, {
    command: "compile",
    input_path: inputPath,
    input_source: shaped.inputSourceOverride,
    compile_mode: "debug",
    entrypoint_exports: shaped.entrypointExports,
  });
  assert(
    response?.ok === false,
    `compile-debug-smoke: expected direct boundary ${name} to fail closed`,
  );
  assert(
    response?.error_code === "compile_placeholder_response",
    `compile-debug-smoke: expected compile_placeholder_response for ${name}, got ${response?.error_code}`,
  );
}

async function run() {
  const tmpDir = await Deno.makeTempDir({
    dir: "/tmp",
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
        name: "supported_bind_operator_maybe",
        source: [
          'import "prelude" { >>=, Just }',
          "",
          "export { main }",
          "",
          "main =",
          "  case ((Just 201) >>= (\\x -> Just x)) of",
          "    Just y -> y",
          "    _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_bind_operator_maybe_root",
        source: [
          'import "prelude" { >>=, Just }',
          "",
          "export { main }",
          "",
          "main = ((Just 201) >>= (\\x -> Just x))",
          "",
        ].join("\n"),
        expectedString: "Just 201",
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
        name: "supported_state_ap_eval",
        source: [
          'import "prelude" { state_ap, state_pure, eval_state, add }',
          "",
          "export { main }",
          "",
          "main = eval_state (state_ap (state_pure (\\x -> add x 1)) (state_pure 200)) 0",
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
        name: "supported_collection_empty_list_root",
        source: [
          'import "prelude" { collection_empty }',
          "",
          "export { main }",
          "",
          "main = collection_empty 0",
          "",
        ].join("\n"),
        expectedString: "[]",
      },
      {
        name: "supported_collection_extend_list_root",
        source: [
          'import "prelude" { collection_empty, collection_extend }',
          "",
          "export { main }",
          "",
          "main = collection_extend (collection_empty 0) 201",
          "",
        ].join("\n"),
        expectedString: "[201]",
      },
      {
        name: "supported_all_list_boolean_override",
        source: [
          'import "prelude" { all, eq }',
          "",
          "export { main }",
          "",
          "main =",
          "  case all (\\x -> eq x 2) [2, 2, 2] of",
          "    true -> 1",
          "    _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 1,
      },
      {
        name: "supported_alt_operator_alias_infix",
        source: [
          'import "prelude" { Just, <|> }',
          "",
          "export { main }",
          "",
          "main =",
          "  case ((Just 201) <|> (Just 1)) of",
          "    Just x -> x",
          "    _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_or_named_root",
        source: [
          'import "prelude" { or }',
          "",
          "export { main }",
          "",
          "main = or true false",
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_xor_named_root",
        source: [
          'import "prelude" { xor }',
          "",
          "export { main }",
          "",
          "main = xor true false",
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_and_named_root",
        source: [
          'import "prelude" { and }',
          "",
          "export { main }",
          "",
          "main = and true true",
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_not_named_root",
        source: [
          'import "prelude" { not }',
          "",
          "export { main }",
          "",
          "main = not false",
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_implies_named_root",
        source: [
          'import "prelude" { implies }',
          "",
          "export { main }",
          "",
          "main = implies true true",
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_multiline_computed_boolean_case_target",
        source: [
          'import "prelude" { eq }',
          "",
          "export { main }",
          "",
          "main =",
          "  case (eq 1 1) of",
          "    true -> 1",
          "    _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 1,
      },
      {
        name: "supported_single_line_computed_boolean_case_target",
        source: [
          'import "prelude" { eq }',
          "",
          "export { main }",
          "",
          "main = case (eq 1 1) of true -> 1; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 1,
      },
      {
        name: "supported_single_line_constructor_case_target",
        source: [
          'import "prelude" { Just }',
          "",
          "export { main }",
          "",
          "main = case (Just 201) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_single_line_record_case_target",
        source: [
          "",
          "export { main }",
          "",
          "main = case { x = 201 } of { x = y } -> y; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
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
      {
        name: "supported_reader_bind_root",
        source: [
          'import "prelude" { reader_bind, reader_pure, add }',
          "",
          "export { main }",
          "",
          "main = reader_bind (reader_pure 200) (\\x -> reader_pure (add x 1))",
          "",
        ].join("\n"),
        expectedString:
          "Reader (\\env -> run_reader (reader_pure (add (run_reader (Reader (\\__ignored0 -> 200)) env) 1)) env)",
      },
      {
        name: "supported_state_map_root",
        source: [
          'import "prelude" { state_map, state_pure, add }',
          "",
          "export { main }",
          "",
          "main = state_map (\\x -> add x 1) (state_pure 200)",
          "",
        ].join("\n"),
        expectedString:
          "State (\\s -> case run_state (State (\\s -> Pair 200 s)) s of Pair value next_state -> Pair (add value 1) next_state)",
      },
      {
        name: "supported_append_maybe_root",
        source: [
          'import "prelude" { append, Just, Nothing }',
          "",
          "export { main }",
          "",
          "main = append (Just 201) Nothing",
          "",
        ].join("\n"),
        expectedString: "Just 201",
      },
      {
        name: "supported_append_list_root",
        source: [
          'import "prelude" { append, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case append (Cons 201 Nil) (Cons 1 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_alt_default_maybe_root",
        source: [
          'import "prelude" { alt_default, Just, Nothing }',
          "",
          "export { main }",
          "",
          "main = alt_default (Just 201) Nothing",
          "",
        ].join("\n"),
        expectedString: "Just 201",
      },
      {
        name: "supported_alt_default_list_root",
        source: [
          'import "prelude" { alt_default, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case alt_default (Cons 201 Nil) (Cons 1 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_alt_operator_list_root",
        source: [
          'import "prelude" { <|>, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case ((Cons 201 Nil) <|> (Cons 1 Nil)) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_reader_ap_eval",
        source: [
          'import "prelude" { add, reader_ap, reader_pure, run_reader }',
          "",
          "export { main }",
          "",
          "main = run_reader (reader_ap (reader_pure (\\x -> add x 1)) (reader_pure 200)) 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_reader_ap_root",
        source: [
          'import "prelude" { add, reader_ap, reader_pure }',
          "",
          "export { main }",
          "",
          "main = reader_ap (reader_pure (\\x -> add x 1)) (reader_pure 200)",
          "",
        ].join("\n"),
        expectedString:
          "Reader (\\env -> run_reader (Reader (\\__ignored0 -> \\x -> add x 1)) env (run_reader (Reader (\\__ignored0 -> 200)) env))",
      },
      {
        name: "supported_asks_reader_eval",
        source: [
          'import "prelude" { asks_reader, run_reader, add }',
          "",
          "export { main }",
          "",
          "main = run_reader (asks_reader (\\x -> add x 1)) 200",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_put_state_eval",
        source: [
          'import "prelude" { put_state, exec_state }',
          "",
          "export { main }",
          "",
          "main = exec_state (put_state 201) 7",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_modify_state_eval",
        source: [
          'import "prelude" { modify_state, exec_state, add }',
          "",
          "export { main }",
          "",
          "main = exec_state (modify_state (\\x -> add x 1)) 200",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_gets_state_eval",
        source: [
          'import "prelude" { gets_state, eval_state, add }',
          "",
          "export { main }",
          "",
          "main = eval_state (gets_state (\\x -> add x 1)) 200",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_force_lazy_eval",
        source: [
          'import "prelude" { force, lazy, add }',
          "",
          "export { main }",
          "",
          "main = force (lazy (\\_ -> add 200 1))",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_id_root",
        source: [
          'import "prelude" { id }',
          "",
          "export { main }",
          "",
          "main = id 201",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_compose_root",
        source: [
          'import "prelude" { compose, add }',
          "",
          "export { main }",
          "",
          "main = compose (\\x -> add x 1) (\\x -> add x 1) 199",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_reader_map_root",
        source: [
          'import "prelude" { reader_map, reader_pure, add }',
          "",
          "export { main }",
          "",
          "main = reader_map (\\x -> add x 1) (reader_pure 200)",
          "",
        ].join("\n"),
        expectedString:
          "Reader (\\env -> add (run_reader (Reader (\\__ignored0 -> 200)) env) 1)",
      },
      {
        name: "supported_lazy_root",
        source: [
          'import "prelude" { lazy }',
          "",
          "export { main }",
          "",
          "main = lazy (\\_ -> 201)",
          "",
        ].join("\n"),
        expectedString: "Lazy (\\__ignored0 -> 201)",
      },
      {
        name: "supported_byte_root",
        source: [
          'import "prelude" { byte }',
          "",
          "export { main }",
          "",
          "main = byte 201",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_char_root",
        source: [
          'import "prelude" { char }',
          "",
          "export { main }",
          "",
          "main = char 201",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_record_byte_char_root",
        source: [
          'import "prelude" { byte, char }',
          "",
          "export { main }",
          "",
          "main = { b = byte 65, c = char 66 }",
          "",
        ].join("\n"),
        expectedString: "{ b = 65, c = 66 }",
      },
      {
        name: "supported_map_member_by_chain",
        source: [
          'import "prelude" { Pair, eq, map_from_list_by, map_member_by }',
          "",
          "export { main }",
          "",
          'm = map_from_list_by eq [Pair "POST" 201]',
          'main = map_member_by eq "POST" m',
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_maybe_map_root",
        source: [
          'import "prelude" { maybe_map, Just, add }',
          "",
          "export { main }",
          "",
          "main = maybe_map (\\x -> add x 1) (Just 200)",
          "",
        ].join("\n"),
        expectedString: "Just 201",
      },
      {
        name: "supported_set_insert_by_root",
        source: [
          'import "prelude" { eq, set_insert_by, set_empty }',
          "",
          "export { main }",
          "",
          "main = set_insert_by eq 201 set_empty",
          "",
        ].join("\n"),
        expectedString: "Set [201]",
      },
      {
        name: "supported_set_remove_by_root",
        source: [
          'import "prelude" { eq, set_from_list_by, set_remove_by }',
          "",
          "export { main }",
          "",
          "s = set_from_list_by eq [201, 1]",
          "main = set_remove_by eq 1 s",
          "",
        ].join("\n"),
        expectedString: "Set [201]",
      },
      {
        name: "supported_asks_reader_root",
        source: [
          'import "prelude" { asks_reader, add }',
          "",
          "export { main }",
          "",
          "main = asks_reader (\\x -> add x 1)",
          "",
        ].join("\n"),
        expectedString: "Reader (\\env -> add env 1)",
      },
      {
        name: "supported_local_reader_root",
        source: [
          'import "prelude" { local_reader, reader_pure, add }',
          "",
          "export { main }",
          "",
          "main = local_reader (\\x -> add x 1) (reader_pure 200)",
          "",
        ].join("\n"),
        expectedString:
          "Reader (\\env -> run_reader (Reader (\\__ignored0 -> 200)) (add env 1))",
      },
      {
        name: "supported_put_state_root",
        source: [
          'import "prelude" { put_state }',
          "",
          "export { main }",
          "",
          "main = put_state 201",
          "",
        ].join("\n"),
        expectedString: "State (\\__ignored0 -> Pair Unit 201)",
      },
      {
        name: "supported_gets_state_root",
        source: [
          'import "prelude" { gets_state, add }',
          "",
          "export { main }",
          "",
          "main = gets_state (\\x -> add x 1)",
          "",
        ].join("\n"),
        expectedString:
          "State (\\s -> case run_state (State (\\s -> Pair s s)) s of Pair value next_state -> Pair (add value 1) next_state)",
      },
      {
        name: "supported_ap_maybe_named",
        source: [
          'import "prelude" { ap, Just, add }',
          "",
          "export { main }",
          "",
          "main = case ap (Just (\\x -> add x 1)) (Just 200) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_ap_operator_maybe",
        source: [
          'import "prelude" { <*>, Just, add }',
          "",
          "export { main }",
          "",
          "main = case ((Just (\\x -> add x 1)) <*> (Just 200)) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_then_m_default_maybe",
        source: [
          'import "prelude" { then_m_default, Just }',
          "",
          "export { main }",
          "",
          "main = case then_m_default (Just 1) (Just 201) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_then_m_maybe",
        source: [
          'import "prelude" { then_m, Just }',
          "",
          "export { main }",
          "",
          "main = case then_m (Just 1) (Just 201) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_any_list_root",
        source: [
          'import "prelude" { any, eq }',
          "",
          "export { main }",
          "",
          "main = any (\\x -> eq x 2) [1, 2, 3]",
          "",
        ].join("\n"),
        expectedString: "true",
      },
      {
        name: "supported_filter_list_root",
        source: [
          'import "prelude" { filter, eq }',
          "",
          "export { main }",
          "",
          "main = filter (\\x -> eq x 2) [1, 2, 3]",
          "",
        ].join("\n"),
        expectedString: "[2]",
      },
      {
        name: "supported_filter_maybe_root",
        source: [
          'import "prelude" { Just, filter, eq }',
          "",
          "export { main }",
          "",
          "main = case filter (\\x -> eq x 201) (Just 201) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_fmap_list_root",
        source: [
          'import "prelude" { fmap, add }',
          "",
          "export { main }",
          "",
          "main = fmap (\\x -> add x 1) [1, 2, 3]",
          "",
        ].join("\n"),
        expectedString: "[2, 3, 4]",
      },
      {
        name: "supported_fmap_maybe_root",
        source: [
          'import "prelude" { Just, fmap, add }',
          "",
          "export { main }",
          "",
          "main = case fmap (\\x -> add x 1) (Just 200) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_fmap_operator_maybe_root",
        source: [
          'import "prelude" { Just, <$>, add }',
          "",
          "export { main }",
          "",
          "main = case ((\\x -> add x 1) <$> (Just 200)) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_map_replace_list_root",
        source: [
          'import "prelude" { map_replace }',
          "",
          "export { main }",
          "",
          "main = map_replace 201 [1, 2, 3]",
          "",
        ].join("\n"),
        expectedString: "[201, 201, 201]",
      },
      {
        name: "supported_map_replace_default_maybe_root",
        source: [
          'import "prelude" { Just, map_replace_default }',
          "",
          "export { main }",
          "",
          "main = case map_replace_default 201 (Just 1) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_map_replace_maybe_root",
        source: [
          'import "prelude" { Just, map_replace }',
          "",
          "export { main }",
          "",
          "main = case map_replace 201 (Just 1) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_map_replace_default_list_root",
        source: [
          'import "prelude" { map_replace_default }',
          "",
          "export { main }",
          "",
          "main = map_replace_default 201 [1, 2, 3]",
          "",
        ].join("\n"),
        expectedString: "[201, 201, 201]",
      },
      {
        name: "supported_replace_operator_maybe_root",
        source: [
          'import "prelude" { Just, <$ }',
          "",
          "export { main }",
          "",
          "main = case (201 <$ (Just 1)) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_replace_operator_list_root",
        source: [
          'import "prelude" { <$ }',
          "",
          "export { main }",
          "",
          "main = (201 <$ [1, 2, 3])",
          "",
        ].join("\n"),
        expectedString: "[201, 201, 201]",
      },
      {
        name: "supported_fmap_operator_list_root",
        source: [
          'import "prelude" { <$>, add }',
          "",
          "export { main }",
          "",
          "main = ((\\x -> add x 1) <$> [1, 2, 3])",
          "",
        ].join("\n"),
        expectedString: "[2, 3, 4]",
      },
      {
        name: "supported_fmap_operator_list_nested_root",
        source: [
          'import "prelude" { <$>, add }',
          "",
          "export { main }",
          "",
          "main = case (((\\x -> add x 1) <$> [200])) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_ap_default_maybe_root",
        source: [
          'import "prelude" { Just, ap_default, add }',
          "",
          "export { main }",
          "",
          "main = case ap_default (Just (\\x -> add x 1)) (Just 200) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_ap_default_list_root",
        source: [
          'import "prelude" { ap_default, add }',
          "",
          "export { main }",
          "",
          "main = case ap_default [\\x -> add x 1] [200] of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_ap_list_root",
        source: [
          'import "prelude" { ap, add }',
          "",
          "export { main }",
          "",
          "main = case ap [\\x -> add x 1] [200] of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_bind_list_root",
        source: [
          'import "prelude" { bind, add }',
          "",
          "export { main }",
          "",
          "main = case bind [200] (\\x -> [add x 1]) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_then_m_list_root",
        source: [
          'import "prelude" { then_m, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case then_m (Cons 1 Nil) (Cons 201 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_pure_list_context",
        source: [
          'import "prelude" { append, pure, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case append (pure 201) (Cons 1 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_empty_maybe_context",
        source: [
          'import "prelude" { append, empty, Just }',
          "",
          "export { main }",
          "",
          "main = case append empty (Just 201) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_empty_list_context",
        source: [
          'import "prelude" { append, empty, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case append empty (Cons 201 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_left_list_root",
        source: [
          'import "prelude" { keep_left, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case keep_left (Cons 201 Nil) (Cons 1 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_left_default_list_root",
        source: [
          'import "prelude" { keep_left_default, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case keep_left_default (Cons 201 Nil) (Cons 1 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_left_operator_list_root",
        source: [
          'import "prelude" { <*, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case ((Cons 201 Nil) <* (Cons 1 Nil)) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_right_list_root",
        source: [
          'import "prelude" { keep_right, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case keep_right (Cons 1 Nil) (Cons 201 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_right_default_list_root",
        source: [
          'import "prelude" { keep_right_default, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case keep_right_default (Cons 1 Nil) (Cons 201 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_right_operator_list_root",
        source: [
          'import "prelude" { *>, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case ((Cons 1 Nil) *> (Cons 201 Nil)) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_bind_operator_list_root",
        source: [
          'import "prelude" { >>=, add }',
          "",
          "export { main }",
          "",
          "main = case ([200] >>= (\\x -> [add x 1])) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_alt_named_list_root",
        source: [
          'import "prelude" { alt, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case alt (Cons 201 Nil) (Cons 1 Nil) of Cons x _ -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_pure_maybe_case_context",
        source: [
          'import "prelude" { pure }',
          "",
          "export { main }",
          "",
          "main = case pure 201 of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_pure_maybe_signature_root",
        source: [
          'import "prelude" { pure, Maybe }',
          "",
          "export { main }",
          "",
          "main : Maybe i64",
          "main = pure 201",
          "",
        ].join("\n"),
        expectedString: "Just 201",
      },
      {
        name: "supported_pure_list_signature_root",
        source: [
          'import "prelude" { pure, List }',
          "",
          "export { main }",
          "",
          "main : List i64",
          "main = pure 201",
          "",
        ].join("\n"),
        expectedString: "[201]",
      },
      {
        name: "supported_empty_list_case_context",
        source: [
          'import "prelude" { empty, Cons, Nil }',
          "",
          "export { main }",
          "",
          "main = case empty of Cons x _ -> x; _ -> 404",
          "",
        ].join("\n"),
        expectedTaggedInt: 404,
      },
      {
        name: "supported_empty_list_signature_root",
        source: [
          'import "prelude" { empty, List }',
          "",
          "export { main }",
          "",
          "main : List i64",
          "main = empty",
          "",
        ].join("\n"),
        expectedString: "[]",
      },
      {
        name: "supported_empty_maybe_signature_root",
        source: [
          'import "prelude" { empty, Maybe }',
          "",
          "export { main }",
          "",
          "main : Maybe i64",
          "main = empty",
          "",
        ].join("\n"),
        expectedString: "Nothing",
      },
      {
        name: "supported_keep_left_maybe_root",
        source: [
          'import "prelude" { Just, keep_left }',
          "",
          "export { main }",
          "",
          "main = case keep_left (Just 201) (Just 1) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_left_default_maybe_root",
        source: [
          'import "prelude" { Just, keep_left_default }',
          "",
          "export { main }",
          "",
          "main = case keep_left_default (Just 201) (Just 1) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_left_operator_maybe_root",
        source: [
          'import "prelude" { Just, <* }',
          "",
          "export { main }",
          "",
          "main = case ((Just 201) <* (Just 1)) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_right_maybe_root",
        source: [
          'import "prelude" { Just, keep_right }',
          "",
          "export { main }",
          "",
          "main = case keep_right (Just 1) (Just 201) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_right_default_maybe_root",
        source: [
          'import "prelude" { Just, keep_right_default }',
          "",
          "export { main }",
          "",
          "main = case keep_right_default (Just 1) (Just 201) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_keep_right_operator_maybe_root",
        source: [
          'import "prelude" { Just, *> }',
          "",
          "export { main }",
          "",
          "main = case ((Just 1) *> (Just 201)) of Just x -> x; _ -> 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_map_empty_root",
        source: [
          'import "prelude" { map_empty }',
          "",
          "export { main }",
          "",
          "main = map_empty",
          "",
        ].join("\n"),
        expectedString: "Map []",
      },
      {
        name: "supported_set_empty_root",
        source: [
          'import "prelude" { set_empty }',
          "",
          "export { main }",
          "",
          "main = set_empty",
          "",
        ].join("\n"),
        expectedString: "Set []",
      },
      {
        name: "supported_map_insert_by_root",
        source: [
          'import "prelude" { eq, map_insert_by, map_empty }',
          "",
          "export { main }",
          "",
          'main = map_insert_by eq "POST" 201 map_empty',
          "",
        ].join("\n"),
        expectedString: 'Map [Pair "POST" 201]',
      },
      {
        name: "supported_map_remove_by_root",
        source: [
          'import "prelude" { Pair, eq, map_from_list_by, map_remove_by }',
          "",
          "export { main }",
          "",
          'm = map_from_list_by eq [Pair "POST" 201]',
          'main = map_remove_by eq "POST" m',
          "",
        ].join("\n"),
        expectedString: "Map []",
      },
      {
        name: "supported_ask_reader_root",
        source: [
          'import "prelude" { ask_reader }',
          "",
          "export { main }",
          "",
          "main = ask_reader",
          "",
        ].join("\n"),
        expectedString: "Reader (\\env -> env)",
      },
      {
        name: "supported_local_reader_eval",
        source: [
          'import "prelude" { local_reader, reader_pure, run_reader, add }',
          "",
          "export { main }",
          "",
          "main = run_reader (local_reader (\\x -> add x 1) (reader_pure 200)) 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 200,
      },
      {
        name: "supported_run_state_root",
        source: [
          'import "prelude" { run_state, state_pure }',
          "",
          "export { main }",
          "",
          "main = run_state (state_pure 201) 7",
          "",
        ].join("\n"),
        expectedString: "Pair 201 7",
      },
      {
        name: "supported_state_map_eval",
        source: [
          'import "prelude" { state_map, state_pure, eval_state, add }',
          "",
          "export { main }",
          "",
          "main = eval_state (state_map (\\x -> add x 1) (state_pure 200)) 0",
          "",
        ].join("\n"),
        expectedTaggedInt: 201,
      },
      {
        name: "supported_modify_state_root",
        source: [
          'import "prelude" { modify_state, add }',
          "",
          "export { main }",
          "",
          "main = modify_state (\\x -> add x 1)",
          "",
        ].join("\n"),
        expectedString: "State (\\s -> Pair Unit (add s 1))",
      },
      {
        name: "supported_build_list_root",
        source: [
          'import "prelude" { build }',
          "",
          "export { main }",
          "",
          "main = build (\\cons -> \\nil -> cons 1 (cons 2 nil))",
          "",
        ].join("\n"),
        expectedString: "[1, 2]",
      },
      {
        name: "supported_foldr_list_eval",
        source: [
          'import "prelude" { foldr, add }',
          "",
          "export { main }",
          "",
          "main = foldr add 0 [1, 2, 3]",
          "",
        ].join("\n"),
        expectedTaggedInt: 6,
      },
      {
        name: "supported_foldl_list_eval",
        source: [
          'import "prelude" { foldl, add }',
          "",
          "export { main }",
          "",
          "main = foldl add 0 [1, 2, 3]",
          "",
        ].join("\n"),
        expectedTaggedInt: 6,
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
        name: "unsupported_pure_without_instance_context",
        source: [
          'import "prelude" { pure }',
          "",
          "export { main }",
          "",
          "main = pure 201",
          "",
        ].join("\n"),
        expectedSubstring: "cannot resolve bare pure without instance context",
      },
      {
        name: "unsupported_empty_without_instance_context",
        source: [
          'import "prelude" { empty }',
          "",
          "export { main }",
          "",
          "main = empty",
          "",
        ].join("\n"),
        expectedSubstring: "cannot resolve bare empty without instance context",
      },
      {
        name: "unsupported_pure_with_incompatible_bool_signature",
        source: [
          'import "prelude" { pure }',
          "",
          "export { main }",
          "",
          "main : bool",
          "main = pure 201",
          "",
        ].join("\n"),
        expectedSubstring: "requires an Applicative instance; signature 'bool' is incompatible",
      },
      {
        name: "unsupported_empty_with_incompatible_bool_signature",
        source: [
          'import "prelude" { empty }',
          "",
          "export { main }",
          "",
          "main : bool",
          "main = empty",
          "",
        ].join("\n"),
        expectedSubstring: "requires an Alternative instance; signature 'bool' is incompatible",
      },
    ];
    for (const testCase of unsupportedCases) {
      await assertUnsupportedCompileDebugCase(
        tmpDir,
        testCase.name,
        testCase.source,
        testCase.expectedSubstring,
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
