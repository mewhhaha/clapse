#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "")
    .trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/latest/clapse_compiler.wasm";
}

function isObject(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function readCompileArtifactsOrThrow(response, label) {
  assert(
    isObject(response),
    `native-entrypoint-exports-dce-gate: ${label} response must be an object`,
  );
  assert(
    response.ok === true,
    `native-entrypoint-exports-dce-gate: ${label} response must be ok=true`,
  );
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `native-entrypoint-exports-dce-gate: ${label} response missing wasm_base64`,
  );
  const artifacts = response.artifacts;
  assert(
    isObject(artifacts),
    `native-entrypoint-exports-dce-gate: ${label} response missing artifacts`,
  );
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  assert(
    typeof lowered === "string" && lowered.length > 0,
    `native-entrypoint-exports-dce-gate: ${label} lowered_ir.txt missing`,
  );
  assert(
    typeof collapsed === "string" && collapsed.length > 0,
    `native-entrypoint-exports-dce-gate: ${label} collapsed_ir.txt missing`,
  );
  return {
    wasmBytes: decodeWasmBase64(response.wasm_base64),
    lowered,
    collapsed,
  };
}

function assertErrorResponse(response, label, expectedErrorFragment) {
  assert(
    isObject(response),
    `native-entrypoint-exports-dce-gate: ${label} response must be an object`,
  );
  assert(
    response.ok === false,
    `native-entrypoint-exports-dce-gate: ${label} response must be ok=false`,
  );
  const errorText = String(response.error ?? "");
  assert(
    errorText.includes(expectedErrorFragment),
    `native-entrypoint-exports-dce-gate: ${label} error should include '${expectedErrorFragment}', got '${errorText}'`,
  );
}

function escapeRegExp(text) {
  return text.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
}

function containsToken(text, token) {
  const pattern = new RegExp(
    `(?:^|[^A-Za-z0-9_])${escapeRegExp(token)}(?:$|[^A-Za-z0-9_])`,
    "u",
  );
  return pattern.test(text);
}

function buildCompileRequest(inputPath, source, entrypointExports = null) {
  const request = {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
  };
  if (Array.isArray(entrypointExports) && entrypointExports.length > 0) {
    request.entrypoint_exports = [...entrypointExports];
  }
  return request;
}

async function run() {
  const wasmPath = resolveCompilerWasmPath();
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-native-entrypoint-exports-dce-gate-",
  });
  try {
    const inputPath = `${tmpDir}/entrypoint_exports_gate.clapse`;
    const deadMarker = `native-entrypoint-exports-dead-${crypto.randomUUID()}`;
    const operatorMarker = `native-entrypoint-exports-op-${crypto.randomUUID()}`;
    const source = [
      "export { main, helper, +. }",
      "main x = keep x",
      "keep x = x",
      `helper x = dead_fn x -- ${deadMarker}`,
      "dead_fn x = x",
      `+. x y = helper x -- ${operatorMarker}`,
      "",
    ].join("\n");
    await Deno.writeTextFile(inputPath, source);

    const baselineResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source),
      {
        validateCompileContract: true,
        withContractMetadata: true,
      },
    );
    const baseline = readCompileArtifactsOrThrow(baselineResponse, "baseline");
    const baselineHasDead = baseline.lowered.includes(deadMarker) ||
      baseline.collapsed.includes(deadMarker);
    assert(
      baselineHasDead,
      "native-entrypoint-exports-dce-gate: baseline artifacts should contain helper/dead marker",
    );

    const subsetResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source, ["main"]),
      {
        validateCompileContract: true,
        withContractMetadata: true,
      },
    );
    const subset = readCompileArtifactsOrThrow(subsetResponse, "subset");
    const subsetHasDead = subset.lowered.includes(deadMarker) ||
      subset.collapsed.includes(deadMarker);
    const subsetHasOperator = subset.lowered.includes(operatorMarker) ||
      subset.collapsed.includes(operatorMarker);
    assert(
      !subsetHasDead,
      "native-entrypoint-exports-dce-gate: subset-root artifacts should prune helper/dead marker",
    );
    assert(
      !subsetHasOperator,
      "native-entrypoint-exports-dce-gate: subset-root artifacts should prune operator marker",
    );
    assert(
      subset.wasmBytes.length < baseline.wasmBytes.length,
      `native-entrypoint-exports-dce-gate: subset-root wasm bytes should strictly shrink (${subset.wasmBytes.length} >= ${baseline.wasmBytes.length})`,
    );

    const operatorRootResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source, ["+."]),
      {
        validateCompileContract: true,
        withContractMetadata: true,
      },
    );
    const operatorRoot = readCompileArtifactsOrThrow(
      operatorRootResponse,
      "operator-root",
    );
    const operatorRootHasOperator = operatorRoot.lowered.includes(
      operatorMarker,
    ) || operatorRoot.collapsed.includes(operatorMarker);
    assert(
      operatorRootHasOperator,
      "native-entrypoint-exports-dce-gate: operator-root artifacts should keep operator marker",
    );

    const preludeInputPath = `${tmpDir}/entrypoint_exports_prelude_gate.clapse`;
    const preludeBoolDeadMarker =
      `native-entrypoint-exports-prelude-bool-${crypto.randomUUID()}`;
    const preludeMaybeDeadMarker =
      `native-entrypoint-exports-prelude-maybe-${crypto.randomUUID()}`;
    const preludeSource = [
      'import "prelude"',
      "",
      "export { main, dead_bool, dead_maybe }",
      "numbers = Cons 1 (Cons 2 (Cons 3 Nil))",
      "main = foldl (+) 0 (fmap (\\x -> x + 1) numbers)",
      `dead_bool = and true false -- ${preludeBoolDeadMarker}`,
      `dead_maybe = maybe_with_default 0 (Just 7) -- ${preludeMaybeDeadMarker}`,
      "",
    ].join("\n");
    await Deno.writeTextFile(preludeInputPath, preludeSource);

    const preludeBaselineResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(preludeInputPath, preludeSource),
      {
        validateCompileContract: true,
        withContractMetadata: true,
      },
    );
    const preludeBaseline = readCompileArtifactsOrThrow(
      preludeBaselineResponse,
      "prelude-baseline",
    );
    const preludeBaselineHasDead = preludeBaseline.lowered.includes(
      preludeBoolDeadMarker,
    ) || preludeBaseline.collapsed.includes(preludeBoolDeadMarker) ||
      preludeBaseline.lowered.includes(preludeMaybeDeadMarker) ||
      preludeBaseline.collapsed.includes(preludeMaybeDeadMarker);
    assert(
      preludeBaselineHasDead,
      "native-entrypoint-exports-dce-gate: prelude-baseline artifacts should retain non-list prelude dead markers",
    );
    const preludeBaselineHasBoolSymbol = containsToken(
      preludeBaseline.lowered,
      "and",
    ) || containsToken(preludeBaseline.collapsed, "and") ||
      containsToken(preludeBaseline.lowered, "bool_and") ||
      containsToken(preludeBaseline.collapsed, "bool_and");
    assert(
      preludeBaselineHasBoolSymbol,
      "native-entrypoint-exports-dce-gate: prelude-baseline artifacts should retain bool/and symbol for dead export",
    );
    const preludeBaselineHasMaybeSymbol = containsToken(
      preludeBaseline.lowered,
      "maybe_with_default",
    ) || containsToken(preludeBaseline.collapsed, "maybe_with_default");
    assert(
      preludeBaselineHasMaybeSymbol,
      "native-entrypoint-exports-dce-gate: prelude-baseline artifacts should retain maybe_with_default symbol for dead export",
    );

    const preludeSubsetResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(preludeInputPath, preludeSource, ["main"]),
      {
        validateCompileContract: true,
        withContractMetadata: true,
      },
    );
    const preludeSubset = readCompileArtifactsOrThrow(
      preludeSubsetResponse,
      "prelude-subset",
    );
    const preludeSubsetHasDead = preludeSubset.lowered.includes(
      preludeBoolDeadMarker,
    ) || preludeSubset.collapsed.includes(preludeBoolDeadMarker) ||
      preludeSubset.lowered.includes(preludeMaybeDeadMarker) ||
      preludeSubset.collapsed.includes(preludeMaybeDeadMarker);
    assert(
      !preludeSubsetHasDead,
      "native-entrypoint-exports-dce-gate: prelude-subset artifacts should prune non-list prelude dead markers",
    );
    const preludeSubsetHasBoolSymbol = containsToken(
      preludeSubset.lowered,
      "and",
    ) || containsToken(preludeSubset.collapsed, "and") ||
      containsToken(preludeSubset.lowered, "bool_and") ||
      containsToken(preludeSubset.collapsed, "bool_and");
    assert(
      !preludeSubsetHasBoolSymbol,
      "native-entrypoint-exports-dce-gate: prelude-subset artifacts should prune bool/and symbol",
    );
    const preludeSubsetHasMaybeSymbol = containsToken(
      preludeSubset.lowered,
      "maybe_with_default",
    ) || containsToken(preludeSubset.collapsed, "maybe_with_default");
    assert(
      !preludeSubsetHasMaybeSymbol,
      "native-entrypoint-exports-dce-gate: prelude-subset artifacts should prune maybe_with_default symbol",
    );
    assert(
      preludeSubset.wasmBytes.length < preludeBaseline.wasmBytes.length,
      `native-entrypoint-exports-dce-gate: prelude subset wasm bytes should strictly shrink (${preludeSubset.wasmBytes.length} >= ${preludeBaseline.wasmBytes.length})`,
    );

    const unknownRootResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source, ["missing_entrypoint_root"]),
      {
        validateCompileContract: true,
        withContractMetadata: true,
      },
    );
    assertErrorResponse(
      unknownRootResponse,
      "unknown-root",
      "unknown entrypoint root",
    );

    console.log(
      `native-entrypoint-exports-dce-gate: PASS (baseline=${baseline.wasmBytes.length}; subset-main=${subset.wasmBytes.length}; subset-operator=${operatorRoot.wasmBytes.length}; roots=main,+.)`,
    );
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await run();
