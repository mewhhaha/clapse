#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";

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
    assertStructuralArtifacts(baseline.lowered, baseline.collapsed, {
      context: "native-entrypoint-exports-dce-gate: baseline",
      requiredDefs: ["main", "keep", "helper", "dead_fn", "+."],
    });

    const subsetResponse = await callCompilerWasmRaw(
      wasmPath,
      buildCompileRequest(inputPath, source, ["main"]),
      {
        validateCompileContract: true,
        withContractMetadata: true,
      },
    );
    const subset = readCompileArtifactsOrThrow(subsetResponse, "subset");
    assertStructuralArtifacts(subset.lowered, subset.collapsed, {
      context: "native-entrypoint-exports-dce-gate: subset-main",
      requiredDefs: ["main", "keep"],
      forbiddenDefs: ["helper", "dead_fn", "+."],
    });
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
    assertStructuralArtifacts(operatorRoot.lowered, operatorRoot.collapsed, {
      context: "native-entrypoint-exports-dce-gate: subset-operator",
      requiredDefs: ["+.", "helper"],
      forbiddenDefs: ["main", "keep"],
    });

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
    assertStructuralArtifacts(preludeBaseline.lowered, preludeBaseline.collapsed, {
      context: "native-entrypoint-exports-dce-gate: prelude-baseline",
      requiredDefs: ["main", "numbers", "dead_bool", "dead_maybe"],
    });

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
    assertStructuralArtifacts(preludeSubset.lowered, preludeSubset.collapsed, {
      context: "native-entrypoint-exports-dce-gate: prelude-subset",
      requiredDefs: ["main", "numbers"],
      forbiddenDefs: ["dead_bool", "dead_maybe"],
    });
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
