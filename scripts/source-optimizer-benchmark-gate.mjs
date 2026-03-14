#!/usr/bin/env -S deno run -A

import {
  benchRustBinary,
  benchWasmBoundaryOnly,
  benchWasmCase,
  benchWasmCaseWasmi,
  CASES,
  compileRustBaseline,
  traceRustBinary,
} from "./bench-rust-compare.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";
import { makeClapTempDir } from "./runtime-env.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";
import { callCompilerWasmRaw } from "./wasm-compiler-abi.mjs";

const PRIMARY_CASE_IDS = new Set([
  "numeric-abstraction",
  "http-request-parser",
  "closure-env-abstraction",
  "wrapper-uncurry-abstraction",
]);

const DEFAULT_ITERATIONS = 20000;
const DEFAULT_WARMUP = 2000;
const DEFAULT_REPEATS = 1;
const DEFAULT_TRACE_COUNT = 64;
const PERF_REGRESSION_TOLERANCE = 1.15;
const PERF_REGRESSION_ABS_SLACK_NS = 25;

let wabtPromise = null;

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAP_COMPILER_WASM_PATH") ?? "").trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/latest/clap_compiler.wasm";
}

function usage() {
  return [
    "Source-owned optimizer benchmark gate",
    "",
    "Usage:",
    "  deno run -A scripts/source-optimizer-benchmark-gate.mjs [--baseline-wasm path] [--iterations N] [--warmup N] [--repeats N]",
    "",
    "Compiles the representative benchmark fixtures through compile_mode=native-debug,",
    "requires source-owned optimizer artifacts, verifies checksum parity against Rust",
    "and both wasm hosts, and checks source-owned code-shape claims.",
  ].join("\n");
}

function fail(message) {
  console.error(`source-optimizer-benchmark-gate: FAIL (${message})`);
  Deno.exit(1);
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function parsePositiveInt(raw, label) {
  const n = Number(raw);
  if (!Number.isInteger(n) || n <= 0) {
    throw new Error(`${label} must be a positive integer, got ${raw}`);
  }
  return n;
}

function parseArgs(args) {
  let baselineWasmPath = "";
  let iterations = DEFAULT_ITERATIONS;
  let warmup = DEFAULT_WARMUP;
  let repeats = DEFAULT_REPEATS;
  for (let i = 0; i < args.length; i += 1) {
    const token = String(args[i] ?? "").trim();
    if (token === "--help" || token === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (token === "--baseline-wasm") {
      baselineWasmPath = String(args[i + 1] ?? "").trim();
      i += 1;
      continue;
    }
    if (token === "--iterations") {
      iterations = parsePositiveInt(args[i + 1], "iterations");
      i += 1;
      continue;
    }
    if (token === "--warmup") {
      warmup = parsePositiveInt(args[i + 1], "warmup");
      i += 1;
      continue;
    }
    if (token === "--repeats") {
      repeats = parsePositiveInt(args[i + 1], "repeats");
      i += 1;
      continue;
    }
    throw new Error(`unknown argument '${token}'`);
  }
  return { baselineWasmPath, iterations, warmup, repeats };
}

async function loadWabt() {
  if (wabtPromise === null) {
    wabtPromise = import("npm:wabt").then(async (mod) => await mod.default());
  }
  return await wabtPromise;
}

async function wasmToWat(wasmBytes) {
  const wabt = await loadWabt();
  const module = wabt.readWasm(wasmBytes, { readDebugNames: true });
  try {
    module.generateNames();
    module.applyNames();
    return module.toText({
      foldExprs: false,
      inlineExport: false,
    });
  } finally {
    module.destroy();
  }
}

function decodeBase64(base64) {
  return Uint8Array.from(atob(base64), (char) => char.charCodeAt(0));
}

function countFunctionsInWat(wat) {
  return (wat.match(/\(func\b/gu) ?? []).length;
}

function opcodeHistogram(wat) {
  const opcodes = [
    "call",
    "local.get",
    "local.set",
    "local.tee",
    "i32.add",
    "i32.sub",
    "i32.mul",
    "i32.div_s",
    "i32.rem_s",
  ];
  const histogram = {};
  for (const opcode of opcodes) {
    const escaped = opcode.replace(".", "\\.");
    histogram[opcode] = (wat.match(new RegExp(`\\b${escaped}\\b`, "gu")) ?? []).length;
  }
  return histogram;
}

function parseNonNegativeIntegerField(line, prefix, fixtureId) {
  if (!line.startsWith(prefix)) {
    fail(`${fixtureId}: malformed codegen_ir line, expected prefix ${JSON.stringify(prefix)} got ${JSON.stringify(line)}`);
  }
  const raw = line.slice(prefix.length).trim();
  const value = Number(raw);
  if (!Number.isInteger(value) || value < 0) {
    fail(`${fixtureId}: malformed codegen_ir integer for ${prefix}: ${JSON.stringify(raw)}`);
  }
  return value;
}

function parseBulletSection(lines, startIndex) {
  const values = [];
  let index = startIndex;
  while (index < lines.length) {
    const line = lines[index].trimEnd();
    if (!line.startsWith("- ")) {
      break;
    }
    const value = line.slice(2).trim();
    if (value.length === 0) {
      break;
    }
    values.push(value);
    index += 1;
  }
  return { values, nextIndex: index };
}

function parseCodegenIr(codegenIr, fixtureId) {
  const lines = codegenIr.replace(/\r/gu, "").split("\n");
  let index = 0;
  const expectedHeader = [
    "(codegen_ir)",
    "phase: kernel-native-phase1",
    "kind: function-plan",
  ];
  for (const expected of expectedHeader) {
    const line = String(lines[index] ?? "");
    assert(line === expected, `${fixtureId}: codegen_ir header mismatch at line ${index + 1}: expected ${JSON.stringify(expected)} got ${JSON.stringify(line)}`);
    index += 1;
  }
  const baselineFunctionCount = parseNonNegativeIntegerField(
    String(lines[index] ?? ""),
    "baseline_function_count:",
    fixtureId,
  );
  index += 1;
  const baselineHelperCount = parseNonNegativeIntegerField(
    String(lines[index] ?? ""),
    "baseline_helper_count:",
    fixtureId,
  );
  index += 1;
  const optimizedFunctionCount = parseNonNegativeIntegerField(
    String(lines[index] ?? ""),
    "optimized_function_count:",
    fixtureId,
  );
  index += 1;
  const optimizedHelperCount = parseNonNegativeIntegerField(
    String(lines[index] ?? ""),
    "optimized_helper_count:",
    fixtureId,
  );
  index += 1;
  assert(String(lines[index] ?? "") === "baseline_functions:",
    `${fixtureId}: missing baseline_functions: section in codegen_ir`);
  index += 1;
  const baselineSection = parseBulletSection(lines, index);
  index = baselineSection.nextIndex;
  assert(String(lines[index] ?? "") === "emitted_functions:",
    `${fixtureId}: missing emitted_functions: section in codegen_ir`);
  index += 1;
  const emittedSection = parseBulletSection(lines, index);
  assert(
    baselineSection.values.length === baselineFunctionCount,
    `${fixtureId}: codegen_ir baseline function list length ${baselineSection.values.length} != baseline_function_count ${baselineFunctionCount}`,
  );
  assert(
    emittedSection.values.length === optimizedFunctionCount,
    `${fixtureId}: codegen_ir emitted function list length ${emittedSection.values.length} != optimized_function_count ${optimizedFunctionCount}`,
  );
  return {
    baselineFunctionCount,
    baselineHelperCount,
    optimizedFunctionCount,
    optimizedHelperCount,
    baselineNames: baselineSection.values,
    emittedNames: emittedSection.values,
  };
}

function makeTaggedArgPools(arity) {
  const poolSize = 1024;
  const pools = [];
  for (let argIx = 0; argIx < arity; argIx += 1) {
    const pool = new Array(poolSize);
    for (let i = 0; i < poolSize; i += 1) {
      pool[i] = encodeInt((i + argIx * 31) & 1023);
    }
    pools.push(pool);
  }
  return pools;
}

function argsForIteration(pools, iteration) {
  if (pools.length === 0) {
    return [];
  }
  const idx = iteration & 1023;
  const args = new Array(pools.length);
  for (let argIx = 0; argIx < pools.length; argIx += 1) {
    args[argIx] = pools[argIx][idx];
  }
  return args;
}

async function traceWasmJs(wasmPath, count, exportName = "main") {
  const wasmBytes = await Deno.readFile(wasmPath);
  const { instance } = await instantiateWithRuntime(wasmBytes);
  const fn = instance.exports[exportName];
  if (typeof fn !== "function") {
    throw new Error(`export '${exportName}' not found in ${wasmPath}`);
  }
  const pools = makeTaggedArgPools(fn.length | 0);
  const out = new Array(count);
  for (let i = 0; i < count; i += 1) {
    out[i] = decodeInt(fn(...argsForIteration(pools, i)) | 0) | 0;
  }
  return out;
}

async function sampledWasmiChecksums(wasmPath, exportName = "main") {
  const sampleCounts = [DEFAULT_TRACE_COUNT, 257];
  const checksums = [];
  for (const count of sampleCounts) {
    const result = await benchWasmCaseWasmi(wasmPath, count, 0, exportName);
    checksums.push(result.checksum | 0);
  }
  return checksums;
}

function arrayEq(a, b) {
  if (a.length !== b.length) {
    return false;
  }
  for (let i = 0; i < a.length; i += 1) {
    if ((a[i] | 0) !== (b[i] | 0)) {
      return false;
    }
  }
  return true;
}

function checksumFromTrace(values) {
  let acc = 0;
  for (const value of values) {
    acc = (acc + (value | 0)) | 0;
  }
  return acc | 0;
}

function maxAllowedRegression(baselineNetNs) {
  return Math.max(
    PERF_REGRESSION_ABS_SLACK_NS,
    baselineNetNs * PERF_REGRESSION_TOLERANCE,
  );
}

function medianNs(results) {
  const sorted = [...results].sort((left, right) => left.nsPerCall - right.nsPerCall);
  return sorted[(sorted.length / 2) | 0];
}

async function medianResult(runOne, repeats) {
  const results = [];
  for (let i = 0; i < repeats; i += 1) {
    results.push(await runOne());
  }
  return medianNs(results);
}

async function compileSourceOwnedCase(compilerWasmPath, fixture) {
  const source = await Deno.readTextFile(fixture.clapInputPath);
  const response = await callCompilerWasmRaw(
    compilerWasmPath,
    {
      command: "compile",
      compile_mode: "native-debug",
      input_path: fixture.clapInputPath,
      input_source: source,
    },
    {
      validateCompileContract: true,
      withContractMetadata: true,
    },
  );
  assert(response && typeof response === "object",
    `${fixture.id}: response must be an object`);
  assert(response.ok === true,
    `${fixture.id}: compile failed (${String(response.error_code ?? response.error ?? "unknown")})`);
  assert(response.backend === "kernel-native",
    `${fixture.id}: expected kernel-native backend, got ${JSON.stringify(response.backend)}`);
  assert(response.compatibility_used !== true,
    `${fixture.id}: used compatibility path (${String(response.compile_strategy)})`);
  const compileStrategy = String(response.compile_strategy ?? "");
  assert(
    compileStrategy === "phase1_passthrough" || compileStrategy === "compiler_raw",
    `${fixture.id}: not yet source-owned (compile_strategy=${compileStrategy})`,
  );
  const artifacts = response.artifacts ?? {};
  assert(typeof artifacts["lowered_ir.txt"] === "string",
    `${fixture.id}: missing lowered_ir.txt`);
  assert(typeof artifacts["collapsed_ir.txt"] === "string",
    `${fixture.id}: missing collapsed_ir.txt`);
  assert(typeof artifacts["codegen_ir.txt"] === "string",
    `${fixture.id}: missing codegen_ir.txt`);
  assert(typeof artifacts["optimizer_stats.json"] === "string",
    `${fixture.id}: missing optimizer_stats.json`);
  assertStructuralArtifacts(
    artifacts["lowered_ir.txt"],
    artifacts["collapsed_ir.txt"],
    {
      context: `${fixture.id}: source-owned compile artifacts`,
      allowLegacyHeaderPrefix: true,
    },
  );
  const stats = JSON.parse(artifacts["optimizer_stats.json"]);
  assert(stats && typeof stats === "object",
    `${fixture.id}: optimizer_stats.json must decode to an object`);
  assert(stats.source_owned === true,
    `${fixture.id}: optimizer_stats must mark source_owned=true`);
  assert(stats.status === "ready",
    `${fixture.id}: optimizer_stats status must be 'ready', got ${JSON.stringify(stats.status)}`);
  const codegenSummary = parseCodegenIr(artifacts["codegen_ir.txt"], fixture.id);
  const wasmBase64 = String(response.wasm_base64 ?? "");
  assert(wasmBase64.length > 0,
    `${fixture.id}: missing wasm_base64`);
  const wasmBytes = decodeBase64(wasmBase64);
  const tmpDir = await makeClapTempDir(`clap-source-optimizer-${fixture.id}-`);
  const wasmPath = `${tmpDir}/${fixture.id}.wasm`;
  await Deno.writeFile(wasmPath, wasmBytes);
  const wat = await wasmToWat(wasmBytes);
  const functionCount = countFunctionsInWat(wat);
  const exportCount = WebAssembly.Module.exports(new WebAssembly.Module(wasmBytes))
    .filter((entry) => entry.kind === "function").length;
  return {
    response,
    artifacts,
    stats,
    wasmBytes,
    wasmPath,
    wat,
    functionCount,
    exportCount,
    codegenSummary,
    histogram: opcodeHistogram(wat),
    cleanup: async () => {
      await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
    },
  };
}

function histogramString(histogram) {
  return Object.entries(histogram).map(([key, value]) => `${key}=${value}`).join(", ");
}

async function verifyChecksums(fixture, wasmPath, rustBinaryPath, iterations, warmup, repeats) {
  const rustResult = await medianResult(
    () => benchRustBinary(rustBinaryPath, fixture.rustCase, iterations, warmup),
    repeats,
  );
  const wasmResult = await medianResult(
    () => benchWasmCase(wasmPath, iterations, warmup),
    repeats,
  );
  const wasmiResult = await medianResult(
    () => benchWasmCaseWasmi(wasmPath, iterations, warmup),
    repeats,
  );
  assert(wasmResult.checksum === rustResult.checksum,
    `${fixture.id}: JS-host wasm checksum ${wasmResult.checksum} != Rust ${rustResult.checksum}`);
  assert(wasmiResult.checksum === rustResult.checksum,
    `${fixture.id}: wasmi checksum ${wasmiResult.checksum} != Rust ${rustResult.checksum}`);
  const rustTrace = await traceRustBinary(rustBinaryPath, fixture.rustCase, DEFAULT_TRACE_COUNT);
  const wasmTrace = await traceWasmJs(wasmPath, DEFAULT_TRACE_COUNT, "main");
  assert(arrayEq(wasmTrace, rustTrace),
    `${fixture.id}: JS-host wasm trace diverges from Rust trace`);
  const rustTraceChecksumA = checksumFromTrace(rustTrace);
  const rustTrace257 = await traceRustBinary(rustBinaryPath, fixture.rustCase, 257);
  const rustTraceChecksumB = checksumFromTrace(rustTrace257);
  const wasmiChecksums = await sampledWasmiChecksums(wasmPath, "main");
  assert(wasmiChecksums[0] === rustTraceChecksumA,
    `${fixture.id}: wasmi sampled checksum[${DEFAULT_TRACE_COUNT}] ${wasmiChecksums[0]} != Rust ${rustTraceChecksumA}`);
  assert(wasmiChecksums[1] === rustTraceChecksumB,
    `${fixture.id}: wasmi sampled checksum[257] ${wasmiChecksums[1]} != Rust ${rustTraceChecksumB}`);
  return { rustResult, wasmResult, wasmiResult };
}

async function main() {
  const { baselineWasmPath, iterations, warmup, repeats } = parseArgs(Deno.args);
  const compilerWasmPath = resolveCompilerWasmPath();
  const fixtures = CASES.filter((fixture) => PRIMARY_CASE_IDS.has(fixture.id));
  const rustTmpDir = await makeClapTempDir("clap-source-optimizer-rust-");
  const rustBinaryPath = await compileRustBaseline(rustTmpDir);
  const jsBoundary = await medianResult(
    () => benchWasmBoundaryOnly(iterations, warmup),
    repeats,
  );
  const boundaryOnlyPath = await awaitBoundaryOnlyPath();
  const nativeBoundary = await medianResult(
    () => benchWasmCaseWasmi(boundaryOnlyPath, iterations, warmup, "boundary_id"),
    repeats,
  );
  let sawStructuralReduction = false;
  const cleanupFns = [];
  try {
    console.log(`source-optimizer-benchmark-gate: compiler=${compilerWasmPath}`);
    if (baselineWasmPath.length > 0) {
      console.log(`source-optimizer-benchmark-gate: baseline=${baselineWasmPath}`);
    }
    console.log(
      `source-optimizer-benchmark-gate: wasm-boundary-only=${jsBoundary.nsPerCall.toFixed(2)} ns/call; wasmi-boundary-only=${nativeBoundary.nsPerCall.toFixed(2)} ns/call`,
    );
    for (const fixture of fixtures) {
      const candidate = await compileSourceOwnedCase(compilerWasmPath, fixture);
      cleanupFns.push(candidate.cleanup);
      assert(candidate.stats.optimized_function_count <= candidate.stats.baseline_function_count,
        `${fixture.id}: optimized_function_count grew (${candidate.stats.optimized_function_count} > ${candidate.stats.baseline_function_count})`);
      assert(candidate.stats.optimized_helper_count <= candidate.stats.baseline_helper_count,
        `${fixture.id}: optimized_helper_count grew (${candidate.stats.optimized_helper_count} > ${candidate.stats.baseline_helper_count})`);
      assert(candidate.codegenSummary.baselineFunctionCount === candidate.stats.baseline_function_count,
        `${fixture.id}: codegen_ir baseline_function_count ${candidate.codegenSummary.baselineFunctionCount} != optimizer_stats ${candidate.stats.baseline_function_count}`);
      assert(candidate.codegenSummary.baselineHelperCount === candidate.stats.baseline_helper_count,
        `${fixture.id}: codegen_ir baseline_helper_count ${candidate.codegenSummary.baselineHelperCount} != optimizer_stats ${candidate.stats.baseline_helper_count}`);
      assert(candidate.codegenSummary.optimizedFunctionCount === candidate.stats.optimized_function_count,
        `${fixture.id}: codegen_ir optimized_function_count ${candidate.codegenSummary.optimizedFunctionCount} != optimizer_stats ${candidate.stats.optimized_function_count}`);
      assert(candidate.codegenSummary.optimizedHelperCount === candidate.stats.optimized_helper_count,
        `${fixture.id}: codegen_ir optimized_helper_count ${candidate.codegenSummary.optimizedHelperCount} != optimizer_stats ${candidate.stats.optimized_helper_count}`);
      assert(candidate.functionCount === candidate.stats.optimized_function_count,
        `${fixture.id}: WAT function count ${candidate.functionCount} != optimizer_stats optimized_function_count ${candidate.stats.optimized_function_count}`);
      assert(candidate.exportCount === candidate.stats.export_count,
        `${fixture.id}: wasm export count ${candidate.exportCount} != optimizer_stats export_count ${candidate.stats.export_count}`);
      if (
        candidate.stats.optimized_function_count < candidate.stats.baseline_function_count ||
        candidate.stats.optimized_helper_count < candidate.stats.baseline_helper_count
      ) {
        sawStructuralReduction = true;
      }
      const checksums = await verifyChecksums(
        fixture,
        candidate.wasmPath,
        rustBinaryPath,
        iterations,
        warmup,
        repeats,
      );
      const jsNet = Math.max(0, checksums.wasmResult.nsPerCall - jsBoundary.nsPerCall);
      const nativeNet = Math.max(0, checksums.wasmiResult.nsPerCall - nativeBoundary.nsPerCall);
      console.log(
        `${fixture.id}: wasm_bytes=${candidate.wasmBytes.length} functions=${candidate.functionCount}/${candidate.stats.baseline_function_count} helpers=${candidate.stats.optimized_helper_count}/${candidate.stats.baseline_helper_count} js=${checksums.wasmResult.nsPerCall.toFixed(2)}ns net=${jsNet.toFixed(2)}ns wasmi=${checksums.wasmiResult.nsPerCall.toFixed(2)}ns native-net=${nativeNet.toFixed(2)}ns histogram=[${histogramString(candidate.histogram)}] rust_checksum=${checksums.rustResult.checksum}`,
      );
      if (baselineWasmPath.length > 0) {
        const baseline = await compileSourceOwnedCase(baselineWasmPath, fixture);
        cleanupFns.push(baseline.cleanup);
        assert(candidate.wasmBytes.length <= baseline.wasmBytes.length,
          `${fixture.id}: candidate wasm grew (${candidate.wasmBytes.length} > ${baseline.wasmBytes.length}) compared with baseline compiler`);
        assert(candidate.functionCount <= baseline.functionCount,
          `${fixture.id}: candidate function count grew (${candidate.functionCount} > ${baseline.functionCount}) compared with baseline compiler`);
        const baselineChecksums = await verifyChecksums(
          fixture,
          baseline.wasmPath,
          rustBinaryPath,
          iterations,
          warmup,
          repeats,
        );
        const baselineJsNet = Math.max(0, baselineChecksums.wasmResult.nsPerCall - jsBoundary.nsPerCall);
        const baselineNativeNet = Math.max(
          0,
          baselineChecksums.wasmiResult.nsPerCall - nativeBoundary.nsPerCall,
        );
        assert(
          jsNet <= maxAllowedRegression(baselineJsNet),
          `${fixture.id}: candidate JS net ns/call regressed (${jsNet.toFixed(2)} > ${maxAllowedRegression(baselineJsNet).toFixed(2)})`,
        );
        assert(
          nativeNet <= maxAllowedRegression(baselineNativeNet),
          `${fixture.id}: candidate wasmi net ns/call regressed (${nativeNet.toFixed(2)} > ${maxAllowedRegression(baselineNativeNet).toFixed(2)})`,
        );
        console.log(
          `${fixture.id}: baseline_compare wasm_bytes ${baseline.wasmBytes.length} -> ${candidate.wasmBytes.length}, functions ${baseline.functionCount} -> ${candidate.functionCount}, js-net ${baselineJsNet.toFixed(2)}ns -> ${jsNet.toFixed(2)}ns, native-net ${baselineNativeNet.toFixed(2)}ns -> ${nativeNet.toFixed(2)}ns`,
        );
      }
    }
    assert(sawStructuralReduction,
      "no representative case showed helper/function reduction in source-owned optimizer stats");
    console.log(`source-optimizer-benchmark-gate: PASS (${fixtures.length} cases)`);
  } finally {
    for (const cleanup of cleanupFns.reverse()) {
      await cleanup();
    }
    await Deno.remove(rustTmpDir, { recursive: true }).catch(() => {});
  }
}

let boundaryOnlyPathPromise = null;

async function awaitBoundaryOnlyPath() {
  if (boundaryOnlyPathPromise === null) {
    boundaryOnlyPathPromise = (async () => {
      const tmpDir = await makeClapTempDir("clap-source-optimizer-boundary-");
      const wasmPath = `${tmpDir}/boundary.wasm`;
      const bytes = new Uint8Array([
        0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
        0x01, 0x06, 0x01, 0x60, 0x01, 0x7f, 0x01, 0x7f,
        0x03, 0x02, 0x01, 0x00,
        0x07, 0x0f, 0x01, 0x0b, 0x62, 0x6f, 0x75, 0x6e, 0x64, 0x61, 0x72, 0x79, 0x5f, 0x69, 0x64, 0x00, 0x00,
        0x0a, 0x06, 0x01, 0x04, 0x00, 0x20, 0x00, 0x0b,
      ]);
      await Deno.writeFile(wasmPath, bytes);
      return wasmPath;
    })();
  }
  return await boundaryOnlyPathPromise;
}

if (import.meta.main) {
  await main().catch((error) => {
    fail(error instanceof Error ? error.message : String(error));
  });
}
