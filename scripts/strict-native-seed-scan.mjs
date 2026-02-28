#!/usr/bin/env -S deno run -A

import { callCompilerWasm } from "./wasm-compiler-abi.mjs";

const DEFAULT_MAX_BYTES = 5 * 1024 * 1024;
const DEFAULT_MARKER = "strict_native_emit_wat_marker";
const DEFAULT_COMPILE_SOURCE = "main x = x\n";
const REQUIRE_NO_BOUNDARY_FALLBACK_ENV =
  "CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/strict-native-seed-scan.mjs [options]",
    "",
    "Options:",
    "  --scan-root <dir>    additional scan root (repeatable)",
    "  --max-bytes <n>      max wasm size to scan (default 5242880)",
    "  --allow-empty        exit 0 when no strict-native seed is found",
    "  --require-no-boundary-fallback",
    `                      fail candidates that require JS ABI tiny-output fallback (env: ${REQUIRE_NO_BOUNDARY_FALLBACK_ENV}=1)`,
    "  --json               print only JSON summary",
    "  --help, -h           show help",
  ].join("\n");
}

function normalizePath(path) {
  return String(path).replace(/\\\\/g, "/");
}

function pathJoin(base, leaf) {
  const a = normalizePath(base).replace(/\/$/u, "");
  const b = normalizePath(leaf).replace(/^\//u, "");
  return `${a}/${b}`;
}

async function isDir(path) {
  try {
    const stat = await Deno.stat(path);
    return stat.isDirectory;
  } catch {
    return false;
  }
}

async function walkWasmFiles(root, out, seenDirs = new Set()) {
  const normalized = normalizePath(root);
  if (seenDirs.has(normalized)) {
    return;
  }
  seenDirs.add(normalized);
  let entries;
  try {
    entries = [];
    for await (const entry of Deno.readDir(normalized)) {
      entries.push(entry);
    }
  } catch {
    return;
  }
  for (const entry of entries) {
    const child = pathJoin(normalized, entry.name);
    if (entry.isDirectory) {
      await walkWasmFiles(child, out, seenDirs);
      continue;
    }
    if (entry.isFile && child.endsWith(".wasm")) {
      out.push(child);
    }
  }
}

async function hasCompilerAbi(path) {
  let bytes;
  try {
    bytes = await Deno.readFile(path);
  } catch {
    return { ok: false, reason: "file-read-failed" };
  }
  let module;
  try {
    module = await WebAssembly.compile(bytes);
  } catch {
    return { ok: false, reason: "invalid-wasm" };
  }
  const exports = WebAssembly.Module.exports(module);
  const hasRun = exports.some((entry) =>
    entry.kind === "function" && entry.name === "clapse_run"
  );
  const hasMemory = exports.some((entry) =>
    entry.kind === "memory" &&
    (entry.name === "memory" || entry.name === "__memory")
  );
  if (!hasRun || !hasMemory) {
    return {
      ok: false,
      reason: hasRun ? "missing-memory-export" : "missing-clapse_run-export",
    };
  }
  return { ok: true };
}

function nonEmptyString(value) {
  return typeof value === "string" && value.length > 0;
}

function boolEnvFlag(name, defaultValue = false) {
  const raw = String(Deno.env.get(name) ?? "").trim().toLowerCase();
  if (raw.length === 0) {
    return defaultValue;
  }
  return raw === "1" || raw === "true" || raw === "yes" || raw === "on";
}

function contractMeta(response) {
  const raw = response?.__clapse_contract;
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    return {};
  }
  return raw;
}

async function probeStrictNative(path, requireNoBoundaryFallback) {
  let compileResponse;
  try {
    compileResponse = await callCompilerWasm(path, {
      command: "compile",
      compile_mode: "kernel-native",
      input_path: "examples/native_boundary_seed_scan.clapse",
      input_source: DEFAULT_COMPILE_SOURCE,
      plugin_wasm_paths: [],
    }, {
      withContractMetadata: true,
      allowTinyKernelOutputFallback: !requireNoBoundaryFallback,
    });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    return { ok: false, reason: `compile-call-failed: ${msg}` };
  }
  if (
    !compileResponse || typeof compileResponse !== "object" ||
    Array.isArray(compileResponse)
  ) {
    return { ok: false, reason: "compile-response-not-object" };
  }
  if (compileResponse.ok !== true) {
    return {
      ok: false,
      reason: `compile-not-ok: ${String(compileResponse.error ?? "unknown")}`,
    };
  }
  if (compileResponse.backend !== "kernel-native") {
    return {
      ok: false,
      reason: `compile-backend-mismatch: ${
        String(compileResponse.backend ?? "<missing>")
      }`,
    };
  }
  if (
    requireNoBoundaryFallback &&
    contractMeta(compileResponse).tiny_output_fallback === true
  ) {
    return { ok: false, reason: "compile-boundary-tiny-output-fallback" };
  }
  const compileArtifacts = compileResponse.artifacts;
  if (
    !compileArtifacts || typeof compileArtifacts !== "object" ||
    Array.isArray(compileArtifacts)
  ) {
    return { ok: false, reason: "compile-artifacts-missing" };
  }
  if (!nonEmptyString(compileArtifacts["lowered_ir.txt"])) {
    return { ok: false, reason: "compile-artifacts-lowered_ir-missing" };
  }
  if (!nonEmptyString(compileArtifacts["collapsed_ir.txt"])) {
    return { ok: false, reason: "compile-artifacts-collapsed_ir-missing" };
  }

  let emitResponse;
  try {
    emitResponse = await callCompilerWasm(path, {
      command: "emit-wat",
      emit_wat_mode: "source-data",
      input_path: "examples/native_boundary_seed_scan_emit_wat.clapse",
      input_source: `${DEFAULT_MARKER} = 1\n`,
    });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    return { ok: false, reason: `emit-wat-call-failed: ${msg}` };
  }
  if (
    !emitResponse || typeof emitResponse !== "object" ||
    Array.isArray(emitResponse)
  ) {
    return { ok: false, reason: "emit-wat-response-not-object" };
  }
  if (emitResponse.ok !== true) {
    return {
      ok: false,
      reason: `emit-wat-not-ok: ${String(emitResponse.error ?? "unknown")}`,
    };
  }
  if (!nonEmptyString(emitResponse.wat)) {
    return { ok: false, reason: "emit-wat-missing-wat" };
  }
  if (!emitResponse.wat.includes(DEFAULT_MARKER)) {
    return { ok: false, reason: "emit-wat-missing-marker" };
  }

  return { ok: true };
}

function parseArgs(args) {
  const scanRoots = [];
  let maxBytes = DEFAULT_MAX_BYTES;
  let allowEmpty = false;
  let jsonOnly = false;
  let requireNoBoundaryFallback = boolEnvFlag(
    REQUIRE_NO_BOUNDARY_FALLBACK_ENV,
    false,
  );
  for (let i = 0; i < args.length; i += 1) {
    const arg = args[i];
    if (arg === "--help" || arg === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (arg === "--scan-root") {
      const value = args[i + 1] ?? "";
      if (value.length === 0) {
        throw new Error("missing value for --scan-root");
      }
      scanRoots.push(value);
      i += 1;
      continue;
    }
    if (arg === "--max-bytes") {
      const raw = args[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        throw new Error(`invalid --max-bytes value: ${raw}`);
      }
      maxBytes = parsed;
      i += 1;
      continue;
    }
    if (arg === "--allow-empty") {
      allowEmpty = true;
      continue;
    }
    if (arg === "--json") {
      jsonOnly = true;
      continue;
    }
    if (arg === "--require-no-boundary-fallback") {
      requireNoBoundaryFallback = true;
      continue;
    }
    throw new Error(`unknown argument: ${arg}`);
  }
  return {
    scanRoots,
    maxBytes,
    allowEmpty,
    jsonOnly,
    requireNoBoundaryFallback,
  };
}

function defaultScanRoots(extraRoots) {
  const roots = [
    pathJoin(Deno.cwd(), "artifacts"),
    pathJoin(Deno.cwd(), "out"),
    pathJoin(Deno.cwd(), "out=out"),
    pathJoin(pathJoin(Deno.cwd(), ".."), "clapse2/artifacts/releases"),
  ];
  for (const root of extraRoots) {
    roots.push(root);
  }
  const seen = new Set();
  const deduped = [];
  for (const root of roots) {
    const normalized = normalizePath(root);
    if (seen.has(normalized)) {
      continue;
    }
    seen.add(normalized);
    deduped.push(normalized);
  }
  return deduped;
}

function printHuman(summary) {
  console.log(
    `strict-native-seed-scan: scanned ${summary.scanned_files} wasm files`,
  );
  console.log(
    `strict-native-seed-scan: compiler candidates ${summary.compiler_candidates}`,
  );
  if (summary.pass_paths.length === 0) {
    console.log(
      "strict-native-seed-scan: no strict-native seed candidates found",
    );
  } else {
    console.log("strict-native-seed-scan: strict-native seed candidates:");
    for (const path of summary.pass_paths) {
      console.log(`  - ${path}`);
    }
  }
  if (summary.first_failures.length > 0) {
    console.log("strict-native-seed-scan: first failing candidates:");
    for (const failure of summary.first_failures) {
      console.log(`  - ${failure.path}: ${failure.reason}`);
    }
  }
}

async function main() {
  const {
    scanRoots: extraRoots,
    maxBytes,
    allowEmpty,
    jsonOnly,
    requireNoBoundaryFallback,
  } = parseArgs(Deno.args);
  const scanRoots = defaultScanRoots(extraRoots);

  const existingRoots = [];
  for (const root of scanRoots) {
    if (await isDir(root)) {
      existingRoots.push(root);
    }
  }

  const allWasm = [];
  for (const root of existingRoots) {
    await walkWasmFiles(root, allWasm);
  }
  const uniqueWasm = [...new Set(allWasm)].sort((a, b) =>
    a.localeCompare(b, "en")
  );

  let scannedFiles = 0;
  let compilerCandidates = 0;
  const passPaths = [];
  const failures = [];

  for (const wasmPath of uniqueWasm) {
    let stat;
    try {
      stat = await Deno.stat(wasmPath);
    } catch {
      continue;
    }
    if (!stat.isFile || stat.size === 0 || stat.size > maxBytes) {
      continue;
    }
    scannedFiles += 1;

    const abi = await hasCompilerAbi(wasmPath);
    if (!abi.ok) {
      continue;
    }
    compilerCandidates += 1;

    const probe = await probeStrictNative(wasmPath, requireNoBoundaryFallback);
    if (probe.ok) {
      passPaths.push(wasmPath);
      continue;
    }
    failures.push({
      path: wasmPath,
      reason: probe.reason ?? "unknown",
    });
  }

  const summary = {
    scanned_files: scannedFiles,
    compiler_candidates: compilerCandidates,
    pass_paths: passPaths,
    first_failures: failures.slice(0, 10),
    total_failures: failures.length,
    scan_roots: existingRoots,
    max_bytes: maxBytes,
  };

  if (!jsonOnly) {
    printHuman(summary);
  }
  console.log(JSON.stringify(summary, null, 2));

  if (!allowEmpty && passPaths.length === 0) {
    throw new Error(
      "strict-native-seed-scan: no strict-native compiler seed found (all candidates failed strict compile/emit-wat contract checks)",
    );
  }
}

await main();
