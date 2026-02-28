#!/usr/bin/env -S deno run -A

import { callCompilerWasm } from "./wasm-compiler-abi.mjs";

const DEFAULT_MAX_BYTES = 5 * 1024 * 1024;
const DEFAULT_MARKER = "strict_native_emit_wat_marker";
const DEFAULT_COMPILE_SOURCE = "main x = x\n";
const DEFAULT_KERNEL_SELFHOST_INPUT_PATH = "lib/compiler/kernel.clapse";
const DEFAULT_KERNEL_SELFHOST_HOPS = 0;
const REQUIRE_NO_BOUNDARY_FALLBACK_ENV =
  "CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK";
const KERNEL_SELFHOST_HOPS_ENV = "CLAPSE_STRICT_NATIVE_KERNEL_SELFHOST_HOPS";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/strict-native-seed-scan.mjs [options]",
    "",
    "Options:",
    "  --scan-root <dir>    additional scan root (repeatable)",
    "  --no-default-roots   scan only explicit --scan-root roots",
    "  --max-bytes <n>      max wasm size to scan (default 5242880)",
    "  --allow-empty        exit 0 when no strict-native seed is found",
    "  --require-no-boundary-fallback",
    `                      fail candidates that require JS ABI tiny-output fallback (env: ${REQUIRE_NO_BOUNDARY_FALLBACK_ENV}=1)`,
    "  --kernel-selfhost-hops <n>",
    `                      require N-hop kernel selfhost compile closure (env: ${KERNEL_SELFHOST_HOPS_ENV}, default ${DEFAULT_KERNEL_SELFHOST_HOPS})`,
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

function decodeWasmBase64(raw) {
  if (!nonEmptyString(raw)) {
    throw new Error("missing non-empty wasm_base64");
  }
  const binary = atob(raw);
  const out = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    out[i] = binary.charCodeAt(i);
  }
  return out;
}

function hasCompilerLikeExports(exportNames) {
  if (!Array.isArray(exportNames) || exportNames.length === 0) {
    return false;
  }
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  const hasRun = exportNames.includes("clapse_run");
  return hasMemory && hasRun;
}

async function probeKernelSelfhostClosure(
  path,
  hops,
  requireNoBoundaryFallback,
  kernelSource,
) {
  if (!Number.isInteger(hops) || hops <= 0) {
    return { ok: true };
  }
  let compilerPath = path;
  const tempCompilers = [];
  try {
    for (let hop = 1; hop <= hops; hop += 1) {
      let response;
      try {
        response = await callCompilerWasm(compilerPath, {
          command: "compile",
          compile_mode: "kernel-native",
          input_path: DEFAULT_KERNEL_SELFHOST_INPUT_PATH,
          input_source: kernelSource,
          plugin_wasm_paths: [],
        }, {
          withContractMetadata: true,
          allowTinyKernelOutputFallback: !requireNoBoundaryFallback,
        });
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-call-failed: ${msg}`,
        };
      }
      if (
        !response || typeof response !== "object" || Array.isArray(response)
      ) {
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-response-not-object`,
        };
      }
      if (response.ok !== true) {
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-not-ok: ${
            String(response.error ?? "unknown")
          }`,
        };
      }
      if (response.backend !== "kernel-native") {
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-backend-mismatch: ${
            String(response.backend ?? "<missing>")
          }`,
        };
      }
      if (
        requireNoBoundaryFallback &&
        contractMeta(response).tiny_output_fallback === true
      ) {
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-boundary-tiny-output-fallback`,
        };
      }
      let wasmBytes;
      try {
        wasmBytes = decodeWasmBase64(response.wasm_base64);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-wasm-base64-invalid: ${msg}`,
        };
      }
      let module;
      try {
        module = await WebAssembly.compile(wasmBytes);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-output-invalid-wasm: ${msg}`,
        };
      }
      const exportNames = WebAssembly.Module.exports(module).map((entry) =>
        entry.name
      );
      if (!hasCompilerLikeExports(exportNames)) {
        return {
          ok: false,
          reason: `kernel-hop-${hop}-compile-output-missing-compiler-exports: ${
            exportNames.join(",")
          }`,
        };
      }
      if (hop < hops) {
        const nextCompilerPath = await Deno.makeTempFile({
          prefix: "clapse-strict-seed-kernel-hop-",
          suffix: ".wasm",
        });
        await Deno.writeFile(nextCompilerPath, wasmBytes);
        tempCompilers.push(nextCompilerPath);
        compilerPath = nextCompilerPath;
      }
    }
  } finally {
    for (const tempPath of tempCompilers) {
      try {
        await Deno.remove(tempPath);
      } catch {
        // best-effort cleanup
      }
    }
  }
  return { ok: true };
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
  let useDefaultRoots = true;
  let maxBytes = DEFAULT_MAX_BYTES;
  let allowEmpty = false;
  let jsonOnly = false;
  let kernelSelfhostHops = Number.parseInt(
    String(
      Deno.env.get(KERNEL_SELFHOST_HOPS_ENV) ?? DEFAULT_KERNEL_SELFHOST_HOPS,
    ),
    10,
  );
  if (!Number.isInteger(kernelSelfhostHops) || kernelSelfhostHops < 0) {
    kernelSelfhostHops = DEFAULT_KERNEL_SELFHOST_HOPS;
  }
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
    if (arg === "--no-default-roots") {
      useDefaultRoots = false;
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
    if (arg === "--kernel-selfhost-hops") {
      const raw = args[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed < 0) {
        throw new Error(`invalid --kernel-selfhost-hops value: ${raw}`);
      }
      kernelSelfhostHops = parsed;
      i += 1;
      continue;
    }
    throw new Error(`unknown argument: ${arg}`);
  }
  return {
    scanRoots,
    useDefaultRoots,
    maxBytes,
    allowEmpty,
    jsonOnly,
    requireNoBoundaryFallback,
    kernelSelfhostHops,
  };
}

function defaultScanRoots(extraRoots, useDefaultRoots) {
  const roots = useDefaultRoots
    ? [
      pathJoin(Deno.cwd(), "artifacts"),
      pathJoin(Deno.cwd(), "out"),
      pathJoin(Deno.cwd(), "out=out"),
      pathJoin(pathJoin(Deno.cwd(), ".."), "clapse2/artifacts/releases"),
    ]
    : [];
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
    useDefaultRoots,
    maxBytes,
    allowEmpty,
    jsonOnly,
    requireNoBoundaryFallback,
    kernelSelfhostHops,
  } = parseArgs(Deno.args);
  const scanRoots = defaultScanRoots(extraRoots, useDefaultRoots);
  const kernelSource = kernelSelfhostHops > 0
    ? await Deno.readTextFile(DEFAULT_KERNEL_SELFHOST_INPUT_PATH)
    : "";

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
    if (!probe.ok) {
      failures.push({
        path: wasmPath,
        reason: probe.reason ?? "unknown",
      });
      continue;
    }
    if (kernelSelfhostHops > 0) {
      const kernelProbe = await probeKernelSelfhostClosure(
        wasmPath,
        kernelSelfhostHops,
        requireNoBoundaryFallback,
        kernelSource,
      );
      if (!kernelProbe.ok) {
        failures.push({
          path: wasmPath,
          reason: kernelProbe.reason ?? "kernel-selfhost-unknown",
        });
        continue;
      }
    }
    passPaths.push(wasmPath);
  }

  const summary = {
    scanned_files: scannedFiles,
    compiler_candidates: compilerCandidates,
    pass_paths: passPaths,
    first_failures: failures.slice(0, 10),
    total_failures: failures.length,
    scan_roots: existingRoots,
    max_bytes: maxBytes,
    kernel_selfhost_hops: kernelSelfhostHops,
  };

  if (!jsonOnly) {
    printHuman(summary);
  }
  console.log(JSON.stringify(summary, null, 2));

  if (!allowEmpty && passPaths.length === 0) {
    throw new Error(
      "strict-native-seed-scan: no strict-native compiler seed found (all candidates failed strict compile/emit-wat and optional kernel selfhost checks)",
    );
  }
}

await main();
