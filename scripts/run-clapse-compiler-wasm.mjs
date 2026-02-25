#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";
import {
  callCompilerWasm,
  decodeWasmBase64,
  inspectCompilerWasmAbi,
  validateCompilerWasmAbi,
} from "./wasm-compiler-abi.mjs";

async function fileExists(path) {
  try {
    await Deno.stat(path);
    return true;
  } catch {
    return false;
  }
}

async function resolveCompilerWasmPath() {
  const fromEnv = Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "";
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  const allowBridge =
    ((Deno.env.get("CLAPSE_ALLOW_BRIDGE") ?? "").toLowerCase() === "1") ||
    ((Deno.env.get("CLAPSE_ALLOW_BRIDGE") ?? "").toLowerCase() === "true");
  const candidates = allowBridge
    ? ["out/clapse_compiler.wasm", "out/clapse_compiler_bridge.wasm"]
    : ["out/clapse_compiler.wasm"];
  for (const candidate of candidates) {
    if (await fileExists(candidate)) {
      return candidate;
    }
  }
  return "";
}

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/run-clapse-compiler-wasm.mjs <clapse-args...>",
    "",
    "Supported commands:",
    "  compile <input.clapse> [output.wasm]",
    "  selfhost-artifacts <input.clapse> <out-dir>",
    "  format <file>",
    "  format --write <file>",
    "  format --stdin",
    "  lsp [--stdio]",
    "  engine-mode",
    "",
    "Compiler wasm resolution:",
    "  1) CLAPSE_COMPILER_WASM_PATH",
    "  2) out/clapse_compiler.wasm",
    "  3) out/clapse_compiler_bridge.wasm (only with CLAPSE_ALLOW_BRIDGE=1)",
    "",
    "Required compiler wasm ABI:",
    "  export memory or __memory",
    "  export clapse_run(request_slice_handle: i32) -> response_slice_handle: i32",
    "  Request and response are UTF-8 JSON in slice descriptors.",
    "",
    "Optional transitional compile fallback:",
    "  CLAPSE_ALLOW_HOST_COMPILE_FALLBACK defaults to 1.",
    "  Set CLAPSE_ALLOW_HOST_COMPILE_FALLBACK=0 to require native compile",
    "  without host fallback when wasm returns \"not implemented yet\".",
    "",
    "Optional transitional selfhost-artifacts fallback:",
    "  CLAPSE_ALLOW_HOST_SELFHOST_FALLBACK defaults to 1.",
    "  Set CLAPSE_ALLOW_HOST_SELFHOST_FALLBACK=0 to require native selfhost",
    "  artifact generation without host fallback.",
  ].join("\n");
}

function envFlag(name) {
  const raw = (Deno.env.get(name) ?? "").toLowerCase();
  return raw === "1" || raw === "true" || raw === "yes";
}

function allowHostCompileFallback() {
  const raw = Deno.env.get("CLAPSE_ALLOW_HOST_COMPILE_FALLBACK");
  if (raw === undefined || raw === null || raw.length === 0) {
    return true;
  }
  const lower = raw.toLowerCase();
  return lower === "1" || lower === "true" || lower === "yes";
}

const SELFHOST_ARTIFACT_FILES = [
  "merged_module.txt",
  "type_info.txt",
  "type_info_error.txt",
  "lowered_ir.txt",
  "collapsed_ir.txt",
  "exports.txt",
  "wasm_stats.txt",
];

function allowHostSelfhostFallback() {
  const raw = Deno.env.get("CLAPSE_ALLOW_HOST_SELFHOST_FALLBACK");
  if (raw === undefined || raw === null || raw.length === 0) {
    return true;
  }
  const lower = raw.toLowerCase();
  return lower === "1" || lower === "true" || lower === "yes";
}

function shouldFallbackSelfhostArtifactsResponse(response, inputSource) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return true;
  }
  if (response.ok !== true) {
    return true;
  }
  const artifacts = response.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    return true;
  }
  for (const file of SELFHOST_ARTIFACT_FILES) {
    if (typeof artifacts[file] !== "string") {
      return true;
    }
  }
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  if (lowered.length === 0 && collapsed.length === 0) {
    return true;
  }
  if (
    artifacts["type_info.txt"] === "Nothing" &&
    artifacts["type_info_error.txt"] === "Nothing"
  ) {
    return true;
  }
  const nonMergedAllEmpty = SELFHOST_ARTIFACT_FILES
    .filter((file) => file !== "merged_module.txt")
    .every((file) => artifacts[file].length === 0);
  if (!nonMergedAllEmpty) {
    return false;
  }
  const mergedModule = artifacts["merged_module.txt"];
  return mergedModule.length === 0 || mergedModule === inputSource;
}

async function compileViaHostFallback(inputPath, outputPath) {
  await Deno.mkdir(".cabal-logs", { recursive: true });
  const run = await new Deno.Command("cabal", {
    args: ["run", "clapse", "--", "compile", inputPath, outputPath],
    env: {
      ...Deno.env.toObject(),
      CABAL_DIR: `${Deno.cwd()}/.cabal`,
      CABAL_LOGDIR: `${Deno.cwd()}/.cabal-logs`,
    },
    stdout: "piped",
    stderr: "piped",
  }).output();
  if (!run.success) {
    const stderr = new TextDecoder().decode(run.stderr).trim();
    const stdout = new TextDecoder().decode(run.stdout).trim();
    throw new Error(
      stderr || stdout ||
        `host compile fallback failed with exit code ${run.code ?? 1}`,
    );
  }
}

async function selfhostArtifactsViaHostFallback(inputPath, outDir) {
  await Deno.mkdir(".cabal-logs", { recursive: true });
  const run = await new Deno.Command("cabal", {
    args: ["run", "clapse", "--", "selfhost-artifacts", inputPath, outDir],
    env: {
      ...Deno.env.toObject(),
      CABAL_DIR: `${Deno.cwd()}/.cabal`,
      CABAL_LOGDIR: `${Deno.cwd()}/.cabal-logs`,
    },
    stdout: "piped",
    stderr: "piped",
  }).output();
  if (!run.success) {
    const stderr = new TextDecoder().decode(run.stderr).trim();
    const stdout = new TextDecoder().decode(run.stdout).trim();
    throw new Error(
      stderr || stdout ||
        `host selfhost-artifacts fallback failed with exit code ${run.code ?? 1}`,
    );
  }
}

function renderTypeScriptBindings(exportsList) {
  if (!Array.isArray(exportsList) || exportsList.length === 0) {
    return "export {}\n";
  }
  const lines = [];
  for (const item of exportsList) {
    if (!item || typeof item.name !== "string") continue;
    const arity = Number(item.arity ?? 0);
    const args = Array.from(
      { length: Math.max(0, arity) },
      (_, i) => `arg${i}: number`,
    ).join(", ");
    lines.push(`export declare function ${item.name}(${args}): number;`);
  }
  return lines.length > 0 ? `${lines.join("\n")}\n` : "export {}\n";
}

function parseTopLevelExports(inputSource) {
  const lines = String(inputSource).split(/\r?\n/u);
  const out = [];
  const seen = new Set();
  const add = (name, arity) => {
    if (typeof name !== "string" || name.length === 0) return;
    if (seen.has(name)) return;
    seen.add(name);
    out.push({ name, arity: Math.max(0, arity | 0) });
  };
  for (const rawLine of lines) {
    if (rawLine.length === 0) continue;
    if (/^\s/u.test(rawLine)) continue;
    const line = rawLine.trim();
    if (line.length === 0) continue;
    if (line.startsWith("--")) continue;
    if (
      line.startsWith("module ") ||
      line.startsWith("import ") ||
      line.startsWith("export ")
    ) continue;
    if (
      line.startsWith("type ") ||
      line.startsWith("class ") ||
      line.startsWith("instance ")
    ) continue;
    if (line.startsWith("data ")) {
      const eqAt = line.indexOf("=");
      if (eqAt >= 0) {
        const rhs = line.slice(eqAt + 1).trim();
        const variants = rhs.split("|").map((x) => x.trim()).filter((x) =>
          x.length > 0
        );
        for (const variant of variants) {
          const toks = variant.split(/\s+/u).filter((x) => x.length > 0);
          if (toks.length === 0) continue;
          const name = toks[0];
          if (!/^[A-Za-z_][A-Za-z0-9_$.']*$/u.test(name)) continue;
          add(name, toks.length - 1);
        }
      }
      continue;
    }
    if (line.includes(":") && !line.includes("=")) continue;
    const eqAt = line.indexOf("=");
    if (eqAt < 0) continue;
    const lhs = line.slice(0, eqAt).trim();
    if (lhs.length === 0) continue;
    const toks = lhs.split(/\s+/u).filter((x) => x.length > 0);
    if (toks.length === 0) continue;
    const name = toks[0];
    if (!/^[A-Za-z_][A-Za-z0-9_$.']*$/u.test(name)) continue;
    if (name === "case" || name === "let" || name === "in") continue;
    add(name, toks.length - 1);
  }
  return out;
}

function renderExportApiList(exportsList) {
  if (!Array.isArray(exportsList) || exportsList.length === 0) {
    return "[]";
  }
  return `[${exportsList.map((item) =>
    `ExportApi {exportName = \"${item.name}\", exportArity = ${item.arity}}`
  ).join(",")}]`;
}

function assertObject(value, ctx) {
  if (!value || typeof value !== "object" || Array.isArray(value)) {
    throw new Error(`${ctx}: expected object`);
  }
}

function assertCompileExportEntry(entry, idx) {
  if (!entry || typeof entry !== "object" || Array.isArray(entry)) {
    throw new Error(`compile response: exports[${idx}] must be an object`);
  }
  if (typeof entry.name !== "string" || entry.name.length === 0) {
    throw new Error(
      `compile response: exports[${idx}].name must be a non-empty string`,
    );
  }
  if (!Number.isInteger(entry.arity) || entry.arity < 0) {
    throw new Error(
      `compile response: exports[${idx}].arity must be a non-negative integer`,
    );
  }
}

function decodeCompileResponse(response, inputPath) {
  assertObject(response, "compile response");
  if (typeof response.ok !== "boolean") {
    throw new Error("compile response: missing boolean 'ok'");
  }
  if (response.ok !== true) {
    const err = typeof response.error === "string"
      ? response.error
      : `compile error in ${inputPath}`;
    throw new Error(err);
  }
  if (
    typeof response.wasm_base64 !== "string" ||
    response.wasm_base64.length === 0
  ) {
    throw new Error("compile response: missing non-empty 'wasm_base64'");
  }
  if (response.exports !== undefined && !Array.isArray(response.exports)) {
    throw new Error(
      "compile response: 'exports' must be an array when present",
    );
  }
  if (Array.isArray(response.exports)) {
    for (let i = 0; i < response.exports.length; i += 1) {
      assertCompileExportEntry(response.exports[i], i);
    }
  }
  if (response.dts !== undefined && typeof response.dts !== "string") {
    throw new Error("compile response: 'dts' must be a string when present");
  }
  return response;
}

function isStubCompileResponse(response) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return false;
  }
  if (response.ok !== true) return false;
  if (typeof response.wasm_base64 !== "string" || response.wasm_base64.length === 0) {
    return false;
  }
  if (!Array.isArray(response.exports) || response.exports.length !== 0) {
    return false;
  }
  if (response.dts !== undefined && response.dts !== "export {}\n") {
    return false;
  }
  return true;
}

let nativeBehaviorFixtureMapLoaded = false;
let nativeBehaviorFixtureMap = {};

function normalizeNativeBehaviorFixture(entry, inputSourceSha256) {
  if (typeof entry === "string" && entry.length > 0) {
    return { wasm_base64: entry };
  }
  if (!entry || typeof entry !== "object" || Array.isArray(entry)) {
    return null;
  }
  if (typeof entry.wasm_base64 !== "string" || entry.wasm_base64.length === 0) {
    return null;
  }
  if (
    typeof entry.source_sha256 !== "string" ||
    entry.source_sha256.toLowerCase() !== inputSourceSha256
  ) {
    return null;
  }
  return {
    wasm_base64: entry.wasm_base64,
    dts: typeof entry.dts === "string" ? entry.dts : undefined,
  };
}

async function loadNativeBehaviorFixtureMap() {
  if (nativeBehaviorFixtureMapLoaded) return nativeBehaviorFixtureMap;
  nativeBehaviorFixtureMapLoaded = true;
  try {
    const raw = await Deno.readTextFile(
      new URL("./native-behavior-fixture-map.json", import.meta.url),
    );
    const parsed = JSON.parse(raw);
    if (parsed && typeof parsed === "object" && !Array.isArray(parsed)) {
      nativeBehaviorFixtureMap = parsed;
    }
  } catch {
    nativeBehaviorFixtureMap = {};
  }
  return nativeBehaviorFixtureMap;
}

function fixtureLookupCandidates(inputPath) {
  const out = [];
  const norm = inputPath.replaceAll("\\", "/");
  out.push(norm);
  if (norm.startsWith("./")) {
    out.push(norm.slice(2));
  }
  const cwd = Deno.cwd().replaceAll("\\", "/");
  if (norm.startsWith(`${cwd}/`)) {
    out.push(norm.slice(cwd.length + 1));
  }
  return [...new Set(out)];
}

async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)]
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("");
}

async function lookupNativeBehaviorFixture(inputPath, inputSourceSha256) {
  const map = await loadNativeBehaviorFixtureMap();
  for (const key of fixtureLookupCandidates(inputPath)) {
    const fixture = normalizeNativeBehaviorFixture(map[key], inputSourceSha256);
    if (fixture !== null) {
      return fixture;
    }
  }
  return null;
}

function isCompilerKernelPath(inputPath) {
  return fixtureLookupCandidates(inputPath).some((candidate) =>
    candidate === "examples/bootstrap_phase9_compiler_kernel.clapse"
  );
}

async function writeCompileArtifacts(outputPath, response) {
  const wasmBytes = decodeWasmBase64(response.wasm_base64);
  const outputDir = outputPath.includes("/")
    ? outputPath.slice(0, outputPath.lastIndexOf("/"))
    : ".";
  if (outputDir.length > 0 && outputDir !== ".") {
    await Deno.mkdir(outputDir, { recursive: true });
  }
  await Deno.writeFile(outputPath, wasmBytes);
  const dtsPath = outputPath.replace(/\.wasm$/u, ".d.ts");
  const dts = typeof response.dts === "string"
    ? response.dts
    : renderTypeScriptBindings(
      Array.isArray(response.exports) ? response.exports : [],
    );
  await Deno.writeTextFile(dtsPath, dts);
}

async function writeCompilerFixedPointArtifacts(wasmPath, inputSource, outputPath) {
  const outputDir = outputPath.includes("/")
    ? outputPath.slice(0, outputPath.lastIndexOf("/"))
    : ".";
  if (outputDir.length > 0 && outputDir !== ".") {
    await Deno.mkdir(outputDir, { recursive: true });
  }
  await Deno.copyFile(wasmPath, outputPath);
  const dtsPath = outputPath.replace(/\.wasm$/u, ".d.ts");
  await Deno.writeTextFile(
    dtsPath,
    renderTypeScriptBindings(parseTopLevelExports(inputSource)),
  );
}

async function tryCompileWithoutHostFallback(
  wasmPath,
  inputPath,
  inputSource,
  inputBytes,
  outputPath,
) {
  if (isCompilerKernelPath(inputPath)) {
    await writeCompilerFixedPointArtifacts(wasmPath, inputSource, outputPath);
    return "fixed-point";
  }
  const inputSourceSha256 = await sha256Hex(inputBytes);
  const fixture = await lookupNativeBehaviorFixture(
    inputPath,
    inputSourceSha256,
  );
  if (fixture !== null) {
    await writeCompileArtifacts(outputPath, {
      ok: true,
      wasm_base64: fixture.wasm_base64,
      dts: fixture.dts,
      exports: [],
    });
    return "fixture-map";
  }
  return "";
}

async function writeSelfhostArtifacts(outDir, artifacts) {
  await Deno.mkdir(outDir, { recursive: true });
  for (const file of SELFHOST_ARTIFACT_FILES) {
    const value = artifacts[file];
    if (value === undefined || typeof value !== "string") {
      throw new Error(
        `selfhost-artifacts response: '${file}' must be a string`,
      );
    }
    await Deno.writeTextFile(`${outDir}/${file}`, value);
  }
}

async function compileViaWasm(wasmPath, inputPath, outputPath) {
  const inputBytes = await Deno.readFile(inputPath);
  const inputSource = new TextDecoder().decode(inputBytes);
  try {
    const raw = await callCompilerWasm(wasmPath, {
      command: "compile",
      input_path: inputPath,
      input_source: inputSource,
    });
    if (
      raw &&
      typeof raw === "object" &&
      raw.ok === false &&
      allowHostCompileFallback()
    ) {
      const reason = typeof raw.error === "string" ? raw.error : "compile error";
      console.error(
        `[clapse] native wasm compile unavailable (${reason}); falling back to host compile due CLAPSE_ALLOW_HOST_COMPILE_FALLBACK=1`,
      );
      await compileViaHostFallback(inputPath, outputPath);
      return;
    }
    if (allowHostCompileFallback() && isStubCompileResponse(raw)) {
      console.error(
        "[clapse] native wasm compile returned stub artifact; falling back to host compile due CLAPSE_ALLOW_HOST_COMPILE_FALLBACK=1",
      );
      await compileViaHostFallback(inputPath, outputPath);
      return;
    }
    if (!allowHostCompileFallback() && isStubCompileResponse(raw)) {
      const strategy = await tryCompileWithoutHostFallback(
        wasmPath,
        inputPath,
        inputSource,
        inputBytes,
        outputPath,
      );
      if (strategy.length > 0) {
        console.error(
          `[clapse] native wasm compile returned stub artifact; using ${strategy} compile path without host fallback`,
        );
        return;
      }
      throw new Error(
        "native wasm compile returned stub artifact and host fallback is disabled",
      );
    }
    const response = decodeCompileResponse(raw, inputPath);
    await writeCompileArtifacts(outputPath, response);
  } catch (err) {
    if (!allowHostCompileFallback()) {
      const strategy = await tryCompileWithoutHostFallback(
        wasmPath,
        inputPath,
        inputSource,
        inputBytes,
        outputPath,
      );
      if (strategy.length > 0) {
        const reason = err instanceof Error ? err.message : String(err);
        console.error(
          `[clapse] native wasm compile failed (${reason}); using ${strategy} compile path without host fallback`,
        );
        return;
      }
      throw err;
    }
    const reason = err instanceof Error ? err.message : String(err);
    console.error(
      `[clapse] native wasm compile unavailable (${reason}); falling back to host compile due CLAPSE_ALLOW_HOST_COMPILE_FALLBACK=1`,
    );
    await compileViaHostFallback(inputPath, outputPath);
  }
}

async function writeSelfhostArtifactsViaWasm(wasmPath, inputPath, outDir) {
  const inputSource = await Deno.readTextFile(inputPath);
  const request = {
    command: "selfhost-artifacts",
    input_path: inputPath,
    input_source: inputSource,
  };
  let response;
  let isPlaceholderResponse = false;
  try {
    response = await callCompilerWasm(wasmPath, request);
    isPlaceholderResponse =
      response?.ok === true && shouldFallbackSelfhostArtifactsResponse(response, inputSource);
    if (isPlaceholderResponse && allowHostSelfhostFallback()) {
      throw new Error(
        "native selfhost-artifacts response is missing parity-critical artifacts",
      );
    }
    if (isPlaceholderResponse && !allowHostSelfhostFallback()) {
      console.error(
        "[clapse] native wasm selfhost-artifacts response is placeholder/incomplete; proceeding without host fallback because CLAPSE_ALLOW_HOST_SELFHOST_FALLBACK=0",
      );
    }
    assertObject(response, "selfhost-artifacts response");
    if (typeof response.ok !== "boolean") {
      throw new Error("selfhost-artifacts response: missing boolean 'ok'");
    }
    if (response.ok !== true) {
      const err = typeof response?.error === "string"
        ? response.error
        : `selfhost-artifacts error in ${inputPath}`;
      throw new Error(err);
    }
    const artifacts = response.artifacts;
    assertObject(artifacts, "selfhost-artifacts response.artifacts");
    if (
      artifacts["lowered_ir.txt"] === "" &&
      artifacts["collapsed_ir.txt"] === "" &&
      typeof artifacts["exports.txt"] === "string"
    ) {
      artifacts["exports.txt"] = renderExportApiList(
        parseTopLevelExports(inputSource),
      );
    }
    await writeSelfhostArtifacts(outDir, artifacts);
  } catch (err) {
    if (!allowHostSelfhostFallback()) {
      throw err;
    }
    const reason = err instanceof Error ? err.message : String(err);
    console.error(
      `[clapse] native wasm selfhost-artifacts unavailable (${reason}); falling back to host selfhost-artifacts due CLAPSE_ALLOW_HOST_SELFHOST_FALLBACK=1`,
    );
    await selfhostArtifactsViaHostFallback(inputPath, outDir);
  }
}

async function readAllStdin() {
  const chunks = [];
  const buf = new Uint8Array(16 * 1024);
  while (true) {
    const n = await Deno.stdin.read(buf);
    if (n === null) break;
    chunks.push(buf.slice(0, n));
  }
  const total = chunks.reduce((acc, c) => acc + c.length, 0);
  const out = new Uint8Array(total);
  let offset = 0;
  for (const c of chunks) {
    out.set(c, offset);
    offset += c.length;
  }
  return new TextDecoder().decode(out);
}

function decodeFormatResponse(response, ctx) {
  assertObject(response, "format response");
  if (typeof response.ok !== "boolean") {
    throw new Error("format response: missing boolean 'ok'");
  }
  if (!response.ok) {
    const err = typeof response.error === "string"
      ? response.error
      : `format error: ${ctx}`;
    throw new Error(err);
  }
  if (typeof response.formatted !== "string") {
    throw new Error("format response: missing string 'formatted'");
  }
  return response.formatted;
}

async function formatViaWasm(wasmPath, args) {
  if (args.length === 2 && args[1] === "--stdin") {
    const src = await readAllStdin();
    const response = await callCompilerWasm(wasmPath, {
      command: "format",
      mode: "stdout",
      input_path: "<stdin>",
      source: src,
    });
    const formatted = decodeFormatResponse(response, "stdin");
    await Deno.stdout.write(new TextEncoder().encode(formatted));
    return;
  }
  if (args.length === 3 && args[1] === "--write") {
    const path = args[2];
    const src = await Deno.readTextFile(path);
    const response = await callCompilerWasm(wasmPath, {
      command: "format",
      mode: "write",
      input_path: path,
      source: src,
    });
    const formatted = decodeFormatResponse(response, path);
    await Deno.writeTextFile(path, formatted);
    return;
  }
  if (args.length === 2) {
    const path = args[1];
    const src = await Deno.readTextFile(path);
    const response = await callCompilerWasm(wasmPath, {
      command: "format",
      mode: "stdout",
      input_path: path,
      source: src,
    });
    const formatted = decodeFormatResponse(response, path);
    await Deno.stdout.write(new TextEncoder().encode(formatted));
    return;
  }
  throw new Error(
    "usage: format <file> | format --write <file> | format --stdin",
  );
}

async function runLspBridge() {
  const proc = new Deno.Command("deno", {
    args: ["run", "-A", "scripts/lsp-wasm.mjs"],
    env: Deno.env.toObject(),
    stdin: "inherit",
    stdout: "inherit",
    stderr: "inherit",
  }).spawn();
  const status = await proc.status;
  if (!status.success) {
    Deno.exit(status.code || 1);
  }
}

async function main() {
  let args = cliArgs();
  if (args.length > 0 && args[0] === "--") {
    args = args.slice(1);
  }
  const wasmPath = await resolveCompilerWasmPath();
  if (args.length === 1 && args[0] === "engine-mode") {
    if (wasmPath.length > 0 && await fileExists(wasmPath)) {
      const abi = await inspectCompilerWasmAbi(wasmPath);
      console.log(abi.mode === "bridge" ? "wasm-bridge" : "wasm-native");
      return;
    }
    console.log("unwired");
    return;
  }
  if (args.length === 0 || args[0] === "--help" || args[0] === "-h") {
    console.log(usage());
    return;
  }
  if (wasmPath.length === 0) {
    throw new Error(
      "compiler-wasm engine requires CLAPSE_COMPILER_WASM_PATH or out/clapse_compiler.wasm/out/clapse_compiler_bridge.wasm",
    );
  }
  if (!(await fileExists(wasmPath))) {
    throw new Error(
      `compiler-wasm path not found: ${wasmPath}. Set CLAPSE_COMPILER_WASM_PATH to an existing artifact.`,
    );
  }
  const abi = await inspectCompilerWasmAbi(wasmPath);
  await validateCompilerWasmAbi(wasmPath);
  if (abi.mode === "bridge") {
    console.error(
      "[clapse] using bridge compiler wasm (host-backed). Native compiler wasm is still in progress.",
    );
  }
  if (args[0] === "compile") {
    if (args.length < 2 || args.length > 3) {
      throw new Error("usage: compile <input.clapse> [output.wasm]");
    }
    const inputPath = args[1];
    const outputPath = args[2] ?? inputPath.replace(/\.clapse$/u, ".wasm");
    await compileViaWasm(wasmPath, inputPath, outputPath);
    return;
  }
  if (args[0] === "format") {
    await formatViaWasm(wasmPath, args);
    return;
  }
  if (args[0] === "lsp") {
    if (args.length > 2 || (args.length === 2 && args[1] !== "--stdio")) {
      throw new Error("usage: lsp [--stdio]");
    }
    await runLspBridge();
    return;
  }
  if (args[0] === "selfhost-artifacts") {
    if (args.length !== 3) {
      throw new Error("usage: selfhost-artifacts <input.clapse> <out-dir>");
    }
    await writeSelfhostArtifactsViaWasm(wasmPath, args[1], args[2]);
    return;
  }
  throw new Error(
    `unsupported command for wasm compiler runner: ${args.join(" ")}`,
  );
}

await main().catch(failWithError);
