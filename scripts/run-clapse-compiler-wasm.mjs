#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";
import { runLspServer } from "./lsp-wasm.mjs";
import {
  callCompilerWasm,
  decodeWasmBase64,
  appendClapseFuncMap,
  inspectCompilerWasmAbi,
  validateCompilerWasmAbi,
} from "./wasm-compiler-abi.mjs";

const REPO_ROOT_URL = new URL("../", import.meta.url);
const PROJECT_CONFIG_FILE = "clapse.json";

function toPath(url) {
  return decodeURIComponent(url.pathname);
}

function normalizePath(path) {
  const normalized = String(path).replace(/\\/g, "/");
  if (normalized.length === 0) {
    return "";
  }
  const hasLeadingSlash = normalized.startsWith("/");
  const parts = normalized.split("/");
  const stack = [];
  for (const part of parts) {
    if (part.length === 0 || part === ".") {
      continue;
    }
    if (part === "..") {
      if (stack.length > 0) {
        stack.pop();
      }
      continue;
    }
    stack.push(part);
  }
  if (stack.length === 0) {
    return hasLeadingSlash ? "/" : "";
  }
  return hasLeadingSlash ? `/${stack.join("/")}` : stack.join("/");
}

function pathDir(path) {
  const normalized = normalizePath(path);
  if (normalized.length === 0 || normalized === ".") {
    return "";
  }
  if (normalized === "/") {
    return "/";
  }
  const idx = normalized.lastIndexOf("/");
  if (idx <= 0) {
    return normalized.startsWith("/") ? "/" : "";
  }
  return normalized.slice(0, idx);
}

function joinPath(base, leaf) {
  const normalizedBase = normalizePath(base);
  if (normalizedBase.length === 0 || normalizedBase === ".") {
    return normalizePath(leaf);
  }
  const normalizedLeaf = normalizePath(leaf);
  if (normalizedLeaf.length === 0) {
    return normalizedBase;
  }
  if (normalizedBase === "/") {
    return `/${normalizedLeaf}`;
  }
  return `${normalizedBase}/${normalizedLeaf}`;
}

function toAbsolutePath(path) {
  const normalized = normalizePath(path);
  if (normalized.startsWith("/")) {
    return normalized;
  }
  return joinPath(Deno.cwd(), normalized);
}

function normalizePluginDirs(value, configDir) {
  if (!Array.isArray(value)) {
    return [];
  }
  const dirs = [];
  const seen = new Set();
  for (const item of value) {
    const raw = String(item).trim();
    if (raw.length === 0) {
      continue;
    }
    const normalizedRaw = raw.replace(/\\/g, "/").trim();
    if (normalizedRaw.length === 0) {
      continue;
    }
    let resolved = normalizedRaw;
    if (!normalizedRaw.startsWith("/")) {
      const base = normalizePath(configDir);
      resolved = base.length > 0
        ? normalizePath(`${base}/${normalizedRaw}`)
        : normalizedRaw;
    }
    if (resolved.length === 0 || seen.has(resolved)) {
      continue;
    }
    seen.add(resolved);
    dirs.push(resolved);
  }
  return dirs;
}

function parseClapseProjectConfig(rawText, configDir) {
  if (typeof rawText !== "string" || rawText.length === 0) {
    return { pluginDirs: [] };
  }
  let data;
  try {
    data = JSON.parse(rawText);
  } catch {
    return { pluginDirs: [] };
  }
  if (!data || typeof data !== "object" || Array.isArray(data)) {
    return { pluginDirs: [] };
  }
  return {
    pluginDirs: normalizePluginDirs(data.plugins, configDir),
  };
}

async function readClapseProjectConfig(startPath = "") {
  const start = normalizePath(startPath || Deno.cwd());
  let dir = pathDir(start);
  if (dir.length === 0 || !start.includes("/")) {
    dir = normalizePath(Deno.cwd());
  }
  if (dir.length === 0) {
    return { pluginDirs: [] };
  }

  const visited = new Set();
  while (true) {
    const candidate = dir;
    if (visited.has(candidate)) {
      break;
    }
    visited.add(candidate);
    const configPath = joinPath(candidate, PROJECT_CONFIG_FILE);
    try {
      const rawText = await Deno.readTextFile(configPath);
      const parsed = parseClapseProjectConfig(rawText, candidate);
      return { pluginDirs: parsed.pluginDirs };
    } catch {
      // no project config in this directory
    }
    const parent = pathDir(candidate);
    if (parent === candidate) {
      break;
    }
    dir = parent;
  }
  return { pluginDirs: [] };
}

async function collectClapseFilesRecursively(rootDir, out = [], seen = new Set()) {
  const normalized = normalizePath(rootDir);
  if (normalized.length === 0 || seen.has(normalized)) {
    return out;
  }
  seen.add(normalized);
  let entries;
  try {
    entries = [];
    for await (const entry of Deno.readDir(normalized)) {
      entries.push(entry);
    }
  } catch {
    return out;
  }
  for (const entry of entries) {
    const child = `${normalized}/${entry.name}`;
    if (entry.isDirectory) {
      await collectClapseFilesRecursively(child, out, seen);
      continue;
    }
    if (entry.isFile && child.endsWith(".clapse")) {
      out.push(child);
    }
  }
  return out;
}

async function compilePluginsWasm(wasmPath, inputPath) {
  const { pluginDirs } = await readClapseProjectConfig(inputPath);
  if (pluginDirs.length === 0) {
    return [];
  }
  const pluginSources = [];
  const seen = new Set();
  for (const pluginDir of pluginDirs) {
    await collectClapseFilesRecursively(pluginDir, pluginSources, seen);
  }
  const uniqueSources = [...new Set(pluginSources)]
    .sort((a, b) => a.localeCompare(b, "en"));
  const pluginWasmPaths = [];
  for (const sourcePath of uniqueSources) {
    const outputPath = sourcePath.endsWith(".clapse")
      ? sourcePath.replace(/\.clapse$/u, ".wasm")
      : `${sourcePath}.wasm`;
    await compileViaWasm(wasmPath, sourcePath, outputPath, {
      pluginWasmPaths: [],
      skipPluginCompilation: true,
    });
    pluginWasmPaths.push(toAbsolutePath(outputPath));
  }
  return pluginWasmPaths;
}

async function fileExists(path) {
  try {
    await Deno.stat(path);
    return true;
  } catch {
    return false;
  }
}

async function materializeEmbeddedCompilerWasm() {
  const embeddedCandidates = [
    new URL("../artifacts/latest/clapse_compiler.wasm", import.meta.url),
    new URL("../out/clapse_compiler.wasm", import.meta.url),
  ];
  for (const candidateUrl of embeddedCandidates) {
    try {
      const wasmBytes = await Deno.readFile(candidateUrl);
      if (wasmBytes.length === 0) continue;
      const tmpPath = await Deno.makeTempFile({
        prefix: "clapse-embedded-compiler-",
        suffix: ".wasm",
      });
      await Deno.writeFile(tmpPath, wasmBytes);
      return tmpPath;
    } catch {
      // keep searching
    }
  }
  return "";
}

async function resolveCompilerWasmPath() {
  const fromEnv = Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "";
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  const candidates = [
    toPath(new URL("artifacts/latest/clapse_compiler.wasm", REPO_ROOT_URL)),
    toPath(new URL("out/clapse_compiler.wasm", REPO_ROOT_URL)),
  ];
  for (const candidate of candidates) {
    if (await fileExists(candidate)) {
      return candidate;
    }
  }
  return materializeEmbeddedCompilerWasm();
}

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/run-clapse-compiler-wasm.mjs <clapse-args...>",
    "",
    "Supported commands:",
    "  compile <input.clapse> [output.wasm]",
    "  compile-debug <input.clapse> [output.wasm] [artifacts-dir]",
    "  selfhost-artifacts <input.clapse> <out-dir>",
    "  format <file>",
    "  format --write <file>",
    "  format --stdin",
    "  lsp [--stdio]",
    "  engine-mode",
    "",
    "Compiler wasm resolution:",
    "  1) CLAPSE_COMPILER_WASM_PATH",
    "  2) artifacts/latest/clapse_compiler.wasm",
    "  3) out/clapse_compiler.wasm",
    "  4) embedded compiler wasm bundled in the clapse binary",
    "",
    "Required compiler wasm ABI:",
    "  export memory or __memory",
    "  export clapse_run(request_slice_handle: i32) -> response_slice_handle: i32",
    "  Request and response are UTF-8 JSON in slice descriptors.",
  ].join("\n");
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
const COMPILE_DEBUG_ARTIFACT_FILES = ["lowered_ir.txt", "collapsed_ir.txt"];

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

function shouldInjectFuncMapInFixedPointArtifacts() {
  const raw = String(Deno.env.get("CLAPSE_DEBUG_FUNC_MAP") ?? "").trim().toLowerCase();
  return raw === "1" || raw === "true" || raw === "yes";
}

function isCompilerKernelPath(inputPath) {
  const normalized = String(inputPath).replaceAll("\\", "/");
  return normalized === "lib/compiler/kernel.clapse" ||
    normalized.endsWith("/lib/compiler/kernel.clapse");
}

async function writeCompileArtifacts(outputPath, response) {
  let wasmBytes = decodeWasmBase64(response.wasm_base64);
  if (shouldInjectFuncMapInFixedPointArtifacts()) {
    try {
      wasmBytes = appendClapseFuncMap(wasmBytes);
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      throw new Error(`compile artifact funcmap injection failed: ${msg}`);
    }
  }
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

function collectCompileDebugArtifacts(artifacts, contextLabel) {
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    throw new Error(`${contextLabel} must include object 'artifacts'`);
  }
  const out = {};
  const missing = [];
  for (const key of COMPILE_DEBUG_ARTIFACT_FILES) {
    if (typeof artifacts[key] !== "string") {
      missing.push(key);
      continue;
    }
    out[key] = artifacts[key];
  }
  if (missing.length > 0) {
    throw new Error(
      `${contextLabel} missing required debug artifacts. Expected response.artifacts keys: ${COMPILE_DEBUG_ARTIFACT_FILES.join(", ")}.`,
    );
  }
  return out;
}

function readCompileDebugArtifacts(response) {
  const artifacts = response?.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    return null;
  }
  return collectCompileDebugArtifacts(artifacts, "compile response for compile_mode debug");
}

async function readCompileDebugArtifactsViaSelfhost(wasmPath, inputPath, inputSource) {
  const response = await callCompilerWasm(wasmPath, {
    command: "selfhost-artifacts",
    input_path: inputPath,
    input_source: inputSource,
  });
  assertObject(response, `selfhost-artifacts response for compile_mode debug (${inputPath})`);
  if (typeof response.ok !== "boolean") {
    throw new Error(`selfhost-artifacts response for compile_mode debug (${inputPath}) missing boolean 'ok'`);
  }
  if (!response.ok) {
    const err = typeof response.error === "string"
      ? response.error
      : `selfhost-artifacts error in ${inputPath}`;
    throw new Error(err);
  }
  return collectCompileDebugArtifacts(
    response.artifacts,
    `selfhost-artifacts response for compile_mode debug (${inputPath})`,
  );
}

async function writeCompileDebugArtifacts(artifactsDir, artifacts) {
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    throw new Error("writeCompileDebugArtifacts requires artifact map");
  }
  await Deno.mkdir(artifactsDir, { recursive: true });
  for (const key of COMPILE_DEBUG_ARTIFACT_FILES) {
    await Deno.writeTextFile(`${artifactsDir}/${key}`, artifacts[key]);
  }
}

async function compileViaWasm(wasmPath, inputPath, outputPath, options = {}) {
  const inputBytes = await Deno.readFile(inputPath);
  const inputSource = new TextDecoder().decode(inputBytes);
  const isKernelCompile = isCompilerKernelPath(inputPath);
  const pluginWasmPaths = Array.isArray(options.pluginWasmPaths)
    ? options.pluginWasmPaths
    : (!options.skipPluginCompilation && !isKernelCompile
      ? await compilePluginsWasm(wasmPath, inputPath)
      : []);
  const request = {
    command: "compile",
    input_path: inputPath,
    input_source: inputSource,
    plugin_wasm_paths: pluginWasmPaths,
  };
  if (typeof options.compileMode === "string" && options.compileMode.length > 0) {
    request.compile_mode = options.compileMode;
  }
  const response = await callCompilerWasm(wasmPath, request);
  const decodedResponse = decodeCompileResponse(response, inputPath);
  let debugArtifacts;
  if (typeof options.artifactsDir === "string" && options.artifactsDir.length > 0) {
    debugArtifacts = readCompileDebugArtifacts(decodedResponse);
    if (debugArtifacts === null) {
      debugArtifacts = await readCompileDebugArtifactsViaSelfhost(
        wasmPath,
        inputPath,
        inputSource,
      );
    }
  }
  await writeCompileArtifacts(outputPath, decodedResponse);
  if (debugArtifacts) {
    await writeCompileDebugArtifacts(options.artifactsDir, debugArtifacts);
  }
}

async function writeSelfhostArtifactsViaWasm(wasmPath, inputPath, outDir) {
  const inputSource = await Deno.readTextFile(inputPath);
  const request = {
    command: "selfhost-artifacts",
    input_path: inputPath,
    input_source: inputSource,
  };
  const response = await callCompilerWasm(wasmPath, request);
  const isPlaceholderResponse =
    response?.ok === true && shouldFallbackSelfhostArtifactsResponse(response, inputSource);
  if (isPlaceholderResponse) {
    throw new Error(
      `selfhost-artifacts response for ${inputPath} is placeholder/incomplete; expected native kernel artifacts`,
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
  await writeSelfhostArtifacts(outDir, artifacts);
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

function stripFrameTokenPunctuation(rawToken) {
  return String(rawToken)
    .replace(/^[\s"'`]+/u, "")
    .replace(/[)\]},"';]+$/u, "");
}

function parseDenoWasmStackOffset(rawToken) {
  const token = stripFrameTokenPunctuation(rawToken);
  const wasmAt = token.indexOf("wasm://");
  if (wasmAt < 0) {
    return null;
  }
  const marker = token.slice(wasmAt);
  if (!marker.startsWith("wasm://")) {
    return null;
  }
  const lastColon = marker.lastIndexOf(":");
  if (lastColon <= 0 || lastColon === marker.length - 1) {
    return null;
  }
  const secondLastColon = marker.lastIndexOf(":", lastColon - 1);
  if (secondLastColon <= 0 || secondLastColon + 1 >= lastColon) {
    return null;
  }
  const lineText = marker.slice(secondLastColon + 1, lastColon);
  const offsetText = marker.slice(lastColon + 1);
  if (!/^\d+$/u.test(lineText) || !/^\d+$/u.test(offsetText)) {
    return null;
  }
  return Number.parseInt(offsetText, 10);
}

function extractWasmOffsetsFromStack(err) {
  const stack = typeof err?.stack === "string" ? err.stack : "";
  if (stack.length === 0) {
    return [];
  }
  const out = [];
  const seen = new Set();
  const pushOffset = (value) => {
    if (!Number.isFinite(value) || value <= 0) {
      return;
    }
    if (seen.has(value)) {
      return;
    }
    seen.add(value);
    out.push(value);
  };
  const framePattern = /wasm-function\[\d+\](?::0x([0-9a-fA-F]+)|\+(\d+))/g;
  for (const match of stack.matchAll(framePattern)) {
    let value = NaN;
    if (typeof match[1] === "string" && match[1].length > 0) {
      value = Number.parseInt(match[1], 16);
    } else if (typeof match[2] === "string" && match[2].length > 0) {
      value = Number.parseInt(match[2], 10);
    }
    pushOffset(value);
    if (out.length >= 24) {
      break;
    }
  }
  if (out.length < 24) {
    const denoWasmFramePattern = /wasm:\/\/[^\s)]+/g;
    for (const match of stack.matchAll(denoWasmFramePattern)) {
      const value = parseDenoWasmStackOffset(match[0]);
      if (value === null) {
        continue;
      }
      pushOffset(value);
      if (out.length >= 24) {
        break;
      }
    }
  }
  return out;
}

function buildFormatterStackMapHint(wasmPath, err) {
  const offsets = extractWasmOffsetsFromStack(err);
  if (offsets.length === 0) {
    return "";
  }
  const offsetCsv = offsets.join(",");
  return [
    "",
    `[clapse] formatter wasm stack offsets: ${offsetCsv}`,
    `[clapse] map with: deno run -A scripts/wasm-stack-map.mjs --wasm ${JSON.stringify(wasmPath)} --offsets ${offsetCsv}`,
  ].join("\n");
}

function isFormatterStackIdentityFallbackEnabled() {
  const raw = String(Deno.env.get("CLAPSE_FORMAT_STACK_IDENTITY_FALLBACK") ?? "")
    .trim()
    .toLowerCase();
  if (raw === "0" || raw === "false" || raw === "off" || raw === "no") {
    return false;
  }
  return true;
}

async function formatRequestWithFallback(wasmPath, request, source, ctx) {
  try {
    const response = await callCompilerWasm(wasmPath, request);
    return decodeFormatResponse(response, ctx);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    const isStackOverflow = message.includes("Maximum call stack size exceeded");
    if (isStackOverflow) {
      const hint = buildFormatterStackMapHint(wasmPath, err);
      if (isFormatterStackIdentityFallbackEnabled()) {
        const lines = [
          `[clapse] formatter stack overflow in ${ctx}; returning input unchanged`,
          hint.trim(),
        ].filter((line) => line.length > 0);
        console.error(lines.join("\n"));
        return source;
      }
      if (err instanceof Error) {
        err.message = `format error: ${ctx}: ${message}${hint}`;
        throw err;
      }
      throw new Error(`format error: ${ctx}: ${message}${hint}`);
    }
    throw err;
  }
}

async function formatViaWasm(wasmPath, args) {
  if (args.length === 2 && args[1] === "--stdin") {
    const src = await readAllStdin();
    const formatted = await formatRequestWithFallback(wasmPath, {
      command: "format",
      mode: "stdout",
      input_path: "<stdin>",
      source: src,
    }, src, "stdin");
    await Deno.stdout.write(new TextEncoder().encode(formatted));
    return;
  }
  if (args.length === 3 && args[1] === "--write") {
    const path = args[2];
    const src = await Deno.readTextFile(path);
    const formatted = await formatRequestWithFallback(wasmPath, {
      command: "format",
      mode: "write",
      input_path: path,
      source: src,
    }, src, path);
    await Deno.writeTextFile(path, formatted);
    return;
  }
  if (args.length === 2) {
    const path = args[1];
    const src = await Deno.readTextFile(path);
    const formatted = await formatRequestWithFallback(wasmPath, {
      command: "format",
      mode: "stdout",
      input_path: path,
      source: src,
    }, src, path);
    await Deno.stdout.write(new TextEncoder().encode(formatted));
    return;
  }
  throw new Error(
    "usage: format <file> | format --write <file> | format --stdin",
  );
}

export async function runWithArgs(rawArgs = cliArgs()) {
  let args = [...rawArgs];
  if (args.length > 0 && args[0] === "--") {
    args = args.slice(1);
  }
  const wasmPath = await resolveCompilerWasmPath();
  if (args.length === 1 && args[0] === "engine-mode") {
    if (wasmPath.length > 0 && await fileExists(wasmPath)) {
      await validateCompilerWasmAbi(wasmPath);
      console.log("wasm-native");
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
      "compiler-wasm engine requires CLAPSE_COMPILER_WASM_PATH, embedded compiler wasm, or artifacts/latest|out compiler wasm files",
    );
  }
  if (!(await fileExists(wasmPath))) {
    throw new Error(
      `compiler-wasm path not found: ${wasmPath}. Set CLAPSE_COMPILER_WASM_PATH to an existing artifact.`,
    );
  }
  await validateCompilerWasmAbi(wasmPath);
  if (args[0] === "compile") {
    if (args.length < 2 || args.length > 3) {
      throw new Error("usage: compile <input.clapse> [output.wasm]");
    }
    const inputPath = args[1];
    const outputPath = args[2] ?? inputPath.replace(/\.clapse$/u, ".wasm");
    await compileViaWasm(wasmPath, inputPath, outputPath);
    return;
  }
  if (args[0] === "compile-debug") {
    if (args.length < 2 || args.length > 4) {
      throw new Error(
        "usage: compile-debug <input.clapse> [output.wasm] [artifacts-dir]",
      );
    }
    const inputPath = args[1];
    const outputPath = args[2] ?? inputPath.replace(/\.clapse$/u, ".wasm");
    const defaultArtifactsDir = pathDir(outputPath);
    const artifactsDir = args[3] ?? (defaultArtifactsDir.length > 0 ? defaultArtifactsDir : ".");
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "debug",
      artifactsDir,
    });
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
    Deno.env.set("CLAPSE_COMPILER_WASM_PATH", wasmPath);
    await runLspServer();
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

if (import.meta.main) {
  await runWithArgs().catch(failWithError);
}
