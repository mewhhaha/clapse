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
    "  Set CLAPSE_ALLOW_HOST_COMPILE_FALLBACK=1 to allow compile-only fallback",
    "  to host cabal runner when native wasm returns \"not implemented yet\".",
  ].join("\n");
}

function envFlag(name) {
  const raw = (Deno.env.get(name) ?? "").toLowerCase();
  return raw === "1" || raw === "true" || raw === "yes";
}

function allowHostCompileFallback() {
  return envFlag("CLAPSE_ALLOW_HOST_COMPILE_FALLBACK");
}

function shouldFallbackCompileFromError(err) {
  return typeof err === "string" && err.includes("native compile not implemented yet");
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

async function compileViaWasm(wasmPath, inputPath, outputPath) {
  const inputSource = await Deno.readTextFile(inputPath);
  const raw = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: inputPath,
    input_source: inputSource,
  });
  if (
    raw &&
    typeof raw === "object" &&
    raw.ok === false &&
    allowHostCompileFallback() &&
    shouldFallbackCompileFromError(raw.error)
  ) {
    console.error(
      "[clapse] native wasm compile unavailable; falling back to host compile due to CLAPSE_ALLOW_HOST_COMPILE_FALLBACK=1",
    );
    await compileViaHostFallback(inputPath, outputPath);
    return;
  }
  const response = decodeCompileResponse(raw, inputPath);
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

async function writeSelfhostArtifactsViaWasm(wasmPath, inputPath, outDir) {
  const inputSource = await Deno.readTextFile(inputPath);
  const response = await callCompilerWasm(wasmPath, {
    command: "selfhost-artifacts",
    input_path: inputPath,
    input_source: inputSource,
  });
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
  const files = [
    "merged_module.txt",
    "type_info.txt",
    "type_info_error.txt",
    "lowered_ir.txt",
    "collapsed_ir.txt",
    "exports.txt",
    "wasm_stats.txt",
  ];
  await Deno.mkdir(outDir, { recursive: true });
  for (const file of files) {
    const value = artifacts[file];
    if (value === undefined) {
      throw new Error(
        `selfhost-artifacts response: missing required key '${file}'`,
      );
    }
    if (typeof value !== "string") {
      throw new Error(
        `selfhost-artifacts response: '${file}' must be a string`,
      );
    }
    await Deno.writeTextFile(`${outDir}/${file}`, value);
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
