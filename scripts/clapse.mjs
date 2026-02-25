#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";

function usage() {
  return [
    "clapse (deno frontend)",
    "",
    "Usage:",
    "  deno run -A scripts/clapse.mjs [--wasm|--host] <command> [args...]",
    "",
    "Commands:",
    "  compile <input.clapse> [output.wasm]      (wasm path by default)",
    "  selfhost-artifacts <input.clapse> <out-dir> (wasm path by default)",
    "  engine-mode                               (wasm runner probe)",
    "  format <file>",
    "  format --write <file>",
    "  format --stdin",
    "  lsp [--stdio]",
    "  bench [iterations]",
    "",
    "Mode flags:",
    "  --wasm   force compiler wasm path (requires CLAPSE_COMPILER_WASM_PATH)",
    "  --host   force host cabal path",
    "",
    "Environment:",
    "  CLAPSE_COMPILER_WASM_PATH=<path>   required for wasm compile path",
  ].join("\n");
}

function splitModeFlags(args) {
  let mode = "auto";
  const rest = [];
  for (const arg of args) {
    if (arg === "--wasm") {
      if (mode === "host") throw new Error("cannot combine --wasm and --host");
      mode = "wasm";
      continue;
    }
    if (arg === "--host") {
      if (mode === "wasm") throw new Error("cannot combine --wasm and --host");
      mode = "host";
      continue;
    }
    rest.push(arg);
  }
  return { mode, rest };
}

function defaultHostEnv() {
  const env = Deno.env.toObject();
  if (!("CABAL_DIR" in env)) {
    env.CABAL_DIR = `${Deno.cwd()}/.cabal`;
  }
  if (!("CABAL_LOGDIR" in env)) {
    env.CABAL_LOGDIR = `${Deno.cwd()}/.cabal-logs`;
  }
  return env;
}

async function run(cmd, args, env) {
  const proc = new Deno.Command(cmd, {
    args,
    env,
    stdin: "inherit",
    stdout: "inherit",
    stderr: "inherit",
  });
  const child = proc.spawn();
  const status = await child.status;
  if (!status.success) {
    Deno.exit(status.code || 1);
  }
}

async function hasExistingPath(path) {
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
    ? [
      "artifacts/latest/clapse_compiler.wasm",
      "artifacts/latest/clapse_compiler_bridge.wasm",
      "out/clapse_compiler.wasm",
      "out/clapse_compiler_bridge.wasm",
    ]
    : ["artifacts/latest/clapse_compiler.wasm", "out/clapse_compiler.wasm"];
  for (const candidate of candidates) {
    if (await hasExistingPath(candidate)) {
      return candidate;
    }
  }
  return "";
}

async function runViaWasmRunner(commandArgs) {
  const wasmPath = await resolveCompilerWasmPath();
  if (wasmPath.length === 0) {
    throw new Error(
      "CLAPSE_COMPILER_WASM_PATH is required for wasm mode (or place compiler wasm at artifacts/latest/clapse_compiler.wasm / artifacts/latest/clapse_compiler_bridge.wasm / out/clapse_compiler.wasm / out/clapse_compiler_bridge.wasm)",
    );
  }
  if (!(await hasExistingPath(wasmPath))) {
    throw new Error(`compiler wasm path not found: ${wasmPath}`);
  }
  const env = Deno.env.toObject();
  env.CLAPSE_COMPILER_WASM_PATH = wasmPath;
  await run(
    "deno",
    ["run", "-A", "scripts/run-clapse-compiler-wasm.mjs", "--", ...commandArgs],
    env,
  );
}

async function runViaHost(commandArgs) {
  await Deno.mkdir(".cabal-logs", { recursive: true });
  await run("cabal", ["run", "clapse", "--", ...commandArgs], defaultHostEnv());
}

function preferWasm(mode) {
  if (mode === "wasm") return true;
  if (mode === "host") return false;
  return true;
}

async function main() {
  let args = cliArgs();
  if (args.length > 0 && args[0] === "--") {
    args = args.slice(1);
  }
  const parsed = splitModeFlags(args);
  const rest = parsed.rest;
  if (rest.length === 0 || rest[0] === "--help" || rest[0] === "-h") {
    console.log(usage());
    return;
  }
  const command = rest[0];
  const commandArgs = rest;
  const wasmRoutedCommands = new Set([
    "compile",
    "selfhost-artifacts",
    "engine-mode",
    "format",
    "lsp",
  ]);
  const hostOnlyCommands = new Set(["bench"]);

  if (wasmRoutedCommands.has(command)) {
    if (preferWasm(parsed.mode)) {
      await runViaWasmRunner(commandArgs);
      return;
    }
    await runViaHost(commandArgs);
    return;
  }
  if (hostOnlyCommands.has(command)) {
    await runViaHost(commandArgs);
    return;
  }

  throw new Error(`unknown command: ${command}`);
}

await main().catch(failWithError);
