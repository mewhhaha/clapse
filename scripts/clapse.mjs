#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";

function usage() {
  return [
    "clapse (wasm-first frontend)",
    "",
    "Usage:",
    "  deno run -A scripts/clapse.mjs [--wasm] <command> [args...]",
    "",
    "Commands:",
    "  compile <input.clapse> [output.wasm]",
    "  selfhost-artifacts <input.clapse> <out-dir>",
    "  format <file>",
    "  format --write <file>",
    "  format --stdin",
    "  lsp [--stdio]",
    "  engine-mode",
    "",
    "Environment:",
    "  CLAPSE_COMPILER_WASM_PATH=<path>   optional; defaults to artifacts/latest/clapse_compiler.wasm",
    "",
    "Notes:",
    "  Non-wasm execution is deprecated and removed. Use wasm compiler artifacts.",
  ].join("\n");
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

function normalizeArgs(args) {
  const out = [];
  for (const arg of args) {
    if (arg === "--wasm") continue;
    if (arg === "--host") {
      throw new Error("--host is no longer supported; use wasm compiler artifacts");
    }
    out.push(arg);
  }
  return out;
}

async function main() {
  let args = cliArgs();
  if (args.length > 0 && args[0] === "--") {
    args = args.slice(1);
  }
  args = normalizeArgs(args);
  if (args.length === 0 || args[0] === "--help" || args[0] === "-h") {
    console.log(usage());
    return;
  }
  if (args[0] === "bench") {
    throw new Error("bench command moved out of clapse frontend; run wasm-specific benches directly");
  }
  await run(
    "deno",
    ["run", "-A", "scripts/run-clapse-compiler-wasm.mjs", "--", ...args],
    Deno.env.toObject(),
  );
}

await main().catch(failWithError);
