#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";
import { runWithArgs } from "./run-clap-compiler-wasm.mjs";

function usage() {
  return [
    "clap (wasm-first frontend)",
    "",
    "Usage:",
    "  clap [--wasm] <command> [args...]",
    "  deno run -A scripts/clap.mjs [--wasm] <command> [args...]",
    "",
    "Commands:",
    "  compile <input.clap> [output.wasm]",
    "  compile-native <input.clap> [output.wasm] (alias: compile_native)",
    "  compile-native-debug <input.clap> [output.wasm] [artifacts-dir] (alias: compile_native_debug)",
    "  compile-debug <input.clap> [output.wasm] [artifacts-dir] (alias: compile_debug)",
    "  parse <input.clap> [out-dir]",
    "  emit-wat <input.clap> [output.wat]",
    "  selfhost-artifacts <input.clap> <out-dir>",
    "  format <file>",
    "  format --write <file>",
    "  format --stdin",
    "  lsp [--stdio]",
    "  engine-mode",
    "",
    "Environment:",
    "  CLAP_COMPILER_WASM_PATH=<path>   optional; defaults to artifacts/latest/clap_compiler.wasm",
    "",
    "Notes:",
    "  Non-wasm execution is deprecated and removed. Use wasm compiler artifacts.",
  ].join("\n");
}

function normalizeArgs(args) {
  const out = [];
  for (const arg of args) {
    if (arg === "--wasm") continue;
    if (arg === "--host") {
      throw new Error(
        "--host is no longer supported; use wasm compiler artifacts",
      );
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
    throw new Error(
      "bench command moved out of clap frontend; run wasm-specific benches directly",
    );
  }
  await runWithArgs(args);
}

if (import.meta.main) {
  await main().catch(failWithError);
}
