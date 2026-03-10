#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

function assertEqual(label, expected, actual) {
  if (expected !== actual) {
    throw new Error(`${label}: expected ${expected}, got ${actual}`);
  }
}

function decodeOutput(bytes) {
  const decoder = new TextDecoder();
  return decoder.decode(bytes);
}

function resolveAtRepoRoot(relPath) {
  return decodeURIComponent(new URL(relPath, new URL("../", import.meta.url)).pathname);
}

async function removeIfExists(path) {
  try {
    await Deno.remove(path);
  } catch (err) {
    if (err instanceof Deno.errors.NotFound) {
      return;
    }
    throw err;
  }
}

async function runCompiler(envPath, args) {
  const proc = new Deno.Command("deno", {
    args,
    env: { CLAPSE_COMPILER_WASM_PATH: envPath },
    stdout: "piped",
    stderr: "piped",
  });
  const result = await proc.output();
  if (result.code !== 0) {
    const stderr = decodeOutput(result.stderr);
    throw new Error(`compiler command failed (code ${result.code}): ${stderr}`);
  }
  return decodeOutput(result.stdout);
}

async function statMTimeMs(path) {
  const fileInfo = await Deno.stat(path);
  return fileInfo.mtime?.getTime() ?? -1;
}

async function main() {
  const [fixtureArg = "examples/fib_memo.clapse"] = cliArgs();
  const fixturePath = fixtureArg;
  const outputWasm = "out/fib_memo_plugin_smoke.wasm";
  const pluginWasm = "examples/plugins/memo_fib_plugin.wasm";
  const pluginDts = "examples/plugins/memo_fib_plugin.d.ts";
  const compilerPath = Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ??
    "artifacts/latest/clapse_compiler.wasm";

  await removeIfExists(pluginWasm);
  await removeIfExists(pluginDts);
  await removeIfExists(outputWasm);
  const pluginWasmAbs = resolveAtRepoRoot(pluginWasm);

  const compileStart = Date.now();
  const compileOutput = await runCompiler(compilerPath, [
    "run",
    "-A",
    "scripts/clapse.mjs",
    "compile",
    fixturePath,
    outputWasm,
  ]);
  if (compileOutput.length > 0) {
    console.log(compileOutput.trim());
  }

  const pluginMtime = await statMTimeMs(pluginWasmAbs);
  if (pluginMtime < compileStart) {
    throw new Error("plugin precompile artifact was not produced during smoke run");
  }

  const outputWasmBytes = await Deno.readFile(outputWasm);
  const { instance } = await instantiateWithRuntime(outputWasmBytes);
  const mainFn = instance.exports.main;
  if (typeof mainFn !== "function") {
    throw new Error("missing export: main");
  }
  const result = decodeInt(mainFn(encodeInt(12)));
  assertEqual("fib(12)", 144, result);

  console.log("fib memo plugin smoke: PASS (memoized compile pre-step verified)");
}

main().catch(failWithError);
