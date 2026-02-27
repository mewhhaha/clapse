#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";
import { decodeInt, isTaggedInt } from "./wasm-runtime.mjs";

const ENTRY = "examples/wildcard_demand_behavior_regressions.clapse";
const EXPECTED_SCENARIOS = [
  { exportName: "main_wildcard_not_force", expected: 11 },
  { exportName: "main_priority_after_reorder", expected: 31 },
  { exportName: "main_ordered_demand", expected: 41 },
];

function decodeUtf8(bytes) {
  return new TextDecoder().decode(bytes);
}

function resolveCompilerWasmPath() {
  const candidates = [
    String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "").trim(),
    "artifacts/latest/clapse_compiler.wasm",
    "out/clapse_compiler.wasm",
  ];
  for (const candidate of candidates) {
    if (candidate.length === 0) continue;
    try {
      const stat = Deno.statSync(candidate);
      if (stat.isFile && stat.size > 0) {
        return candidate;
      }
    } catch {
      // continue
    }
  }
  throw new Error(
    "wildcard-demand-check: compiler wasm not found; set CLAPSE_COMPILER_WASM_PATH or build artifacts/latest|out/clapse_compiler.wasm",
  );
}

async function compileFixture(compilerPath, outWasmPath) {
  const proc = await new Deno.Command("deno", {
    args: [
      "run",
      "-A",
      "scripts/run-clapse-compiler-wasm.mjs",
      "compile",
      ENTRY,
      outWasmPath,
    ],
    env: {
      ...Deno.env.toObject(),
      CLAPSE_COMPILER_WASM_PATH: compilerPath,
    },
    stdout: "piped",
    stderr: "piped",
  }).output();

  if (!proc.success) {
    const stderr = decodeUtf8(proc.stderr).trim();
    const stdout = decodeUtf8(proc.stdout).trim();
    throw new Error(
      `wildcard-demand-check: compile failed (${stderr || stdout || `exit=${proc.code}`})`,
    );
  }
}

function assertFnExport(instance, name) {
  const fn = instance.exports[name];
  if (typeof fn !== "function") {
    throw new Error(`wildcard-demand-check: missing export '${name}'`);
  }
  return fn;
}

function normalizeResult(value) {
  const n = Number(value);
  if (!Number.isInteger(n)) {
    return n;
  }
  if (isTaggedInt(n)) {
    return decodeInt(n);
  }
  return n;
}

async function main() {
  const compilerPath = resolveCompilerWasmPath();
  const outWasmPath = await Deno.makeTempFile({
    prefix: "clapse_wildcard_demand_",
    suffix: ".wasm",
    dir: "/tmp",
  });

  try {
    await compileFixture(compilerPath, outWasmPath);
    const wasmBytes = await Deno.readFile(outWasmPath);
    const module = await WebAssembly.compile(wasmBytes);
    const instance = await WebAssembly.instantiate(module, {});

    for (const scenario of EXPECTED_SCENARIOS) {
      const fn = assertFnExport(instance, scenario.exportName);
      const actual = normalizeResult(fn());
      if (actual !== scenario.expected) {
        throw new Error(
          `wildcard-demand-check: ${scenario.exportName} expected ${scenario.expected}, got ${actual}`,
        );
      }
    }

    console.log(
      `wildcard-demand-check: PASS (${EXPECTED_SCENARIOS.length} scenarios)`,
    );
  } finally {
    await Deno.remove(outWasmPath).catch(() => {});
  }
}

await main().catch(failWithError);
