#!/usr/bin/env -S deno run -A

import { buildWasmSeedCompileResponse } from "./wasm-bootstrap-seed.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function hasSyntheticArtifactMarkers(value) {
  if (typeof value !== "string") {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

function decodeBase64(raw) {
  const binary = atob(raw);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}

function resolveWasmPath(argv) {
  for (let i = 0; i < argv.length; i += 1) {
    if (argv[i] === "--wasm") {
      const candidate = argv[i + 1] ?? "";
      if (candidate.length === 0) {
        throw new Error("native-bootstrap-seed-smoke: missing value for --wasm");
      }
      return candidate;
    }
  }
  const envPath = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "").trim();
  if (envPath.length > 0) {
    return envPath;
  }
  return "artifacts/latest/clapse_compiler.wasm";
}

async function runRunnerCompileSmoke(wasmPath, sourceText, probeToken) {
  const tmpDir = await Deno.makeTempDir({
    prefix: "clapse-wasm-bootstrap-seed-",
  });
  const inputPath = `${tmpDir}/seed_smoke_input.clapse`;
  const outputPath = `${tmpDir}/seed_smoke_output.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  await Deno.writeTextFile(inputPath, sourceText);

  const env = {
    ...Deno.env.toObject(),
    CLAPSE_USE_WASM_BOOTSTRAP_SEED: "1",
    CLAPSE_COMPILER_WASM_PATH: wasmPath,
  };
  const cmd = new Deno.Command(Deno.execPath(), {
    args: [
      "run",
      "-A",
      "scripts/run-clapse-compiler-wasm.mjs",
      "compile-native-debug",
      inputPath,
      outputPath,
      artifactsDir,
    ],
    env,
    stdout: "piped",
    stderr: "piped",
  });
  const result = await cmd.output();
  if (result.code !== 0) {
    const stderr = new TextDecoder().decode(result.stderr).trim();
    const stdout = new TextDecoder().decode(result.stdout).trim();
    throw new Error(
      `native-bootstrap-seed-smoke: run-clapse compile-native-debug failed (code=${result.code})\nstdout: ${stdout}\nstderr: ${stderr}`,
    );
  }

  const compiledWasm = await Deno.readFile(outputPath);
  assert(
    compiledWasm.length >= 4096,
    "native-bootstrap-seed-smoke: runner output wasm is too small",
  );
  assert(
    compiledWasm[0] === 0x00 && compiledWasm[1] === 0x61 &&
      compiledWasm[2] === 0x73 && compiledWasm[3] === 0x6d,
    "native-bootstrap-seed-smoke: runner output wasm magic header mismatch",
  );
  const module = new WebAssembly.Module(compiledWasm);
  const exportNames = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  assert(
    exportNames.includes("clapse_run"),
    "native-bootstrap-seed-smoke: runner output missing clapse_run export",
  );
  assert(
    exportNames.includes("memory") || exportNames.includes("__memory"),
    "native-bootstrap-seed-smoke: runner output missing memory export",
  );

  const lowered = await Deno.readTextFile(`${artifactsDir}/lowered_ir.txt`);
  const collapsed = await Deno.readTextFile(`${artifactsDir}/collapsed_ir.txt`);
  assert(
    !hasSyntheticArtifactMarkers(lowered),
    "native-bootstrap-seed-smoke: lowered_ir.txt contains synthetic markers",
  );
  assert(
    !hasSyntheticArtifactMarkers(collapsed),
    "native-bootstrap-seed-smoke: collapsed_ir.txt contains synthetic markers",
  );
  assert(
    lowered.includes("main x = x") && collapsed.includes("main x = x"),
    "native-bootstrap-seed-smoke: runner artifacts missing source content",
  );
  assert(
    lowered.includes(probeToken) && collapsed.includes(probeToken),
    "native-bootstrap-seed-smoke: runner artifacts missing probe token",
  );
}

async function run() {
  const wasmPath = resolveWasmPath(Deno.args);
  const probeToken = `native-bootstrap-seed-smoke-${crypto.randomUUID()}`;
  const sourceText = [`main x = x`, `-- ${probeToken}`, ""].join("\n");

  const response = await buildWasmSeedCompileResponse(
    {
      command: "compile",
      compile_mode: "kernel-native",
      input_path: "examples/native_bootstrap_seed_smoke.clapse",
      input_source: sourceText,
      plugin_wasm_paths: [],
    },
    { seedWasmPath: wasmPath },
  );

  assert(
    response && typeof response === "object" && response.ok === true,
    "native-bootstrap-seed-smoke: bootstrap response should be ok=true object",
  );
  assert(
    response.backend === "kernel-native",
    `native-bootstrap-seed-smoke: expected backend=kernel-native (got ${String(response.backend)})`,
  );
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    "native-bootstrap-seed-smoke: missing wasm_base64",
  );

  const seedWasmBytes = decodeBase64(response.wasm_base64);
  const seedWasmModule = new WebAssembly.Module(seedWasmBytes);
  const seedExportNames = WebAssembly.Module.exports(seedWasmModule).map((entry) =>
    entry.name
  );
  assert(
    seedExportNames.includes("clapse_run"),
    "native-bootstrap-seed-smoke: seed response wasm missing clapse_run export",
  );
  assert(
    seedExportNames.includes("memory") || seedExportNames.includes("__memory"),
    "native-bootstrap-seed-smoke: seed response wasm missing memory export",
  );

  await runRunnerCompileSmoke(wasmPath, sourceText, probeToken);
  console.log(`native-bootstrap-seed-smoke: PASS (${wasmPath})`);
}

await run();
