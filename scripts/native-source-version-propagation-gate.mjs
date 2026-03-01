#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";

const DEFAULT_COMPILER_WASM_PATH = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_INPUT_PATH = "lib/compiler/kernel.clapse";
const DEFAULT_HOPS = 2;
const DEFAULT_COMPILE_MODE = "kernel-native";
const MIN_COMPILER_BYTES = 4096;
const NATIVE_COMPILE_SOURCE_PATH = "lib/compiler/native_compile.clapse";
const REQUIRED_SOURCE_VERSION_ENV = "CLAPSE_NATIVE_SOURCE_VERSION_REQUIRED";
const EXPECTED_COMPILE_CONTRACT_VERSION = "native-v1";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/native-source-version-propagation-gate.mjs [--wasm <path>] [--input <path>] [--hops <n>] [--source-version <token>] [--out <path>] [--keep-temp]",
    "",
    "Compiles kernel source with the selected compiler wasm, then probes the",
    "produced compiler artifact with native-producer-raw-probe while requiring",
    "__clapse_contract.source_version transitivity.",
    "",
    `Env fallback: ${REQUIRED_SOURCE_VERSION_ENV}=<token>`,
  ].join("\n");
}

function fail(msg) {
  console.error(`native-source-version-propagation-gate: FAIL (${msg})`);
  Deno.exit(1);
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

function parseArgs(argv) {
  let wasmPath = DEFAULT_COMPILER_WASM_PATH;
  let inputPath = DEFAULT_INPUT_PATH;
  let hops = DEFAULT_HOPS;
  let sourceVersion = "";
  let outPath = "";
  let keepTemp = boolEnvFlag("CLAPSE_NATIVE_SOURCE_GATE_KEEP_TEMP", false);
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--help" || arg === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (arg === "--wasm") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        fail("missing value for --wasm");
      }
      wasmPath = value;
      i += 1;
      continue;
    }
    if (arg === "--input") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        fail("missing value for --input");
      }
      inputPath = value;
      i += 1;
      continue;
    }
    if (arg === "--hops") {
      const raw = argv[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        fail(`invalid --hops value: ${raw}`);
      }
      hops = parsed;
      i += 1;
      continue;
    }
    if (arg === "--source-version") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        fail("missing value for --source-version");
      }
      sourceVersion = value;
      i += 1;
      continue;
    }
    if (arg === "--out") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        fail("missing value for --out");
      }
      outPath = value;
      i += 1;
      continue;
    }
    if (arg === "--keep-temp") {
      keepTemp = true;
      continue;
    }
    fail(`unknown argument: ${arg}`);
  }
  return {
    wasmPath,
    inputPath,
    hops,
    sourceVersion,
    outPath,
    keepTemp,
  };
}

async function extractSourceVersionFromNativeCompile() {
  let sourceText = "";
  try {
    sourceText = await Deno.readTextFile(NATIVE_COMPILE_SOURCE_PATH);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(
      `failed reading ${NATIVE_COMPILE_SOURCE_PATH} while resolving required source version (${msg})`,
    );
  }
  const escapedMatch = sourceText.match(
    /source_version\\":\\"([^"\\]+)\\",\\"compile_contract_version/u,
  );
  if (
    Array.isArray(escapedMatch) &&
    escapedMatch.length > 1 &&
    nonEmptyString(escapedMatch[1])
  ) {
    return escapedMatch[1];
  }
  const plainMatch = sourceText.match(
    /"source_version":"([^"]+)","compile_contract_version"/u,
  );
  if (
    Array.isArray(plainMatch) &&
    plainMatch.length > 1 &&
    nonEmptyString(plainMatch[1])
  ) {
    return plainMatch[1];
  }
  fail(
    `could not resolve __clapse_contract.source_version from ${NATIVE_COMPILE_SOURCE_PATH}`,
  );
}

async function resolveRequiredSourceVersion(cliSourceVersion) {
  if (nonEmptyString(cliSourceVersion)) {
    return cliSourceVersion;
  }
  const envVersion = String(Deno.env.get(REQUIRED_SOURCE_VERSION_ENV) ?? "")
    .trim();
  if (nonEmptyString(envVersion)) {
    return envVersion;
  }
  return extractSourceVersionFromNativeCompile();
}

function parseCompileContract(response, context) {
  const raw = response?.__clapse_contract;
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    fail(`${context}: compile response missing __clapse_contract object`);
  }
  const sourceVersion = raw.source_version;
  if (!nonEmptyString(sourceVersion)) {
    fail(`${context}: compile response missing __clapse_contract.source_version`);
  }
  const compileContractVersion = raw.compile_contract_version;
  if (compileContractVersion !== EXPECTED_COMPILE_CONTRACT_VERSION) {
    fail(
      `${context}: compile_contract_version must be ${EXPECTED_COMPILE_CONTRACT_VERSION} (got ${
        String(compileContractVersion ?? "<missing>")
      })`,
    );
  }
  return { sourceVersion, compileContractVersion };
}

function assertCompilerAbi(moduleBytes, context) {
  let module;
  try {
    module = new WebAssembly.Module(moduleBytes);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`${context}: emitted wasm is invalid (${msg})`);
  }
  const exportNames = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  if (!hasMemory) {
    fail(
      `${context}: emitted wasm missing memory export (${exportNames.join(",")})`,
    );
  }
  if (!exportNames.includes("clapse_run")) {
    fail(
      `${context}: emitted wasm missing clapse_run export (${
        exportNames.join(",")
      })`,
    );
  }
  if (moduleBytes.length < MIN_COMPILER_BYTES) {
    fail(
      `${context}: emitted wasm too small (${moduleBytes.length} < ${MIN_COMPILER_BYTES})`,
    );
  }
}

async function compileKernelWithCompilerWasm(
  wasmPath,
  inputPath,
  requiredSourceVersion,
) {
  let inputSource = "";
  try {
    inputSource = await Deno.readTextFile(inputPath);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`failed reading input source '${inputPath}' (${msg})`);
  }

  let response;
  try {
    response = await callCompilerWasmRaw(wasmPath, {
      command: "compile",
      compile_mode: DEFAULT_COMPILE_MODE,
      input_path: inputPath,
      input_source: inputSource,
      plugin_wasm_paths: [],
    });
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`kernel compile request failed (${msg})`);
  }

  if (!response || typeof response !== "object" || Array.isArray(response)) {
    fail("kernel compile response was not an object");
  }
  if (response.ok !== true) {
    fail(
      `kernel compile response returned ok=false (${
        String(response.error ?? "unknown")
      })`,
    );
  }
  if (response.backend !== DEFAULT_COMPILE_MODE) {
    fail(
      `kernel compile response backend must be ${DEFAULT_COMPILE_MODE} (got ${
        String(response.backend ?? "<missing>")
      })`,
    );
  }
  if (!nonEmptyString(response.wasm_base64)) {
    fail("kernel compile response missing non-empty wasm_base64");
  }
  const contract = parseCompileContract(response, "kernel compile");
  if (contract.sourceVersion !== requiredSourceVersion) {
    fail(
      `kernel compile source_version mismatch (expected ${requiredSourceVersion}, got ${contract.sourceVersion})`,
    );
  }

  let compilerBytes;
  try {
    compilerBytes = decodeWasmBase64(response.wasm_base64);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`failed decoding kernel compile wasm_base64 (${msg})`);
  }
  assertCompilerAbi(compilerBytes, "kernel compile");
  return {
    compilerBytes,
    sourceVersion: contract.sourceVersion,
    compileContractVersion: contract.compileContractVersion,
  };
}

function trimText(bytes) {
  return new TextDecoder().decode(bytes).trim();
}

async function runRawProbe(probeWasmPath, hops, requiredSourceVersion) {
  const scriptPath = decodeURIComponent(
    new URL("./native-producer-raw-probe.mjs", import.meta.url).pathname,
  );
  const cmd = new Deno.Command("deno", {
    args: [
      "run",
      "-A",
      scriptPath,
      "--wasm",
      probeWasmPath,
      "--hops",
      String(hops),
      "--require-source-version",
      requiredSourceVersion,
    ],
    stdout: "piped",
    stderr: "piped",
  });
  const output = await cmd.output();
  if (output.code !== 0) {
    const stderr = trimText(output.stderr);
    const stdout = trimText(output.stdout);
    const detail = stderr.length > 0 ? stderr : stdout;
    fail(`native-producer-raw-probe failed (${detail})`);
  }
  const probeOutput = trimText(output.stdout);
  if (probeOutput.length > 0) {
    console.log(probeOutput);
  }
}

async function main() {
  const opts = parseArgs(Deno.args);
  const requiredSourceVersion = await resolveRequiredSourceVersion(
    opts.sourceVersion,
  );
  const compileResult = await compileKernelWithCompilerWasm(
    opts.wasmPath,
    opts.inputPath,
    requiredSourceVersion,
  );

  let outPath = opts.outPath;
  let isTempOut = false;
  if (!nonEmptyString(outPath)) {
    outPath = await Deno.makeTempFile({
      prefix: "clapse-source-version-propagation-",
      suffix: ".wasm",
    });
    isTempOut = true;
  } else {
    const outDir = outPath.includes("/")
      ? outPath.slice(0, outPath.lastIndexOf("/"))
      : ".";
    if (outDir.length > 0 && outDir !== ".") {
      await Deno.mkdir(outDir, { recursive: true });
    }
  }

  try {
    await Deno.writeFile(outPath, compileResult.compilerBytes);
    await runRawProbe(outPath, opts.hops, requiredSourceVersion);
  } finally {
    if (isTempOut && !opts.keepTemp) {
      try {
        await Deno.remove(outPath);
      } catch {
        // best-effort cleanup
      }
    }
  }

  const outLabel = isTempOut && !opts.keepTemp ? "<temp-cleaned>" : outPath;
  console.log(
    `native-source-version-propagation-gate: PASS (bootstrap=${opts.wasmPath}; input=${opts.inputPath}; output=${outLabel}; hops=${opts.hops}; source_version=${requiredSourceVersion}; compile_contract_version=${compileResult.compileContractVersion}; output_bytes=${compileResult.compilerBytes.length})`,
  );
}

await main();
