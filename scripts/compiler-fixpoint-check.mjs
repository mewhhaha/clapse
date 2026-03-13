#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";

const DEFAULT_COMPILER_WASM = "artifacts/latest/clap_compiler.wasm";
const DEFAULT_COMPILER_DTS = "artifacts/latest/clap_compiler.d.ts";
const DEFAULT_MAX_COMPILER_BYTES = 67_108_864;

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAP_COMPILER_WASM_PATH") ?? "").trim();
  return fromEnv.length > 0 ? fromEnv : DEFAULT_COMPILER_WASM;
}

function resolveCompilerDtsPath() {
  const fromEnv = String(Deno.env.get("CLAP_COMPILER_DTS_PATH") ?? "").trim();
  return fromEnv.length > 0 ? fromEnv : DEFAULT_COMPILER_DTS;
}

function resolveMaxCompilerBytes() {
  const raw = String(Deno.env.get("CLAP_MAX_COMPILER_WASM_BYTES") ?? "").trim();
  if (raw.length === 0) {
    return DEFAULT_MAX_COMPILER_BYTES;
  }
  const parsed = Number(raw);
  if (!Number.isInteger(parsed) || parsed <= 0) {
    throw new Error(
      `invalid CLAP_MAX_COMPILER_WASM_BYTES value '${raw}'`,
    );
  }
  return parsed;
}

function toHex(bytes) {
  let out = "";
  for (const value of bytes) {
    out += value.toString(16).padStart(2, "0");
  }
  return out;
}

async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return toHex(new Uint8Array(digest));
}

async function runStrictSeedRebuild(
  tempOutputPath,
  tempMetaPath,
  compilerWasmPath,
  maxCompilerBytes,
) {
  const command = new Deno.Command("deno", {
    args: [
      "run",
      "-A",
      "scripts/build-strict-native-seed.mjs",
      "--bootstrap-wasm",
      compilerWasmPath,
      "--out",
      tempOutputPath,
      "--meta",
      tempMetaPath,
    ],
    cwd: Deno.cwd(),
    env: {
      ...Deno.env.toObject(),
      CLAP_MAX_COMPILER_WASM_BYTES: String(maxCompilerBytes),
    },
    stdout: "piped",
    stderr: "piped",
  });
  const result = await command.output();
  if (!result.success) {
    const stderr = new TextDecoder().decode(result.stderr).trim();
    const stdout = new TextDecoder().decode(result.stdout).trim();
    throw new Error(
      `build-strict-native-seed failed: ${stderr || stdout || `exit ${result.code}`}`,
    );
  }
}

async function main() {
  const compilerWasmPath = resolveCompilerWasmPath();
  const compilerDtsPath = resolveCompilerDtsPath();
  const maxCompilerBytes = resolveMaxCompilerBytes();
  const committedWasm = await Deno.readFile(compilerWasmPath);
  const committedDts = await Deno.readTextFile(compilerDtsPath);
  const requestedTempDir = String(Deno.env.get("TMPDIR") ?? "").trim();
  if (requestedTempDir.length > 0) {
    await Deno.mkdir(requestedTempDir, { recursive: true });
  }
  const tempDir = await Deno.makeTempDir({
    dir: requestedTempDir.length > 0 ? requestedTempDir : undefined,
    prefix: "clap-fixpoint-",
  });
  const rebuiltWasmPath = `${tempDir}/rebuilt.wasm`;
  const rebuiltMetaPath = `${tempDir}/rebuilt.meta.json`;
  const rebuiltDtsPath = `${tempDir}/rebuilt.d.ts`;
  try {
    await runStrictSeedRebuild(
      rebuiltWasmPath,
      rebuiltMetaPath,
      compilerWasmPath,
      maxCompilerBytes,
    );
    const rebuiltWasm = await Deno.readFile(rebuiltWasmPath);
    const rebuiltDts = await Deno.readTextFile(rebuiltDtsPath).catch(() =>
      Deno.readTextFile("artifacts/strict-native/seed.d.ts")
    );
    const sameWasm = committedWasm.length === rebuiltWasm.length &&
      committedWasm.every((value, index) => value === rebuiltWasm[index]);
    const sameDts = committedDts === rebuiltDts;
    if (!sameWasm || !sameDts) {
      const committedSha = await sha256Hex(committedWasm);
      const rebuiltSha = await sha256Hex(rebuiltWasm);
      throw new Error(
        `fixpoint mismatch (wasm_equal=${sameWasm}; dts_equal=${sameDts}; committed_sha=${committedSha}; rebuilt_sha=${rebuiltSha}; committed_bytes=${committedWasm.length}; rebuilt_bytes=${rebuiltWasm.length})`,
      );
    }

    const committedSha = await sha256Hex(committedWasm);
    console.log(
      `compiler-fixpoint-check: PASS (wasm=${compilerWasmPath}; sha256=${committedSha}; bytes=${committedWasm.length})`,
    );
  } finally {
    await Deno.remove(tempDir, { recursive: true }).catch(() => {});
  }
}

await main().catch(failWithError);
