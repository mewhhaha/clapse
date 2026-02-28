#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";
import { callCompilerWasm, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";

const DEFAULT_OUT_WASM = "artifacts/strict-native/seed.wasm";
const DEFAULT_OUT_META = "artifacts/strict-native/seed.meta.json";
const DEFAULT_BOOTSTRAP_STRICT_SEED = "artifacts/strict-native/seed.wasm";
const DEFAULT_BOOTSTRAP_WASM_FALLBACK = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_INPUT_PATH = "lib/compiler/kernel.clapse";
const DEFAULT_COMPILE_MODE = "kernel-native";
const MIN_OUTPUT_BYTES = 4096;
const DEFAULT_PROBE_HOPS = 1;
const DEFAULT_BOOTSTRAP_HELP =
  "CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH | CLAPSE_COMPILER_WASM_PATH | CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH | artifacts/strict-native/seed.wasm | artifacts/latest/clapse_compiler.wasm";
const REQUIRE_NO_BOUNDARY_FALLBACK_ENV =
  "CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/build-strict-native-seed.mjs [options]",
    "",
    "Options:",
    `  --out <path>              output strict-native seed wasm (default: ${DEFAULT_OUT_WASM})`,
    `  --meta <path>             output metadata json (default: ${DEFAULT_OUT_META})`,
    `  --bootstrap-wasm <path>   bootstrap compiler wasm input (default: ${DEFAULT_BOOTSTRAP_HELP})`,
    `  --input <path>            input compiler source for seed compile (default: ${DEFAULT_INPUT_PATH})`,
    `  --compile-mode <mode>     compile mode for seed build (default: ${DEFAULT_COMPILE_MODE})`,
    `  --probe-hops <n>          selfhost probe compile hops (default: ${DEFAULT_PROBE_HOPS})`,
    "  --require-no-boundary-fallback",
    `                           fail when kernel compile uses JS ABI tiny-output fallback (env: ${REQUIRE_NO_BOUNDARY_FALLBACK_ENV}=1)`,
    "  --no-meta                 skip metadata output",
    "  --keep-temp               keep temporary build directory",
    "  --help, -h                show help",
    "",
    "Description:",
    "  Native-first strict seed builder.",
    "  1) Attempts native bootstrap compile of kernel source.",
    "  2) Probes whether resulting candidate can selfhost-compile kernel to compiler ABI.",
    "  3) Fails hard unless the native candidate is already selfhost-stable.",
  ].join("\n");
}

function isNonEmptyFile(path) {
  if (typeof path !== "string" || path.length === 0) {
    return false;
  }
  try {
    const stat = Deno.statSync(path);
    return stat.isFile && stat.size > 0;
  } catch {
    return false;
  }
}

function resolveDefaultBootstrapWasm() {
  const strictSeedFromEnv = String(
    Deno.env.get("CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH") ?? "",
  ).trim();
  const strictSeedPath = strictSeedFromEnv.length > 0
    ? strictSeedFromEnv
    : DEFAULT_BOOTSTRAP_STRICT_SEED;
  if (isNonEmptyFile(strictSeedPath)) {
    return strictSeedPath;
  }
  if (isNonEmptyFile(DEFAULT_BOOTSTRAP_WASM_FALLBACK)) {
    return DEFAULT_BOOTSTRAP_WASM_FALLBACK;
  }
  // Defer to runtime error path with explicit stat details.
  return strictSeedPath;
}

function parseArgs(argv) {
  let outWasm = DEFAULT_OUT_WASM;
  let outMeta = DEFAULT_OUT_META;
  const bootstrapFromEnv = String(
    Deno.env.get("CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH") ??
      Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ??
      "",
  ).trim();
  let bootstrapWasm = bootstrapFromEnv.length > 0
    ? bootstrapFromEnv
    : resolveDefaultBootstrapWasm();
  let inputPath = DEFAULT_INPUT_PATH;
  let compileMode = DEFAULT_COMPILE_MODE;
  let probeHops = Number.parseInt(
    String(
      Deno.env.get("CLAPSE_STRICT_NATIVE_SEED_PROBE_HOPS") ??
        DEFAULT_PROBE_HOPS,
    ),
    10,
  );
  if (!Number.isInteger(probeHops) || probeHops <= 0) {
    probeHops = DEFAULT_PROBE_HOPS;
  }
  let writeMeta = true;
  let keepTemp = false;
  let requireNoBoundaryFallback = boolEnvFlag(
    REQUIRE_NO_BOUNDARY_FALLBACK_ENV,
    false,
  );

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--help" || arg === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (arg === "--out") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        throw new Error("missing value for --out");
      }
      outWasm = value;
      i += 1;
      continue;
    }
    if (arg === "--meta") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        throw new Error("missing value for --meta");
      }
      outMeta = value;
      writeMeta = true;
      i += 1;
      continue;
    }
    if (arg === "--bootstrap-wasm") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        throw new Error("missing value for --bootstrap-wasm");
      }
      bootstrapWasm = value;
      i += 1;
      continue;
    }
    if (arg === "--input") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        throw new Error("missing value for --input");
      }
      inputPath = value;
      i += 1;
      continue;
    }
    if (arg === "--compile-mode") {
      const value = argv[i + 1] ?? "";
      if (value.length === 0) {
        throw new Error("missing value for --compile-mode");
      }
      compileMode = value;
      i += 1;
      continue;
    }
    if (arg === "--probe-hops") {
      const raw = argv[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        throw new Error(`invalid --probe-hops value: ${raw}`);
      }
      probeHops = parsed;
      i += 1;
      continue;
    }
    if (arg === "--no-meta") {
      writeMeta = false;
      continue;
    }
    if (arg === "--require-no-boundary-fallback") {
      requireNoBoundaryFallback = true;
      continue;
    }
    if (arg === "--keep-temp") {
      keepTemp = true;
      continue;
    }
    throw new Error(`unknown argument: ${arg}`);
  }

  return {
    outWasm,
    outMeta,
    writeMeta,
    bootstrapWasm,
    inputPath,
    compileMode,
    probeHops,
    keepTemp,
    requireNoBoundaryFallback,
  };
}

function boolEnvFlag(name, defaultValue = false) {
  const raw = String(Deno.env.get(name) ?? "").trim().toLowerCase();
  if (raw.length === 0) {
    return defaultValue;
  }
  return raw === "1" || raw === "true" || raw === "yes" || raw === "on";
}

function contractMeta(response) {
  const raw = response?.__clapse_contract;
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    return {};
  }
  return raw;
}

function pathDir(path) {
  const normalized = String(path).replace(/\\/g, "/");
  const idx = normalized.lastIndexOf("/");
  if (idx <= 0) {
    return idx === 0 ? "/" : "";
  }
  return normalized.slice(0, idx);
}

async function ensureParentDir(path) {
  const dir = pathDir(path);
  if (dir.length === 0 || dir === ".") {
    return;
  }
  await Deno.mkdir(dir, { recursive: true });
}

function dtsPathForWasm(wasmPath) {
  return wasmPath.endsWith(".wasm")
    ? wasmPath.slice(0, -5) + ".d.ts"
    : `${wasmPath}.d.ts`;
}

function renderTypeScriptBindings(exportsList) {
  if (!Array.isArray(exportsList) || exportsList.length === 0) {
    return "export {}\n";
  }
  const lines = [];
  for (const item of exportsList) {
    if (!item || typeof item.name !== "string" || item.name.length === 0) {
      continue;
    }
    const arityRaw = Number(item.arity ?? 0);
    const arity = Number.isInteger(arityRaw) && arityRaw > 0 ? arityRaw : 0;
    const args = Array.from({ length: arity }, (_, i) => `arg${i}: number`)
      .join(
        ", ",
      );
    lines.push(`export declare function ${item.name}(${args}): number;`);
  }
  return lines.length > 0 ? `${lines.join("\n")}\n` : "export {}\n";
}

async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)]
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
}

function ensureCompilerOutputAbi(wasmBytes, exportNames) {
  if (!(wasmBytes instanceof Uint8Array)) {
    throw new Error("seed compile output was not wasm bytes");
  }
  if (wasmBytes.length < MIN_OUTPUT_BYTES) {
    throw new Error(
      `seed compile output too small (${wasmBytes.length} < ${MIN_OUTPUT_BYTES})`,
    );
  }
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  if (!hasMemory) {
    throw new Error(
      `seed compile output missing memory export (exports: ${
        exportNames.join(", ")
      })`,
    );
  }
  if (!exportNames.includes("clapse_run")) {
    throw new Error(
      `seed compile output missing clapse_run export (exports: ${
        exportNames.join(", ")
      })`,
    );
  }
}

async function compileNativeCandidate(opts, inputSource) {
  const response = await callCompilerWasm(opts.bootstrapWasm, {
    command: "compile",
    compile_mode: opts.compileMode,
    input_path: opts.inputPath,
    input_source: inputSource,
    plugin_wasm_paths: [],
  }, {
    withContractMetadata: true,
    allowTinyKernelOutputFallback: !opts.requireNoBoundaryFallback,
  });

  if (!response || typeof response !== "object" || Array.isArray(response)) {
    throw new Error("bootstrap compile response was not an object");
  }
  if (response.ok !== true) {
    const err = typeof response.error === "string"
      ? response.error
      : "unknown bootstrap compile failure";
    throw new Error(`bootstrap compile failed: ${err}`);
  }
  if (response.backend !== "kernel-native") {
    throw new Error(
      `bootstrap compile returned backend=${
        String(response.backend ?? "<missing>")
      }; expected kernel-native`,
    );
  }
  if (
    opts.requireNoBoundaryFallback &&
    contractMeta(response).tiny_output_fallback === true
  ) {
    throw new Error(
      "bootstrap compile used JS ABI tiny-output fallback while strict no-boundary-fallback mode is enabled",
    );
  }

  const wasmBytes = decodeWasmBase64(response.wasm_base64);
  const module = await WebAssembly.compile(wasmBytes);
  const exportNames = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  ensureCompilerOutputAbi(wasmBytes, exportNames);
  const dts = typeof response.dts === "string" && response.dts.length > 0
    ? response.dts
    : renderTypeScriptBindings(
      Array.isArray(response.exports) ? response.exports : [],
    );
  return { response, wasmBytes, exportNames, dts };
}

async function probeSelfhostCompile(
  wasmPath,
  inputPath,
  inputSource,
  probeHops,
  requireNoBoundaryFallback,
) {
  const tempCompilers = [];
  let compilerPath = wasmPath;
  let finalBytes = new Uint8Array();
  let finalExports = [];
  try {
    for (let hop = 1; hop <= probeHops; hop += 1) {
      let response;
      try {
        response = await callCompilerWasm(compilerPath, {
          command: "compile",
          compile_mode: "kernel-native",
          input_path: inputPath,
          input_source: inputSource,
          plugin_wasm_paths: [],
        }, {
          withContractMetadata: true,
          allowTinyKernelOutputFallback: !requireNoBoundaryFallback,
        });
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return { ok: false, reason: `hop-${hop}-compile-call-failed: ${msg}` };
      }
      if (
        !response || typeof response !== "object" || Array.isArray(response)
      ) {
        return { ok: false, reason: `hop-${hop}-compile-response-not-object` };
      }
      if (response.ok !== true) {
        return {
          ok: false,
          reason: `hop-${hop}-compile-not-ok: ${
            String(response.error ?? "unknown")
          }`,
        };
      }
      if (response.backend !== "kernel-native") {
        return {
          ok: false,
          reason: `hop-${hop}-compile-backend-mismatch: ${
            String(response.backend ?? "<missing>")
          }`,
        };
      }
      if (
        requireNoBoundaryFallback &&
        contractMeta(response).tiny_output_fallback === true
      ) {
        return {
          ok: false,
          reason: `hop-${hop}-boundary-tiny-output-fallback`,
        };
      }
      try {
        finalBytes = decodeWasmBase64(response.wasm_base64);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return {
          ok: false,
          reason: `hop-${hop}-compile-wasm-base64-invalid: ${msg}`,
        };
      }
      if (finalBytes.length < MIN_OUTPUT_BYTES) {
        return {
          ok: false,
          reason: `hop-${hop}-compile-output-too-small: ${finalBytes.length}`,
        };
      }
      let module;
      try {
        module = await WebAssembly.compile(finalBytes);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return {
          ok: false,
          reason: `hop-${hop}-compile-output-invalid-wasm: ${msg}`,
        };
      }
      finalExports = WebAssembly.Module.exports(module).map((entry) =>
        entry.name
      );
      const hasMemory = finalExports.includes("memory") ||
        finalExports.includes("__memory");
      if (!hasMemory) {
        return {
          ok: false,
          reason: `hop-${hop}-compile-output-missing-memory-export: ${
            finalExports.join(",")
          }`,
        };
      }
      if (!finalExports.includes("clapse_run")) {
        return {
          ok: false,
          reason: `hop-${hop}-compile-output-missing-clapse_run-export: ${
            finalExports.join(",")
          }`,
        };
      }
      if (hop < probeHops) {
        const nextCompilerPath = await Deno.makeTempFile({
          prefix: "clapse-strict-native-hop-",
          suffix: ".wasm",
        });
        await Deno.writeFile(nextCompilerPath, finalBytes);
        tempCompilers.push(nextCompilerPath);
        compilerPath = nextCompilerPath;
      }
    }
  } finally {
    for (const tempPath of tempCompilers) {
      try {
        await Deno.remove(tempPath);
      } catch {
        // best-effort cleanup
      }
    }
  }
  return {
    ok: true,
    hops: probeHops,
    output_bytes: finalBytes.length,
    output_exports: finalExports,
  };
}

async function main() {
  const opts = parseArgs(Deno.args);

  try {
    const stat = await Deno.stat(opts.bootstrapWasm);
    if (!stat.isFile || stat.size <= 0) {
      throw new Error("not a non-empty file");
    }
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    throw new Error(
      `bootstrap compiler wasm missing/unreadable: ${opts.bootstrapWasm} (${msg})`,
    );
  }

  const inputSource = await Deno.readTextFile(opts.inputPath);

  const tempDir = await Deno.makeTempDir({
    prefix: "clapse-strict-native-seed-",
  });
  const selectedMode = "native-bootstrap-compile";
  let selectedWasmBytes;
  let selectedDts;
  let selectedExports;
  let nativeProbe = null;

  try {
    const nativeCandidate = await compileNativeCandidate(opts, inputSource);
    const nativeCandidatePath = `${tempDir}/native-candidate.wasm`;
    await Deno.writeFile(nativeCandidatePath, nativeCandidate.wasmBytes);
    nativeProbe = await probeSelfhostCompile(
      nativeCandidatePath,
      opts.inputPath,
      inputSource,
      opts.probeHops,
      opts.requireNoBoundaryFallback,
    );

    if (!nativeProbe.ok) {
      throw new Error(
        `native candidate failed selfhost probe: ${
          String(nativeProbe.reason ?? "unknown-native-probe-failure")
        }`,
      );
    }
    selectedWasmBytes = nativeCandidate.wasmBytes;
    selectedDts = nativeCandidate.dts;
    selectedExports = nativeCandidate.exportNames;

    await ensureParentDir(opts.outWasm);
    await Deno.writeFile(opts.outWasm, selectedWasmBytes);

    const outDts = dtsPathForWasm(opts.outWasm);
    await ensureParentDir(outDts);
    await Deno.writeTextFile(outDts, selectedDts);

    if (opts.writeMeta) {
      const metadata = {
        generated_at: new Date().toISOString(),
        tool: "scripts/build-strict-native-seed.mjs",
        mode: selectedMode,
        bootstrap: {
          wasm: opts.bootstrapWasm,
        },
        compile_request: {
          input_path: opts.inputPath,
          compile_mode: opts.compileMode,
          probe_hops: opts.probeHops,
          require_no_boundary_fallback: opts.requireNoBoundaryFallback,
        },
        probes: {
          native_candidate: nativeProbe,
          wrapper_candidate: null,
          fallback_reason: null,
        },
        output: {
          wasm: opts.outWasm,
          dts: outDts,
          size_bytes: selectedWasmBytes.length,
          sha256: await sha256Hex(selectedWasmBytes),
          exports: selectedExports,
        },
      };
      await ensureParentDir(opts.outMeta);
      await Deno.writeTextFile(
        opts.outMeta,
        `${JSON.stringify(metadata, null, 2)}\n`,
      );
    }

    if (opts.keepTemp) {
      console.log(`build-strict-native-seed: kept temp dir ${tempDir}`);
    } else {
      await Deno.remove(tempDir, { recursive: true });
    }

    console.log(
      `build-strict-native-seed: wrote ${opts.outWasm} (bootstrap=${opts.bootstrapWasm}, mode=${selectedMode})`,
    );
  } catch (err) {
    if (opts.keepTemp) {
      console.error(
        `build-strict-native-seed: temp dir kept for debugging: ${tempDir}`,
      );
    } else {
      try {
        await Deno.remove(tempDir, { recursive: true });
      } catch {
        // best-effort cleanup
      }
    }
    throw err;
  }
}

if (import.meta.main) {
  await main().catch(failWithError);
}
