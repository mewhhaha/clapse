#!/usr/bin/env -S deno run -A

import {
  callCompilerWasmRaw,
  decodeWasmBase64,
  inspectCompilerWasmAbi,
} from "./wasm-compiler-abi.mjs";

const DEFAULT_SEED_WASM = "artifacts/latest/clapse_compiler.wasm";
const DEFAULT_OUT_WASM = "artifacts/strict-native/native_producer_seed.wasm";
const DEFAULT_META = "artifacts/strict-native/native_producer_seed.meta.json";
const DEFAULT_SOURCE_VERSION = "native-source-2026-03-01-r2";
const EXPECTED_COMPILE_CONTRACT_VERSION = "native-v1";
const EMIT_WAT_TEMPLATE_MEMORY_NEEDLE = '(memory (export "__memory") 1)';
const WASM_PAGE_SIZE = 65536;
const MIN_INITIAL_MEMORY_BYTES = 8 * 1024 * 1024;
const INITIAL_MEMORY_HEADROOM_BYTES = 4 * 1024 * 1024;
const DEFAULT_DEPTH = Number.parseInt(
  String(Deno.env.get("CLAPSE_NATIVE_PRODUCER_SEED_DEPTH") ?? "8"),
  10,
);
const TEMPLATE_PATH = "scripts/native-producer-seed-template.c";

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/build-native-producer-seed.mjs [--seed <path>] [--out <path>] [--meta <path>] [--depth <n>] [--source-version <token>]",
    "",
    "Builds a wasm-native producer seed compiler artifact by compiling",
    "scripts/native-producer-seed-template.c with a chained seed depth.",
  ].join("\n");
}

function fail(message) {
  console.error(`build-native-producer-seed: FAIL (${message})`);
  Deno.exit(1);
}

function nonEmptyString(value) {
  return typeof value === "string" && value.length > 0;
}

function parseArgs(argv) {
  let seedWasm = DEFAULT_SEED_WASM;
  let outWasm = DEFAULT_OUT_WASM;
  let outMeta = DEFAULT_META;
  let sourceVersion = DEFAULT_SOURCE_VERSION;
  let depth = Number.isInteger(DEFAULT_DEPTH) && DEFAULT_DEPTH > 0
    ? DEFAULT_DEPTH
    : 8;

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--help" || arg === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (arg === "--seed") {
      seedWasm = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    if (arg === "--out") {
      outWasm = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    if (arg === "--meta") {
      outMeta = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    if (arg === "--depth") {
      const raw = argv[i + 1] ?? "";
      const parsed = Number.parseInt(raw, 10);
      if (!Number.isInteger(parsed) || parsed <= 0) {
        fail(`invalid --depth value: ${raw}`);
      }
      depth = parsed;
      i += 1;
      continue;
    }
    if (arg === "--source-version") {
      sourceVersion = argv[i + 1] ?? "";
      i += 1;
      continue;
    }
    fail(`unknown argument: ${arg}`);
  }

  if (!nonEmptyString(seedWasm)) {
    fail("missing --seed <path>");
  }
  if (!nonEmptyString(outWasm)) {
    fail("missing --out <path>");
  }
  if (!nonEmptyString(outMeta)) {
    fail("missing --meta <path>");
  }
  if (!nonEmptyString(sourceVersion)) {
    fail("missing --source-version <token>");
  }

  return {
    seedWasm,
    outWasm,
    outMeta,
    sourceVersion,
    depth,
  };
}

async function requireFile(path) {
  try {
    const stat = await Deno.stat(path);
    if (!stat.isFile || stat.size <= 0) {
      fail(`required file missing or empty: ${path}`);
    }
  } catch {
    fail(`required file missing: ${path}`);
  }
}

function toBase64(bytes) {
  const CHUNK = 0x8000;
  let binary = "";
  for (let i = 0; i < bytes.length; i += CHUNK) {
    const chunk = bytes.subarray(i, Math.min(bytes.length, i + CHUNK));
    binary += String.fromCharCode(...chunk);
  }
  return btoa(binary);
}

function toCStringLiteralChunks(raw, chunkSize = 96) {
  const escaped = raw.replaceAll("\\", "\\\\").replaceAll('"', '\\"');
  const chunks = [];
  for (let i = 0; i < escaped.length; i += chunkSize) {
    chunks.push(`"${escaped.slice(i, i + chunkSize)}"`);
  }
  if (chunks.length === 0) {
    return '""';
  }
  return chunks.join("\n  ");
}

async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return Array.from(new Uint8Array(digest)).map((b) =>
    b.toString(16).padStart(2, "0")
  ).join("");
}

function hasSyntheticArtifactMarkers(value) {
  if (typeof value !== "string") {
    return true;
  }
  return value.includes("kernel:compile:") ||
    /seed-stage[0-9]+:[^)\s"]+/u.test(value);
}

function ensure(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function computeInitialMemoryBytes(seedBytesLength) {
  const requested = Math.max(
    MIN_INITIAL_MEMORY_BYTES,
    seedBytesLength + INITIAL_MEMORY_HEADROOM_BYTES,
  );
  return Math.ceil(requested / WASM_PAGE_SIZE) * WASM_PAGE_SIZE;
}

function compilerAbiExports(bytes, context) {
  let module;
  try {
    module = new WebAssembly.Module(bytes);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    fail(`${context}: wasm validation failed (${msg})`);
  }
  const exportsList = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  const hasMemory = exportsList.includes("memory") ||
    exportsList.includes("__memory");
  ensure(hasMemory, `${context}: missing memory export`);
  ensure(
    exportsList.includes("clapse_run"),
    `${context}: missing clapse_run export`,
  );
  return exportsList;
}

async function buildStage({
  templateText,
  stageIndex,
  seedBytes,
  sourceVersion,
  tmpDir,
}) {
  const seedB64 = toBase64(seedBytes);
  const seedLiteral = toCStringLiteralChunks(seedB64);
  const sourceVersionLiteral = sourceVersion.replaceAll('"', "");
  const cText = templateText
    .replace("{{SEED_WASM_BASE64_LITERAL}}", seedLiteral)
    .replaceAll("{{SOURCE_VERSION_LITERAL}}", sourceVersionLiteral);

  const cPath = `${tmpDir}/stage-${stageIndex}.c`;
  const wasmPath = `${tmpDir}/stage-${stageIndex}.wasm`;
  const initialMemoryBytes = computeInitialMemoryBytes(seedBytes.length);
  await Deno.writeTextFile(cPath, cText);

  const cmd = new Deno.Command("clang", {
    args: [
      "--target=wasm32",
      "-Oz",
      "-fno-builtin",
      "-fno-stack-protector",
      "-nostdlib",
      "-Wl,--no-entry",
      "-Wl,--export=clapse_run",
      "-Wl,--export=main",
      "-Wl,--export-memory",
      `-Wl,--initial-memory=${initialMemoryBytes}`,
      "-o",
      wasmPath,
      cPath,
    ],
    stdout: "piped",
    stderr: "piped",
  });
  const { code, stdout, stderr } = await cmd.output();
  if (code !== 0) {
    const out = new TextDecoder().decode(stdout).trim();
    const err = new TextDecoder().decode(stderr).trim();
    fail(
      `clang stage ${stageIndex} failed (code=${code})\nstdout: ${out}\nstderr: ${err}`,
    );
  }

  const bytes = await Deno.readFile(wasmPath);
  ensure(
    bytes.length > 4096,
    `stage ${stageIndex}: output wasm too small (${bytes.length})`,
  );
  compilerAbiExports(bytes, `stage ${stageIndex}`);
  return { wasmPath, bytes };
}

async function validateRawProducer(wasmPath, sourceVersion, hops = 2) {
  const previousDisable = Deno.env.get(
    "CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK",
  );
  Deno.env.set("CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK", "1");
  try {
    const probeToken = `native_producer_seed_${
      crypto.randomUUID().replaceAll("-", "_")
    }`;
    const inputSource = [
      `${probeToken} x = x`,
      `main x = ${probeToken} x`,
      "",
    ].join("\n");

    let compilerPath = wasmPath;
    let firstSourceVersion = "";
    let finalBytes = new Uint8Array();
    for (let hop = 1; hop <= hops; hop += 1) {
      const response = await callCompilerWasmRaw(compilerPath, {
        command: "compile",
        compile_mode: "kernel-native",
        input_path: "examples/native_producer_seed_probe.clapse",
        input_source: inputSource,
        plugin_wasm_paths: [],
      });
      ensure(
        response && typeof response === "object",
        `hop ${hop}: compile response must be object`,
      );
      ensure(
        response.ok === true,
        `hop ${hop}: compile failed (${String(response?.error ?? "unknown")})`,
      );
      ensure(
        response.backend === "kernel-native",
        `hop ${hop}: backend must be kernel-native`,
      );

      const artifacts = response.artifacts;
      ensure(
        artifacts && typeof artifacts === "object",
        `hop ${hop}: missing artifacts object`,
      );
      const lowered = artifacts["lowered_ir.txt"];
      const collapsed = artifacts["collapsed_ir.txt"];
      ensure(
        typeof lowered === "string" && lowered.length > 0,
        `hop ${hop}: missing lowered_ir.txt`,
      );
      ensure(
        typeof collapsed === "string" && collapsed.length > 0,
        `hop ${hop}: missing collapsed_ir.txt`,
      );
      ensure(
        !hasSyntheticArtifactMarkers(lowered),
        `hop ${hop}: lowered_ir.txt contains synthetic markers`,
      );
      ensure(
        !hasSyntheticArtifactMarkers(collapsed),
        `hop ${hop}: collapsed_ir.txt contains synthetic markers`,
      );
      ensure(
        lowered.includes(probeToken),
        `hop ${hop}: lowered_ir.txt missing source probe token`,
      );
      ensure(
        collapsed.includes(probeToken),
        `hop ${hop}: collapsed_ir.txt missing source probe token`,
      );

      const contract = response.__clapse_contract;
      ensure(
        contract && typeof contract === "object",
        `hop ${hop}: missing __clapse_contract`,
      );
      ensure(
        contract.compile_contract_version === EXPECTED_COMPILE_CONTRACT_VERSION,
        `hop ${hop}: compile_contract_version mismatch (${
          String(contract.compile_contract_version)
        })`,
      );
      ensure(
        contract.source_version === sourceVersion,
        `hop ${hop}: source_version mismatch (${
          String(contract.source_version)
        })`,
      );
      if (firstSourceVersion.length === 0) {
        firstSourceVersion = contract.source_version;
      } else {
        ensure(
          firstSourceVersion === contract.source_version,
          `hop ${hop}: source_version changed across hops (${firstSourceVersion} -> ${contract.source_version})`,
        );
      }

      ensure(
        typeof response.wasm_base64 === "string" &&
          response.wasm_base64.length > 0,
        `hop ${hop}: missing wasm_base64`,
      );
      const outBytes = decodeWasmBase64(response.wasm_base64);
      compilerAbiExports(outBytes, `hop ${hop}: output compiler abi`);
      ensure(
        outBytes.length > 4096,
        `hop ${hop}: output compiler too small (${outBytes.length})`,
      );
      finalBytes = outBytes;

      if (hop < hops) {
        const tempNext = await Deno.makeTempFile({
          prefix: `native-producer-seed-hop-${hop}-`,
          suffix: ".wasm",
        });
        await Deno.writeFile(tempNext, outBytes);
        compilerPath = tempNext;
      }
    }

    const emitMarker = `emit_wat_marker_${crypto.randomUUID()}`;
    const emitResp = await callCompilerWasmRaw(wasmPath, {
      command: "emit-wat",
      emit_wat_mode: "source-data",
      input_path: "examples/native_emit_wat.clapse",
      input_source: `${emitMarker} = 42\n`,
    });
    ensure(
      emitResp && emitResp.ok === true,
      "emit-wat response should be ok=true",
    );
    ensure(
      typeof emitResp.wat === "string" && emitResp.wat.includes(emitMarker),
      "emit-wat response should include source marker",
    );
    const emitTemplateResp = await callCompilerWasmRaw(wasmPath, {
      command: "emit-wat",
      emit_wat_mode: "template",
      input_path: "examples/native_emit_wat_template.clapse",
      input_source: `${emitMarker} = 42\n`,
    });
    ensure(
      emitTemplateResp && emitTemplateResp.ok === true,
      "emit-wat template response should be ok=true",
    );
    ensure(
      typeof emitTemplateResp.wat === "string" &&
        emitTemplateResp.wat.includes(EMIT_WAT_TEMPLATE_MEMORY_NEEDLE),
      "emit-wat template response should include template memory shape",
    );

    return {
      hops,
      output_bytes: finalBytes.length,
    };
  } finally {
    if (previousDisable === undefined) {
      Deno.env.delete("CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK");
    } else {
      Deno.env.set("CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK", previousDisable);
    }
  }
}

async function main() {
  const opts = parseArgs(Deno.args);
  await requireFile(opts.seedWasm);
  await requireFile(TEMPLATE_PATH);

  const outDir = opts.outWasm.includes("/")
    ? opts.outWasm.slice(0, opts.outWasm.lastIndexOf("/"))
    : ".";
  const metaDir = opts.outMeta.includes("/")
    ? opts.outMeta.slice(0, opts.outMeta.lastIndexOf("/"))
    : ".";
  await Deno.mkdir(outDir, { recursive: true });
  await Deno.mkdir(metaDir, { recursive: true });

  const templateText = await Deno.readTextFile(TEMPLATE_PATH);
  let seedBytes = await Deno.readFile(opts.seedWasm);
  const stages = [];
  const tmpDir = await Deno.makeTempDir({
    prefix: "native-producer-seed-build-",
  });

  try {
    for (let stage = 1; stage <= opts.depth; stage += 1) {
      const result = await buildStage({
        templateText,
        stageIndex: stage,
        seedBytes,
        sourceVersion: opts.sourceVersion,
        tmpDir,
      });
      const digest = await sha256Hex(result.bytes);
      stages.push({
        stage,
        bytes: result.bytes.length,
        sha256: digest,
      });
      seedBytes = result.bytes;
    }

    await Deno.writeFile(opts.outWasm, seedBytes);

    const outDts = opts.outWasm.replace(/\.wasm$/u, ".d.ts");
    await Deno.writeTextFile(
      outDts,
      [
        "export declare function clapse_run(request_handle: number): number;",
        "export declare function main(arg0: number): number;",
        "",
      ].join("\n"),
    );

    const abi = await inspectCompilerWasmAbi(opts.outWasm);
    ensure(
      abi && abi.ok === true,
      `output compiler ABI check failed for ${opts.outWasm}`,
    );
    ensure(
      abi.mode === "native",
      `output compiler should be native mode (got ${String(abi.mode)})`,
    );

    const validation = await validateRawProducer(
      opts.outWasm,
      opts.sourceVersion,
      2,
    );

    const seedDigest = await sha256Hex(await Deno.readFile(opts.seedWasm));
    const outDigest = await sha256Hex(seedBytes);
    const meta = {
      tool: "build-native-producer-seed",
      created_at: new Date().toISOString(),
      seed: {
        input_path: opts.seedWasm,
        input_sha256: seedDigest,
      },
      output: {
        wasm_path: opts.outWasm,
        dts_path: outDts,
        sha256: outDigest,
        bytes: seedBytes.length,
      },
      contract: {
        source_version: opts.sourceVersion,
        compile_contract_version: EXPECTED_COMPILE_CONTRACT_VERSION,
      },
      chain_depth: opts.depth,
      stages,
      verification: {
        raw_producer_probe: validation,
      },
    };
    await Deno.writeTextFile(
      opts.outMeta,
      `${JSON.stringify(meta, null, 2)}\n`,
    );

    console.log(
      `build-native-producer-seed: PASS (out=${opts.outWasm}; depth=${opts.depth}; bytes=${seedBytes.length}; source_version=${opts.sourceVersion})`,
    );
  } finally {
    try {
      await Deno.remove(tmpDir, { recursive: true });
    } catch {
      // best-effort cleanup
    }
  }
}

await main();
