#!/usr/bin/env -S deno run -A

import {
  cliArgs,
  failWithError,
  nowNs,
  readBinaryFile,
} from "./runtime-env.mjs";
import { runWithArgs } from "./run-clapse-compiler-wasm.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime } from "./wasm-runtime.mjs";

const CASES = [
  {
    id: "numeric-hand",
    clapseInputPath: "examples/bench_wasm_hand.clapse",
    rustCase: "numeric",
  },
  {
    id: "numeric-abstraction",
    clapseInputPath: "examples/bench_wasm_abstraction.clapse",
    rustCase: "numeric",
  },
  {
    id: "http-request-parser",
    clapseInputPath: "examples/bench_wasm_http_request_parser.clapse",
    rustCase: "http-request-parser",
  },
];

function usage() {
  return [
    "Clapse vs Rust benchmark",
    "",
    "Usage:",
    "  deno run -A scripts/bench-rust-compare.mjs [iterations] [warmup]",
    "",
    "Defaults:",
    "  iterations = 2000000",
    "  warmup = 20000",
    "",
    "Benchmarks the current Clapse wasm output against optimized native Rust",
    "baselines for the current benchmark fixtures.",
  ].join("\n");
}

function parsePositiveInt(raw, label) {
  const n = Number(raw);
  if (!Number.isInteger(n) || n <= 0) {
    throw new Error(`${label} must be a positive integer, got: ${raw}`);
  }
  return n;
}

function parseNonNegativeInt(raw, label) {
  const n = Number(raw);
  if (!Number.isInteger(n) || n < 0) {
    throw new Error(`${label} must be a non-negative integer, got: ${raw}`);
  }
  return n;
}

function makeTaggedArgPools(arity) {
  const poolSize = 1024;
  const pools = [];
  for (let argIx = 0; argIx < arity; argIx += 1) {
    const pool = new Array(poolSize);
    for (let i = 0; i < poolSize; i += 1) {
      const n = (i + argIx * 31) & 1023;
      pool[i] = encodeInt(n);
    }
    pools.push(pool);
  }
  return pools;
}

function makePlainArgPools(arity) {
  const poolSize = 1024;
  const pools = [];
  for (let argIx = 0; argIx < arity; argIx += 1) {
    const pool = new Array(poolSize);
    for (let i = 0; i < poolSize; i += 1) {
      pool[i] = (i + argIx * 31) & 1023;
    }
    pools.push(pool);
  }
  return pools;
}

function argsForIteration(pools, i) {
  if (pools.length === 0) {
    return [];
  }
  const idx = i & 1023;
  const args = new Array(pools.length);
  for (let argIx = 0; argIx < pools.length; argIx += 1) {
    args[argIx] = pools[argIx][idx];
  }
  return args;
}

async function compileClapseCase(tmpDir, inputPath) {
  const outputPath = `${tmpDir}/${inputPath.split("/").pop()?.replace(/\.clapse$/u, ".wasm") ?? "out.wasm"}`;
  await runWithArgs(["compile-native", inputPath, outputPath]);
  return outputPath;
}

async function benchWasmCase(wasmPath, iterations, warmup, exportName = "main") {
  const wasmBytes = await readBinaryFile(wasmPath);
  const { instance } = await instantiateWithRuntime(wasmBytes);
  const fn = instance.exports[exportName];
  if (typeof fn !== "function") {
    throw new Error(`export '${exportName}' not found in ${wasmPath}`);
  }
  const arity = fn.length | 0;
  const pools = makeTaggedArgPools(arity);
  for (let i = 0; i < warmup; i += 1) {
    fn(...argsForIteration(pools, i));
  }
  let checksum = 0;
  const start = nowNs();
  for (let i = 0; i < iterations; i += 1) {
    const raw = fn(...argsForIteration(pools, i));
    checksum = (checksum + (decodeInt(raw | 0) | 0)) | 0;
  }
  const end = nowNs();
  const elapsedNs = Number(end - start);
  return {
    arity,
    checksum: checksum | 0,
    elapsedNs,
    elapsedMs: elapsedNs / 1_000_000,
    nsPerCall: elapsedNs / iterations,
    opsPerSec: (iterations * 1_000_000_000) / elapsedNs,
  };
}

function rustSource() {
  return `
use std::env;
use std::time::Instant;

fn numeric(x: i32) -> i32 {
    x * x + (x * 3 - x / 2)
}

fn encode_int(n: i32) -> i32 {
    n.wrapping_mul(2).wrapping_add(1)
}

fn method_code(packed: i32) -> i32 {
    packed / 10000
}

fn without_method(packed: i32) -> i32 {
    packed - method_code(packed) * 10000
}

fn path_code(packed: i32) -> i32 {
    without_method(packed) / 100
}

fn version_code(packed: i32) -> i32 {
    without_method(packed) - path_code(packed) * 100
}

#[derive(Copy, Clone)]
struct HttpRequest {
    method: i32,
    path: i32,
    version: i32,
}

fn parse_request(packed: i32) -> HttpRequest {
    HttpRequest {
        method: method_code(packed),
        path: path_code(packed),
        version: version_code(packed),
    }
}

fn normalized_path(path: i32) -> i32 {
    path - 1
}

fn version_major(version: i32) -> i32 {
    version / 10
}

fn request_score(method: i32, path: i32, version: i32) -> i32 {
    method * 1000 + path * 10 + version
}

fn http_request_parser(packed: i32) -> i32 {
    let req = parse_request(packed);
    request_score(req.method, normalized_path(req.path), version_major(req.version))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let case_id = args.get(1).map(|s| s.as_str()).unwrap_or("numeric");
    let iterations: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(2_000_000);
    let warmup: usize = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(20_000);
    let arity: usize = 1;
    let mut pools: Vec<Vec<i32>> = Vec::new();
    for arg_ix in 0..arity {
        let mut pool = Vec::with_capacity(1024);
        for i in 0..1024usize {
            let n = ((i + arg_ix * 31) & 1023) as i32;
            pool.push(encode_int(n));
        }
        pools.push(pool);
    }
    for i in 0..warmup {
        let idx = i & 1023;
        let x = pools[0][idx];
        let _ = match case_id {
            "numeric" => numeric(x),
            "http-request-parser" => http_request_parser(x),
            _ => panic!("unknown rust benchmark case: {}", case_id),
        };
    }
    let mut checksum: i32 = 0;
    let start = Instant::now();
    for i in 0..iterations {
        let idx = i & 1023;
        let x = pools[0][idx];
        let value = match case_id {
            "numeric" => numeric(x),
            "http-request-parser" => http_request_parser(x),
            _ => panic!("unknown rust benchmark case: {}", case_id),
        };
        checksum = checksum.wrapping_add(value);
    }
    let elapsed = start.elapsed();
    let elapsed_ns = elapsed.as_nanos() as f64;
    let ns_per_call = elapsed_ns / iterations as f64;
    let ops_per_sec = (iterations as f64 * 1_000_000_000.0) / elapsed_ns;
    println!("elapsed_ms: {:.3}", elapsed.as_secs_f64() * 1000.0);
    println!("ns_per_call: {:.2}", ns_per_call);
    println!("ops_per_sec: {:.2}", ops_per_sec);
    println!("checksum: {}", checksum);
}
`.trimStart();
}

async function compileRustBaseline(tmpDir) {
  const rustPath = `${tmpDir}/baseline.rs`;
  const binaryPath = `${tmpDir}/baseline-rust`;
  await Deno.writeTextFile(rustPath, rustSource());
  const compile = await new Deno.Command("rustc", {
    args: [
      rustPath,
      "-O",
      "-C",
      "target-cpu=native",
      "-C",
      "codegen-units=1",
      "-C",
      "panic=abort",
      "-o",
      binaryPath,
    ],
  }).output();
  if (!compile.success) {
    throw new Error(
      `rustc failed: ${new TextDecoder().decode(compile.stderr).trim() || "unknown error"}`,
    );
  }
  return binaryPath;
}

async function benchRustBinary(binaryPath, caseId, iterations, warmup) {
  const out = await new Deno.Command(binaryPath, {
    args: [caseId, String(iterations), String(warmup)],
  }).output();
  if (!out.success) {
    throw new Error(
      `rust benchmark failed: ${new TextDecoder().decode(out.stderr).trim() || "unknown error"}`,
    );
  }
  const text = new TextDecoder().decode(out.stdout);
  const result = {};
  for (const line of text.split(/\r?\n/u)) {
    const [key, rawValue] = line.split(":", 2);
    if (!key || rawValue === undefined) {
      continue;
    }
    result[key.trim()] = rawValue.trim();
  }
  return {
    elapsedMs: Number(result.elapsed_ms),
    nsPerCall: Number(result.ns_per_call),
    opsPerSec: Number(result.ops_per_sec),
    checksum: Number(result.checksum),
  };
}

function formatRatio(numerator, denominator) {
  if (!Number.isFinite(numerator) || !Number.isFinite(denominator) || denominator === 0) {
    return "n/a";
  }
  return `${(numerator / denominator).toFixed(2)}x`;
}

async function main() {
  const args = cliArgs();
  if (args.includes("--help") || args.includes("-h")) {
    console.log(usage());
    return;
  }
  const iterations = args[0] === undefined ? 2_000_000 : parsePositiveInt(args[0], "iterations");
  const warmup = args[1] === undefined ? 20_000 : parseNonNegativeInt(args[1], "warmup");
  const tmpDir = await Deno.makeTempDir({ dir: "/tmp", prefix: "clapse-rust-bench-" });
  try {
    const rustBinary = await compileRustBaseline(tmpDir);
    console.log("benchmark: clapse vs rust");
    console.log(`iterations: ${iterations}`);
    console.log(`warmup: ${warmup}`);
    console.log("");
    console.log([
      "case".padEnd(22),
      "engine".padEnd(14),
      "ns/call".padStart(12),
      "ops/sec".padStart(14),
      "checksum".padStart(12),
      "vs rust".padStart(10),
    ].join(" "));
    console.log("-".repeat(92));
    for (const benchmarkCase of CASES) {
      const rustResult = await benchRustBinary(
        rustBinary,
        benchmarkCase.rustCase,
        iterations,
        warmup,
      );
      console.log([
        `${benchmarkCase.id}-rust`.padEnd(22),
        "rust".padEnd(14),
        rustResult.nsPerCall.toFixed(2).padStart(12),
        rustResult.opsPerSec.toFixed(2).padStart(14),
        String(rustResult.checksum).padStart(12),
        "1.00x".padStart(10),
      ].join(" "));
      const wasmPath = await compileClapseCase(tmpDir, benchmarkCase.clapseInputPath);
      const wasmResult = await benchWasmCase(wasmPath, iterations, warmup);
      console.log([
        benchmarkCase.id.padEnd(22),
        "clapse-wasm".padEnd(14),
        wasmResult.nsPerCall.toFixed(2).padStart(12),
        wasmResult.opsPerSec.toFixed(2).padStart(14),
        String(wasmResult.checksum).padStart(12),
        formatRatio(wasmResult.nsPerCall, rustResult.nsPerCall).padStart(10),
      ].join(" "));
      if (wasmResult.checksum !== rustResult.checksum) {
        throw new Error(
          `${benchmarkCase.id}: checksum mismatch (clapse=${wasmResult.checksum}, rust=${rustResult.checksum})`,
        );
      }
    }
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

await main().catch(failWithError);
