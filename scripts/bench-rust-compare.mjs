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
  {
    id: "closure-env-abstraction",
    clapseInputPath: "examples/bench_wasm_closure_env_abstraction.clapse",
    rustCase: "closure-env",
  },
  {
    id: "struct-field-abstraction",
    clapseInputPath: "examples/bench_wasm_struct_field_abstraction.clapse",
    rustCase: "struct-field",
  },
  {
    id: "wrapper-uncurry-abstraction",
    clapseInputPath: "examples/bench_wasm_wrapper_uncurry_abstraction.clapse",
    rustCase: "wrapper-uncurry",
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

function makeRawArgPools(arity) {
  const poolSize = 1024;
  const pools = [];
  for (let argIx = 0; argIx < arity; argIx += 1) {
    const pool = new Array(poolSize);
    for (let i = 0; i < poolSize; i += 1) {
      const n = (i + argIx * 31) & 1023;
      pool[i] = n;
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

async function benchWasmBoundaryOnly(iterations, warmup) {
  const wasmBytes = new Uint8Array([
    0x00,0x61,0x73,0x6d,0x01,0x00,0x00,0x00,
    0x01,0x06,0x01,0x60,0x01,0x7f,0x01,0x7f,
    0x03,0x02,0x01,0x00,
    0x07,0x0f,0x01,0x0b,0x62,0x6f,0x75,0x6e,0x64,0x61,0x72,0x79,0x5f,0x69,0x64,0x00,0x00,
    0x0a,0x06,0x01,0x04,0x00,0x20,0x00,0x0b,
  ]);
  const instance = await WebAssembly.instantiate(wasmBytes);
  const fn = instance.instance.exports.boundary_id;
  if (typeof fn !== "function") {
    throw new Error("boundary_id export missing");
  }
  const pools = makeRawArgPools(1);
  for (let i = 0; i < warmup; i += 1) {
    fn(...argsForIteration(pools, i));
  }
  let checksum = 0;
  const start = nowNs();
  for (let i = 0; i < iterations; i += 1) {
    checksum = (checksum + (fn(...argsForIteration(pools, i)) | 0)) | 0;
  }
  const end = nowNs();
  const elapsedNs = Number(end - start);
  return {
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

fn closure_env(x: i32) -> i32 {
    x * 3 + 5
}

fn struct_field(x: i32) -> i32 {
    x + (x + 1)
}

fn sum4(a: i32, b: i32, c: i32, d: i32) -> i32 {
    a + b + c + d
}

fn wrapper_uncurry(x: i32) -> i32 {
    sum4(x, x + 1, x + 2, x + 3)
        + (sum4(x, x + 4, x + 5, x + 6) + sum4(x, x + 7, x + 8, x + 9))
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
            "closure-env" => closure_env(x),
            "struct-field" => struct_field(x),
            "wrapper-uncurry" => wrapper_uncurry(x),
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
            "closure-env" => closure_env(x),
            "struct-field" => struct_field(x),
            "wrapper-uncurry" => wrapper_uncurry(x),
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

function adjustedNsPerCall(totalNsPerCall, boundaryNsPerCall) {
  const adjusted = totalNsPerCall - boundaryNsPerCall;
  return adjusted > 0 ? adjusted : 0;
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
    const boundaryOnly = await benchWasmBoundaryOnly(iterations, warmup);
    console.log([
      "wasm-boundary-only".padEnd(22),
      "clapse-wasm".padEnd(14),
      boundaryOnly.nsPerCall.toFixed(2).padStart(12),
      boundaryOnly.opsPerSec.toFixed(2).padStart(14),
      String(boundaryOnly.checksum).padStart(12),
      "n/a".padStart(10),
    ].join(" "));
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
      const adjustedNs = adjustedNsPerCall(wasmResult.nsPerCall, boundaryOnly.nsPerCall);
      console.log([
        benchmarkCase.id.padEnd(22),
        "clapse-wasm".padEnd(14),
        wasmResult.nsPerCall.toFixed(2).padStart(12),
        wasmResult.opsPerSec.toFixed(2).padStart(14),
        String(wasmResult.checksum).padStart(12),
        formatRatio(wasmResult.nsPerCall, rustResult.nsPerCall).padStart(10),
      ].join(" "));
      console.log([
        `${benchmarkCase.id}-net`.padEnd(22),
        "clapse-wasm".padEnd(14),
        adjustedNs.toFixed(2).padStart(12),
        (adjustedNs > 0 ? (1_000_000_000 / adjustedNs) : 0).toFixed(2).padStart(14),
        String(wasmResult.checksum).padStart(12),
        formatRatio(adjustedNs, rustResult.nsPerCall).padStart(10),
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
