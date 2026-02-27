#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";
import { decodeInt, isTaggedInt } from "./wasm-runtime.mjs";

const ENTRY = "examples/wildcard_demand_behavior_regressions.clapse";
const FIXTURE_MAP = "scripts/wasm-behavior-fixture-map.json";
const EXPECTED_SCENARIOS = [
  { exportName: "main_wildcard_not_force", expected: 11 },
  { exportName: "main_priority_after_reorder", expected: 31 },
  { exportName: "main_ordered_demand", expected: 41 },
];

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

function decodeBase64(base64Text) {
  const binary = atob(base64Text);
  const out = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    out[i] = binary.charCodeAt(i);
  }
  return out;
}

async function sha256Hex(path) {
  const bytes = await Deno.readFile(path);
  const hash = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(hash)].map((b) => b.toString(16).padStart(2, "0"))
    .join("");
}

async function loadFixtureWasmBytes() {
  const raw = await Deno.readTextFile(FIXTURE_MAP);
  const parsed = JSON.parse(raw);
  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error(`wildcard-demand-check: invalid fixture map (${FIXTURE_MAP})`);
  }
  const entry = parsed[ENTRY];
  if (!entry || typeof entry !== "object" || Array.isArray(entry)) {
    throw new Error(`wildcard-demand-check: missing fixture entry for ${ENTRY}`);
  }
  if (typeof entry.wasm_base64 !== "string" || entry.wasm_base64.length === 0) {
    throw new Error(`wildcard-demand-check: missing wasm_base64 for ${ENTRY}`);
  }
  if (typeof entry.source_sha256 !== "string" || entry.source_sha256.length === 0) {
    throw new Error(`wildcard-demand-check: missing source_sha256 for ${ENTRY}`);
  }
  const actualSourceSha = await sha256Hex(ENTRY);
  if (actualSourceSha !== entry.source_sha256) {
    throw new Error(
      `wildcard-demand-check: source hash mismatch for ${ENTRY} (expected ${entry.source_sha256}, got ${actualSourceSha}); refresh ${FIXTURE_MAP}`,
    );
  }
  return decodeBase64(entry.wasm_base64);
}

async function main() {
  const wasmBytes = await loadFixtureWasmBytes();
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
}

await main().catch(failWithError);
