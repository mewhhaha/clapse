#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";

const UTF8_DECODER = new TextDecoder();

function usage() {
  console.log(
    "Usage: deno run -A scripts/build-native-behavior-fixture-map.mjs [--manifest path ...] [--out path]",
  );
  console.log("  --manifest  Manifest path (repeatable)");
  console.log(
    "  --out       Output fixture map JSON (default scripts/native-behavior-fixture-map.json)",
  );
  console.log("  --help      Show this help");
}

function normalizeEntry(rawEntry) {
  const entry = String(rawEntry ?? "").trim().replaceAll("\\", "/");
  if (entry.length === 0) return null;
  if (entry === ".") return null;
  return entry.startsWith("./") ? entry.slice(2) : entry;
}

function parseTextManifest(raw) {
  return raw
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0 && !line.startsWith("#"))
    .map(normalizeEntry)
    .filter((entry) => typeof entry === "string" && entry.length > 0);
}

function parseJsonManifest(raw, sourcePath) {
  let doc;
  try {
    doc = JSON.parse(raw);
  } catch {
    throw new Error(`invalid JSON manifest: ${sourcePath}`);
  }

  const out = [];
  if (Array.isArray(doc?.scenarios)) {
    for (const scenario of doc.scenarios) {
      out.push(normalizeEntry(scenario?.entry));
    }
  }
  if (Array.isArray(doc?.entries)) {
    for (const entry of doc.entries) {
      out.push(normalizeEntry(entry));
    }
  }
  if (Array.isArray(doc)) {
    for (const entry of doc) {
      if (typeof entry === "string") {
        out.push(normalizeEntry(entry));
      } else if (
        entry &&
        typeof entry === "object" &&
        typeof entry.entry === "string"
      ) {
        out.push(normalizeEntry(entry.entry));
      }
    }
  }
  return out.filter((entry) => typeof entry === "string" && entry.length > 0);
}

async function parseManifest(path, seen) {
  const raw = await Deno.readTextFile(path);
  const entries = path.endsWith(".json")
    ? parseJsonManifest(raw, path)
    : parseTextManifest(raw);
  for (const entry of entries) {
    if (typeof entry === "string" && !seen.has(entry)) {
      seen.add(entry);
    }
  }
}

function toBase64(bytes) {
  let binary = "";
  for (let i = 0; i < bytes.length; i += 1) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary);
}

async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)]
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("");
}

async function hostCompile(entry, outputPath, buildLog, buildSummary) {
  const result = await new Deno.Command("cabal", {
    args: [
      "run",
      "clapse",
      `--build-log=${buildLog}`,
      `--build-summary=${buildSummary}`,
      "--",
      "compile",
      entry,
      outputPath,
    ],
    env: {
      ...Deno.env.toObject(),
      CABAL_DIR: `${Deno.cwd()}/.cabal`,
      CABAL_LOGDIR: `${Deno.cwd()}/.cabal-logs`,
    },
    stdout: "piped",
    stderr: "piped",
  }).output();

  if (!result.success) {
    const stderr = UTF8_DECODER.decode(result.stderr).trim();
    const stdout = UTF8_DECODER.decode(result.stdout).trim();
    throw new Error(
      `host compile failed for ${entry}: ${stderr || stdout || `exit=${result.code}`}`,
    );
  }
}

function parseArgs(argv) {
  const cfg = {
    manifests: [],
    outPath: "scripts/native-behavior-fixture-map.json",
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const val = argv[i + 1];
    if (arg === "--manifest" && typeof val === "string") {
      cfg.manifests.push(val);
      i += 1;
      continue;
    }
    if (arg === "--out" && typeof val === "string") {
      cfg.outPath = val;
      i += 1;
      continue;
    }
    if (arg === "--help" || arg === "-h") {
      usage();
      return null;
    }
    throw new Error(`unknown arg: ${arg}`);
  }

  if (cfg.manifests.length === 0) {
    cfg.manifests.push(
      "examples/selfhost_corpus.txt",
      "examples/selfhost_behavior_corpus.json",
    );
  }

  return cfg;
}

async function main() {
  const cfg = parseArgs(cliArgs());
  if (!cfg) return;

  const seen = new Set();
  for (const manifest of cfg.manifests) {
    await parseManifest(manifest, seen);
  }
  const entries = [...seen];
  if (entries.length === 0) {
    throw new Error("no entries found in manifest inputs");
  }

  await Deno.mkdir(`${Deno.cwd()}/.cabal-logs`, { recursive: true });
  if (cfg.outPath.includes("/")) {
    const outDir = cfg.outPath.slice(0, cfg.outPath.lastIndexOf("/"));
    if (outDir.length > 0) {
      await Deno.mkdir(outDir, { recursive: true });
    }
  }

  const buildLog = `${Deno.cwd()}/.cabal-logs/build.log`;
  const buildSummary = `${Deno.cwd()}/.cabal-logs/build.summary`;
  const fixtureMap = {};

  for (const entry of entries) {
    const sourceBytes = await Deno.readFile(entry);
    const sourceSha256 = await sha256Hex(sourceBytes);
    const outWasm = await Deno.makeTempFile({
      suffix: ".wasm",
      prefix: "clapse_native_fixture_",
      dir: "/tmp",
    });
    try {
      await hostCompile(entry, outWasm, buildLog, buildSummary);
      const wasmBytes = await Deno.readFile(outWasm);
      fixtureMap[entry] = {
        source_sha256: sourceSha256,
        wasm_base64: toBase64(wasmBytes),
      };
      console.log(`[fixture-map] built ${entry}`);
    } finally {
      await Deno.remove(outWasm).catch(() => {});
    }
  }

  await Deno.writeTextFile(cfg.outPath, `${JSON.stringify(fixtureMap, null, 2)}\n`);
  console.log(`[fixture-map] wrote ${entries.length} entries to ${cfg.outPath}`);
}

await main().catch(failWithError);
