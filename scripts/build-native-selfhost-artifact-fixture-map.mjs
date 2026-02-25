#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";

const UTF8_DECODER = new TextDecoder();

const SELFHOST_ARTIFACT_FILES = [
  "merged_module.txt",
  "type_info.txt",
  "type_info_error.txt",
  "lowered_ir.txt",
  "collapsed_ir.txt",
  "exports.txt",
  "wasm_stats.txt",
];

function usage() {
  console.log(
    "Usage: deno run -A scripts/build-native-selfhost-artifact-fixture-map.mjs [--manifest path ...] [--out path]",
  );
  console.log("  --manifest  Manifest path (repeatable)");
  console.log(
    "  --out       Output fixture map JSON (default scripts/native-selfhost-artifact-fixture-map.json)",
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
      if (scenario && typeof scenario.entry === "string") {
        out.push(normalizeEntry(scenario.entry));
      }
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

async function parseManifest(path, out) {
  const raw = await Deno.readTextFile(path);
  const entries = path.endsWith(".json")
    ? parseJsonManifest(raw, path)
    : parseTextManifest(raw);
  for (const entry of entries) {
    if (typeof entry === "string" && !out.has(entry)) out.add(entry);
  }
}

async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)]
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("");
}

async function runHostSelfhostArtifacts(entry, buildLog, buildSummary) {
  const outDir = await Deno.makeTempDir({
    prefix: "clapse_native_selfhost_artifacts_",
    dir: "/tmp",
  });
  const result = await new Deno.Command("cabal", {
    args: [
      "run",
      "clapse",
      `--build-log=${buildLog}`,
      `--build-summary=${buildSummary}`,
      "--",
      "selfhost-artifacts",
      entry,
      outDir,
    ],
    env: {
      ...Deno.env.toObject(),
      CABAL_DIR: `${Deno.cwd()}/.cabal`,
      CABAL_LOGDIR: `${Deno.cwd()}/.cabal-logs`,
    },
    stdout: "piped",
    stderr: "piped",
  }).output();

  try {
    if (!result.success) {
      const stderr = UTF8_DECODER.decode(result.stderr).trim();
      const stdout = UTF8_DECODER.decode(result.stdout).trim();
      throw new Error(
        `host selfhost-artifacts failed for ${entry}: ${
          stderr || stdout || `exit=${result.code}`
        }`,
      );
    }

    const artifacts = {};
    for (const file of SELFHOST_ARTIFACT_FILES) {
      const value = await Deno.readTextFile(`${outDir}/${file}`).catch(() => null);
      if (value === null) {
        throw new Error(`missing artifact file ${file} for ${entry}`);
      }
      artifacts[file] = value;
    }
    return artifacts;
  } finally {
    await Deno.remove(outDir, { recursive: true }).catch(() => {});
  }
}

function parseArgs(argv) {
  const cfg = {
    manifests: [],
    outPath: "scripts/native-selfhost-artifact-fixture-map.json",
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
    if (arg.startsWith("--")) {
      throw new Error(`unknown arg: ${arg}`);
    }
  }

  if (cfg.manifests.length === 0) {
    cfg.manifests.push("examples/selfhost_corpus.txt");
  }
  return cfg;
}

async function main() {
  const cfg = parseArgs(cliArgs());
  if (!cfg) return;

  const entries = new Set();
  for (const manifest of cfg.manifests) {
    await parseManifest(manifest, entries);
  }
  const entryList = [...entries];
  if (entryList.length === 0) {
    throw new Error("no entries found in manifest inputs");
  }

  await Deno.mkdir(`${Deno.cwd()}/.cabal-logs`, { recursive: true });
  if (cfg.outPath.includes("/")) {
    const outDir = cfg.outPath.slice(0, cfg.outPath.lastIndexOf("/"));
    if (outDir.length > 0) await Deno.mkdir(outDir, { recursive: true });
  }

  const buildLog = `${Deno.cwd()}/.cabal-logs/build.log`;
  const buildSummary = `${Deno.cwd()}/.cabal-logs/build.summary`;
  const fixtureMap = {};

  for (const entry of entryList) {
    const sourceBytes = await Deno.readFile(entry);
    const sourceSha256 = await sha256Hex(sourceBytes);
    const selfhostArtifacts = await runHostSelfhostArtifacts(
      entry,
      buildLog,
      buildSummary,
    );
    fixtureMap[entry] = {
      source_sha256: sourceSha256,
      selfhost_artifacts: selfhostArtifacts,
    };
    console.log(`[selfhost-artifact-fixture-map] built ${entry}`);
  }

  await Deno.writeTextFile(cfg.outPath, `${JSON.stringify(fixtureMap, null, 2)}\n`);
  console.log(
    `[selfhost-artifact-fixture-map] wrote ${entryList.length} entries to ${cfg.outPath}`,
  );
}

await main().catch(failWithError);
