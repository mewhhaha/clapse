#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";

const decoder = new TextDecoder();

function parseArgs(argv) {
  const out = {
    releaseId: "",
    clapseVersion: "",
    compilerWasm: "",
    cliBin: "",
    behaviorMap: "",
    artifactMap: "",
    preludeSource: "",
    outPath: "out/releases/release-manifest.json",
    checksumsPath: "out/releases/checksums.sha256",
  };
  for (let i = 0; i < argv.length; i += 1) {
    const key = argv[i];
    const val = argv[i + 1];
    if (!val) break;
    if (key === "--release-id") out.releaseId = val;
    if (key === "--clapse-version") out.clapseVersion = val;
    if (key === "--compiler-wasm") out.compilerWasm = val;
    if (key === "--cli-bin") out.cliBin = val;
    if (key === "--behavior-map") out.behaviorMap = val;
    if (key === "--artifact-map") out.artifactMap = val;
    if (key === "--prelude-source") out.preludeSource = val;
    if (key === "--out") out.outPath = val;
    if (key === "--checksums") out.checksumsPath = val;
    if (key.startsWith("--")) i += 1;
  }
  return out;
}

function requireArg(name, value) {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`missing required argument: ${name}`);
  }
}

function parentDir(path) {
  const slash = path.lastIndexOf("/");
  return slash === -1 ? "." : path.slice(0, slash);
}

async function hasPath(path) {
  try {
    await Deno.stat(path);
    return true;
  } catch {
    return false;
  }
}

function defaultCompilerWasmPath() {
  return Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ??
    "artifacts/latest/clapse_compiler.wasm";
}

function parseVersion(versionText) {
  return versionText.split(/\r?\n/u).find((line) => line.trim().length > 0)
    ?.trim() ?? "";
}

async function runOutput(command, args) {
  try {
    const run = new Deno.Command(command, {
      args,
      stdout: "piped",
      stderr: "null",
    });
    const out = await run.output();
    if (!out.success) return "";
    return decoder.decode(out.stdout).trim();
  } catch {
    return "";
  }
}

async function sha256Hex(path) {
  const bytes = await Deno.readFile(path);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return Array.from(new Uint8Array(digest))
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
}

async function artifact(path) {
  const stat = await Deno.stat(path);
  return {
    path,
    bytes: stat.size,
    sha256: await sha256Hex(path),
  };
}

async function main() {
  const cfg = parseArgs(Deno.args);
  requireArg("--release-id", cfg.releaseId);
  requireArg("--compiler-wasm", cfg.compilerWasm);
  requireArg("--behavior-map", cfg.behaviorMap);
  requireArg("--artifact-map", cfg.artifactMap);
  requireArg("--prelude-source", cfg.preludeSource);

  await Deno.mkdir(parentDir(cfg.outPath), { recursive: true });
  await Deno.mkdir(parentDir(cfg.checksumsPath), { recursive: true });

  const versionText = await Deno.readTextFile("VERSION");
  const sourceVersion = parseVersion(versionText);
  const clapseVersion = cfg.clapseVersion.length > 0
    ? cfg.clapseVersion
    : sourceVersion;
  const sourceDateEpoch = Deno.env.get("SOURCE_DATE_EPOCH");
  const builtAtUtc = sourceDateEpoch
    ? new Date(Number(sourceDateEpoch) * 1000).toISOString()
    : new Date().toISOString();

  const manifest = {
    schema_version: 1,
    release_id: cfg.releaseId,
    clapse_version: clapseVersion,
    source_version: sourceVersion,
    built_at_utc: builtAtUtc,
    platform: `${Deno.build.os}-${Deno.build.arch}`,
    tool_versions: {
      deno: (await runOutput("deno", ["--version"])).split(/\r?\n/u)[0]?.replace(
        /^deno\s+/u,
        "",
      ) ?? "",
      clapse_compiler_wasm_path: defaultCompilerWasmPath(),
      clapse_compiler_wasm_exists: (await hasPath(defaultCompilerWasmPath())),
    },
    git: {
      commit: await runOutput("git", ["rev-parse", "HEAD"]),
      short_commit: await runOutput("git", ["rev-parse", "--short=12", "HEAD"]),
    },
    artifacts: {
      compiler_wasm: await artifact(cfg.compilerWasm),
      ...(cfg.cliBin.length > 0 ? { cli_binary: await artifact(cfg.cliBin) } : {}),
      prelude_source: await artifact(cfg.preludeSource),
      native_behavior_fixture_map: await artifact(cfg.behaviorMap),
      native_selfhost_artifact_fixture_map: await artifact(cfg.artifactMap),
    },
  };

  await Deno.writeTextFile(cfg.outPath, `${JSON.stringify(manifest, null, 2)}\n`);
  const checksums = [
    `${manifest.artifacts.compiler_wasm.sha256}  ${cfg.compilerWasm}`,
    ...(cfg.cliBin.length > 0 && manifest.artifacts.cli_binary
      ? [`${manifest.artifacts.cli_binary.sha256}  ${cfg.cliBin}`]
      : []),
    `${manifest.artifacts.prelude_source.sha256}  ${cfg.preludeSource}`,
    `${manifest.artifacts.native_behavior_fixture_map.sha256}  ${cfg.behaviorMap}`,
    `${manifest.artifacts.native_selfhost_artifact_fixture_map.sha256}  ${cfg.artifactMap}`,
  ].join("\n") + "\n";
  await Deno.writeTextFile(cfg.checksumsPath, checksums);

  console.log(`release metadata written: ${cfg.outPath}`);
  console.log(`release checksums written: ${cfg.checksumsPath}`);
}

await main().catch(failWithError);
