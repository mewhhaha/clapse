#!/usr/bin/env -S deno run -A

const FILES = [
  "merged_module.txt",
  "type_info.txt",
  "type_info_error.txt",
  "lowered_ir.txt",
  "collapsed_ir.txt",
  "exports.txt",
  "wasm_stats.txt",
];

function parseArgs(argv) {
  const out = {
    manifest: "examples/selfhost_corpus.txt",
    leftName: "haskell",
    rightName: "haskell",
    requireDistinctEngines: false,
    requireRightEngineMode: "",
    left: 'CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --',
    right: 'CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --',
    out: "out/selfhost-diff",
  };
  for (let i = 0; i < argv.length; i += 1) {
    const key = argv[i];
    const val = argv[i + 1];
    if (!val) break;
    if (key === "--manifest") out.manifest = val;
    if (key === "--left-name") out.leftName = val;
    if (key === "--right-name") out.rightName = val;
    if (key === "--left") out.left = val;
    if (key === "--right") out.right = val;
    if (key === "--out") out.out = val;
    if (key === "--require-distinct-engines") {
      out.requireDistinctEngines = val === "1" || val === "true";
    }
    if (key === "--require-right-engine-mode") out.requireRightEngineMode = val;
    if (key.startsWith("--")) i += 1;
  }
  return out;
}

function shQuote(s) {
  return `'${s.replaceAll("'", "'\\''")}'`;
}

function stableName(path) {
  return path.replaceAll("/", "__").replaceAll(".clapse", "");
}

async function runShell(cmd) {
  const proc = new Deno.Command("bash", {
    args: ["-lc", cmd],
    stdout: "piped",
    stderr: "piped",
  });
  const out = await proc.output();
  return {
    ok: out.success,
    code: out.code,
    stdout: new TextDecoder().decode(out.stdout),
    stderr: new TextDecoder().decode(out.stderr),
  };
}

function loadManifest(raw) {
  return raw
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.length > 0 && !line.startsWith("#"));
}

async function readMaybe(path) {
  try {
    return await Deno.readTextFile(path);
  } catch {
    return null;
  }
}

async function ensureCleanDir(path) {
  await Deno.mkdir(path, { recursive: true });
}

async function main() {
  const cfg = parseArgs(Deno.args);
  if (cfg.requireDistinctEngines && cfg.left === cfg.right) {
    console.error(
      "selfhost-diff: strict mode enabled but left and right engine commands are identical",
    );
    Deno.exit(2);
  }
  if (cfg.requireRightEngineMode.length > 0) {
    const probe = await runShell(`${cfg.right} engine-mode`);
    const mode = probe.stdout.trim();
    if (!probe.ok || mode !== cfg.requireRightEngineMode) {
      console.error(
        `selfhost-diff: right engine mode mismatch (expected '${cfg.requireRightEngineMode}', got '${mode || "<error>"}')`,
      );
      if (!probe.ok && probe.stderr.trim().length > 0) {
        console.error(probe.stderr.trim());
      }
      Deno.exit(3);
    }
  }
  await ensureCleanDir(cfg.out);
  const manifestRaw = await Deno.readTextFile(cfg.manifest);
  const entries = loadManifest(manifestRaw);
  const results = [];

  for (const entry of entries) {
    const name = stableName(entry);
    const leftDir = `${cfg.out}/left/${name}`;
    const rightDir = `${cfg.out}/right/${name}`;
    await ensureCleanDir(leftDir);
    await ensureCleanDir(rightDir);

    const leftCmd = `${cfg.left} selfhost-artifacts ${shQuote(entry)} ${shQuote(leftDir)}`;
    const rightCmd = `${cfg.right} selfhost-artifacts ${shQuote(entry)} ${shQuote(rightDir)}`;
    const left = await runShell(leftCmd);
    const right = await runShell(rightCmd);
    const mismatches = [];
    if (left.ok && right.ok) {
      for (const file of FILES) {
        const l = await readMaybe(`${leftDir}/${file}`);
        const r = await readMaybe(`${rightDir}/${file}`);
        if (l !== r) mismatches.push(file);
      }
    }
    results.push({
      entry,
      left_ok: left.ok,
      right_ok: right.ok,
      left_code: left.code,
      right_code: right.code,
      left_stderr: left.stderr.trim(),
      right_stderr: right.stderr.trim(),
      mismatches,
    });
    const status = left.ok && right.ok && mismatches.length === 0 ? "PASS" : "FAIL";
    console.log(`[${status}] ${entry}`);
  }

  const failures = results.filter((r) =>
    !r.left_ok || !r.right_ok || r.mismatches.length > 0
  );
  const report = {
    generated_at: new Date().toISOString(),
    manifest: cfg.manifest,
    engines: {
      left_name: cfg.leftName,
      right_name: cfg.rightName,
      require_distinct: cfg.requireDistinctEngines,
      require_right_engine_mode: cfg.requireRightEngineMode,
      left_cmd: cfg.left,
      right_cmd: cfg.right,
    },
    total: results.length,
    failed: failures.length,
    results,
  };
  await Deno.writeTextFile(`${cfg.out}/report.json`, JSON.stringify(report, null, 2));
  if (failures.length > 0) {
    console.error(`selfhost-diff: ${failures.length}/${results.length} entries failed`);
    Deno.exit(1);
  }
  console.log(`selfhost-diff: PASS (${results.length} entries)`);
}

await main();
