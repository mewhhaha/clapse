#!/usr/bin/env -S deno run -A

function parseArgs(argv) {
  const defaultCompilerCommand =
    "deno run -A scripts/run-clapse-compiler-wasm.mjs --";
  const out = {
    manifest: "examples/selfhost_parser_corpus.txt",
    leftName: "left",
    rightName: "right",
    requireDistinctEngines: false,
    requireRightEngineMode: "",
    requireExactMerged: true,
    left:
      defaultCompilerCommand,
    right:
      defaultCompilerCommand,
    out: "out/selfhost-parser-parity",
  };

  for (let i = 0; i < argv.length; i += 1) {
    const key = argv[i];
    const val = argv[i + 1];
    if (!val) break;
    if (key === "--manifest") out.manifest = val;
    if (key === "--left") out.left = val;
    if (key === "--right") out.right = val;
    if (key === "--left-name") out.leftName = val;
    if (key === "--right-name") out.rightName = val;
    if (key === "--out") out.out = val;
    if (key === "--require-distinct-engines") {
      out.requireDistinctEngines = val === "1" || val === "true";
    }
    if (key === "--require-right-engine-mode") out.requireRightEngineMode = val;
    if (key === "--require-exact-merged") {
      out.requireExactMerged = val === "1" || val === "true";
    }
    if (key.startsWith("--")) i += 1;
  }

  return out;
}

function shQuote(s) {
  return `'${s.replaceAll("'", "'\\''")}'`;
}

function stableName(path) {
  return path
    .replaceAll("/", "__")
    .replaceAll(".clapse", "");
}

function modeMatches(expected, actual) {
  if (expected === "wasm") {
    return actual.startsWith("wasm");
  }
  if (expected.includes("|")) {
    return expected.split("|").map((x) => x.trim()).includes(actual);
  }
  return actual === expected;
}

function parseManifest(raw) {
  return raw
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.length > 0 && !line.startsWith("#"));
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

async function ensureDir(path) {
  await Deno.mkdir(path, { recursive: true });
}

async function readMaybe(path) {
  try {
    return await Deno.readTextFile(path);
  } catch {
    return null;
  }
}

function boolToString(value) {
  return value ? "true" : "false";
}

function entryStatus(result) {
  if (!result.left_ok) return `left-failed (${result.left_code})`;
  if (!result.right_ok) return `right-failed (${result.right_code})`;
  if (result.mismatches.length > 0) return `mismatch (${result.mismatches.join(", ")})`;
  return "pass";
}

function writeSummary(cfg, results) {
  const failures = results.filter((result) => !result.pass);
  const now = new Date().toISOString();
  const lines = [
    "# Selfhost parser parity summary",
    `Generated at: ${now}`,
    `Manifest: ${cfg.manifest}`,
    `Left: ${cfg.leftName}`,
    `Right: ${cfg.rightName}`,
    `Total: ${results.length}`,
    `Passed: ${results.length - failures.length}`,
    `Failed: ${failures.length}`,
    "",
    "| entry | status |",
    "| --- | --- |",
  ];

  for (const result of results) {
    const status = result.pass ? "PASS" : `FAIL (${entryStatus(result)})`;
    lines.push(`| ${result.entry} | ${status} |`);
  }

  return `${lines.join("\n")}\n`;
}

async function main() {
  const cfg = parseArgs(Deno.args);

  if (cfg.requireDistinctEngines && cfg.left === cfg.right) {
    console.error(
      "selfhost-parser-parity: strict mode requires different left/right commands",
    );
    Deno.exit(2);
  }

  if (cfg.requireRightEngineMode.length > 0) {
    const probe = await runShell(`${cfg.right} engine-mode`);
    const mode = probe.stdout.trim();
    if (!probe.ok || !modeMatches(cfg.requireRightEngineMode, mode)) {
      console.error(
        `selfhost-parser-parity: right mode mismatch (expected '${cfg.requireRightEngineMode}', got '$${
          mode || "<error>"
        }')`,
      );
      if (probe.stderr.trim().length > 0) {
        console.error(probe.stderr.trim());
      }
      Deno.exit(3);
    }
  }

  await ensureDir(cfg.out);
  const manifestRaw = await Deno.readTextFile(cfg.manifest);
  const entries = parseManifest(manifestRaw);

  const results = [];

  for (const entry of entries) {
    const name = stableName(entry);
    const leftOut = `${cfg.out}/left/${name}`;
    const rightOut = `${cfg.out}/right/${name}`;
    await ensureDir(leftOut);
    await ensureDir(rightOut);

    const leftCmd = `${cfg.left} selfhost-artifacts ${shQuote(entry)} ${shQuote(leftOut)}`;
    const rightCmd = `${cfg.right} selfhost-artifacts ${shQuote(entry)} ${shQuote(rightOut)}`;

    const left = await runShell(leftCmd);
    const right = await runShell(rightCmd);

    const mismatches = [];
    if (left.ok && right.ok) {
      const leftMerged = await readMaybe(`${leftOut}/merged_module.txt`);
      const rightMerged = await readMaybe(`${rightOut}/merged_module.txt`);
      if (leftMerged === null || rightMerged === null) {
        mismatches.push("merged_module.txt");
      } else if (leftMerged !== rightMerged) {
        mismatches.push("merged_module.txt");
      }
    }

    const pass = left.ok &&
      right.ok &&
      (!cfg.requireExactMerged || mismatches.length === 0);

    const result = {
      entry,
      stable_name: name,
      left_ok: left.ok,
      right_ok: right.ok,
      left_code: left.code,
      right_code: right.code,
      left_stderr: left.stderr.trim(),
      right_stderr: right.stderr.trim(),
      mismatches,
      pass,
      require_distinct_engines: cfg.requireDistinctEngines,
      require_right_engine_mode: cfg.requireRightEngineMode,
      require_exact_merged: cfg.requireExactMerged,
      left_name: cfg.leftName,
      right_name: cfg.rightName,
      left_cmd: cfg.left,
      right_cmd: cfg.right,
      boolean_report: boolToString(pass),
    };

    results.push(result);
    console.log(`[${pass ? "PASS" : "FAIL"}] ${entry}`);
  }

  const failures = results.filter((result) => !result.pass);

  const report = {
    generated_at: new Date().toISOString(),
    manifest: cfg.manifest,
    output_dir: cfg.out,
    total: results.length,
    failed: failures.length,
    engines: {
      left_name: cfg.leftName,
      right_name: cfg.rightName,
      left_cmd: cfg.left,
      right_cmd: cfg.right,
      require_distinct_engines: cfg.requireDistinctEngines,
      require_right_engine_mode: cfg.requireRightEngineMode,
      require_exact_merged: cfg.requireExactMerged,
    },
    results,
  };

  await Deno.writeTextFile(
    `${cfg.out}/report.json`,
    JSON.stringify(report, null, 2),
  );
  await Deno.writeTextFile(`${cfg.out}/summary.md`, writeSummary(cfg, results));

  if (failures.length > 0) {
    console.error(
      `selfhost-parser-parity: ${failures.length}/${results.length} entries failed`,
    );
    Deno.exit(1);
  }

  console.log(`selfhost-parser-parity: PASS (${results.length} entries)`);
}

await main();
