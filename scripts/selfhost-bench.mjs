#!/usr/bin/env -S deno run -A

function parseArgs(argv) {
  const defaultCompilerCommand =
    "deno run -A scripts/run-clapse-compiler-wasm.mjs --";
  const out = {
    manifest: "examples/selfhost_behavior_corpus.json",
    leftName: "wasm",
    rightName: "wasm",
    requireDistinctEngines: false,
    requireRightEngineMode: "",
    left:
      defaultCompilerCommand,
    right:
      defaultCompilerCommand,
    out: "out/selfhost-bench",
    repeats: 1,
    reuseCompilesAcrossRepeats: true,
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
    if (key === "--repeats") {
      const n = Number(val);
      if (!Number.isInteger(n) || n <= 0) {
        throw new Error(`invalid --repeats: ${val}`);
      }
      out.repeats = n;
    }
    if (key === "--reuse-compiles-across-repeats") {
      out.reuseCompilesAcrossRepeats = val === "1" || val === "true";
    }
    if (key.startsWith("--")) i += 1;
  }
  return out;
}

function shQuote(s) {
  return `'${s.replaceAll("'", "'\\''")}'`;
}

async function runShellTimed(cmd) {
  const start = performance.now();
  const proc = new Deno.Command("bash", {
    args: ["-lc", cmd],
    stdout: "piped",
    stderr: "piped",
  });
  const out = await proc.output();
  const end = performance.now();
  return {
    ok: out.success,
    code: out.code,
    stdout: new TextDecoder().decode(out.stdout),
    stderr: new TextDecoder().decode(out.stderr),
    ms: end - start,
  };
}

function modeMatches(expected, actual) {
  if (expected === "wasm") return actual.startsWith("wasm");
  if (expected.includes("|")) {
    return expected.split("|").map((x) => x.trim()).includes(actual);
  }
  return actual === expected;
}

function normalizeOut(s) {
  return s.trim();
}

function parseManifest(raw) {
  const decoded = JSON.parse(raw);
  if (!decoded || !Array.isArray(decoded.scenarios)) {
    throw new Error("invalid behavior manifest: expected { scenarios: [...] }");
  }
  return decoded.scenarios;
}

async function ensureDir(path) {
  await Deno.mkdir(path, { recursive: true });
}

async function main() {
  const cfg = parseArgs(Deno.args);
  if (cfg.requireDistinctEngines && cfg.left === cfg.right) {
    console.error(
      "selfhost-bench: strict mode enabled but left and right engine commands are identical",
    );
    Deno.exit(2);
  }
  if (cfg.requireRightEngineMode.length > 0) {
    const probe = await runShellTimed(`${cfg.right} engine-mode`);
    const mode = probe.stdout.trim();
    if (!probe.ok || !modeMatches(cfg.requireRightEngineMode, mode)) {
      console.error(
        `selfhost-bench: right engine mode mismatch (expected '${cfg.requireRightEngineMode}', got '${
          mode || "<error>"
        }')`,
      );
      if (!probe.ok && probe.stderr.trim().length > 0) {
        console.error(probe.stderr.trim());
      }
      Deno.exit(3);
    }
  }
  await ensureDir(cfg.out);
  await ensureDir(`${cfg.out}/left`);
  await ensureDir(`${cfg.out}/right`);

  const scenarios = parseManifest(await Deno.readTextFile(cfg.manifest));
  const results = [];
  const totals = {
    left_compile_ms: 0,
    left_run_ms: 0,
    right_compile_ms: 0,
    right_run_ms: 0,
  };
  const perRepeat = new Map();
  const leftCompileCache = new Map();
  const rightCompileCache = new Map();

  for (let rep = 0; rep < cfg.repeats; rep += 1) {
    if (!cfg.reuseCompilesAcrossRepeats) {
      leftCompileCache.clear();
      rightCompileCache.clear();
    }
    for (const s of scenarios) {
      const outLeft = `${cfg.out}/left/${
        s.entry.replaceAll("/", "__").replaceAll(".clapse", "")
      }.wasm`;
      const outRight = `${cfg.out}/right/${
        s.entry.replaceAll("/", "__").replaceAll(".clapse", "")
      }.wasm`;

      const entry = s.entry;
      const exportName = s.export ?? "main";
      const expect = s.expect ?? "value";
      const args = Array.isArray(s.args) ? s.args.map(String) : [];
      const argsJoined = args.map(shQuote).join(" ");

      let leftCompile = leftCompileCache.get(entry);
      if (!leftCompile) {
        leftCompile = await runShellTimed(
          `${cfg.left} compile ${shQuote(entry)} ${shQuote(outLeft)}`,
        );
        leftCompileCache.set(entry, leftCompile);
      }
      let rightCompile = rightCompileCache.get(entry);
      if (!rightCompile) {
        rightCompile = await runShellTimed(
          `${cfg.right} compile ${shQuote(entry)} ${shQuote(outRight)}`,
        );
        rightCompileCache.set(entry, rightCompile);
      }
      totals.left_compile_ms += leftCompile.ms;
      totals.right_compile_ms += rightCompile.ms;

      let leftRun = { ok: false, stdout: "", stderr: "compile failed", ms: 0 };
      let rightRun = { ok: false, stdout: "", stderr: "compile failed", ms: 0 };
      if (leftCompile.ok) {
        leftRun = await runShellTimed(
          `deno run -A scripts/run-wasm.mjs ${shQuote(outLeft)} ${
            shQuote(exportName)
          } ${argsJoined}`,
        );
      }
      if (rightCompile.ok) {
        rightRun = await runShellTimed(
          `deno run -A scripts/run-wasm.mjs ${shQuote(outRight)} ${
            shQuote(exportName)
          } ${argsJoined}`,
        );
      }
      totals.left_run_ms += leftRun.ms;
      totals.right_run_ms += rightRun.ms;
      const repTotals = perRepeat.get(rep) ?? {
        left_compile_ms: 0,
        left_run_ms: 0,
        right_compile_ms: 0,
        right_run_ms: 0,
      };
      repTotals.left_compile_ms += leftCompile.ms;
      repTotals.left_run_ms += leftRun.ms;
      repTotals.right_compile_ms += rightCompile.ms;
      repTotals.right_run_ms += rightRun.ms;
      perRepeat.set(rep, repTotals);

      const sameOutput = leftRun.ok &&
        rightRun.ok &&
        normalizeOut(leftRun.stdout) === normalizeOut(rightRun.stdout);
      const sameTrap = !leftRun.ok &&
        !rightRun.ok &&
        normalizeOut(leftRun.stderr) === normalizeOut(rightRun.stderr);
      const scenarioPass = expect === "trap"
        ? leftCompile.ok && rightCompile.ok && sameTrap
        : leftCompile.ok && rightCompile.ok && sameOutput;

      results.push({
        repeat: rep,
        entry,
        export: exportName,
        args: s.args ?? [],
        expect,
        scenario_pass: scenarioPass,
        left_compile_ms: leftCompile.ms,
        left_run_ms: leftRun.ms,
        right_compile_ms: rightCompile.ms,
        right_run_ms: rightRun.ms,
      });
    }
  }

  const failures = results.filter((r) => !r.scenario_pass);
  const scenarioCount = scenarios.length * cfg.repeats;
  const leftTotal = totals.left_compile_ms + totals.left_run_ms;
  const rightTotal = totals.right_compile_ms + totals.right_run_ms;
  const ratio = leftTotal > 0 ? rightTotal / leftTotal : 0;
  const perRepeatRows = [];
  for (let rep = 0; rep < cfg.repeats; rep += 1) {
    const row = perRepeat.get(rep) ?? {
      left_compile_ms: 0,
      left_run_ms: 0,
      right_compile_ms: 0,
      right_run_ms: 0,
    };
    const leftTotalRep = row.left_compile_ms + row.left_run_ms;
    const rightTotalRep = row.right_compile_ms + row.right_run_ms;
    const ratioRep = leftTotalRep > 0 ? rightTotalRep / leftTotalRep : 0;
    perRepeatRows.push({
      repeat: rep,
      left_compile_ms: row.left_compile_ms,
      left_run_ms: row.left_run_ms,
      left_total_ms: leftTotalRep,
      right_compile_ms: row.right_compile_ms,
      right_run_ms: row.right_run_ms,
      right_total_ms: rightTotalRep,
      right_over_left_ratio: ratioRep,
    });
  }
  const ratioMedian = median(perRepeatRows.map((r) => r.right_over_left_ratio));

  const report = {
    generated_at: new Date().toISOString(),
    manifest: cfg.manifest,
    repeats: cfg.repeats,
    reuse_compiles_across_repeats: cfg.reuseCompilesAcrossRepeats,
    engines: {
      left_name: cfg.leftName,
      right_name: cfg.rightName,
      require_distinct: cfg.requireDistinctEngines,
      require_right_engine_mode: cfg.requireRightEngineMode,
      left_cmd: cfg.left,
      right_cmd: cfg.right,
    },
    totals: {
      scenario_count: scenarioCount,
      left_compile_ms: totals.left_compile_ms,
      left_run_ms: totals.left_run_ms,
      left_total_ms: leftTotal,
      right_compile_ms: totals.right_compile_ms,
      right_run_ms: totals.right_run_ms,
      right_total_ms: rightTotal,
      right_over_left_ratio: ratio,
      right_over_left_ratio_median: ratioMedian,
    },
    per_repeat: perRepeatRows,
    failed: failures.length,
    results,
  };
  await Deno.writeTextFile(
    `${cfg.out}/report.json`,
    JSON.stringify(report, null, 2),
  );

  console.log(`scenarios: ${scenarioCount}`);
  console.log(`left_total_ms: ${leftTotal.toFixed(3)}`);
  console.log(`right_total_ms: ${rightTotal.toFixed(3)}`);
  console.log(`right_over_left_ratio: ${ratio.toFixed(4)}`);
  console.log(`right_over_left_ratio_median: ${ratioMedian.toFixed(4)}`);
  if (failures.length > 0) {
    console.error(
      `selfhost-bench: parity failed (${failures.length}/${scenarioCount})`,
    );
    Deno.exit(1);
  }
  console.log("selfhost-bench: PASS");
}

await main();

function median(xs) {
  if (xs.length === 0) return 0;
  const ys = [...xs].sort((a, b) => a - b);
  const mid = Math.floor(ys.length / 2);
  if (ys.length % 2 === 1) return ys[mid];
  return (ys[mid - 1] + ys[mid]) / 2;
}
