#!/usr/bin/env -S deno run -A

function parseArgs(argv) {
  const out = {
    manifest: "examples/selfhost_behavior_corpus.json",
    leftName: "haskell",
    rightName: "haskell",
    requireDistinctEngines: false,
    requireRightEngineMode: "",
    left: 'CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --',
    right: 'CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --',
    out: "out/selfhost-behavior-diff",
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

function scenarioKey(s) {
  const args = Array.isArray(s.args) ? s.args.join("_") : "";
  return `${s.entry}__${s.export ?? "main"}__${args}`.replaceAll("/", "__").replaceAll(".clapse", "");
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

async function main() {
  const cfg = parseArgs(Deno.args);
  if (cfg.requireDistinctEngines && cfg.left === cfg.right) {
    console.error(
      "selfhost-behavior-diff: strict mode enabled but left and right engine commands are identical",
    );
    Deno.exit(2);
  }
  if (cfg.requireRightEngineMode.length > 0) {
    const probe = await runShell(`${cfg.right} engine-mode`);
    const mode = probe.stdout.trim();
    if (!probe.ok || mode !== cfg.requireRightEngineMode) {
      console.error(
        `selfhost-behavior-diff: right engine mode mismatch (expected '${cfg.requireRightEngineMode}', got '${mode || "<error>"}')`,
      );
      if (!probe.ok && probe.stderr.trim().length > 0) {
        console.error(probe.stderr.trim());
      }
      Deno.exit(3);
    }
  }
  await ensureDir(cfg.out);
  const scenarios = parseManifest(await Deno.readTextFile(cfg.manifest));
  const results = [];
  const leftCompileCache = new Map();
  const rightCompileCache = new Map();

  for (const s of scenarios) {
    const key = scenarioKey(s);
    const outLeft = `${cfg.out}/left/${s.entry.replaceAll("/", "__").replaceAll(".clapse", "")}.wasm`;
    const outRight = `${cfg.out}/right/${s.entry.replaceAll("/", "__").replaceAll(".clapse", "")}.wasm`;
    await ensureDir(`${cfg.out}/left`);
    await ensureDir(`${cfg.out}/right`);

    const entry = s.entry;
    const exportName = s.export ?? "main";
    const expect = s.expect ?? "value";
    const args = Array.isArray(s.args) ? s.args.map(String) : [];
    const argsJoined = args.map(shQuote).join(" ");

    let leftCompile = leftCompileCache.get(entry);
    if (!leftCompile) {
      leftCompile = await runShell(
        `${cfg.left} compile ${shQuote(entry)} ${shQuote(outLeft)}`,
      );
      leftCompileCache.set(entry, leftCompile);
    }
    let rightCompile = rightCompileCache.get(entry);
    if (!rightCompile) {
      rightCompile = await runShell(
        `${cfg.right} compile ${shQuote(entry)} ${shQuote(outRight)}`,
      );
      rightCompileCache.set(entry, rightCompile);
    }

    let leftRun = { ok: false, code: -1, stdout: "", stderr: "compile failed" };
    let rightRun = { ok: false, code: -1, stdout: "", stderr: "compile failed" };
    if (leftCompile.ok) {
      leftRun = await runShell(
        `deno run -A scripts/run-wasm.mjs ${shQuote(outLeft)} ${shQuote(exportName)} ${argsJoined}`,
      );
    }
    if (rightCompile.ok) {
      rightRun = await runShell(
        `deno run -A scripts/run-wasm.mjs ${shQuote(outRight)} ${shQuote(exportName)} ${argsJoined}`,
      );
    }

    const sameOutput =
      leftRun.ok &&
      rightRun.ok &&
      normalizeOut(leftRun.stdout) === normalizeOut(rightRun.stdout);
    const sameTrap =
      !leftRun.ok &&
      !rightRun.ok &&
      normalizeOut(leftRun.stderr) === normalizeOut(rightRun.stderr);
    const scenarioPass =
      expect === "trap"
        ? leftCompile.ok && rightCompile.ok && sameTrap
        : leftCompile.ok && rightCompile.ok && sameOutput;

    results.push({
      entry,
      export: exportName,
      args: s.args ?? [],
      expect,
      left_compile_ok: leftCompile.ok,
      right_compile_ok: rightCompile.ok,
      left_run_ok: leftRun.ok,
      right_run_ok: rightRun.ok,
      left_out: normalizeOut(leftRun.stdout),
      right_out: normalizeOut(rightRun.stdout),
      left_err: normalizeOut(leftRun.stderr || leftCompile.stderr),
      right_err: normalizeOut(rightRun.stderr || rightCompile.stderr),
      same_output: sameOutput,
      same_trap: sameTrap,
      scenario_pass: scenarioPass,
    });
    console.log(
      `[${scenarioPass ? "PASS" : "FAIL"}] ${entry} ${exportName}(${(s.args ?? []).join(",")})`,
    );
  }

  const failures = results.filter((r) => !r.scenario_pass);
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
    console.error(`selfhost-behavior-diff: ${failures.length}/${results.length} scenarios failed`);
    Deno.exit(1);
  }
  console.log(`selfhost-behavior-diff: PASS (${results.length} scenarios)`);
}

await main();
