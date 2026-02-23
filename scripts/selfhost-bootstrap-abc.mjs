#!/usr/bin/env -S deno run -A

function parseArgs(argv) {
  const out = {
    manifest: "examples/selfhost_corpus.txt",
    behaviorManifest: "examples/selfhost_behavior_corpus.json",
    leftName: "haskell",
    rightName: "haskell",
    requireDistinctEngines: false,
    requireRightEngineMode: "",
    out: "out/selfhost-bootstrap",
    left: 'CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --',
    right: 'CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run clapse --',
  };
  for (let i = 0; i < argv.length; i += 1) {
    const key = argv[i];
    const val = argv[i + 1];
    if (!val) break;
    if (key === "--manifest") out.manifest = val;
    if (key === "--behavior-manifest") out.behaviorManifest = val;
    if (key === "--left-name") out.leftName = val;
    if (key === "--right-name") out.rightName = val;
    if (key === "--require-distinct-engines") {
      out.requireDistinctEngines = val === "1" || val === "true";
    }
    if (key === "--require-right-engine-mode") out.requireRightEngineMode = val;
    if (key === "--out") out.out = val;
    if (key === "--left") out.left = val;
    if (key === "--right") out.right = val;
    if (key.startsWith("--")) i += 1;
  }
  return out;
}

async function runShell(cmd) {
  const proc = new Deno.Command("bash", {
    args: ["-lc", cmd],
    stdout: "piped",
    stderr: "piped",
  });
  const out = await proc.output();
  const stdout = new TextDecoder().decode(out.stdout);
  const stderr = new TextDecoder().decode(out.stderr);
  return { ok: out.success, code: out.code, stdout, stderr };
}

async function main() {
  const cfg = parseArgs(Deno.args);
  await Deno.mkdir(cfg.out, { recursive: true });

  console.log("Stage A: build host compiler");
  const stageA = await runShell(
    'mkdir -p .cabal-logs && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal build exe:clapse',
  );
  if (!stageA.ok) {
    await Deno.writeTextFile(`${cfg.out}/stage_a.stderr.log`, stageA.stderr);
    console.error("Stage A failed");
    Deno.exit(stageA.code || 1);
  }

  console.log("Stage B/C: differential artifact comparison");
  const stageBC = await runShell(
    `deno run -A scripts/selfhost-diff.mjs --manifest "${cfg.manifest}" --left-name '${cfg.leftName}' --right-name '${cfg.rightName}' --left '${cfg.left}' --right '${cfg.right}' --require-distinct-engines '${cfg.requireDistinctEngines ? "1" : "0"}' --require-right-engine-mode '${cfg.requireRightEngineMode}' --out "${cfg.out}/diff"`,
  );
  await Deno.writeTextFile(`${cfg.out}/stage_bc.stdout.log`, stageBC.stdout);
  await Deno.writeTextFile(`${cfg.out}/stage_bc.stderr.log`, stageBC.stderr);
  if (!stageBC.ok) {
    console.error("Stage B/C failed");
    Deno.exit(stageBC.code || 1);
  }

  console.log("Stage B/C behavior: wasm output comparison");
  const stageBehavior = await runShell(
    `deno run -A scripts/selfhost-behavior-diff.mjs --manifest "${cfg.behaviorManifest}" --left-name '${cfg.leftName}' --right-name '${cfg.rightName}' --left '${cfg.left}' --right '${cfg.right}' --require-distinct-engines '${cfg.requireDistinctEngines ? "1" : "0"}' --require-right-engine-mode '${cfg.requireRightEngineMode}' --out "${cfg.out}/behavior-diff"`,
  );
  await Deno.writeTextFile(`${cfg.out}/stage_behavior.stdout.log`, stageBehavior.stdout);
  await Deno.writeTextFile(`${cfg.out}/stage_behavior.stderr.log`, stageBehavior.stderr);
  if (!stageBehavior.ok) {
    console.error("Stage behavior diff failed");
    Deno.exit(stageBehavior.code || 1);
  }

  const reportPath = `${cfg.out}/report.json`;
  const report = {
    generated_at: new Date().toISOString(),
    stage_a_ok: stageA.ok,
    stage_bc_ok: stageBC.ok,
    stage_behavior_ok: stageBehavior.ok,
    engines: {
      left_name: cfg.leftName,
      right_name: cfg.rightName,
      require_distinct: cfg.requireDistinctEngines,
      require_right_engine_mode: cfg.requireRightEngineMode,
      left_cmd: cfg.left,
      right_cmd: cfg.right,
    },
    diff_report: `${cfg.out}/diff/report.json`,
    behavior_diff_report: `${cfg.out}/behavior-diff/report.json`,
  };
  await Deno.writeTextFile(reportPath, JSON.stringify(report, null, 2));
  console.log(`selfhost-bootstrap-abc: PASS (report: ${reportPath})`);
}

await main();
