#!/usr/bin/env -S deno run -A

function parseArgs(argv) {
  const out = {
    manifest: "examples/selfhost_corpus.txt",
    behaviorManifest: "examples/selfhost_behavior_corpus.json",
    leftName: "haskell",
    rightName: "haskell",
    requireDistinctEngines: false,
    requireRightEngineMode: "",
    requireExactArtifacts: true,
    forbidHostClapseImports: false,
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
    if (key === "--require-exact-artifacts") {
      out.requireExactArtifacts = val === "1" || val === "true";
    }
    if (key === "--forbid-host-clapse-imports") {
      out.forbidHostClapseImports = val === "1" || val === "true";
    }
    if (key === "--out") out.out = val;
    if (key === "--left") out.left = val;
    if (key === "--right") out.right = val;
    if (key.startsWith("--")) i += 1;
  }
  return out;
}

function loadManifest(raw) {
  return raw
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0 && !line.startsWith("#"));
}

function parseBehaviorManifest(raw) {
  const decoded = JSON.parse(raw);
  if (!decoded || !Array.isArray(decoded.scenarios)) {
    throw new Error("invalid behavior manifest: expected { scenarios: [...] }");
  }
  return decoded.scenarios;
}

function extractBehaviorEntries(scenarios) {
  return scenarios
    .map((scenario) => scenario?.entry)
    .filter((entry) => typeof entry === "string" && entry.length > 0);
}

function hasHostClapseImport(sourceText) {
  return sourceText.includes("import host.clapse");
}

async function scanEntryFilesForHostClapseImports(entries) {
  const bad = [];
  const seen = new Set();
  for (const entry of entries) {
    if (seen.has(entry)) continue;
    seen.add(entry);
    const text = await Deno.readTextFile(entry);
    if (hasHostClapseImport(text)) {
      bad.push(entry);
    }
  }
  return bad;
}

async function enforceNoHostClapseImports(cfg) {
  const manifestEntries = loadManifest(await Deno.readTextFile(cfg.manifest));
  const behaviorScenarios = parseBehaviorManifest(
    await Deno.readTextFile(cfg.behaviorManifest),
  );
  const behaviorEntries = extractBehaviorEntries(behaviorScenarios);
  const badEntries = await scanEntryFilesForHostClapseImports([
    ...manifestEntries,
    ...behaviorEntries,
  ]);
  if (badEntries.length > 0) {
    console.error(
      "selfhost-bootstrap-abc: forbidden host.clapse import found in entry files",
    );
    for (const entry of badEntries) {
      console.error(`  - ${entry}`);
    }
    Deno.exit(4);
  }
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
  if (cfg.forbidHostClapseImports) {
    await enforceNoHostClapseImports(cfg);
  }
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
    `deno run -A scripts/selfhost-diff.mjs --manifest "${cfg.manifest}" --left-name '${cfg.leftName}' --right-name '${cfg.rightName}' --left '${cfg.left}' --right '${cfg.right}' --require-distinct-engines '${cfg.requireDistinctEngines ? "1" : "0"}' --require-right-engine-mode '${cfg.requireRightEngineMode}' --require-exact-artifacts '${cfg.requireExactArtifacts ? "1" : "0"}' --out "${cfg.out}/diff"`,
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
      require_exact_artifacts: cfg.requireExactArtifacts,
      forbid_host_clapse_imports: cfg.forbidHostClapseImports,
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
