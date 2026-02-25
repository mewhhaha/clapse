#!/usr/bin/env -S deno run -A

function parseArgs(argv) {
  const defaultCompilerCommand =
    "deno run -A scripts/run-clapse-compiler-wasm.mjs --";
  const out = {
    manifest: "examples/compiler_source_corpus.txt",
    cmd: defaultCompilerCommand,
    out: "out/formatter-idempotence",
  };

  for (let i = 0; i < argv.length; i += 1) {
    const key = argv[i];
    const val = argv[i + 1];
    if (!val) break;
    if (key === "--manifest") out.manifest = val;
    if (key === "--cmd") out.cmd = val;
    if (key === "--out") out.out = val;
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

async function runFormatter(cmdPrefix, source) {
  const tmp = await Deno.makeTempFile();
  try {
    await Deno.writeTextFile(tmp, source);
    const fullCmd = `${cmdPrefix} format --stdin < ${shQuote(tmp)}`;
    return await runShell(fullCmd);
  } finally {
    await Deno.remove(tmp);
  }
}

function writeSummary(cfg, results) {
  const failures = results.filter((result) => !result.pass);
  const now = new Date().toISOString();
  const lines = [
    "# Formatter idempotence summary",
    `Generated at: ${now}`,
    `Manifest: ${cfg.manifest}`,
    `Command prefix: ${cfg.cmd}`,
    `Total files: ${results.length}`,
    `Passed: ${results.length - failures.length}`,
    `Failed: ${failures.length}`,
    "",
    "| file | status |",
    "| --- | --- |",
  ];

  for (const result of results) {
    const status = result.pass
      ? "PASS"
      : `FAIL (pass=${result.first_and_second_equal}, first=${result.first_ok}, second=${result.second_ok})`;
    lines.push(`| ${result.entry} | ${status} |`);
  }

  return `${lines.join("\n")}\n`;
}

async function main() {
  const cfg = parseArgs(Deno.args);
  await ensureDir(cfg.out);
  await ensureDir(`${cfg.out}/files`);

  const manifestRaw = await Deno.readTextFile(cfg.manifest);
  const entries = parseManifest(manifestRaw);

  const results = [];

  for (const entry of entries) {
    const source = await readMaybe(entry);
    const name = stableName(entry);
    const out = `${cfg.out}/files/${name}`;

    if (source === null) {
      const missing = {
        entry,
        stable_name: name,
        first_ok: false,
        second_ok: false,
        first_and_second_equal: false,
        first_code: 0,
        second_code: 0,
        first_stderr: `missing source file: ${entry}`,
        second_stderr: "",
        pass: false,
      };
      results.push(missing);
      continue;
    }

    const first = await runFormatter(cfg.cmd, source);
    const firstPassText = first.stdout;
    const firstOk = first.ok;
    const firstCode = first.code;

    let second = { ok: false, code: 0, stdout: "", stderr: "" };
    if (firstOk) {
      second = await runFormatter(cfg.cmd, firstPassText);
    }
    const secondPassText = second.stdout;
    const secondOk = second.ok;
    const secondCode = second.code;

    const same = firstOk && secondOk && firstPassText === secondPassText;

    await Deno.writeTextFile(`${out}.formatted-first.clapse`, firstPassText);
    await Deno.writeTextFile(`${out}.formatted-second.clapse`, secondPassText);

    const result = {
      entry,
      stable_name: name,
      first_ok: firstOk,
      second_ok: secondOk,
      first_and_second_equal: same,
      first_code: firstCode,
      second_code: secondCode,
      first_stderr: first.stderr.trim(),
      second_stderr: second.stderr.trim(),
      first_bytes: firstPassText.length,
      second_bytes: secondPassText.length,
      pass: same,
    };

    results.push(result);
    console.log(`[${result.pass ? "PASS" : "FAIL"}] ${entry}`);
  }

  const failures = results.filter((result) => !result.pass);

  const report = {
    generated_at: new Date().toISOString(),
    manifest: cfg.manifest,
    cmd: cfg.cmd,
    output_dir: cfg.out,
    total: results.length,
    failed: failures.length,
    results,
  };

  await Deno.writeTextFile(
    `${cfg.out}/report.json`,
    JSON.stringify(report, null, 2),
  );
  await Deno.writeTextFile(`${cfg.out}/summary.md`, writeSummary(cfg, results));

  if (failures.length > 0) {
    console.error(
      `formatter-idempotence-corpus: ${failures.length}/${results.length} files failed`,
    );
    Deno.exit(1);
  }

  console.log(`formatter-idempotence-corpus: PASS (${results.length} files)`);
}

await main();
