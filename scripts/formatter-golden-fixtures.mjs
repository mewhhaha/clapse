#!/usr/bin/env -S deno run -A

function parseArgs(argv) {
  const out = {
    fixtures: "examples/formatter_golden_fixtures.json",
    cmd: "deno run -A scripts/run-clapse-compiler-wasm.mjs --",
    out: "out/formatter-golden-fixtures",
  };

  for (let i = 0; i < argv.length; i += 1) {
    const key = argv[i];
    const val = argv[i + 1];
    if (!val) break;
    if (key === "--fixtures") out.fixtures = val;
    if (key === "--cmd") out.cmd = val;
    if (key === "--out") out.out = val;
    if (key.startsWith("--")) {
      i += 1;
    }
  }

  return out;
}

function parseManifest(raw, path) {
  const decoded = JSON.parse(raw);
  if (!decoded || typeof decoded !== "object" || !Array.isArray(decoded.fixtures)) {
    throw new Error(`invalid formatter fixture manifest: expected { fixtures: [...] } in ${path}`);
  }
  return decoded.fixtures;
}

function normalizeName(name) {
  return name.trim().replace(/[^a-zA-Z0-9._-]+/gu, "_");
}

async function readText(path) {
  try {
    return await Deno.readTextFile(path);
  } catch (error) {
    return null;
  }
}

function shQuote(s) {
  return `'${s.replaceAll("'", "'\''")}'`;
}

function ensureDir(path) {
  return Deno.mkdir(path, { recursive: true });
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

function writeSummary(results) {
  const failures = results.filter((x) => !x.pass);
  const lines = [
    "# Formatter golden fixtures",
    `Generated at: ${new Date().toISOString()}`,
    `Total: ${results.length}`,
    `Passed: ${results.length - failures.length}`,
    `Failed: ${failures.length}`,
    "",
    "| fixture | status |",
    "| --- | --- |",
  ];

  for (const result of results) {
    const status = result.pass
      ? "PASS"
      : `FAIL (formatted_ok=${result.formatted_ok}, matched=${result.output_match})`;
    lines.push(`| ${result.name} | ${status} |`);
  }

  return `${lines.join("\n")}\n`;
}

async function main() {
  const cfg = parseArgs(Deno.args);
  await ensureDir(cfg.out);
  await ensureDir(`${cfg.out}/files`);

  const fixturesRaw = await Deno.readTextFile(cfg.fixtures);
  const entries = parseManifest(fixturesRaw, cfg.fixtures);

  const results = [];

  for (const entry of entries) {
    const name = normalizeName(
      `${typeof entry?.name === "string" && entry.name.length > 0 ? entry.name : entry?.input}` || "fixture",
    );

    const inputPath = entry?.input;
    const expectedPath = entry?.expected;
    if (typeof inputPath !== "string" || inputPath.length === 0) {
      results.push({
        name,
        input_path: String(inputPath),
        expected_path: String(expectedPath),
        formatted_ok: false,
        output_match: false,
        pass: false,
        formatted_error: "missing fixture.input",
      });
      continue;
    }
    if (typeof expectedPath !== "string" || expectedPath.length === 0) {
      results.push({
        name,
        input_path: inputPath,
        expected_path: String(expectedPath),
        formatted_ok: false,
        output_match: false,
        pass: false,
        formatted_error: "missing fixture.expected",
      });
      continue;
    }

    const inputText = await readText(inputPath);
    const expectedText = await readText(expectedPath);
    if (inputText === null) {
      results.push({
        name,
        input_path: inputPath,
        expected_path: expectedPath,
        formatted_ok: false,
        output_match: false,
        pass: false,
        formatted_error: `could not read input fixture: ${inputPath}`,
      });
      continue;
    }
    if (expectedText === null) {
      results.push({
        name,
        input_path: inputPath,
        expected_path: expectedPath,
        formatted_ok: false,
        output_match: false,
        pass: false,
        formatted_error: `could not read expected fixture: ${expectedPath}`,
      });
      continue;
    }

    const format = await runFormatter(cfg.cmd, inputText);
    const actualText = format.ok ? format.stdout : "";
    const outputMatch = actualText === expectedText;
    const outputPath = `${cfg.out}/files/${name}.actual.clapse`;
    await Deno.writeTextFile(outputPath, actualText);

    const pass = format.ok && outputMatch;
    results.push({
      name,
      input_path: inputPath,
      expected_path: expectedPath,
      formatted_ok: format.ok,
      formatted_code: format.code,
      output_match: outputMatch,
      formatted_error: format.stderr.trim(),
      pass,
      actual_path: outputPath,
    });

    console.log(`[${pass ? "PASS" : "FAIL"}] ${name}`);
  }

  const failures = results.filter((result) => !result.pass);
  const report = {
    generated_at: new Date().toISOString(),
    fixtures: cfg.fixtures,
    cmd: cfg.cmd,
    out: cfg.out,
    total: results.length,
    failed: failures.length,
    results,
  };

  await Deno.writeTextFile(
    `${cfg.out}/report.json`,
    JSON.stringify(report, null, 2),
  );
  await Deno.writeTextFile(`${cfg.out}/summary.md`, writeSummary(results));

  if (failures.length > 0) {
    console.error(
      `formatter-golden-fixtures: ${failures.length}/${results.length} fixtures failed`,
    );
    Deno.exit(1);
  }

  console.log(`formatter-golden-fixtures: PASS (${results.length} fixtures)`);
}

await main();
