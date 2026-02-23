#!/usr/bin/env -S deno run -A

function parseCorpus(raw) {
  return raw
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.length > 0 && !line.startsWith("#"));
}

function setDiff(a, b) {
  const out = [];
  for (const x of a) {
    if (!b.has(x)) out.push(x);
  }
  return out.sort();
}

async function listExpectedEntryExamples() {
  const skip = new Set([
    // Intentional non-compiling trait-catalog fixture (contains placeholder symbol references).
    "examples/traits.clapse",
  ]);
  const out = [];
  for await (const e of Deno.readDir("examples")) {
    if (e.isFile && e.name.endsWith(".clapse")) {
      const path = `examples/${e.name}`;
      if (!skip.has(path)) out.push(path);
    }
  }
  return out.sort();
}

async function main() {
  const expected = await listExpectedEntryExamples();
  const expectedSet = new Set(expected);

  const corpus = parseCorpus(await Deno.readTextFile("examples/selfhost_corpus.txt"));
  const corpusSet = new Set(corpus);
  const missingInCorpus = setDiff(expected, corpusSet);
  const extraInCorpus = setDiff(corpus, expectedSet);

  const behaviorDoc = JSON.parse(await Deno.readTextFile("examples/selfhost_behavior_corpus.json"));
  const scenarios = Array.isArray(behaviorDoc.scenarios) ? behaviorDoc.scenarios : [];
  const behaviorEntries = scenarios.map((s) => s.entry).filter((x) => typeof x === "string");
  const behaviorBadEntries = behaviorEntries.filter((entry) => !expectedSet.has(entry)).sort();
  const badExpectKinds = scenarios
    .filter((s) => s.expect !== undefined && s.expect !== "value" && s.expect !== "trap")
    .map((s) => `${s.entry} (${String(s.expect)})`)
    .sort();

  const behaviorWithoutMain = [];
  for (const entry of new Set(behaviorEntries)) {
    const src = await Deno.readTextFile(entry);
    if (!/^main\b/m.test(src)) {
      behaviorWithoutMain.push(entry);
    }
  }
  behaviorWithoutMain.sort();

  if (
    missingInCorpus.length > 0 ||
    extraInCorpus.length > 0 ||
    behaviorBadEntries.length > 0 ||
    behaviorWithoutMain.length > 0 ||
    badExpectKinds.length > 0
  ) {
    console.error("selfhost-manifest check: FAIL");
    if (missingInCorpus.length > 0) {
      console.error("missing in examples/selfhost_corpus.txt:");
      for (const x of missingInCorpus) console.error(`  - ${x}`);
    }
    if (extraInCorpus.length > 0) {
      console.error("extra in examples/selfhost_corpus.txt:");
      for (const x of extraInCorpus) console.error(`  - ${x}`);
    }
    if (behaviorBadEntries.length > 0) {
      console.error("behavior scenarios reference unknown/non-entry examples:");
      for (const x of behaviorBadEntries) console.error(`  - ${x}`);
    }
    if (behaviorWithoutMain.length > 0) {
      console.error("behavior scenarios reference entries without top-level main:");
      for (const x of behaviorWithoutMain) console.error(`  - ${x}`);
    }
    if (badExpectKinds.length > 0) {
      console.error("behavior scenarios have invalid expect kind (allowed: value, trap):");
      for (const x of badExpectKinds) console.error(`  - ${x}`);
    }
    Deno.exit(1);
  }

  console.log(
    `selfhost-manifest check: PASS (entries=${expected.length}, behavior_scenarios=${scenarios.length})`,
  );
}

await main();
