#!/usr/bin/env -S deno run -A

const MANIFEST_PATH = "docs/clapse-language/references/pass-manifest.json";
const SPEC_PATH = "docs/clapse-language/references/optimization-and-collapse-ir.md";

const VALID_STATUS = new Set([
  "implemented",
  "partially implemented",
  "not implemented",
]);

function fail(msg, violations) {
  console.error(`${msg}: FAIL`);
  for (const v of violations) console.error(`  - ${v}`);
  Deno.exit(1);
}

function parseSpecEntries(source) {
  const rx =
    /- \[pass:([a-z0-9_]+)\]\s+status:\s+(implemented|partially implemented|not implemented)\b/gu;
  const entries = [];
  let m = rx.exec(source);
  while (m !== null) {
    entries.push({ id: m[1], status: m[2] });
    m = rx.exec(source);
  }
  return entries;
}

const manifestRaw = await Deno.readTextFile(MANIFEST_PATH);
const manifest = JSON.parse(manifestRaw);
const specRaw = await Deno.readTextFile(SPEC_PATH);
const specEntries = parseSpecEntries(specRaw);

const violations = [];

if (!Array.isArray(manifest.passes)) {
  violations.push(`manifest missing passes array: ${MANIFEST_PATH}`);
}

if (specEntries.length === 0) {
  violations.push(`no [pass:*] status entries found in ${SPEC_PATH}`);
}

const manifestMap = new Map();
for (const pass of manifest.passes ?? []) {
  if (typeof pass?.id !== "string" || pass.id.length === 0) {
    violations.push(`manifest pass with invalid id: ${JSON.stringify(pass)}`);
    continue;
  }
  if (!VALID_STATUS.has(pass.status)) {
    violations.push(`manifest pass ${pass.id} has invalid status ${JSON.stringify(pass.status)}`);
  }
  if (manifestMap.has(pass.id)) {
    violations.push(`duplicate manifest pass id: ${pass.id}`);
  } else {
    manifestMap.set(pass.id, pass.status);
  }
}

const seenSpec = new Set();
for (const entry of specEntries) {
  if (seenSpec.has(entry.id)) {
    violations.push(`duplicate pass tag in spec: ${entry.id}`);
    continue;
  }
  seenSpec.add(entry.id);
  if (!manifestMap.has(entry.id)) {
    violations.push(`spec references unknown pass id: ${entry.id}`);
    continue;
  }
  const expected = manifestMap.get(entry.id);
  if (expected !== entry.status) {
    violations.push(
      `status mismatch for ${entry.id}: spec=${JSON.stringify(entry.status)} manifest=${JSON.stringify(expected)}`,
    );
  }
}

for (const id of manifestMap.keys()) {
  if (!seenSpec.has(id)) {
    violations.push(`manifest pass not listed in spec status inventory: ${id}`);
  }
}

if (violations.length > 0) {
  fail("pass-manifest-check", violations);
}

console.log(`pass-manifest-check: PASS (${specEntries.length} entries)`);
