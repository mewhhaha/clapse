#!/usr/bin/env -S deno run -A

const ALLOWLIST_PATHS = [
  "docs/clapse-language/references/self-hosting-roadmap.md",
  "scripts/guard-no-host-surface.mjs",
];

const SKIP_EXACT_PATHS = [".gitignore"];

const SKIP_PATH_PREFIXES = [".agents/", ".helix/"];

const DISALLOWED_PATH_PATTERNS = [
  { name: "*.hs/*.lhs", test: (path) => /\.(?:hs|lhs)$/u.test(path) },
  { name: "*.cabal", test: (path) => path.endsWith(".cabal") },
  {
    name: "cabal.project*",
    test: (path) => /(^|\/)cabal\.project(?:$|\.)/u.test(path),
  },
];

const DISALLOWED_CONTENT_PATTERNS = [
  { name: "\\bcabal\\b", test: (line) => /\bcabal\b/u.test(line) },
  {
    name: "cabal run clapse",
    test: (line) => /cabal\s+run\s+clapse\b/u.test(line),
  },
];

async function runGit(args) {
  const proc = new Deno.Command("git", { args, stdout: "piped", stderr: "piped" });
  const out = await proc.output();
  if (!out.success) {
    const message = new TextDecoder().decode(out.stderr).trim();
    throw new Error(`git ${args.join(" ")} failed: ${message || "command failed"}`);
  }
  return new TextDecoder().decode(out.stdout);
}

async function listTrackedFiles() {
  const raw = await runGit(["ls-files"]);
  return raw
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0);
}

async function scanFile(path, violations) {
  if (
    ALLOWLIST_PATHS.includes(path) ||
    SKIP_EXACT_PATHS.includes(path) ||
    SKIP_PATH_PREFIXES.some((prefix) => path.startsWith(prefix))
  ) {
    return;
  }
  for (const pattern of DISALLOWED_PATH_PATTERNS) {
    if (pattern.test(path)) {
      violations.push(`path ${path} matches ${pattern.name}`);
      return;
    }
  }

  let source;
  try {
    source = await Deno.readTextFile(path);
  } catch {
    return;
  }

  const lines = source.split(/\r?\n/u);
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    for (const pattern of DISALLOWED_CONTENT_PATTERNS) {
      if (pattern.test(line)) {
        violations.push(`${path}:${i + 1} contains ${pattern.name}`);
      }
    }
  }
}

async function main() {
  const files = await listTrackedFiles();
  const violations = [];
  for (const path of files) {
    await scanFile(path, violations);
  }

  if (violations.length > 0) {
    console.error("guard-no-host-surface: FAIL");
    for (const v of violations) console.error(`  - ${v}`);
    Deno.exit(1);
  }

  console.log("guard-no-host-surface: PASS");
}

await main();
