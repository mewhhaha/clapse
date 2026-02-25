#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";

async function walkMarkdownFiles(root) {
  const out = [];
  for await (const entry of Deno.readDir(root)) {
    const path = `${root}/${entry.name}`;
    if (entry.isDirectory) {
      out.push(...await walkMarkdownFiles(path));
      continue;
    }
    if (entry.isFile && path.endsWith(".md")) {
      out.push(path);
    }
  }
  return out;
}

function extractClapseFences(path, source) {
  const lines = source.split(/\r?\n/u);
  const out = [];
  let inFence = false;
  let fenceStart = 0;
  let buf = [];
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    if (!inFence) {
      const m = line.match(/^```\s*([A-Za-z0-9_+-]+)(.*)$/u);
      if (!m) continue;
      const lang = (m[1] ?? "").toLowerCase();
      const rest = (m[2] ?? "").toLowerCase();
      if (lang === "clapse" && !rest.includes("skip")) {
        inFence = true;
        fenceStart = i + 1;
        buf = [];
      }
      continue;
    }
    if (line.trim() === "```") {
      out.push({
        path,
        startLine: fenceStart + 1,
        source: `${buf.join("\n")}\n`,
      });
      inFence = false;
      fenceStart = 0;
      buf = [];
      continue;
    }
    buf.push(line);
  }
  return out;
}

async function runCompile(snippet, idx, wasmPath) {
  const dir = await Deno.makeTempDir({ prefix: "clapse-docs-" });
  const inputPath = `${dir}/doc_${idx}.clapse`;
  const outputPath = `${dir}/doc_${idx}.wasm`;
  await Deno.writeTextFile(inputPath, snippet.source);
  const run = await new Deno.Command("deno", {
    args: [
      "run",
      "-A",
      "scripts/run-clapse-compiler-wasm.mjs",
      "compile",
      inputPath,
      outputPath,
    ],
    env: {
      ...Deno.env.toObject(),
      CLAPSE_COMPILER_WASM_PATH: wasmPath,
    },
    stdout: "piped",
    stderr: "piped",
  }).output();
  if (!run.success) {
    const stderr = new TextDecoder().decode(run.stderr).trim();
    const stdout = new TextDecoder().decode(run.stdout).trim();
    const reason = stderr || stdout || `exit ${run.code ?? 1}`;
    throw new Error(`${snippet.path}:${snippet.startLine}: ${reason}`);
  }
}

async function main() {
  const wasmPath = Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ??
    "artifacts/latest/clapse_compiler.wasm";
  const files = await walkMarkdownFiles("docs");
  const snippets = [];
  for (const file of files.sort()) {
    const source = await Deno.readTextFile(file);
    snippets.push(...extractClapseFences(file, source));
  }
  if (snippets.length === 0) {
    console.log("docs validation: no clapse code fences found");
    return;
  }
  for (let i = 0; i < snippets.length; i += 1) {
    await runCompile(snippets[i], i, wasmPath);
    console.log(`[PASS] ${snippets[i].path}:${snippets[i].startLine}`);
  }
  console.log(`docs validation: PASS (${snippets.length} snippets)`);
}

await main().catch(failWithError);
