#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError, readBinaryFile } from "./runtime-env.mjs";
import { buildExplorerSourceAnnotations } from "./lsp-wasm.mjs";
import { runWithArgs } from "./run-clap-compiler-wasm.mjs";
import {
  encodeInt,
  instantiateWithRuntime,
  renderResult,
} from "./wasm-runtime.mjs";

const EXPLORER_HTML_PATH = new URL("../explorer.html", import.meta.url);
const DEFAULT_PORT = 36627;
const DEFAULT_HOST = "127.0.0.1";
let wabtPromise = null;

function getExplorerWasmPath() {
  return Deno.env.get("CLAP_COMPILER_WASM_PATH")?.trim() ||
    `${Deno.cwd()}/artifacts/latest/clap_compiler.wasm`;
}

function usage() {
  return [
    "Clap explorer server",
    "",
    "Usage:",
    "  deno run -A scripts/explorer.mjs [--port 36627] [--host 127.0.0.1]",
    "",
    "Serves explorer.html and exposes /api/inspect for compile-debug + emit-wat.",
  ].join("\n");
}

function parseOptions(args) {
  let port = DEFAULT_PORT;
  let host = DEFAULT_HOST;
  for (let index = 0; index < args.length; index += 1) {
    const token = String(args[index] ?? "").trim();
    if (token === "--help" || token === "-h") {
      console.log(usage());
      Deno.exit(0);
    }
    if (token === "--port") {
      port = Number(args[index + 1]);
      index += 1;
      continue;
    }
    if (token === "--host") {
      host = String(args[index + 1] ?? "").trim() || DEFAULT_HOST;
      index += 1;
      continue;
    }
    if (/^\d+$/u.test(token)) {
      port = Number(token);
      continue;
    }
    throw new Error(`unknown argument '${token}'`);
  }
  if (!Number.isInteger(port) || port < 1 || port > 65535) {
    throw new Error(`invalid port '${port}'`);
  }
  return { port, host };
}

async function runSampleMain(wasmPath, sampleArg = null) {
  const wasmBytes = await readBinaryFile(wasmPath);
  const { instance, runtime } = await instantiateWithRuntime(wasmBytes);
  const main = instance.exports.main;
  if (typeof main !== "function") {
    throw new Error("compiled wasm does not export main");
  }
  const result = sampleArg === null ? main() : main(encodeInt(sampleArg));
  return renderResult(result, runtime.state);
}

async function inspectSource(sourceText, sampleArg = null, skipSampleRun = false) {
  const tmpDir = await Deno.makeTempDir({
    dir: "/tmp",
    prefix: "clap-explorer-",
  });
  const inputPath = `${tmpDir}/input.clap`;
  const wasmPath = `${tmpDir}/module.wasm`;
  const artifactsDir = `${tmpDir}/artifacts`;
  const payload = {
    collapsedIr: "",
    wat: "",
    compileError: "",
    watError: "",
    sampleResult: "",
    sampleError: "",
    hoverables: [],
    inlayHints: [],
  };
  try {
    await Deno.writeTextFile(inputPath, sourceText);
    try {
      const annotations = await buildExplorerSourceAnnotations(
        getExplorerWasmPath(),
        sourceText,
        `file://${inputPath}`,
      );
      payload.hoverables = Array.isArray(annotations?.hoverables)
        ? annotations.hoverables
        : [];
      payload.inlayHints = Array.isArray(annotations?.inlayHints)
        ? annotations.inlayHints
        : [];
    } catch (err) {
      console.warn(
        `explorer: source annotations failed: ${
          err instanceof Error ? err.message : String(err)
        }`,
      );
    }
    try {
      await runWithArgs([
        "compile-debug",
        inputPath,
        wasmPath,
        artifactsDir,
      ]);
      payload.collapsedIr = await Deno.readTextFile(
        `${artifactsDir}/collapsed_ir.txt`,
      );
    } catch (err) {
      payload.compileError = err instanceof Error ? err.message : String(err);
    }
    try {
      if (payload.compileError.length > 0) {
        throw new Error(payload.compileError);
      }
      const wasmBytes = await Deno.readFile(wasmPath);
      payload.wat = await wasmToWat(wasmBytes);
    } catch (err) {
      payload.watError = err instanceof Error ? err.message : String(err);
    }
    if (!skipSampleRun) {
      try {
        if (payload.compileError.length > 0) {
          throw new Error(payload.compileError);
        }
        payload.sampleResult = await runSampleMain(wasmPath, sampleArg);
      } catch (err) {
        payload.sampleError = err instanceof Error ? err.message : String(err);
      }
    }
    return payload;
  } finally {
    await Deno.remove(tmpDir, { recursive: true }).catch(() => {});
  }
}

function jsonResponse(body, status = 200) {
  return new Response(JSON.stringify(body), {
    status,
    headers: {
      "content-type": "application/json; charset=utf-8",
      "cache-control": "no-store",
    },
  });
}

async function loadWabt() {
  if (wabtPromise === null) {
    wabtPromise = import("npm:wabt").then(async (mod) => await mod.default());
  }
  return await wabtPromise;
}

async function wasmToWat(wasmBytes) {
  const wabt = await loadWabt();
  const module = wabt.readWasm(wasmBytes, { readDebugNames: true });
  try {
    module.generateNames();
    module.applyNames();
    return module.toText({
      foldExprs: false,
      inlineExport: false,
    });
  } finally {
    module.destroy();
  }
}

async function handleRequest(request) {
  const url = new URL(request.url);
  if (request.method === "GET" && (url.pathname === "/" || url.pathname === "/explorer.html")) {
    const html = await Deno.readTextFile(EXPLORER_HTML_PATH);
    return new Response(html, {
      headers: {
        "content-type": "text/html; charset=utf-8",
        "cache-control": "no-store",
      },
    });
  }
  if (request.method === "POST" && url.pathname === "/api/inspect") {
    let payload;
    try {
      payload = await request.json();
    } catch {
      return jsonResponse({ error: "invalid JSON body" }, 400);
    }
    const source = typeof payload?.source === "string" ? payload.source : "";
    const sampleArg = payload?.sampleArg;
    const skipSampleRun = payload?.skipSampleRun === true;
    if (source.trim().length === 0) {
      return jsonResponse({ error: "source must be a non-empty string" }, 400);
    }
    if (
      sampleArg !== null &&
      sampleArg !== undefined &&
      (!Number.isInteger(sampleArg) || !Number.isFinite(sampleArg))
    ) {
      return jsonResponse({ error: "sampleArg must be an integer when provided" }, 400);
    }
    const result = await inspectSource(
      source,
      Number.isInteger(sampleArg) ? sampleArg : null,
      skipSampleRun,
    );
    return jsonResponse(result);
  }
  return new Response("not found", { status: 404 });
}

async function main() {
  const { port, host } = parseOptions(cliArgs());
  console.log(`explorer: http://${host}:${port}/`);
  await Deno.serve({ port, hostname: host }, handleRequest).finished;
}

if (import.meta.main) {
  await main().catch(failWithError);
}
