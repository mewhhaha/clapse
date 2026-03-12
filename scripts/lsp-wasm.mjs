#!/usr/bin/env -S deno run -A

import { failWithError } from "./runtime-env.mjs";
import {
  callCompilerWasm,
  decodeWasmBase64,
  validateCompilerWasmAbi,
} from "./wasm-compiler-abi.mjs";

const encoder = new TextEncoder();
const decoder = new TextDecoder();
const REPO_ROOT_URL = new URL("../", import.meta.url);

function toPath(url) {
  return decodeURIComponent(url.pathname);
}

const PROJECT_CONFIG_FILE = "clapse.json";
const projectConfigCache = new Map();
const projectPluginWasmCache = new Map();

function uriToPath(uri) {
  if (typeof uri !== "string" || uri.length === 0) {
    return "";
  }
  if (!uri.startsWith("file:")) {
    return uri;
  }
  try {
    return decodeURIComponent(new URL(uri).pathname);
  } catch {
    return uri.replace(/^file:\/\//, "");
  }
}

function pathDir(path) {
  if (typeof path !== "string" || path.length === 0) {
    return "";
  }
  const normalized = path.replace(/\\/g, "/");
  const idx = normalized.lastIndexOf("/");
  if (idx <= 0) {
    return "/";
  }
  return normalized.slice(0, idx);
}

function normalizeIncludeValue(value) {
  if (!Array.isArray(value)) {
    return [];
  }
  return value
    .map((item) => String(item).trim())
  .filter((name) => name.length > 0);
}

function normalizePluginDirs(value, configDir) {
  const rawPluginDirs = normalizeIncludeValue(value);
  const pluginDirs = [];
  const seen = new Set();
  for (const rawDir of rawPluginDirs) {
    const resolved = resolveModuleDir(rawDir, configDir);
    if (resolved.length === 0 || seen.has(resolved)) {
      continue;
    }
    seen.add(resolved);
    pluginDirs.push(resolved);
  }
  return pluginDirs;
}

function normalizePath(path) {
  const normalized = String(path).replace(/\\/g, "/");
  if (normalized.length === 0) {
    return "";
  }
  const hasLeadingSlash = normalized.startsWith("/");
  const parts = normalized.split("/");
  const stack = [];
  for (const part of parts) {
    if (part.length === 0 || part === ".") {
      continue;
    }
    if (part === "..") {
      if (stack.length > 0) {
        stack.pop();
      }
      continue;
    }
    stack.push(part);
  }
  if (hasLeadingSlash) {
    return `/${stack.join("/")}`;
  }
  return stack.join("/");
}

function resolveModuleDir(rawDir, configPathDir) {
  const normalized = String(rawDir).replace(/\\/g, "/").trim();
  if (normalized.length === 0) {
    return "";
  }
  if (normalized.startsWith("/")) {
    return normalizePath(normalized);
  }
  const base = normalizePath(configPathDir || "");
  if (base.length === 0 || base === "/") {
    return normalizePath(normalized);
  }
  return normalizePath(`${base}/${normalized}`);
}

function candidateModulePath(moduleName, dir) {
  const relativePath = `${moduleName.replace(/[.$]/g, "/")}.clapse`;
  if (typeof dir !== "string" || dir.length === 0) {
    return relativePath;
  }
  return `${dir}/${relativePath}`;
}

function parseProjectConfigText(raw, sourcePath) {
  if (typeof raw !== "string" || raw.length === 0) {
    return {
      moduleSearchDirs: new Set(),
      pluginDirs: new Set(),
      moduleResolutionCache: new Map(),
      sourcePath,
      raw: null,
    };
  }

  let data;
  try {
    data = JSON.parse(raw);
  } catch {
    return {
      moduleSearchDirs: new Set(),
      pluginDirs: new Set(),
      moduleResolutionCache: new Map(),
      sourcePath,
      raw: null,
    };
  }

  if (data === null || typeof data !== "object") {
    return {
      moduleSearchDirs: new Set(),
      pluginDirs: new Set(),
      moduleResolutionCache: new Map(),
      sourcePath,
      raw: data,
    };
  }

  const configDir = pathDir(sourcePath);
  const moduleSearchDirs = normalizeIncludeValue(data.include)
    .map((path) => resolveModuleDir(path, configDir))
    .filter((path) => path.length > 0);
  const pluginDirs = normalizePluginDirs(data.plugins, configDir);

  return {
    moduleSearchDirs: new Set(moduleSearchDirs),
    pluginDirs: new Set(pluginDirs),
    moduleResolutionCache: new Map(),
    sourcePath,
    raw: data,
  };
}

async function collectClapseFilesRecursively(rootDir, out, seen = new Set()) {
  const normalized = normalizePath(rootDir);
  if (normalized.length === 0 || seen.has(normalized)) {
    return;
  }
  seen.add(normalized);
  let entries = [];
  try {
    for await (const entry of Deno.readDir(normalized)) {
      entries.push(entry);
    }
  } catch {
    return;
  }
  for (const entry of entries) {
    const child = `${normalized}/${entry.name}`;
    if (entry.isDirectory) {
      await collectClapseFilesRecursively(child, out, seen);
      continue;
    }
    if (entry.isFile && child.endsWith(".clapse")) {
      out.push(child);
    }
  }
}

async function compilePluginWasm(wasmPath, pluginSourcePath, outputPath, inputSource) {
  const response = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: pluginSourcePath,
    input_source: inputSource,
    plugin_wasm_paths: [],
  });
  if (
    !response || typeof response !== "object" || response.ok !== true
  ) {
    const err = typeof response?.error === "string"
      ? response.error
      : `plugin compile failed for ${pluginSourcePath}`;
    throw new Error(err);
  }
  if (typeof response.wasm_base64 !== "string" || response.wasm_base64.length === 0) {
    throw new Error(`plugin compile produced empty wasm for ${pluginSourcePath}`);
  }
  const outDir = outputPath.includes("/")
    ? outputPath.slice(0, outputPath.lastIndexOf("/"))
    : "";
  if (outDir.length > 0) {
    await Deno.mkdir(outDir, { recursive: true });
  }
  await Deno.writeFile(outputPath, decodeWasmBase64(response.wasm_base64));
}

async function compileProjectPlugins(wasmPath, config) {
  const pluginDirs = (config?.pluginDirs instanceof Set)
    ? Array.from(config.pluginDirs)
    : [];
  if (pluginDirs.length === 0) {
    return [];
  }
  const pluginSources = [];
  const seenDirs = new Set();
  const sortedDirs = [...new Set(pluginDirs)].sort((a, b) => a.localeCompare(b, "en"));
  for (const pluginDir of sortedDirs) {
    await collectClapseFilesRecursively(pluginDir, pluginSources, seenDirs);
  }

  const uniquePluginSources = [...new Set(pluginSources)].sort((a, b) => a.localeCompare(b, "en"));
  const pluginWasmPaths = [];
  for (const pluginSource of uniquePluginSources) {
    const outputPath = pluginSource.endsWith(".clapse")
      ? pluginSource.replace(/\.clapse$/u, ".wasm")
      : `${pluginSource}.wasm`;
    const pluginSourceText = await Deno.readTextFile(pluginSource);
    await compilePluginWasm(wasmPath, pluginSource, outputPath, pluginSourceText);
    pluginWasmPaths.push(outputPath);
  }
  return pluginWasmPaths;
}

async function resolveProjectPluginWasmPaths(config, wasmPath) {
  const pluginDirs = (config?.pluginDirs instanceof Set)
    ? Array.from(config.pluginDirs)
    : [];
  if (pluginDirs.length === 0) {
    return [];
  }
  const cacheKey = String(config?.sourcePath ?? "");
  if (projectPluginWasmCache.has(cacheKey)) {
    return projectPluginWasmCache.get(cacheKey);
  }
  const inFlight = (async () => {
    return compileProjectPlugins(wasmPath, config);
  })();
  projectPluginWasmCache.set(cacheKey, inFlight);
  try {
    return await inFlight;
  } catch (err) {
    projectPluginWasmCache.delete(cacheKey);
    throw err;
  }
}

async function resolveProjectConfig(uri, rootHint) {
  const sourcePath = uriToPath(uri);
  const startDir = sourcePath.length > 0 ? pathDir(sourcePath) : pathDir(rootHint ?? "");

  if (startDir.length === 0) {
    return {
      moduleSearchDirs: new Set(),
      moduleResolutionCache: new Map(),
      sourcePath,
    };
  }

  const tryDirs = [];
  let dir = startDir;
  while (dir.length > 0) {
    tryDirs.push(dir);
    const parent = pathDir(dir);
    if (parent === dir) {
      break;
    }
    dir = parent;
  }

  for (const candidateDir of tryDirs) {
    const configPath = `${candidateDir}/${PROJECT_CONFIG_FILE}`;
    if (projectConfigCache.has(configPath)) {
      return projectConfigCache.get(configPath);
    }
    try {
      const raw = await Deno.readTextFile(configPath);
      const parsed = parseProjectConfigText(raw, configPath);
      projectConfigCache.set(configPath, parsed);
      return parsed;
    } catch {
      // no config in this directory; keep searching
    }
  }

  if (typeof rootHint === "string" && rootHint.length > 0 && !rootHint.startsWith(startDir)) {
    const rootDir = pathDir(uriToPath(rootHint));
    if (rootDir.length > 0 && rootDir !== startDir) {
      const fallback = `${rootDir}/${PROJECT_CONFIG_FILE}`;
      if (projectConfigCache.has(fallback)) {
        return projectConfigCache.get(fallback);
      }
      try {
        const raw = await Deno.readTextFile(fallback);
        const parsed = parseProjectConfigText(raw, fallback);
        projectConfigCache.set(fallback, parsed);
        return parsed;
      } catch {
        // no fallback config
      }
    }
  }

  return {
    moduleSearchDirs: new Set(),
    moduleResolutionCache: new Map(),
    sourcePath,
  };
}

function importFromLine(line) {
  const match = String(line).match(/^\s*import\s+([A-Za-z_][A-Za-z0-9_$.']*)/u);
  if (!match) {
    return null;
  }
  return match[1];
}

async function isModuleAllowed(moduleName, config) {
  if (typeof moduleName !== "string" || moduleName.length === 0) {
    return true;
  }
  if (moduleName.startsWith("host.")) {
    return true;
  }
  const searchDirs = config?.moduleSearchDirs;
  if (!(searchDirs instanceof Set) || searchDirs.size === 0) {
    return true;
  }

  const cache = config?.moduleResolutionCache;
  if (cache instanceof Map && cache.has(moduleName)) {
    return cache.get(moduleName) === true;
  }

  for (const dir of searchDirs) {
    const candidate = candidateModulePath(moduleName, dir);
    try {
      const stat = await Deno.stat(candidate);
      if (stat.isFile) {
        if (cache instanceof Map) cache.set(moduleName, true);
        return true;
      }
    } catch {
      // keep searching
    }
  }
  if (cache instanceof Map) cache.set(moduleName, false);
  return false;
}

async function scopeDiagnosticsForSource(source, config) {
  const diagnostics = [];
  const lines = String(source).split("\n");
  for (let i = 0; i < lines.length; i += 1) {
    const moduleName = importFromLine(lines[i]);
    if (moduleName === null) {
      continue;
    }
    const allowed = await isModuleAllowed(moduleName, config);
    if (!allowed) {
      diagnostics.push({
        range: {
          start: { line: i, character: 0 },
          end: { line: i, character: lines[i].length },
        },
        severity: 1,
        source: "clapse",
        message: `module '${moduleName}' was not found in clapse.json include`,
      });
    }
  }
  return diagnostics;
}

function isSuppressedCompileDiagnosticMessage(message) {
  const normalized = String(message);
  return (
    normalized.includes("native compile not implemented yet") ||
    normalized.includes("compile [backend=kernel-native] failed") ||
    normalized.includes("compile response: missing non-empty string 'backend'") ||
    normalized.includes("unknown entrypoint root: main")
  );
}

function getWasmPath() {
  const candidates = [
    Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "",
    toPath(new URL("artifacts/latest/clapse_compiler.wasm", REPO_ROOT_URL)),
    toPath(new URL("out/clapse_compiler.wasm", REPO_ROOT_URL)),
  ];
  for (const wasmPath of candidates) {
    if (wasmPath.length === 0) continue;
    try {
      Deno.statSync(wasmPath);
      return wasmPath;
    } catch {
      // keep searching
    }
  }
  throw new Error(
    "wasm LSP mode requires CLAPSE_COMPILER_WASM_PATH or artifacts/latest|out clapse_compiler.wasm",
  );
}

function parseLineError(message) {
  const m = message.match(/line\s+(\d+):\s*(.*)$/i);
  if (!m) return null;
  const line = Math.max(0, Number(m[1]) - 1);
  const msg = m[2] || message;
  return { line, msg };
}

function fullRangeForText(text) {
  const lines = text.split("\n");
  const endLine = Math.max(0, lines.length - 1);
  const endCharacter = lines.length === 0 ? 0 : lines[endLine].length;
  return {
    start: { line: 0, character: 0 },
    end: { line: endLine, character: endCharacter },
  };
}

function stripDocPrefix(line) {
  const trimmed = line.trim();
  if (trimmed.startsWith("--|")) return trimmed.slice(3).trimStart();
  if (trimmed.startsWith("///")) return trimmed.slice(3).trimStart();
  return null;
}

const IDENT_RE = /[A-Za-z_][A-Za-z0-9_$.']*/gu;

function isKeywordToken(token) {
  const keyword = token.toLowerCase();
  return (
    keyword === "module" || keyword === "import" || keyword === "type" ||
    keyword === "data" || keyword === "class" || keyword === "instance" ||
    keyword === "law" || keyword === "infix" || keyword === "infixl" ||
    keyword === "infixr" || keyword === "where" || keyword === "let" ||
    keyword === "in" || keyword === "of" || keyword === "case" ||
    keyword === "if" || keyword === "then" || keyword === "else"
  );
}

function classifyFunctionDeclLine(rawLine) {
  const line = String(rawLine ?? "").trim();
  if (line.length === 0) return null;
  if (line.startsWith("--")) return null;
  if (line.startsWith("#[")) return null;
  if (
    line.startsWith("module ") ||
    line.startsWith("import ") ||
    line.startsWith("type ") ||
    line.startsWith("data ") ||
    line.startsWith("class ") ||
    line.startsWith("instance ") ||
    line.startsWith("law ") ||
    line.startsWith("infix ") ||
    line.startsWith("infixl ") ||
    line.startsWith("infixr ")
  ) {
    return null;
  }
  const eqAt = line.indexOf("=");
  const colonAt = line.indexOf(":");
  const sepAt = eqAt > 0 && colonAt > 0
    ? Math.min(eqAt, colonAt)
    : (eqAt > 0 ? eqAt : colonAt);
  if (sepAt <= 0) return null;
  const lhs = line.slice(0, sepAt).trim();
  if (lhs.length === 0) return null;
  const toks = lhs.split(/\s+/u).filter((x) => x.length > 0);
  if (toks.length === 0) return null;
  const name = toks[0];
  if (!/^[A-Za-z_][A-Za-z0-9_$.']*$/u.test(name)) return null;
  const kind = colonAt > 0 && (eqAt <= 0 || colonAt < eqAt)
    ? "signature"
    : "definition";
  return { name, kind };
}

export function buildFunctionDocIndex(text) {
  const sourceText = String(text);
  const lines = sourceText.split("\n");
  const out = new Map();
  const occurrences = new Map();
  let pending = [];
  for (let i = 0; i < lines.length; i += 1) {
    const raw = lines[i];
    const trimmed = raw.trim();
    if (trimmed.length === 0) {
      if (pending.length > 0) pending.push("");
      continue;
    }
    IDENT_RE.lastIndex = 0;
    let match;
    while ((match = IDENT_RE.exec(raw)) !== null) {
      const token = match[0];
      if (isKeywordToken(token)) {
        continue;
      }
      const entry = occurrences.get(token);
      const item = { line: i, start: match.index, end: match.index + token.length };
      if (entry === undefined) {
        occurrences.set(token, [item]);
      } else {
        entry.push(item);
      }
    }

    const docLine = stripDocPrefix(raw);
    if (docLine !== null) {
      pending.push(docLine);
      continue;
    }
    if (trimmed.startsWith("--") || trimmed.startsWith("#[")) {
      continue;
    }
    const decl = classifyFunctionDeclLine(raw);
    if (leadingIndentCount(raw) > 0 && decl?.kind === "definition") {
      pending = [];
      continue;
    }
    if (decl !== null) {
      const start = raw.indexOf(decl.name);
      const existing = out.get(decl.name) ?? null;
      const next = existing ?? {
        doc: "",
        line: i,
        start: Math.max(0, start),
        end: Math.max(0, start) + decl.name.length,
        hasSignature: false,
        signatureLine: null,
        definitionLine: null,
      };
      if (pending.length > 0 && next.doc.length === 0) {
        next.doc = pending.join("\n").trim();
      }
      if (decl.kind === "signature") {
        next.hasSignature = true;
        next.signatureLine = i;
        next.line = i;
        next.start = Math.max(0, start);
        next.end = Math.max(0, start) + decl.name.length;
      } else if (next.definitionLine === null) {
        next.definitionLine = i;
        if (!next.hasSignature) {
          next.line = i;
          next.start = Math.max(0, start);
          next.end = Math.max(0, start) + decl.name.length;
        }
      }
      out.set(decl.name, next);
      pending = [];
      continue;
    }
    pending = [];
  }
  return {
    declarations: out,
    occurrences: occurrences,
    sourceText,
  };
}

function declarationRangeFromSignature(sourceText, symbol, signature) {
  const signatureLine = String(signature);
  if (signatureLine.length === 0) {
    return null;
  }
  const lines = sourceText.split("\n");
  for (let line = 0; line < lines.length; line += 1) {
    if (lines[line].trim() !== signatureLine.trim()) {
      continue;
    }
    const start = lines[line].indexOf(symbol);
    if (start < 0) {
      continue;
    }
    return {
      uri: null,
      range: {
        start: { line, character: start },
        end: { line, character: start + symbol.length },
      },
    };
  }
  return null;
}

function leadingIndentCount(lineText) {
  const match = String(lineText ?? "").match(/^\s*/u);
  return match ? match[0].length : 0;
}

function topLevelTypeSuffix(signatureLine) {
  const text = String(signatureLine ?? "");
  const colon = text.indexOf(":");
  if (colon < 0) {
    return "";
  }
  let depthParen = 0;
  let depthBracket = 0;
  let depthBrace = 0;
  for (let i = colon + 1; i + 1 < text.length; i += 1) {
    const ch = text[i];
    if (ch === "(") depthParen += 1;
    else if (ch === ")") depthParen = Math.max(0, depthParen - 1);
    else if (ch === "[") depthBracket += 1;
    else if (ch === "]") depthBracket = Math.max(0, depthBracket - 1);
    else if (ch === "{") depthBrace += 1;
    else if (ch === "}") depthBrace = Math.max(0, depthBrace - 1);
    else if (
      ch === "=" &&
      text[i + 1] === ">" &&
      depthParen === 0 &&
      depthBracket === 0 &&
      depthBrace === 0
    ) {
      return text.slice(i + 2).trim();
    }
  }
  return text.slice(colon + 1).trim();
}

function splitTopLevelFunctionType(typeText) {
  const text = String(typeText ?? "").trim();
  if (text.length === 0) {
    return [];
  }
  const parts = [];
  let depthParen = 0;
  let depthBracket = 0;
  let depthBrace = 0;
  let start = 0;
  for (let i = 0; i + 1 < text.length; i += 1) {
    const ch = text[i];
    if (ch === "(") depthParen += 1;
    else if (ch === ")") depthParen = Math.max(0, depthParen - 1);
    else if (ch === "[") depthBracket += 1;
    else if (ch === "]") depthBracket = Math.max(0, depthBracket - 1);
    else if (ch === "{") depthBrace += 1;
    else if (ch === "}") depthBrace = Math.max(0, depthBrace - 1);
    if (
      ch === "-" &&
      text[i + 1] === ">" &&
      depthParen === 0 &&
      depthBracket === 0 &&
      depthBrace === 0
    ) {
      parts.push(text.slice(start, i).trim());
      start = i + 2;
      i += 1;
    }
  }
  parts.push(text.slice(start).trim());
  return parts.filter((part) => part.length > 0);
}

function parseDefinitionParams(rawLine, symbol) {
  const text = String(rawLine ?? "");
  const eqAt = text.indexOf("=");
  const pipeAt = text.indexOf("|");
  const stop = eqAt > 0 && pipeAt > 0
    ? Math.min(eqAt, pipeAt)
    : (eqAt > 0 ? eqAt : pipeAt);
  if (stop <= 0) {
    return [];
  }
  const lhs = text.slice(0, stop).trim();
  const tokens = lhs.split(/\s+/u).filter((token) => token.length > 0);
  if (tokens.length === 0 || tokens[0] !== symbol) {
    return [];
  }
  return tokens.slice(1);
}

function declarationEntries(index) {
  const entries = Array.from(index?.declarations?.entries?.() ?? [])
    .map(([name, decl]) => {
      const signatureLine = Number.isFinite(Number(decl?.signatureLine))
        ? Number(decl.signatureLine)
        : null;
      const definitionLine = Number.isFinite(Number(decl?.definitionLine))
        ? Number(decl.definitionLine)
        : null;
      const startLine = signatureLine ?? definitionLine ?? Number(decl?.line ?? 0);
      return { name, decl, signatureLine, definitionLine, startLine };
    })
    .filter((entry) => Number.isFinite(entry.startLine))
    .sort((a, b) => a.startLine - b.startLine);
  return entries;
}

function findEnclosingFunctionContext(index, line) {
  const entries = declarationEntries(index);
  for (let i = 0; i < entries.length; i += 1) {
    const entry = entries[i];
    if (!Number.isFinite(entry.definitionLine)) {
      continue;
    }
    const nextStartLine = i + 1 < entries.length ? entries[i + 1].startLine : Infinity;
    if (line >= entry.definitionLine && line < nextStartLine) {
      return {
        ...entry,
        endLineExclusive: nextStartLine,
      };
    }
  }
  return null;
}

function splitTopLevelApplyTerms(exprText) {
  const text = String(exprText ?? "").trim();
  if (text.length === 0) {
    return [];
  }
  const terms = [];
  let depthParen = 0;
  let depthBracket = 0;
  let depthBrace = 0;
  let start = 0;
  let i = 0;
  while (i < text.length) {
    const ch = text[i];
    if (ch === "(") depthParen += 1;
    else if (ch === ")") depthParen = Math.max(0, depthParen - 1);
    else if (ch === "[") depthBracket += 1;
    else if (ch === "]") depthBracket = Math.max(0, depthBracket - 1);
    else if (ch === "{") depthBrace += 1;
    else if (ch === "}") depthBrace = Math.max(0, depthBrace - 1);
    else if (
      /\s/u.test(ch) &&
      depthParen === 0 &&
      depthBracket === 0 &&
      depthBrace === 0
    ) {
      const part = text.slice(start, i).trim();
      if (part.length > 0) {
        terms.push(part);
      }
      while (i < text.length && /\s/u.test(text[i])) {
        i += 1;
      }
      start = i;
      continue;
    }
    i += 1;
  }
  const finalPart = text.slice(start).trim();
  if (finalPart.length > 0) {
    terms.push(finalPart);
  }
  return terms;
}

function splitTopLevelAlternatives(text, separator = "|") {
  const input = String(text ?? "").trim();
  if (input.length === 0) {
    return [];
  }
  const parts = [];
  let depthParen = 0;
  let depthBracket = 0;
  let depthBrace = 0;
  let start = 0;
  for (let i = 0; i < input.length; i += 1) {
    const ch = input[i];
    if (ch === "(") depthParen += 1;
    else if (ch === ")") depthParen = Math.max(0, depthParen - 1);
    else if (ch === "[") depthBracket += 1;
    else if (ch === "]") depthBracket = Math.max(0, depthBracket - 1);
    else if (ch === "{") depthBrace += 1;
    else if (ch === "}") depthBrace = Math.max(0, depthBrace - 1);
    else if (
      ch === separator &&
      depthParen === 0 &&
      depthBracket === 0 &&
      depthBrace === 0
    ) {
      const part = input.slice(start, i).trim();
      if (part.length > 0) {
        parts.push(part);
      }
      start = i + 1;
    }
  }
  const finalPart = input.slice(start).trim();
  if (finalPart.length > 0) {
    parts.push(finalPart);
  }
  return parts;
}

function splitTopLevelDelimited(text, delimiter = ",") {
  return splitTopLevelAlternatives(text, delimiter);
}

function stripBalancedOuterParens(exprText) {
  let text = String(exprText ?? "").trim();
  while (text.startsWith("(") && text.endsWith(")")) {
    let depth = 0;
    let balanced = true;
    let closesAtEnd = false;
    for (let i = 0; i < text.length; i += 1) {
      const ch = text[i];
      if (ch === "(") depth += 1;
      else if (ch === ")") {
        depth -= 1;
        if (depth < 0) {
          balanced = false;
          break;
        }
        if (depth === 0) {
          closesAtEnd = i === text.length - 1;
          if (!closesAtEnd) {
            balanced = false;
            break;
          }
        }
      }
    }
    if (!balanced || depth !== 0 || !closesAtEnd) {
      break;
    }
    text = text.slice(1, -1).trim();
  }
  return text;
}

function parseRecordFieldEntries(text) {
  const input = stripBalancedOuterParens(String(text ?? "").trim());
  if (!input.startsWith("{") || !input.endsWith("}")) {
    return null;
  }
  const inner = input.slice(1, -1).trim();
  if (inner.length === 0) {
    return [];
  }
  const entries = [];
  for (const part of splitTopLevelDelimited(inner, ",")) {
    const match = part.match(/^([A-Za-z_][A-Za-z0-9_']*)\s*[:=]\s*(.+)$/u);
    if (!match) {
      return null;
    }
    entries.push({
      name: match[1],
      value: match[2].trim(),
    });
  }
  return entries;
}

function renderInlineRecordType(fields) {
  const entries = Array.from(fields.entries());
  if (entries.length === 0) {
    return "{ }";
  }
  return `{ ${entries.map(([name, type]) => `${name}: ${type}`).join(", ")} }`;
}

function buildTypeAliasIndex(index) {
  const cached = index?.typeAliases;
  if (cached instanceof Map) {
    return cached;
  }
  const aliases = new Map();
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  for (const rawLine of sourceLines) {
    if (leadingIndentCount(rawLine) > 0) {
      continue;
    }
    const trimmed = safeTextForLine(rawLine).trim();
    const match = trimmed.match(/^type\s+([A-Z][A-Za-z0-9_']*)(.*?)=\s*(.+)$/u);
    if (!match) {
      continue;
    }
    const [, name, paramsRaw, bodyRaw] = match;
    const fields = parseRecordFieldEntries(bodyRaw);
    if (fields === null) {
      continue;
    }
    aliases.set(name, {
      params: splitTopLevelApplyTerms(paramsRaw)
        .filter((part) => /^[a-z][A-Za-z0-9_']*$/u.test(part)),
      fields: new Map(fields.map((entry) => [entry.name, entry.value])),
    });
  }
  if (index && typeof index === "object") {
    index.typeAliases = aliases;
  }
  return aliases;
}

function resolveRecordFieldType(index, recordTypeText, fieldName) {
  const recordType = stripBalancedOuterParens(String(recordTypeText ?? "").trim());
  if (recordType.length === 0 || typeof fieldName !== "string" || fieldName.length === 0) {
    return null;
  }
  const inlineFields = parseRecordFieldEntries(recordType);
  if (Array.isArray(inlineFields)) {
    const field = inlineFields.find((entry) => entry.name === fieldName) ?? null;
    return field?.value ?? null;
  }
  const terms = splitTopLevelApplyTerms(recordType);
  if (terms.length === 0) {
    return null;
  }
  const alias = buildTypeAliasIndex(index).get(terms[0]);
  if (!alias) {
    return null;
  }
  const rawFieldType = alias.fields.get(fieldName);
  if (typeof rawFieldType !== "string" || rawFieldType.length === 0) {
    return null;
  }
  const bindings = new Map();
  for (let i = 0; i < alias.params.length; i += 1) {
    const actual = terms[i + 1];
    if (typeof actual === "string" && actual.length > 0) {
      bindings.set(alias.params[i], actual);
    }
  }
  const resolved = substituteSimpleTypeVars(rawFieldType, bindings);
  return resolved.length > 0 ? resolved : null;
}

function listRecordFieldsForType(index, recordTypeText) {
  const recordType = stripBalancedOuterParens(String(recordTypeText ?? "").trim());
  if (recordType.length === 0) {
    return [];
  }
  const inlineFields = parseRecordFieldEntries(recordType);
  if (Array.isArray(inlineFields)) {
    return inlineFields.map((field) => ({
      name: field.name,
      type: field.value,
    }));
  }
  const terms = splitTopLevelApplyTerms(recordType);
  if (terms.length === 0) {
    return [];
  }
  const alias = buildTypeAliasIndex(index).get(terms[0]);
  if (!alias) {
    return [];
  }
  const bindings = new Map();
  for (let i = 0; i < alias.params.length; i += 1) {
    const actual = terms[i + 1];
    if (typeof actual === "string" && actual.length > 0) {
      bindings.set(alias.params[i], actual);
    }
  }
  return Array.from(alias.fields.entries()).map(([name, rawType]) => ({
    name,
    type: substituteSimpleTypeVars(rawType, bindings),
  }));
}

function projectionChainAtPosition(lineText, character) {
  const text = String(lineText ?? "");
  if (text.length === 0) {
    return null;
  }
  const tokenRange = wordRangeAtPosition(text, character);
  if (tokenRange === null) {
    return null;
  }
  const token = text.slice(tokenRange.start, tokenRange.end);
  if (!token.includes(".")) {
    return null;
  }
  const parts = token.split(".").filter((part) => part.length > 0);
  if (parts.length < 2) {
    return null;
  }
  let segmentStart = tokenRange.start;
  let activeIndex = -1;
  for (let i = 0; i < parts.length; i += 1) {
    const partStart = segmentStart;
    const partEnd = partStart + parts[i].length;
    if (character >= partStart && character <= partEnd) {
      activeIndex = i;
      break;
    }
    segmentStart = partEnd + 1;
  }
  return {
    token,
    tokenRange,
    parts,
    activeIndex,
  };
}

function projectionBaseBeforeCursor(lineText, character) {
  const text = String(lineText ?? "");
  const pos = Math.max(0, Math.min(Number(character) || 0, text.length));
  const prefix = text.slice(0, pos);
  const match = prefix.match(/([A-Za-z_][A-Za-z0-9_$.']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)\.$/u);
  if (!match) {
    return null;
  }
  const token = match[1];
  const parts = token.split(".").filter((part) => part.length > 0);
  if (parts.length === 0) {
    return null;
  }
  return {
    token,
    parts,
    range: {
      start: prefix.length - token.length,
      end: prefix.length,
    },
  };
}

function buildDataConstructorIndex(index) {
  const cached = index?.dataConstructors;
  if (cached instanceof Map) {
    return cached;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const constructors = new Map();
  for (const rawLine of sourceLines) {
    if (leadingIndentCount(rawLine) > 0) {
      continue;
    }
    const trimmed = safeTextForLine(rawLine).trim();
    if (!trimmed.startsWith("data ")) {
      continue;
    }
    const match = trimmed.match(/^data\s+([A-Z][A-Za-z0-9_']*)(.*)$/u);
    if (!match) {
      continue;
    }
    const typeName = match[1];
    const rest = String(match[2] ?? "");
    const eqAt = rest.indexOf("=");
    if (eqAt < 0) {
      continue;
    }
    const paramText = rest.slice(0, eqAt).trim();
    const typeParams = splitTopLevelApplyTerms(paramText)
      .filter((part) => /^[a-z][A-Za-z0-9_']*$/u.test(part));
    const ctorText = rest.slice(eqAt + 1).trim();
    for (const alternative of splitTopLevelAlternatives(ctorText, "|")) {
      const terms = splitTopLevelApplyTerms(alternative);
      if (terms.length === 0) {
        continue;
      }
      const ctorName = terms[0];
      if (!/^[A-Z][A-Za-z0-9_']*$/u.test(ctorName)) {
        continue;
      }
      constructors.set(ctorName, {
        typeName,
        typeParams,
        fieldTypes: terms.slice(1),
      });
    }
  }
  if (index && typeof index === "object") {
    index.dataConstructors = constructors;
  }
  return constructors;
}

function sourceSignaturePartsForSymbol(index, symbol) {
  if (typeof symbol !== "string" || symbol.length === 0) {
    return null;
  }
  const declaration = index?.declarations?.get?.(symbol) ?? null;
  const signatureLine = typeof declaration?.signatureLine === "number"
    ? declaration.signatureLine
    : null;
  if (signatureLine === null) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const signatureText = safeTextForLine(sourceLines[signatureLine]);
  const parts = splitTopLevelFunctionType(topLevelTypeSuffix(signatureText));
  return parts.length > 0 ? parts : null;
}

function sourceSignatureTypeForSymbol(index, symbol) {
  const parts = sourceSignaturePartsForSymbol(index, symbol);
  if (!Array.isArray(parts) || parts.length === 0) {
    return null;
  }
  return parts.join(" -> ");
}

function sourceNullaryValueTypeForSymbol(index, symbol) {
  const parts = sourceSignaturePartsForSymbol(index, symbol);
  if (!Array.isArray(parts) || parts.length !== 1) {
    return null;
  }
  return parts[0];
}

function inferTopLevelNullaryDefinitionType(index, symbol, seen = new Set()) {
  if (typeof symbol !== "string" || symbol.length === 0 || seen.has(symbol)) {
    return null;
  }
  const declaration = index?.declarations?.get?.(symbol) ?? null;
  const definitionLine = Number.isFinite(Number(declaration?.definitionLine))
    ? Number(declaration.definitionLine)
    : null;
  if (definitionLine === null) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const rawLine = safeTextForLine(sourceLines[definitionLine]);
  if (parseDefinitionParams(rawLine, symbol).length !== 0) {
    return null;
  }
  const eqAt = rawLine.indexOf("=");
  if (eqAt < 0) {
    return null;
  }
  const rhs = rawLine.slice(eqAt + 1).trim();
  if (rhs.length === 0) {
    return null;
  }
  const nextSeen = new Set(seen);
  nextSeen.add(symbol);
  return inferSimpleExprType(rhs, new Map(), index, nextSeen);
}

function topLevelDefinitionBodyText(index, symbol) {
  const declaration = index?.declarations?.get?.(symbol) ?? null;
  const definitionLine = typeof declaration?.definitionLine === "number"
    ? declaration.definitionLine
    : null;
  if (definitionLine === null) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const rawLine = safeTextForLine(sourceLines[definitionLine]);
  const eqAt = rawLine.indexOf("=");
  if (eqAt < 0) {
    return null;
  }
  const rhs = rawLine.slice(eqAt + 1).trim();
  if (rhs.length > 0 && rhs !== "let") {
    return rhs;
  }
  const context = findEnclosingFunctionContext(index, definitionLine);
  if (context === null) {
    return rhs.length > 0 ? rhs : null;
  }
  for (
    let line = Math.min(Number(context.endLineExclusive ?? sourceLines.length) - 1, sourceLines.length - 1);
    line > definitionLine;
    line -= 1
  ) {
    const trimmed = safeTextForLine(sourceLines[line]).trim();
    if (trimmed.length === 0 || trimmed.startsWith("--")) {
      continue;
    }
    if (trimmed.startsWith("in ")) {
      return trimmed.slice(3).trim();
    }
    return trimmed;
  }
  return rhs.length > 0 ? rhs : null;
}

function inferTopLevelDefinitionReturnType(index, symbol, seen = new Set()) {
  if (typeof symbol !== "string" || symbol.length === 0 || seen.has(symbol)) {
    return null;
  }
  const declaration = index?.declarations?.get?.(symbol) ?? null;
  const definitionLine = typeof declaration?.definitionLine === "number"
    ? declaration.definitionLine
    : null;
  if (definitionLine === null) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const rawLine = safeTextForLine(sourceLines[definitionLine]);
  const decl = classifyFunctionDeclLine(rawLine);
  if (decl === null || decl.kind !== "definition") {
    return null;
  }
  const params = parseDefinitionParams(rawLine, symbol);
  const eqAt = rawLine.indexOf("=");
  const rhsHead = eqAt >= 0 ? rawLine.slice(eqAt + 1).trim() : "";
  const body = topLevelDefinitionBodyText(index, symbol);
  if (typeof body !== "string" || body.length === 0) {
    return null;
  }
  const env = new Map();
  for (const param of params) {
    env.set(param, "_");
  }
  if (rhsHead === "let") {
    const baseIndent = leadingIndentCount(rawLine);
    for (let line = definitionLine + 1; line < sourceLines.length; line += 1) {
      const currentRaw = safeTextForLine(sourceLines[line]);
      const currentTrimmed = currentRaw.trim();
      if (currentTrimmed.length === 0 || currentTrimmed.startsWith("--")) {
        continue;
      }
      if (leadingIndentCount(currentRaw) <= baseIndent) {
        break;
      }
      if (currentTrimmed.startsWith("in ")) {
        const finalExpr = currentTrimmed.slice(3).trim();
        const nextSeen = new Set(seen);
        nextSeen.add(symbol);
        return inferSimpleExprType(finalExpr, env, index, nextSeen);
      }
      const localBind = currentRaw.match(/^\s*([A-Za-z_][A-Za-z0-9_$.']*)\s*=\s*(.+)$/u);
      if (!localBind) {
        continue;
      }
      const [, name, rhs] = localBind;
      const cleanedRhs = String(rhs).replace(/;\s*$/u, "").trim();
      const inferred = inferSimpleExprType(cleanedRhs, env, index, seen);
      if (typeof inferred === "string" && inferred.length > 0) {
        env.set(name, inferred);
      }
    }
  }
  const context = findEnclosingFunctionContext(index, definitionLine);
  if (context !== null) {
    const localEnv = buildLocalTypeEnv(
      index,
      context,
      Math.max(definitionLine, Number(context.endLineExclusive ?? definitionLine + 1) - 1),
    );
    for (const [name, inferred] of localEnv.entries()) {
      if (!env.has(name)) {
        env.set(name, inferred);
      }
    }
  }
  const nextSeen = new Set(seen);
  nextSeen.add(symbol);
  return inferSimpleExprType(body, env, index, nextSeen);
}

function inferredDefinitionSignatureForSymbol(index, symbol) {
  if (typeof symbol !== "string" || symbol.length === 0) {
    return null;
  }
  const declaration = index?.declarations?.get?.(symbol) ?? null;
  const definitionLine = typeof declaration?.definitionLine === "number"
    ? declaration.definitionLine
    : null;
  if (definitionLine === null) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const rawLine = safeTextForLine(sourceLines[definitionLine]);
  const decl = classifyFunctionDeclLine(rawLine);
  if (decl === null || decl.kind !== "definition") {
    return null;
  }
  const params = parseDefinitionParams(rawLine, symbol);
  const returnType = inferTopLevelDefinitionReturnType(index, symbol);
  if (typeof returnType !== "string" || returnType.length === 0) {
    return null;
  }
  const paramTypes = params.map(() => "_");
  return `${symbol} : ${[...paramTypes, returnType].join(" -> ")}`;
}

function isSimpleTypeVariable(typeText) {
  return /^[a-z][A-Za-z0-9_']*$/u.test(String(typeText ?? "").trim());
}

function substituteSimpleTypeVars(typeText, bindings) {
  const text = String(typeText ?? "").trim();
  if (text.length === 0) {
    return "";
  }
  return text.replace(/\b([a-z][A-Za-z0-9_']*)\b/gu, (match, name) =>
    bindings.get(name) ?? match
  );
}

function unifySimpleTypePattern(patternText, actualType, bindings) {
  const pattern = stripBalancedOuterParens(patternText);
  const actual = stripBalancedOuterParens(actualType);
  if (pattern.length === 0 || actual.length === 0) {
    return false;
  }
  if (pattern === actual) {
    return true;
  }
  if (!isSimpleTypeVariable(pattern)) {
    const patternTerms = splitTopLevelApplyTerms(pattern);
    const actualTerms = splitTopLevelApplyTerms(actual);
    if (patternTerms.length <= 1 || actualTerms.length <= 1) {
      return false;
    }
    if (patternTerms.length !== actualTerms.length) {
      return false;
    }
    for (let i = 0; i < patternTerms.length; i += 1) {
      if (!unifySimpleTypePattern(patternTerms[i], actualTerms[i], bindings)) {
        return false;
      }
    }
    return true;
  }
  const existing = bindings.get(pattern);
  if (typeof existing === "string") {
    return existing === actual;
  }
  bindings.set(pattern, actual);
  return true;
}

function inferSimpleExprType(exprText, env, index, seen = new Set()) {
  const text = stripBalancedOuterParens(exprText);
  if (/^-?\d+$/u.test(text)) {
    return "i64";
  }
  if (/^"(?:[^"\\]|\\.)*"$/u.test(text)) {
    return "string";
  }
  if (text === "true" || text === "false") {
    return "bool";
  }
  const arithmeticBinary = findTopLevelBinaryOperator(text, ["+", "-", "*", "/"]);
  if (arithmeticBinary !== null) {
    return "i64";
  }
  const booleanBinary = findTopLevelBinaryOperator(text, ["&&", "||", "=="]);
  if (booleanBinary !== null) {
    return "bool";
  }
  const recordFields = parseRecordFieldEntries(text);
  if (Array.isArray(recordFields)) {
    const typedFields = new Map();
    for (const field of recordFields) {
      const fieldType = inferSimpleExprType(field.value, env, index, seen);
      if (typeof fieldType !== "string" || fieldType.length === 0) {
        continue;
      }
      typedFields.set(field.name, fieldType);
    }
    if (typedFields.size > 0) {
      return renderInlineRecordType(typedFields);
    }
  }
  const projectionTerms = projectionChainAtPosition(text, text.length - 1);
  if (projectionTerms !== null) {
    let currentType = inferSimpleExprType(projectionTerms.parts[0], env, index, seen);
    if (typeof currentType !== "string" || currentType.length === 0) {
      return null;
    }
    for (let i = 1; i < projectionTerms.parts.length; i += 1) {
      currentType = resolveRecordFieldType(index, currentType, projectionTerms.parts[i]);
      if (typeof currentType !== "string" || currentType.length === 0) {
        return null;
      }
    }
    return currentType;
  }
  if (/^[A-Za-z_][A-Za-z0-9_$.']*$/u.test(text)) {
    return env.get(text) ?? sourceSignatureTypeForSymbol(index, text) ??
      sourceNullaryValueTypeForSymbol(index, text) ??
      inferTopLevelNullaryDefinitionType(index, text, seen) ??
      inferTopLevelDefinitionReturnType(index, text, seen) ??
      null;
  }
  const terms = splitTopLevelApplyTerms(text);
  if (terms.length > 0) {
    const ctorInfo = buildDataConstructorIndex(index).get(terms[0]);
    if (ctorInfo && terms.length - 1 === ctorInfo.fieldTypes.length) {
      const bindings = new Map();
      let ok = true;
      for (let i = 0; i < ctorInfo.fieldTypes.length; i += 1) {
        const argType = inferSimpleExprType(terms[i + 1], env, index, seen);
        if (typeof argType !== "string" || argType.length === 0) {
          ok = false;
          break;
        }
        if (!unifySimpleTypePattern(ctorInfo.fieldTypes[i], argType, bindings)) {
          ok = false;
          break;
        }
      }
      if (ok) {
        const appliedParams = ctorInfo.typeParams.map((name) =>
          substituteSimpleTypeVars(name, bindings)
        );
        return appliedParams.length > 0
          ? `${ctorInfo.typeName} ${appliedParams.join(" ")}`
          : ctorInfo.typeName;
      }
    }
  }
  if (terms.length > 1) {
    const [head, ...args] = terms;
    const envHeadType = env.get(head) ?? null;
    const signatureParts = sourceSignaturePartsForSymbol(index, head) ??
      (typeof envHeadType === "string"
        ? splitTopLevelFunctionType(envHeadType)
        : null);
    if (Array.isArray(signatureParts) && signatureParts.length === args.length + 1) {
      const bindings = new Map();
      for (let i = 0; i < args.length; i += 1) {
        const argType = inferSimpleExprType(args[i], env, index, seen);
        if (typeof argType !== "string" || argType.length === 0) {
          return null;
        }
        if (!unifySimpleTypePattern(signatureParts[i], argType, bindings)) {
          return null;
        }
      }
      const resultType = substituteSimpleTypeVars(
        signatureParts[signatureParts.length - 1],
        bindings,
      );
      return resultType.length > 0 ? resultType : null;
    }
    const inferredReturn = inferTopLevelDefinitionReturnType(index, head, seen);
    const declaration = index?.declarations?.get?.(head) ?? null;
    const definitionLine = typeof declaration?.definitionLine === "number"
      ? declaration.definitionLine
      : null;
    if (
      typeof inferredReturn === "string" &&
      inferredReturn.length > 0 &&
      definitionLine !== null
    ) {
      const sourceLines = String(index?.sourceText ?? "").split("\n");
      const rawLine = safeTextForLine(sourceLines[definitionLine]);
      const paramCount = parseDefinitionParams(rawLine, head).length;
      if (paramCount === args.length) {
        return inferredReturn;
      }
    }
  }
  return null;
}

function findTopLevelBinaryOperator(text, operators) {
  const source = String(text ?? "");
  let paren = 0;
  let bracket = 0;
  let brace = 0;
  let inString = false;
  let escaped = false;
  for (let i = 0; i < source.length; i += 1) {
    const ch = source[i];
    if (inString) {
      if (escaped) {
        escaped = false;
        continue;
      }
      if (ch === "\\") {
        escaped = true;
        continue;
      }
      if (ch === "\"") {
        inString = false;
      }
      continue;
    }
    if (ch === "\"") {
      inString = true;
      continue;
    }
    if (ch === "(") paren += 1;
    else if (ch === ")") paren = Math.max(0, paren - 1);
    else if (ch === "[") bracket += 1;
    else if (ch === "]") bracket = Math.max(0, bracket - 1);
    else if (ch === "{") brace += 1;
    else if (ch === "}") brace = Math.max(0, brace - 1);
    if (paren !== 0 || bracket !== 0 || brace !== 0) {
      continue;
    }
    for (const operator of operators) {
      if (source.startsWith(operator, i)) {
        return {
          operator,
          index: i,
        };
      }
    }
  }
  return null;
}

function buildLocalTypeEnv(index, context, uptoLine) {
  const env = new Map();
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const declaration = context?.decl ?? null;
  const signatureLine = Number.isFinite(context?.signatureLine)
    ? Number(context.signatureLine)
    : null;
  const definitionLine = Number.isFinite(context?.definitionLine)
    ? Number(context.definitionLine)
    : null;
  if (declaration && signatureLine !== null && definitionLine !== null) {
    const signatureText = safeTextForLine(sourceLines[signatureLine]);
    const params = parseDefinitionParams(
      safeTextForLine(sourceLines[definitionLine]),
      context.name,
    );
    const typeParts = splitTopLevelFunctionType(topLevelTypeSuffix(signatureText));
    if (params.length > 0 && typeParts.length === params.length + 1) {
      for (let i = 0; i < params.length; i += 1) {
        env.set(params[i], typeParts[i]);
      }
    }
  }
  if (definitionLine === null) {
    return env;
  }
  const baseIndent = leadingIndentCount(safeTextForLine(sourceLines[definitionLine]));
  for (
    let line = definitionLine;
    line <= Math.min(uptoLine, context.endLineExclusive - 1, sourceLines.length - 1);
    line += 1
  ) {
    const raw = safeTextForLine(sourceLines[line]);
    const trimmed = raw.trim();
    if (trimmed.length === 0 || trimmed.startsWith("--")) {
      continue;
    }
    const sameLineLet = raw.match(/\blet\s+([A-Za-z_][A-Za-z0-9_$.']*)\s*=\s*(.+?)(?:\s+in\b|$)/u);
    if (sameLineLet) {
      const [, name, rhs] = sameLineLet;
      const cleanedRhs = String(rhs).replace(/;\s*$/u, "").trim();
      const inferred = inferSimpleExprType(cleanedRhs, env, index);
      if (inferred) {
        env.set(name, inferred);
      }
    }
    if (line === definitionLine) {
      continue;
    }
    if (leadingIndentCount(raw) <= baseIndent) {
      continue;
    }
    const localBind = raw.match(/^\s*([A-Za-z_][A-Za-z0-9_$.']*)\s*=\s*(.+)$/u);
    if (!localBind) {
      continue;
    }
    const [, name, rhs] = localBind;
    const cleanedRhs = String(rhs).replace(/;\s*$/u, "").trim();
    const inferred = inferSimpleExprType(cleanedRhs, env, index);
    if (inferred) {
      env.set(name, inferred);
    }
  }
  return env;
}

function inferCaseBinderType(index, context, line, symbol, env) {
  if (context === null || typeof symbol !== "string" || symbol.length === 0) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const rawLine = safeTextForLine(sourceLines[line]);
  const trimmed = rawLine.trim().replace(/^\|\s*/u, "");
  const armMatch = trimmed.match(/^([A-Z][A-Za-z0-9_']*)(?:\s+(.*?))?\s*->/u);
  if (!armMatch) {
    return null;
  }
  const ctorName = armMatch[1];
  const binderTerms = splitTopLevelApplyTerms(String(armMatch[2] ?? ""))
    .filter((part) => /^[A-Za-z_][A-Za-z0-9_$.']*$/u.test(part) && part !== "_");
  const binderIndex = binderTerms.indexOf(symbol);
  if (binderIndex < 0) {
    return null;
  }
  const ctorInfo = buildDataConstructorIndex(index).get(ctorName);
  if (!ctorInfo || binderIndex >= ctorInfo.fieldTypes.length) {
    return null;
  }
  const baseIndent = Number.isFinite(context?.definitionLine)
    ? leadingIndentCount(safeTextForLine(sourceLines[context.definitionLine]))
    : 0;
  const caseStack = [];
  for (let currentLine = context.definitionLine; currentLine <= line; currentLine += 1) {
    const currentRaw = safeTextForLine(sourceLines[currentLine]);
    const currentTrimmed = currentRaw.trim();
    if (currentTrimmed.length === 0 || currentTrimmed.startsWith("--")) {
      continue;
    }
    const indent = leadingIndentCount(currentRaw);
    while (caseStack.length > 0 && indent <= caseStack[caseStack.length - 1].indent) {
      caseStack.pop();
    }
    const caseMatch = currentTrimmed.match(/^(?:[A-Za-z_][A-Za-z0-9_$.']*\s*=\s*)?case\s+(.+?)\s+of\s*$/u);
    if (
      caseMatch &&
      indent >= baseIndent
    ) {
      const scrutineeType = inferSimpleExprType(caseMatch[1], env, index);
      caseStack.push({ indent, scrutineeType });
    }
    if (currentLine !== line) {
      continue;
    }
    const activeCase = caseStack[caseStack.length - 1] ?? null;
    const scrutineeType = String(activeCase?.scrutineeType ?? "").trim();
    if (scrutineeType.length === 0) {
      return null;
    }
    const scrutineeTerms = splitTopLevelApplyTerms(stripBalancedOuterParens(scrutineeType));
    if (scrutineeTerms.length === 0 || scrutineeTerms[0] !== ctorInfo.typeName) {
      return null;
    }
    if (scrutineeTerms.length !== ctorInfo.typeParams.length + 1) {
      return null;
    }
    const bindings = new Map();
    for (let i = 0; i < ctorInfo.typeParams.length; i += 1) {
      bindings.set(ctorInfo.typeParams[i], scrutineeTerms[i + 1]);
    }
    const fieldType = substituteSimpleTypeVars(
      ctorInfo.fieldTypes[binderIndex],
      bindings,
    );
    return fieldType.length > 0 ? fieldType : null;
  }
  return null;
}

function inferParamTypeFromContext(index, context, symbol) {
  if (typeof symbol !== "string" || symbol.length === 0) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const signatureLine = Number.isFinite(context?.signatureLine)
    ? Number(context.signatureLine)
    : null;
  const definitionLine = Number.isFinite(context?.definitionLine)
    ? Number(context.definitionLine)
    : null;
  if (signatureLine === null || definitionLine === null) {
    return null;
  }
  const params = parseDefinitionParams(
    safeTextForLine(sourceLines[definitionLine]),
    context.name,
  );
  const typeParts = splitTopLevelFunctionType(
    topLevelTypeSuffix(safeTextForLine(sourceLines[signatureLine])),
  );
  if (params.length === 0 || typeParts.length !== params.length + 1) {
    return null;
  }
  const indexOfParam = params.indexOf(symbol);
  if (indexOfParam < 0) {
    return null;
  }
  return typeParts[indexOfParam] ?? null;
}

function inferParamTypeFromCurrentLine(index, line, symbol) {
  if (!Number.isFinite(line) || typeof symbol !== "string" || symbol.length === 0) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const rawLine = safeTextForLine(sourceLines[line]);
  const decl = classifyFunctionDeclLine(rawLine);
  if (decl === null || decl.kind !== "definition") {
    return null;
  }
  const params = parseDefinitionParams(rawLine, decl.name);
  const indexOfParam = params.indexOf(symbol);
  if (indexOfParam < 0) {
    return null;
  }
  for (let i = line - 1; i >= 0; i -= 1) {
    const candidate = classifyFunctionDeclLine(safeTextForLine(sourceLines[i]));
    if (candidate === null) {
      continue;
    }
    if (candidate.name !== decl.name) {
      break;
    }
    if (candidate.kind !== "signature") {
      continue;
    }
    const typeParts = splitTopLevelFunctionType(
      topLevelTypeSuffix(safeTextForLine(sourceLines[i])),
    );
    if (typeParts.length === params.length + 1) {
      return typeParts[indexOfParam] ?? null;
    }
    return null;
  }
  return null;
}

function buildLocalHover(index, line, character) {
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const lineText = safeTextForLine(sourceLines[line]);
  const token = symbolAtPosition(index, line, character);
  const context = findEnclosingFunctionContext(index, line);
  const env = context === null ? new Map() : buildLocalTypeEnv(index, context, line);
  const projection = projectionChainAtPosition(lineText, character);
  if (projection !== null) {
    let currentType = inferSimpleExprType(projection.parts[0], env, index);
    if (typeof currentType === "string" && currentType.length > 0) {
      for (let i = 1; i <= projection.activeIndex; i += 1) {
        currentType = resolveRecordFieldType(index, currentType, projection.parts[i]);
        if (typeof currentType !== "string" || currentType.length === 0) {
          currentType = null;
          break;
        }
      }
    }
    if (typeof currentType === "string" && currentType.length > 0) {
      const hoveredLabel = projection.parts.slice(0, projection.activeIndex + 1).join(".");
      const hoveredStart = projection.tokenRange.start;
      const hoveredEnd = hoveredStart + hoveredLabel.length;
      return {
        contents: {
          kind: "markdown",
          value:
            `### ${hoveredLabel}\n\n\`\`\`clapse\n${hoveredLabel} : ${currentType}\n\`\`\``,
        },
        range: buildRange(line, hoveredStart, hoveredEnd),
        backend: "js",
      };
    }
  }
  if (token.symbol.length === 0) {
    return null;
  }
  const inferredType = env.get(token.symbol) ??
    inferCaseBinderType(index, context, line, token.symbol, env) ??
    (context === null ? null : inferParamTypeFromContext(index, context, token.symbol)) ??
    inferParamTypeFromCurrentLine(index, line, token.symbol);
  if (typeof inferredType !== "string" || inferredType.length === 0) {
    return null;
  }
  const fallbackRange = wordRangeAtPosition(lineText, character);
  return {
    contents: {
      kind: "markdown",
      value: `### ${token.symbol}\n\n\`\`\`clapse\n${token.symbol} : ${inferredType}\n\`\`\``,
    },
    range: buildRange(
      token.occurrence?.line ?? line,
      token.occurrence?.start ?? fallbackRange?.start ?? 0,
      token.occurrence?.end ?? fallbackRange?.end ?? 0,
    ),
    backend: "js",
  };
}

function buildHoverMarkdown(symbol, signature, doc) {
  const safeSymbol = String(symbol ?? "").trim();
  const safeSignature = String(signature ?? "").trim();
  const safeDoc = typeof doc === "string" ? doc.trim() : "";
  const parts = [`### ${safeSymbol}`];
  if (safeSignature.length > 0) {
    parts.push(`\`\`\`clapse\n${safeSignature}\n\`\`\``);
  }
  if (safeDoc.length > 0) {
    parts.push(safeDoc);
  }
  return parts.join("\n\n");
}

function explorerHoverSignatureForSymbol(index, declaration, symbol) {
  const explicitType = sourceSignatureTypeForSymbol(index, symbol);
  if (typeof explicitType === "string" && explicitType.length > 0) {
    return `${symbol} : ${explicitType}`;
  }
  const inferredSignature = inferredDefinitionSignatureForSymbol(index, symbol);
  if (typeof inferredSignature === "string" && inferredSignature.length > 0) {
    return inferredSignature;
  }
  const inferredNullary = inferTopLevelNullaryDefinitionType(index, symbol);
  if (typeof inferredNullary === "string" && inferredNullary.length > 0) {
    return `${symbol} : ${inferredNullary}`;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const signatureLine = typeof declaration?.signatureLine === "number"
    ? declaration.signatureLine
    : null;
  if (signatureLine !== null) {
    const rawSignature = safeTextForLine(sourceLines[signatureLine]).trim();
    if (rawSignature.length > 0) {
      return rawSignature;
    }
  }
  return null;
}

async function resolveExplorerHoverForOccurrence(
  wasmPath,
  uri,
  source,
  index,
  occurrence,
  symbol,
  kernelHoverCache,
) {
  const localHover = buildLocalHover(index, occurrence.line, occurrence.start);
  if (localHover && typeof localHover?.contents?.value === "string") {
    return {
      line: occurrence.line,
      start: occurrence.start,
      end: occurrence.end,
      markdown: localHover.contents.value,
      backend: String(localHover.backend ?? "js"),
      symbol,
    };
  }
  const declaration = index?.declarations?.get?.(symbol) ?? null;
  if (!declaration) {
    return null;
  }
  let kernelHover = null;
  if (kernelHoverCache.has(symbol)) {
    kernelHover = kernelHoverCache.get(symbol);
  } else {
    kernelHover = await requestKernelHover(wasmPath, uri, source, symbol);
    kernelHoverCache.set(symbol, kernelHover);
  }
  if (kernelHover && kernelHover.found === true && typeof kernelHover.signature === "string") {
    return {
      line: occurrence.line,
      start: occurrence.start,
      end: occurrence.end,
      markdown: buildHoverMarkdown(symbol, kernelHover.signature, kernelHover.doc),
      backend: "clapse",
      symbol,
    };
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const signatureLine = explorerHoverSignatureForSymbol(index, declaration, symbol) ??
    safeTextForLine(sourceLines[Number(declaration.line ?? 0)]).trim();
  return {
    line: occurrence.line,
    start: occurrence.start,
    end: occurrence.end,
    markdown: buildHoverMarkdown(symbol, signatureLine, declaration.doc),
    backend: "js",
    symbol,
  };
}

function buildExplorerLetInlayHints(index) {
  const hints = [];
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const seen = new Set();
  for (const [, entry] of index?.declarations ?? []) {
    if (!Number.isFinite(Number(entry?.definitionLine))) {
      continue;
    }
    const context = findEnclosingFunctionContext(index, Number(entry.definitionLine));
    if (context === null) {
      continue;
    }
    const baseIndent = Number.isFinite(context?.definitionLine)
      ? leadingIndentCount(safeTextForLine(sourceLines[context.definitionLine]))
      : 0;
    for (
      let line = Number(context.definitionLine) + 1;
      line < Math.min(Number(context.endLineExclusive ?? sourceLines.length), sourceLines.length);
      line += 1
    ) {
      const raw = safeTextForLine(sourceLines[line]);
      if (raw.trim().length === 0 || raw.trim().startsWith("--")) {
        continue;
      }
      if (leadingIndentCount(raw) <= baseIndent) {
        continue;
      }
      const localBind = raw.match(/^\s*([A-Za-z_][A-Za-z0-9_$.']*)\s*=\s*(.+)$/u);
      if (!localBind) {
        continue;
      }
      const [, name, rhs] = localBind;
      const nameStart = raw.indexOf(name);
      if (nameStart < 0) {
        continue;
      }
      const prefixKey = `${line}:before:${nameStart}`;
      if (!seen.has(prefixKey)) {
        seen.add(prefixKey);
        hints.push({
          line,
          position: nameStart,
          side: "before",
          label: "let ",
          kind: "keyword",
        });
      }
      const env = buildLocalTypeEnv(index, context, line);
      const inferred = inferSimpleExprType(rhs, env, index);
      if (typeof inferred !== "string" || inferred.length === 0) {
        continue;
      }
      const suffixPos = nameStart + name.length;
      const suffixKey = `${line}:after:${suffixPos}:${inferred}`;
      if (seen.has(suffixKey)) {
        continue;
      }
      seen.add(suffixKey);
      hints.push({
        line,
        position: suffixPos,
        side: "after",
        label: ` : ${inferred}`,
        kind: "type",
      });
    }
  }
  hints.sort((a, b) =>
    a.line - b.line ||
    a.position - b.position ||
    String(a.side).localeCompare(String(b.side), "en")
  );
  return hints;
}

export async function buildExplorerSourceAnnotations(
  wasmPath,
  source,
  uri = "file:///explorer.clapse",
) {
  const index = buildFunctionDocIndex(source);
  const hoverables = [];
  const kernelHoverCache = new Map();
  const seenRanges = new Set();
  for (const [symbol, occurrences] of index.occurrences.entries()) {
    for (const occurrence of occurrences) {
      const hover = await resolveExplorerHoverForOccurrence(
        wasmPath,
        uri,
        source,
        index,
        occurrence,
        symbol,
        kernelHoverCache,
      );
      if (!hover) {
        continue;
      }
      const key = `${hover.line}:${hover.start}:${hover.end}:${hover.symbol}:${hover.backend}`;
      if (seenRanges.has(key)) {
        continue;
      }
      seenRanges.add(key);
      hoverables.push(hover);
    }
  }
  hoverables.sort((a, b) =>
    a.line - b.line ||
    a.start - b.start ||
    a.end - b.end
  );
  return {
    hoverables,
    inlayHints: buildExplorerLetInlayHints(index),
  };
}

async function requestKernelSymbolIndex(wasmPath, source) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-symbol-index",
    input_source: source,
  });
  return response;
}

async function requestKernelHover(wasmPath, uri, source, symbol) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-hover",
    input_source: source,
    symbol,
  });
  if (response && response.ok === true && String(response.backend ?? "") === "clapse") {
    return response;
  }
  return null;
}

async function requestKernelDefinition(wasmPath, uri, source, symbol) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-definition",
    input_source: source,
    symbol,
  });
  if (response && response.ok === true && String(response.backend ?? "") === "clapse") {
    return response;
  }
  return null;
}

async function requestKernelCompletion(wasmPath, source, query) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-completion",
    input_source: source,
    query,
  });
  if (response && response.ok === true && response.backend === "clapse") {
    return response;
  }
  return null;
}

async function requestKernelSignatureHelp(wasmPath, source, query) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-signature-help",
    input_source: source,
    query,
  });
  if (response && response.ok === true && response.backend === "clapse") {
    return response;
  }
  return null;
}

async function requestKernelSemanticTokens(wasmPath, source) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-semantic-tokens",
    input_source: source,
  });
  if (response && response.ok === true && response.backend === "clapse") {
    return response;
  }
  return null;
}

async function requestKernelWorkspaceSymbol(wasmPath, source, query) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-workspace-symbol",
    input_source: source,
    query,
  });
  if (response && response.ok === true && response.backend === "clapse") {
    return response;
  }
  return null;
}

async function requestKernelReferences(wasmPath, source, symbol) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-references",
    input_source: source,
    symbol,
  });
  if (response && response.ok === true && response.backend === "clapse") {
    return response;
  }
  return null;
}

async function requestKernelRename(wasmPath, source, symbol, newName) {
  const response = await callCompilerWasm(wasmPath, {
    command: "lsp-rename",
    input_source: source,
    symbol,
    new_name: newName,
  });
  if (response && response.ok === true && response.backend === "clapse") {
    return response;
  }
  return null;
}

function parseAritiesFromDts(dts, symbol) {
  if (typeof dts !== "string" || dts.length === 0 || typeof symbol !== "string" || symbol.length === 0) {
    return null;
  }
  const escaped = symbol.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
  const match = dts.match(new RegExp(
    `export\\s+declare\\s+function\\s+${escaped}\\s*\\(([^)]*)\\)\\s*:\\s*[^;]+;`,
    "u",
  ));
  if (!match) {
    return null;
  }
  const params = String(match[1] ?? "").trim();
  if (params.length === 0) {
    return 0;
  }
  return params.split(",").map((part) => part.trim()).filter((part) => part.length > 0).length;
}

function renderMissingSignatureScaffold(symbol, arity) {
  const safeArity = Math.max(0, Number(arity) || 0);
  const parts = new Array(safeArity + 1).fill("_");
  return `${symbol} : ${parts.join(" -> ")}\n`;
}

async function buildMissingSignatureCodeAction(wasmPath, uri, source, declaration, symbol, pluginWasmPaths) {
  if (
    !declaration ||
    declaration.hasSignature === true ||
    typeof declaration.definitionLine !== "number"
  ) {
    return null;
  }
  let response = null;
  try {
    response = await callCompilerWasm(wasmPath, {
      command: "compile",
      input_path: uri,
      input_source: source,
      entrypoint_exports: [symbol],
      plugin_wasm_paths: pluginWasmPaths,
    });
  } catch {
    return null;
  }
  if (!response || response.ok !== true) {
    return null;
  }
  const arity = parseAritiesFromDts(response.dts, symbol) ??
    Number(response?.public_exports?.find?.((entry) => String(entry?.name ?? "") === symbol)?.arity ?? NaN);
  if (!Number.isFinite(arity) || arity < 0) {
    return null;
  }
  const insertionRange = buildRange(declaration.definitionLine, 0, 0);
  return {
    title: `Add signature scaffold for '${symbol}'`,
    kind: "quickfix",
    edit: {
      changes: {
        [uri]: [{
          range: insertionRange,
          newText: renderMissingSignatureScaffold(symbol, arity),
        }],
      },
    },
  };
}

function isIdentChar(ch) {
  return /[A-Za-z0-9_$.']/u.test(ch);
}

function wordAtPosition(lineText, character) {
  if (typeof lineText !== "string" || lineText.length === 0) return "";
  const pos = Math.max(0, Math.min(character, lineText.length));
  let left = pos;
  let right = pos;
  if (left > 0 && !isIdentChar(lineText[left]) && isIdentChar(lineText[left - 1])) {
    left -= 1;
    right = left + 1;
  }
  while (left > 0 && isIdentChar(lineText[left - 1])) left -= 1;
  while (right < lineText.length && isIdentChar(lineText[right])) right += 1;
  return lineText.slice(left, right);
}

function wordRangeAtPosition(lineText, character) {
  if (typeof lineText !== "string" || lineText.length === 0) return null;
  const pos = Math.max(0, Math.min(character, lineText.length));
  let left = pos;
  let right = pos;
  if (left > 0 && !isIdentChar(lineText[left]) && isIdentChar(lineText[left - 1])) {
    left -= 1;
    right = left + 1;
  }
  while (left > 0 && isIdentChar(lineText[left - 1])) left -= 1;
  while (right < lineText.length && isIdentChar(lineText[right])) right += 1;
  if (right <= left) {
    return null;
  }
  return { start: left, end: right };
}

async function formatSource(wasmPath, uri, source) {
  const response = await callCompilerWasm(wasmPath, {
    command: "format",
    mode: "stdout",
    input_path: uri,
    source,
  });
  if (
    !response || typeof response !== "object" || response.ok !== true ||
    typeof response.formatted !== "string"
  ) {
    const err = typeof response?.error === "string"
      ? response.error
      : "format failed";
    throw new Error(err);
  }
  return response.formatted;
}

async function compileDiagnostics(wasmPath, uri, source, config) {
  const scopeDiagnostics = await scopeDiagnosticsForSource(source, config);
  let pluginWasmPaths = [];
  try {
    pluginWasmPaths = await resolveProjectPluginWasmPaths(config, wasmPath);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    if (isSuppressedCompileDiagnosticMessage(message)) {
      return scopeDiagnostics;
    }
    throw err;
  }
  let response = null;
  try {
    response = await callCompilerWasm(wasmPath, {
      command: "compile",
      input_path: uri,
      input_source: source,
      plugin_wasm_paths: pluginWasmPaths,
    });
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    if (isSuppressedCompileDiagnosticMessage(message)) {
      return scopeDiagnostics;
    }
    throw err;
  }
  if (response && typeof response === "object" && response.ok === true) {
    return scopeDiagnostics;
  }
  const message = response && typeof response.error === "string"
    ? response.error
    : "compile failed";
  if (isSuppressedCompileDiagnosticMessage(message)) {
    return scopeDiagnostics;
  }
  const parsed = parseLineError(message);
  const diagnostics = [...scopeDiagnostics];
  if (parsed) {
    diagnostics.push({
      range: {
        start: { line: parsed.line, character: 0 },
        end: { line: parsed.line, character: 1 },
      },
      severity: 1,
      source: "clapse",
      message: parsed.msg,
    });
    return diagnostics;
  }
  diagnostics.push({
    range: {
      start: { line: 0, character: 0 },
      end: { line: 0, character: 1 },
    },
    severity: 1,
    source: "clapse",
    message,
  });
  return diagnostics;
}

function diagnosticsFromError(err) {
  const message = err instanceof Error ? err.message : String(err);
  return [{
    range: {
      start: { line: 0, character: 0 },
      end: { line: 0, character: 1 },
    },
    severity: 1,
    source: "clapse",
    message,
  }];
}

function encodeMessage(payload) {
  const body = encoder.encode(JSON.stringify(payload));
  const header = encoder.encode(`Content-Length: ${body.length}\r\n\r\n`);
  const out = new Uint8Array(header.length + body.length);
  out.set(header, 0);
  out.set(body, header.length);
  return out;
}

async function writeMessage(payload) {
  await Deno.stdout.write(encodeMessage(payload));
}

async function sendResponse(id, result) {
  await writeMessage({ jsonrpc: "2.0", id, result });
}

async function sendError(id, code, message) {
  await writeMessage({ jsonrpc: "2.0", id, error: { code, message } });
}

async function sendNotification(method, params) {
  await writeMessage({ jsonrpc: "2.0", method, params });
}

async function readMessages(onMessage) {
  let buffer = new Uint8Array(0);
  const chunk = new Uint8Array(16 * 1024);
  while (true) {
    const n = await Deno.stdin.read(chunk);
    if (n === null) break;
    const next = new Uint8Array(buffer.length + n);
    next.set(buffer, 0);
    next.set(chunk.slice(0, n), buffer.length);
    buffer = next;

    while (true) {
      const headerEnd = findHeaderEnd(buffer);
      if (headerEnd < 0) break;
      const headerBytes = buffer.slice(0, headerEnd);
      const header = decoder.decode(headerBytes);
      const len = parseContentLength(header);
      if (len < 0) {
        throw new Error("invalid LSP header: missing Content-Length");
      }
      const bodyStart = headerEnd + 4;
      const bodyEnd = bodyStart + len;
      if (buffer.length < bodyEnd) break;
      const body = buffer.slice(bodyStart, bodyEnd);
      buffer = buffer.slice(bodyEnd);
      const message = JSON.parse(decoder.decode(body));
      await onMessage(message);
    }
  }
}

function findHeaderEnd(bytes) {
  for (let i = 0; i + 3 < bytes.length; i += 1) {
    if (
      bytes[i] === 13 &&
      bytes[i + 1] === 10 &&
      bytes[i + 2] === 13 &&
      bytes[i + 3] === 10
    ) {
      return i;
    }
  }
  return -1;
}

function parseContentLength(header) {
  const lines = header.split("\r\n");
  for (const line of lines) {
    const m = line.match(/^Content-Length:\s*(\d+)$/i);
    if (m) return Number(m[1]);
  }
  return -1;
}

function getSymbolPosition(index, line, character) {
  const source = index.sourceText ?? "";
  const lines = source.split("\n");
  const lineText = lines[Math.max(0, Math.min(line, lines.length - 1))] ?? "";
  const symbol = wordAtPosition(lineText, character);
  return { symbol, lineText };
}

function buildRange(line, start, end) {
  return {
    start: { line, character: Math.max(0, start) },
    end: { line, character: Math.max(0, end) },
  };
}

function isValidPosition(pos) {
  return pos !== null &&
    typeof pos === "object" &&
    Number.isFinite(Number(pos.line)) &&
    Number.isFinite(Number(pos.character)) &&
    pos.line >= 0 &&
    pos.character >= 0;
}

function isValidRange(range) {
  if (range === null || typeof range !== "object") {
    return false;
  }
  if (!isValidPosition(range.start) || !isValidPosition(range.end)) {
    return false;
  }
  return true;
}

function isValidUri(uri) {
  return typeof uri === "string" && uri.length > 0;
}

function normalizeReferenceOrRenameRange(edit, fallbackRange) {
  const range = edit?.range;
  if (isValidRange(range)) {
    return range;
  }
  return fallbackRange;
}

function normalizeRangeFromIndex(index, symbol) {
  const occurrences = index?.occurrences?.get?.(String(symbol));
  if (!Array.isArray(occurrences) || occurrences.length === 0) {
    return buildRange(0, 0, 0);
  }
  const first = occurrences[0];
  return buildRange(first.line, first.start, first.end);
}

function sortLocations(a, b) {
  if (a.uri !== b.uri) {
    return String(a.uri).localeCompare(String(b.uri));
  }
  if (a.range.start.line !== b.range.start.line) {
    return a.range.start.line - b.range.start.line;
  }
  if (a.range.start.character !== b.range.start.character) {
    return a.range.start.character - b.range.start.character;
  }
  return a.range.end.character - b.range.end.character;
}

function uniqueLocations(entries) {
  const seen = new Set();
  const out = [];
  for (const entry of entries) {
    const uri = String(entry?.uri ?? "");
    const s = entry?.range?.start ?? {};
    const e = entry?.range?.end ?? {};
    const key = [uri, Number(s.line), Number(s.character), Number(e.line), Number(e.character)]
      .join(":");
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    out.push(entry);
  }
  return out;
}

function declarationRangeFromIndex(index, symbol) {
  const declaration = index?.declarations?.get?.(String(symbol));
  if (declaration === undefined || declaration === null) {
    return buildRange(0, 0, 0);
  }
  return buildRange(declaration.line, declaration.start, declaration.end);
}

function buildCompletionItemsFromIndex(index, query) {
  const declarationIndex = index?.declarations;
  if (!(declarationIndex instanceof Map)) {
    return [];
  }
  const queryText = String(query ?? "");
  if (queryText.length === 0) {
    return [];
  }
  return Array.from(declarationIndex.entries())
    .filter(([name]) => name.includes(queryText))
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([name, entry]) => ({
      label: name,
      kind: 3,
      detail: entry?.doc ?? "",
      sortText: "0",
      documentation: "",
    }));
}

function typeSignatureForLocalEnvSymbol(symbol, env) {
  const type = env.get(symbol);
  if (typeof type !== "string" || type.trim().length === 0) {
    return null;
  }
  return `${symbol} : ${type.trim()}`;
}

function typeSignatureForTopLevelSymbol(index, symbol) {
  const explicit = sourceSignatureTypeForSymbol(index, symbol);
  if (typeof explicit === "string" && explicit.length > 0) {
    return `${symbol} : ${explicit}`;
  }
  const inferred = inferredDefinitionSignatureForSymbol(index, symbol);
  if (typeof inferred === "string" && inferred.length > 0) {
    return inferred;
  }
  const nullary = inferTopLevelNullaryDefinitionType(index, symbol);
  if (typeof nullary === "string" && nullary.length > 0) {
    return `${symbol} : ${nullary}`;
  }
  const ret = inferTopLevelDefinitionReturnType(index, symbol);
  if (typeof ret === "string" && ret.length > 0) {
    return `${symbol} : ${ret}`;
  }
  return null;
}

function completionItemKindForSignature(signature) {
  const typeText = topLevelTypeSuffix(signature);
  const parts = splitTopLevelFunctionType(typeText);
  return parts.length > 1 ? 3 : 6;
}

function collectHoleCompletionCandidates(index, env) {
  const candidates = [];
  const seen = new Set();
  for (const [name] of env.entries()) {
    if (name === "_") {
      continue;
    }
    const signature = typeSignatureForLocalEnvSymbol(name, env);
    if (typeof signature !== "string" || signature.length === 0 || seen.has(name)) {
      continue;
    }
    seen.add(name);
    candidates.push({
      label: name,
      signature,
      detail: "local",
      kind: completionItemKindForSignature(signature),
      source: "local",
    });
  }
  for (const [name, decl] of index?.declarations ?? []) {
    if (name === "_" || seen.has(name)) {
      continue;
    }
    const signature = typeSignatureForTopLevelSymbol(index, name);
    if (typeof signature !== "string" || signature.length === 0) {
      continue;
    }
    seen.add(name);
    candidates.push({
      label: name,
      signature,
      detail: decl?.doc ? String(decl.doc) : "",
      kind: completionItemKindForSignature(signature),
      source: "top-level",
    });
  }
  return candidates;
}

function signaturePartsForCompletionCandidate(candidate) {
  const signature = String(candidate?.signature ?? "");
  const typeText = topLevelTypeSuffix(signature);
  return splitTopLevelFunctionType(typeText);
}

function inferExpectedReturnTypeForHole(index, context, line) {
  if (context === null) {
    return null;
  }
  const sourceLines = String(index?.sourceText ?? "").split("\n");
  const signatureLine = Number.isFinite(context?.signatureLine)
    ? Number(context.signatureLine)
    : null;
  if (signatureLine === null) {
    return null;
  }
  const parts = splitTopLevelFunctionType(
    topLevelTypeSuffix(safeTextForLine(sourceLines[signatureLine])),
  );
  if (parts.length === 0) {
    return null;
  }
  return parts[parts.length - 1] ?? null;
}

function findExpressionRangeForHole(lineText, holeStart) {
  const text = String(lineText ?? "");
  const topLevelArrow = (() => {
    let paren = 0;
    let bracket = 0;
    let brace = 0;
    for (let i = 0; i + 1 < text.length; i += 1) {
      const ch = text[i];
      if (ch === "(") paren += 1;
      else if (ch === ")") paren = Math.max(0, paren - 1);
      else if (ch === "[") bracket += 1;
      else if (ch === "]") bracket = Math.max(0, bracket - 1);
      else if (ch === "{") brace += 1;
      else if (ch === "}") brace = Math.max(0, brace - 1);
      if (
        ch === "-" &&
        text[i + 1] === ">" &&
        paren === 0 &&
        bracket === 0 &&
        brace === 0 &&
        i < holeStart
      ) {
        return i;
      }
    }
    return -1;
  })();
  const eqAt = text.indexOf("=");
  const inMatch = text.match(/\bin\s/u);
  const inAt = inMatch?.index ?? -1;
  let start = 0;
  if (topLevelArrow >= 0) {
    start = topLevelArrow + 2;
  } else if (eqAt >= 0 && eqAt < holeStart) {
    start = eqAt + 1;
  } else if (inAt >= 0 && inAt < holeStart) {
    start = inAt + 2;
  }
  return {
    start,
    end: text.length,
    text: text.slice(start).trim(),
    holeOffset: Math.max(0, holeStart - start),
  };
}

function buildHoleCompletionItems(index, line, character) {
  const source = String(index?.sourceText ?? "");
  const sourceLines = source.split("\n");
  const lineText = safeTextForLine(sourceLines[line]);
  const holeRange = wordRangeAtPosition(lineText, character);
  if (holeRange === null || lineText.slice(holeRange.start, holeRange.end) !== "_") {
    return [];
  }
  const context = findEnclosingFunctionContext(index, line);
  const env = context === null ? new Map() : buildLocalTypeEnv(index, context, line);
  const exprRange = findExpressionRangeForHole(lineText, holeRange.start);
  const exprText = exprRange.text;
  if (exprText.length === 0) {
    return [];
  }
  const terms = splitTopLevelApplyTerms(exprText);
  const holeTermIndex = terms.indexOf("_");
  const candidates = collectHoleCompletionCandidates(index, env);
  if (candidates.length === 0) {
    return [];
  }
  const filtered = [];
  if (holeTermIndex === 0 && terms.length > 1) {
    const argTypes = terms.slice(1)
      .map((term) => inferSimpleExprType(term, env, index))
      .filter((type) => typeof type === "string" && type.length > 0);
    for (const candidate of candidates) {
      const parts = signaturePartsForCompletionCandidate(candidate);
      if (parts.length < argTypes.length + 1) {
        continue;
      }
      const bindings = new Map();
      let ok = true;
      for (let i = 0; i < argTypes.length; i += 1) {
        if (!unifySimpleTypePattern(parts[i], argTypes[i], bindings)) {
          ok = false;
          break;
        }
      }
      if (ok) {
        filtered.push(candidate);
      }
    }
  } else if (holeTermIndex > 0) {
    const head = terms[0];
    const headType = env.get(head) ?? sourceSignatureTypeForSymbol(index, head) ?? null;
    const headParts = splitTopLevelFunctionType(String(headType ?? ""));
    if (headParts.length > holeTermIndex) {
      const bindings = new Map();
      let ok = true;
      for (let i = 1; i < holeTermIndex; i += 1) {
        const argType = inferSimpleExprType(terms[i], env, index);
        if (typeof argType !== "string" || argType.length === 0) {
          ok = false;
          break;
        }
        if (!unifySimpleTypePattern(headParts[i - 1], argType, bindings)) {
          ok = false;
          break;
        }
      }
      if (ok) {
        const expectedType = substituteSimpleTypeVars(headParts[holeTermIndex - 1], bindings);
        for (const candidate of candidates) {
          const candidateParts = signaturePartsForCompletionCandidate(candidate);
          const candidateType = candidateParts.join(" -> ");
          if (
            typeof candidateType === "string" &&
            candidateType.length > 0 &&
            unifySimpleTypePattern(expectedType, candidateType, new Map())
          ) {
            filtered.push(candidate);
          }
        }
      }
    }
  } else {
    const expectedType = inferExpectedReturnTypeForHole(index, context, line);
    if (typeof expectedType === "string" && expectedType.length > 0) {
      for (const candidate of candidates) {
        const candidateType = signaturePartsForCompletionCandidate(candidate).join(" -> ");
        if (
          typeof candidateType === "string" &&
          candidateType.length > 0 &&
          unifySimpleTypePattern(expectedType, candidateType, new Map())
        ) {
          filtered.push(candidate);
        }
      }
    }
  }
  const items = (filtered.length > 0 ? filtered : candidates)
    .sort((left, right) =>
      (left.source === right.source ? 0 : left.source === "local" ? -1 : 1) ||
      left.label.localeCompare(right.label, "en")
    )
    .map((candidate, index) => ({
      label: candidate.label,
      kind: candidate.kind,
      detail: candidate.signature,
      documentation: candidate.detail,
      sortText: String(index).padStart(4, "0"),
    }));
  return items;
}

function buildRecordProjectionCompletionItems(index, lineText, character, env) {
  const projection = projectionBaseBeforeCursor(lineText, character);
  if (projection === null) {
    return [];
  }
  let currentType = inferSimpleExprType(projection.parts[0], env, index);
  if (typeof currentType !== "string" || currentType.length === 0) {
    return [];
  }
  for (let i = 1; i < projection.parts.length; i += 1) {
    currentType = resolveRecordFieldType(index, currentType, projection.parts[i]);
    if (typeof currentType !== "string" || currentType.length === 0) {
      return [];
    }
  }
  return listRecordFieldsForType(index, currentType)
    .sort((a, b) => a.name.localeCompare(b.name))
    .map((field) => ({
      label: field.name,
      kind: 5,
      detail: field.type,
      sortText: "0",
      documentation: "",
    }));
}

function buildSignatureHelpFromIndex(index, symbol) {
  const symbolText = String(symbol ?? "");
  if (symbolText.length === 0) {
    return {
      signatures: [],
      activeSignature: 0,
      activeParameter: 0,
    };
  }
  const declaration = index?.declarations?.get?.(symbolText);
  const sourceText = String(index?.sourceText ?? "");
  const lines = sourceText.split("\n");
  const signatureLine = declaration
    ? String(lines[declaration.line] ?? "").trim()
    : symbolText;
  const label = signatureLine.length > 0 ? signatureLine : symbolText;
  return {
    signatures: [{
      label,
      documentation: declaration?.doc ? String(declaration.doc) : null,
      parameters: [],
      activeParameter: 0,
    }],
    activeSignature: 0,
    activeParameter: 0,
  };
}

function buildWorkspaceSymbolsFromIndex(index, uri, query) {
  const declarationIndex = index?.declarations;
  const queryText = String(query ?? "");
  if (!(declarationIndex instanceof Map) || queryText.length === 0) {
    return [];
  }
  return Array.from(declarationIndex.entries())
    .filter(([name]) => name.includes(queryText))
      .sort(([, left], [, right]) =>
        (left.line - right.line) || (left.start - right.start) || left.end - right.end
      )
      .map(([name, entry]) => ({
        name,
        kind: 12,
        location: {
          uri: String(uri),
          range: declarationRangeFromIndex({ declarations: declarationIndex }, name),
        },
        containerName: "",
        detail: String(entry?.doc ?? ""),
        deprecated: false,
      }));
}

function buildReferenceLocationsFromIndex(index, symbol, uri) {
  const occurrences = index?.occurrences?.get?.(String(symbol));
  if (!Array.isArray(occurrences)) {
    return [];
  }
  return occurrences
    .map((entry) => ({
      uri: String(uri),
      range: buildRange(entry.line, entry.start, entry.end),
    }))
    .sort(sortLocations);
}

function buildRenameChangesFromIndex(index, symbol, uri, newName) {
  const locations = buildReferenceLocationsFromIndex(index, symbol, uri);
  return {
    [String(uri)]: locations.map((location) => ({
      range: location.range,
      newText: String(newName),
    })),
  };
}

function normalizeReferenceResponse(coreResp, index, symbol, uri) {
  if (coreResp === null || typeof coreResp !== "object") {
    return null;
  }
  const raw = coreResp.locations;
  if (!Array.isArray(raw)) {
    return null;
  }

  const defaultUri = String(uri);
  const fallback = Array.isArray(index?.occurrences?.get?.(symbol))
    ? index.occurrences.get(symbol).map((entry) => ({
      uri: defaultUri,
      range: buildRange(entry.line, entry.start, entry.end),
    }))
    : [];
  const baseFallback = fallback;

  const normalized = raw.map((entry) => {
    const occurrence = Array.isArray(index?.occurrences?.get?.(String(symbol)))
      ? index.occurrences.get(String(symbol))[0]
      : null;
    const fallbackRange = occurrence === null
      ? normalizeRangeFromIndex(index, symbol)
      : buildRange(occurrence.line, occurrence.start, occurrence.end);
    const fallbackUri = isValidUri(entry?.uri) ? entry.uri : defaultUri;
    const range = normalizeReferenceOrRenameRange(entry, fallbackRange);
    return {
      uri: fallbackUri,
      range,
    };
  });

  if (normalized.length > 0) {
    return uniqueLocations(normalized).sort(sortLocations);
  }

  return baseFallback.map((entry) => ({
    uri: entry.uri,
    range: entry.range,
  }));
}

function normalizeRenamePayloadPayload(response, index, symbol, uri, newName) {
  if (response === null || typeof response !== "object") {
    return null;
  }
  const changes = response.changes;
  if (changes === null || typeof changes !== "object") {
    return null;
  }
  const raw = Array.isArray(changes[String(symbol)]) ? changes[String(symbol)] : null;
  if (raw === null) {
    return null;
  }

  const defaultUri = String(uri);
  const occurrences = index?.occurrences?.get?.(String(symbol));
  const fallback = Array.isArray(occurrences)
    ? occurrences
    : [];

  const normalized = raw.map((entry, i) => {
    const selected = Array.isArray(fallback) && fallback.length > 0
      ? fallback[i] ?? fallback[0]
      : null;
    const fallbackRange = selected === null
      ? normalizeRangeFromIndex(index, symbol)
      : buildRange(selected.line, selected.start, selected.end);
    const range = normalizeReferenceOrRenameRange(entry, fallbackRange);
    const rawNewText = entry?.newText;
    const newText = typeof rawNewText === "string" && rawNewText.length > 0
      ? rawNewText
      : String(newName);
    if (!isValidPosition(range.start) || !isValidPosition(range.end)) {
      return {
        range: buildRange(0, 0, 0),
        newText,
      };
    }
    return {
      range,
      newText,
    };
  });

  if (normalized.length > 0) {
    return {
      [defaultUri]: normalized,
    };
  }

  return {
    [defaultUri]: fallback.map((entry) => ({
      range: buildRange(entry.line, entry.start, entry.end),
      newText: String(newName),
    })),
  };
}

function safeTextForLine(lineText) {
  return typeof lineText === "string" ? lineText : "";
}

function symbolAtPosition(index, line, character) {
  const source = index.sourceText ?? "";
  const lines = source.split("\n");
  const lineNumber = Math.max(0, Math.min(line, Math.max(0, lines.length - 1)));
  const lineText = lines[lineNumber] ?? "";
  const symbol = wordAtPosition(lineText, character);
  if (symbol.length === 0) {
    return { symbol: "", occurrence: null, declaration: null, occurrences: [] };
  }
  const occurrences = Array.isArray(index.occurrences.get(symbol))
    ? index.occurrences.get(symbol)
    : [];
  const occurrence = occurrences.find((entry) =>
    entry.line === lineNumber &&
    character >= entry.start &&
    character <= entry.end
  ) ?? null;
  const declaration = index.declarations.get(symbol) ?? null;
  return { symbol, occurrence, declaration, occurrences };
}

function toDocumentSymbols(index) {
  const entries = Array.from(index.declarations.entries())
    .sort((a, b) => (a[1].line - b[1].line) || (a[1].start - b[1].start));
  return entries.map(([name, decl]) => {
    const lineText = safeTextForLine(index.sourceText?.split("\n")[decl.line]).trim();
    const range = buildRange(decl.line, 0, Math.max(lineText.length, decl.end));
    return {
      name,
      detail: lineText,
      kind: 12,
      range,
      selectionRange: buildRange(decl.line, decl.start, decl.end),
      children: [],
    };
  });
}

function buildCompletionItemsFromKernel(coreResp) {
  const items = Array.isArray(coreResp?.items) ? coreResp.items : [];
  return items.map((entry) => {
    const kindName = String(entry?.type_hint ?? "value");
    const label = String(entry?.label ?? "");
    const rank = Number(entry?.rank ?? 0);
    return {
      label,
      kind: kindName === "function" ? 3 : 1,
      detail: String(entry?.type_hint ?? ""),
      sortText: String(rank),
      documentation: typeof entry?.detail === "string" ? entry.detail : "",
    };
  });
}

function buildSignatureHelpFromKernel(coreResp) {
  const signatures = Array.isArray(coreResp?.signatures)
    ? coreResp.signatures
    : [];
  return {
    signatures: signatures.map((sig) => ({
      label: String(sig?.label ?? ""),
      documentation: sig?.documentation ?? null,
      parameters: Array.isArray(sig?.parameters) ? sig.parameters : [],
      activeParameter: Number(sig?.activeParameter ?? 0),
    })),
    activeSignature: Number(coreResp?.activeSignature ?? 0),
    activeParameter: Number(coreResp?.activeParameter ?? 0),
  };
}

function buildWorkspaceSymbolsFromKernelWithIndex(uri, coreResp, index) {
  const symbols = Array.isArray(coreResp?.symbols) ? coreResp.symbols : [];
  if (symbols.length === 0) {
    return [];
  }
  return symbols.map((entry) => {
    const name = String(entry?.name ?? "");
    const declaration = index?.declarations?.get?.(name);
    return {
      name,
      kind: Number(entry?.kind ?? 12),
      location: {
        uri: String(uri),
        range: declaration
          ? declarationRangeFromIndex(index, name)
          : buildRange(0, 0, 0),
      },
      containerName: "",
      detail: String(entry?.detail ?? ""),
      deprecated: false,
    };
  });
}

export async function runLspServer() {
  const wasmPath = getWasmPath();
  await validateCompilerWasmAbi(wasmPath);
  const docs = new Map();
  const docIndex = new Map();
  const coreSymbolIndex = new Map();
  const docConfigs = new Map();
  let workspaceRootPath = "";
  let shutdownRequested = false;

  await readMessages(async (msg) => {
    const method = msg?.method;
    const id = msg?.id;

    try {
      if (method === "initialize") {
        const rootUri = String(msg.params?.rootUri ?? "");
        const workspacePath = uriToPath(rootUri);
        workspaceRootPath = workspacePath.length > 0 ? workspacePath : "";
        await sendResponse(id, {
          capabilities: {
            textDocumentSync: 1,
            documentFormattingProvider: true,
            completionProvider: { resolveProvider: false, triggerCharacters: ["."] },
            signatureHelpProvider: { triggerCharacters: ["("] },
            semanticTokensProvider: {
              full: { delta: false },
              range: false,
              legend: {
                tokenTypes: [
                  "namespace",
                  "type",
                  "class",
                  "enum",
                  "interface",
                  "struct",
                  "typeParameter",
                  "parameter",
                  "variable",
                  "property",
                  "enumMember",
                  "event",
                  "function",
                  "method",
                  "macro",
                  "keyword",
                  "modifier",
                  "comment",
                  "string",
                  "number",
                  "regexp",
                  "operator",
                ],
                tokenModifiers: [
                  "declaration",
                  "definition",
                  "readonly",
                  "static",
                  "deprecated",
                  "abstract",
                  "async",
                  "modification",
                  "documentation",
                  "defaultLibrary",
                  "local",
                ],
              },
            },
            hoverProvider: true,
            definitionProvider: true,
            referencesProvider: true,
            workspaceSymbolProvider: true,
            documentSymbolProvider: true,
            renameProvider: { prepareProvider: true },
            codeActionProvider: { codeActionKinds: ["quickfix"] },
            inlayHintProvider: false,
          },
          serverInfo: { name: "clapse-wasm-lsp", version: "0.1.0" },
        });
        return;
      }
      if (method === "initialized") {
        return;
      }
      if (method === "shutdown") {
        shutdownRequested = true;
        await sendResponse(id, null);
        return;
      }
      if (method === "exit") {
        Deno.exit(shutdownRequested ? 0 : 1);
      }

      if (method === "textDocument/didOpen") {
        const uri = msg.params?.textDocument?.uri;
        const text = msg.params?.textDocument?.text ?? "";
        if (typeof uri === "string") {
          docs.set(uri, String(text));
          coreSymbolIndex.set(uri, null);
          docIndex.set(uri, buildFunctionDocIndex(String(text)));
          requestKernelSymbolIndex(wasmPath, String(text)).then((response) => {
            if (response && response.ok === true && typeof response.symbols === "string") {
              coreSymbolIndex.set(uri, response.symbols);
            } else {
              coreSymbolIndex.delete(uri);
            }
          }).catch(() => {
            coreSymbolIndex.delete(uri);
          });
          const config = await resolveProjectConfig(uri, workspaceRootPath);
          docConfigs.set(uri, config);
          let diagnostics = [];
          try {
            diagnostics = await compileDiagnostics(
              wasmPath,
              uri,
              String(text),
              config,
            );
          } catch (err) {
            diagnostics = diagnosticsFromError(err);
          }
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics,
          });
        }
        return;
      }
      if (method === "textDocument/didChange") {
        const uri = msg.params?.textDocument?.uri;
        const changes = msg.params?.contentChanges;
        if (
          typeof uri === "string" && Array.isArray(changes) &&
          changes.length > 0
        ) {
          const text = String(changes[changes.length - 1].text ?? "");
          docs.set(uri, text);
          coreSymbolIndex.set(uri, null);
          docIndex.set(uri, buildFunctionDocIndex(text));
          requestKernelSymbolIndex(wasmPath, text).then((response) => {
            if (response && response.ok === true && typeof response.symbols === "string") {
              coreSymbolIndex.set(uri, response.symbols);
            } else {
              coreSymbolIndex.delete(uri);
            }
          }).catch(() => {
            coreSymbolIndex.delete(uri);
          });
          const config = docConfigs.get(uri) ?? await resolveProjectConfig(uri, workspaceRootPath);
          let diagnostics = [];
          try {
            diagnostics = await compileDiagnostics(wasmPath, uri, text, config);
          } catch (err) {
            diagnostics = diagnosticsFromError(err);
          }
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics,
          });
        }
        return;
      }
      if (method === "textDocument/didSave") {
        const uri = msg.params?.textDocument?.uri;
        if (typeof uri === "string") {
          const text = docs.get(uri) ?? "";
          const config = docConfigs.get(uri) ?? await resolveProjectConfig(uri, workspaceRootPath);
          let diagnostics = [];
          try {
            diagnostics = await compileDiagnostics(wasmPath, uri, text, config);
          } catch (err) {
            diagnostics = diagnosticsFromError(err);
          }
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics,
          });
        }
        return;
      }
      if (method === "textDocument/didClose") {
        const uri = msg.params?.textDocument?.uri;
        if (typeof uri === "string") {
          docs.delete(uri);
          docIndex.delete(uri);
          coreSymbolIndex.delete(uri);
          docConfigs.delete(uri);
          await sendNotification("textDocument/publishDiagnostics", {
            uri,
            diagnostics: [],
          });
        }
        return;
      }

      if (method === "textDocument/completion") {
        const uri = msg.params?.textDocument?.uri;
        if (typeof uri !== "string") {
          await sendResponse(id, []);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        const source = docs.get(uri) ?? "";
        if (index === null) {
          await sendResponse(id, []);
          return;
        }
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        const lineText = safeTextForLine(String(source).split("\n")[line]);
        const context = findEnclosingFunctionContext(index, line);
        const env = context === null ? new Map() : buildLocalTypeEnv(index, context, line);
        const projectionItems = buildRecordProjectionCompletionItems(
          index,
          lineText,
          character,
          env,
        );
        if (projectionItems.length > 0) {
          await sendResponse(id, projectionItems);
          return;
        }
        const holeItems = buildHoleCompletionItems(index, line, character);
        if (holeItems.length > 0) {
          await sendResponse(id, holeItems);
          return;
        }
        const { symbol } = getSymbolPosition(index, line, character);
        const coreResp = await requestKernelCompletion(wasmPath, source, symbol);
        const completionItems = coreResp === null
          ? []
          : buildCompletionItemsFromKernel(coreResp);
        if (completionItems.length > 0) {
          await sendResponse(id, completionItems);
          return;
        }
        await sendResponse(id, buildCompletionItemsFromIndex(index, symbol));
        return;
      }

      if (method === "textDocument/signatureHelp") {
        const uri = msg.params?.textDocument?.uri;
        if (typeof uri !== "string") {
          await sendResponse(id, null);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        const source = docs.get(uri) ?? "";
        if (index === null) {
          await sendResponse(id, null);
          return;
        }
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        const { symbol } = getSymbolPosition(index, line, character);
        const coreResp = await requestKernelSignatureHelp(wasmPath, source, symbol);
        const signature = coreResp === null
          ? buildSignatureHelpFromIndex(index, symbol)
          : buildSignatureHelpFromKernel(coreResp);
        await sendResponse(id, signature);
        return;
      }

      if (method === "textDocument/semanticTokens/full") {
        const uri = msg.params?.textDocument?.uri;
        const source = typeof uri === "string" ? (docs.get(uri) ?? "") : "";
        const coreResp = await requestKernelSemanticTokens(wasmPath, source);
        if (coreResp === null) {
          await sendResponse(id, { data: [] });
          return;
        }
        await sendResponse(id, { data: Array.isArray(coreResp?.data) ? coreResp.data : [] });
        return;
      }

      if (method === "workspace/symbol") {
        const query = String(msg.params?.query ?? "");
        const docEntries = Array.from(docs.entries());
        const symbols = [];
        for (const [docUri, docSource] of docEntries) {
          const source = typeof docSource === "string" ? docSource : "";
          const docIndexEntry = docIndex.get(docUri) ?? null;
          const coreResp = await requestKernelWorkspaceSymbol(wasmPath, source, query);
          if (coreResp === null) {
            symbols.push(...buildWorkspaceSymbolsFromIndex(docIndexEntry, docUri, query));
            continue;
          }
          const docSymbols = buildWorkspaceSymbolsFromKernelWithIndex(
            docUri,
            coreResp,
            docIndexEntry,
          );
          if (docSymbols.length === 0) {
            symbols.push(...buildWorkspaceSymbolsFromIndex(docIndexEntry, docUri, query));
            continue;
          }
          for (const item of docSymbols) {
            symbols.push(item);
          }
        }
        await sendResponse(id, symbols);
        return;
      }

      if (method === "textDocument/formatting") {
        const uri = msg.params?.textDocument?.uri;
        const text = typeof uri === "string" ? (docs.get(uri) ?? "") : "";
        const formatted = await formatSource(
          wasmPath,
          uri ?? "<unknown>",
          text,
        );
        await sendResponse(id, [{
          range: fullRangeForText(text),
          newText: formatted,
        }]);
        return;
      }

      if (method === "textDocument/hover") {
        const uri = msg.params?.textDocument?.uri;
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        if (typeof uri !== "string") {
          await sendResponse(id, null);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        const source = docs.get(uri) ?? "";
        if (index === null) {
          await sendResponse(id, null);
          return;
        }
        const { symbol } = getSymbolPosition(index, line, character);
        if (symbol.length === 0) {
          await sendResponse(id, null);
          return;
        }
        const coreResp = await requestKernelHover(wasmPath, uri, source, symbol);
        if (coreResp && coreResp.found === true && typeof coreResp.signature === "string") {
          const foundRange = declarationRangeFromSignature(source, symbol, coreResp.signature);
          const signature = coreResp.signature.trim();
          const doc = typeof coreResp.doc === "string" ? coreResp.doc.trim() : "";
          const contents = doc.length > 0
            ? `### ${symbol}\n\n${doc}`
            : `### ${symbol}\n\n\`\`\`clapse\n${signature}\n\`\`\``;
          if (foundRange !== null && foundRange.range !== undefined) {
            await sendResponse(id, {
              contents: { kind: "markdown", value: contents },
              range: foundRange.range,
              backend: "clapse",
            });
            return;
          }
        }
        const entry = index.declarations.get(symbol);
        if (!entry || typeof symbol !== "string") {
          const localHover = buildLocalHover(index, line, character);
          await sendResponse(id, localHover);
          return;
        }
        const signatureLine = safeTextForLine(index.sourceText?.split("\n")[entry.line]);
        const signature = signatureLine.trim().length > 0 ? signatureLine.trim() : `${symbol}`;
        const contents = entry.doc.length > 0
          ? `### ${symbol}\n\n${entry.doc}`
          : `### ${symbol}\n\n\`\`\`clapse\n${signature}\n\`\`\``;
        const range = buildRange(entry.line, entry.start, entry.end);
        await sendResponse(id, {
          contents: { kind: "markdown", value: contents },
          range,
        });
        return;
      }

      if (method === "textDocument/definition") {
        const uri = msg.params?.textDocument?.uri;
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        if (typeof uri !== "string") {
          await sendResponse(id, null);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        if (index === null) {
          await sendResponse(id, []);
          return;
        }
        const token = symbolAtPosition(index, line, character);
        if (token.symbol.length === 0 || token.occurrence === null) {
          await sendResponse(id, []);
          return;
        }
        const coreResp = await requestKernelDefinition(wasmPath, uri, docs.get(uri) ?? "", token.symbol);
        if (coreResp && coreResp.found === true && typeof coreResp.signature === "string") {
          const foundRange = declarationRangeFromSignature(docs.get(uri) ?? "", token.symbol, coreResp.signature);
          if (foundRange !== null && foundRange.range !== undefined) {
            await sendResponse(id, [{
              uri,
              range: foundRange.range,
              backend: "clapse",
            }]);
            return;
          }
        }
        const declaration = token.declaration;
        if (declaration === undefined || declaration === null) {
          await sendResponse(id, []);
          return;
        }
        await sendResponse(id, [{
          uri,
          range: buildRange(declaration.line, declaration.start, declaration.end),
        }]);
        return;
      }

      if (method === "textDocument/prepareRename") {
        const uri = msg.params?.textDocument?.uri;
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        if (typeof uri !== "string") {
          await sendResponse(id, null);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        if (index === null) {
          await sendResponse(id, null);
          return;
        }
        const token = symbolAtPosition(index, line, character);
        if (token.symbol.length === 0 || token.occurrence === null) {
          await sendResponse(id, null);
          return;
        }
        await sendResponse(id, {
          range: buildRange(token.occurrence.line, token.occurrence.start, token.occurrence.end),
          placeholder: token.symbol,
        });
        return;
      }

      if (method === "textDocument/rename") {
        const uri = msg.params?.textDocument?.uri;
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        const newName = String(msg.params?.newName ?? "");
        if (typeof uri !== "string" || newName.length === 0) {
          await sendResponse(id, null);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        if (index === null) {
          await sendResponse(id, null);
          return;
        }
        const token = symbolAtPosition(index, line, character);
        if (token.symbol.length === 0 || token.occurrence === null) {
          await sendResponse(id, null);
          return;
        }
        const coreResp = await requestKernelRename(
          wasmPath,
          docs.get(uri) ?? "",
          token.symbol,
          newName,
        );
        const normalized = coreResp === null
          ? buildRenameChangesFromIndex(index, token.symbol, uri, newName)
          : normalizeRenamePayloadPayload(
            coreResp,
            index,
            token.symbol,
            uri,
            newName,
          );
        if (normalized === null) {
          await sendResponse(id, {
            changes: buildRenameChangesFromIndex(index, token.symbol, uri, newName),
          });
          return;
        }
        await sendResponse(id, {
          ...(coreResp ?? {}),
          changes: normalized,
        });
        return;
      }

      if (method === "textDocument/references") {
        const uri = msg.params?.textDocument?.uri;
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        if (typeof uri !== "string") {
          await sendResponse(id, []);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        if (index === null) {
          await sendResponse(id, []);
          return;
        }
        const token = symbolAtPosition(index, line, character);
        if (token.symbol.length === 0 || token.occurrence === null) {
          await sendResponse(id, []);
          return;
        }
        const coreResp = await requestKernelReferences(
          wasmPath,
          docs.get(uri) ?? "",
          token.symbol,
        );
        const normalized = coreResp === null
          ? buildReferenceLocationsFromIndex(index, token.symbol, uri)
          : normalizeReferenceResponse(
            coreResp,
            index,
            token.symbol,
            uri,
          );
        if (normalized === null) {
          await sendResponse(id, buildReferenceLocationsFromIndex(index, token.symbol, uri));
          return;
        }
        await sendResponse(id, normalized);
        return;
      }

      if (method === "textDocument/documentSymbol") {
        const uri = msg.params?.textDocument?.uri;
        if (typeof uri !== "string") {
          await sendResponse(id, []);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        if (index === null) {
          await sendResponse(id, []);
          return;
        }
        await sendResponse(id, toDocumentSymbols(index));
        return;
      }

      if (method === "textDocument/codeAction") {
        const uri = msg.params?.textDocument?.uri;
        const range = msg.params?.range ?? {};
        const line = Number(range.start?.line ?? 0);
        const character = Number(range.start?.character ?? 0);
        if (typeof uri !== "string") {
          await sendResponse(id, []);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        if (index === null) {
          await sendResponse(id, []);
          return;
        }
        const { symbol } = getSymbolPosition(index, line, character);
        if (symbol.length === 0) {
          await sendResponse(id, []);
          return;
        }
        const source = docs.get(uri) ?? "";
        const declaration = index.declarations.get(symbol);
        const actions = [];
        actions.push({
          title: `Rename '${symbol}'`,
          kind: "quickfix",
          command: {
            title: "Rename Symbol",
            command: "editor.action.rename",
            arguments: [uri, { line, character }],
          },
        });
        if (declaration) {
          const config = docConfigs.get(uri) ?? await resolveProjectConfig(uri, workspaceRootPath);
          let pluginWasmPaths = [];
          try {
            pluginWasmPaths = await resolveProjectPluginWasmPaths(config, wasmPath);
          } catch {
            pluginWasmPaths = [];
          }
          const signatureAction = await buildMissingSignatureCodeAction(
            wasmPath,
            uri,
            source,
            declaration,
            symbol,
            pluginWasmPaths,
          );
          if (signatureAction !== null) {
            actions.push(signatureAction);
          }
        }
        if (declaration && declaration.doc.length === 0) {
          const insertionRange = buildRange(declaration.line, 0, 0);
          actions.push({
            title: `Add doc comment for '${symbol}'`,
            kind: "quickfix",
            edit: {
              changes: {
                [uri]: [{
                  range: insertionRange,
                  newText: `--| ${symbol} ...\n`,
                }],
              },
            },
          });
        }
        await sendResponse(id, actions);
        return;
      }

      if (id !== undefined) {
        await sendResponse(id, null);
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      if (id !== undefined) {
        await sendError(id, -32603, message);
      }
    }
  });
}

if (import.meta.main) {
  await runLspServer().catch(failWithError);
}
