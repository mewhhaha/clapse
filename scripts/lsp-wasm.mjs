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

function isFunctionDeclLine(rawLine) {
  const line = rawLine.trim();
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
    line.startsWith("infix ")
  ) {
    return null;
  }
  if (
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
  return name;
}

function buildFunctionDocIndex(text) {
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
    const fnName = isFunctionDeclLine(raw);
    if (fnName !== null) {
      const start = raw.indexOf(fnName);
      out.set(fnName, {
        doc: pending.length > 0 ? pending.join("\n").trim() : "",
        line: i,
        start: Math.max(0, start),
        end: Math.max(0, start) + fnName.length,
      });
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
  const pluginWasmPaths = await resolveProjectPluginWasmPaths(config, wasmPath);
  const response = await callCompilerWasm(wasmPath, {
    command: "compile",
    input_path: uri,
    input_source: source,
    plugin_wasm_paths: pluginWasmPaths,
  });
  if (response && typeof response === "object" && response.ok === true) {
    return scopeDiagnostics;
  }
  const message = response && typeof response.error === "string"
    ? response.error
    : "compile failed";
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
            hoverProvider: true,
            definitionProvider: true,
            referencesProvider: true,
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
          const diagnostics = await compileDiagnostics(
            wasmPath,
            uri,
            String(text),
            config,
          );
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
          const diagnostics = await compileDiagnostics(wasmPath, uri, text, config);
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
          const diagnostics = await compileDiagnostics(wasmPath, uri, text, config);
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
          await sendResponse(id, null);
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
        if (typeof uri !== "string" || newName.length === 0 || !/^[A-Za-z_][A-Za-z0-9_$.']*$/u.test(newName)) {
          await sendResponse(id, null);
          return;
        }
        const index = docIndex.get(uri) ?? null;
        if (index === null) {
          await sendResponse(id, null);
          return;
        }
        const token = symbolAtPosition(index, line, character);
        const positions = token.occurrences;
        if (token.symbol.length === 0 || token.occurrence === null || positions.length === 0) {
          await sendResponse(id, null);
          return;
        }
        const edits = positions
          .slice()
          .sort((a, b) => (a.line - b.line) || (a.start - b.start))
          .map((pos) => ({
            range: buildRange(pos.line, pos.start, pos.end),
            newText: newName,
          }));
        await sendResponse(id, { changes: { [uri]: edits } });
        return;
      }

      if (method === "textDocument/references") {
        const uri = msg.params?.textDocument?.uri;
        const line = Number(msg.params?.position?.line ?? 0);
        const character = Number(msg.params?.position?.character ?? 0);
        const includeDeclaration = msg.params?.context?.includeDeclaration !== false;
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
        if (token.symbol.length === 0 || token.occurrence === null || token.occurrences.length === 0) {
          await sendResponse(id, []);
          return;
        }
        const locations = token.occurrences
          .filter((entry) => {
            if (includeDeclaration) {
              return true;
            }
            return !(
              token.declaration !== null &&
              entry.line === token.declaration.line &&
              entry.start === token.declaration.start &&
              entry.end === token.declaration.end
            );
          })
          .sort((a, b) => (a.line - b.line) || (a.start - b.start))
          .map((entry) => ({
            uri,
            range: buildRange(entry.line, entry.start, entry.end),
          }));
        await sendResponse(id, locations);
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
