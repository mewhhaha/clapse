#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";
import { runLspServer } from "./lsp-wasm.mjs";
import {
  appendClapseFuncMap,
  callCompilerWasm,
  decodeWasmBase64,
  rewriteWasmTailCallOpcodes,
  validateCompilerWasmAbi,
} from "./wasm-compiler-abi.mjs";
import {
  buildWasmSeedCompileResponse,
  isWasmBootstrapSeedEnabled,
} from "./wasm-bootstrap-seed.mjs";

const REPO_ROOT_URL = new URL("../", import.meta.url);
const PROJECT_CONFIG_FILE = "clapse.json";
const CLAPSE_ENTRYPOINT_DCE_ENV = "CLAPSE_ENTRYPOINT_DCE";
const CLAPSE_ENTRYPOINT_DCE_FORCE_ENV = "CLAPSE_ENTRYPOINT_DCE_FORCE";
const TOP_LEVEL_IMPORT_RX = /^\s*import\s+([A-Za-z_][A-Za-z0-9_$.']*)/u;
const TOP_LEVEL_MODULE_RX = /^\s*module\s+([A-Za-z_][A-Za-z0-9_$.']*)/u;
const TOP_LEVEL_EXPORT_RX = /^\s*export\s+(.+)$/u;
const TOP_LEVEL_SIGNATURE_RX = /^([A-Za-z_][A-Za-z0-9_']*)\s*:/u;
const TOP_LEVEL_FUNCTION_RX = /^([A-Za-z_][A-Za-z0-9_']*)\b[^=]*=/u;
const IDENTIFIER_REF_RX =
  /[A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*/gu;
const TOP_LEVEL_DECL_KEYWORDS = new Set([
  "module",
  "import",
  "export",
  "data",
  "type",
  "class",
  "instance",
  "primitive",
]);
const IDENTIFIER_KEYWORDS = new Set([
  "module",
  "import",
  "export",
  "data",
  "type",
  "class",
  "instance",
  "primitive",
  "case",
  "of",
  "let",
  "in",
  "if",
  "then",
  "else",
  "where",
  "true",
  "false",
]);

function toPath(url) {
  return decodeURIComponent(url.pathname);
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
  if (stack.length === 0) {
    return hasLeadingSlash ? "/" : "";
  }
  return hasLeadingSlash ? `/${stack.join("/")}` : stack.join("/");
}

function pathDir(path) {
  const normalized = normalizePath(path);
  if (normalized.length === 0 || normalized === ".") {
    return "";
  }
  if (normalized === "/") {
    return "/";
  }
  const idx = normalized.lastIndexOf("/");
  if (idx <= 0) {
    return normalized.startsWith("/") ? "/" : "";
  }
  return normalized.slice(0, idx);
}

function joinPath(base, leaf) {
  const normalizedBase = normalizePath(base);
  if (normalizedBase.length === 0 || normalizedBase === ".") {
    return normalizePath(leaf);
  }
  const normalizedLeaf = normalizePath(leaf);
  if (normalizedLeaf.length === 0) {
    return normalizedBase;
  }
  if (normalizedBase === "/") {
    return `/${normalizedLeaf}`;
  }
  return `${normalizedBase}/${normalizedLeaf}`;
}

function toAbsolutePath(path) {
  const normalized = normalizePath(path);
  if (normalized.startsWith("/")) {
    return normalized;
  }
  return joinPath(Deno.cwd(), normalized);
}

function normalizeSearchDirs(value, configDir) {
  if (!Array.isArray(value)) {
    return [];
  }
  const out = [];
  const seen = new Set();
  for (const item of value) {
    const raw = String(item).trim();
    if (raw.length === 0) {
      continue;
    }
    const normalizedRaw = raw.replace(/\\/g, "/").trim();
    if (normalizedRaw.length === 0) {
      continue;
    }
    let resolved = normalizedRaw;
    if (!normalizedRaw.startsWith("/")) {
      const base = normalizePath(configDir);
      resolved = base.length > 0
        ? normalizePath(`${base}/${normalizedRaw}`)
        : normalizedRaw;
    }
    if (resolved.length === 0 || seen.has(resolved)) {
      continue;
    }
    seen.add(resolved);
    out.push(resolved);
  }
  return out;
}

function normalizeIncludeDirs(value, configDir) {
  return normalizeSearchDirs(value, configDir);
}

function normalizePluginDirs(value, configDir) {
  return normalizeSearchDirs(value, configDir);
}

function candidateModulePath(moduleName, dir) {
  const relativePath = `${String(moduleName).replace(/[.$]/g, "/")}.clapse`;
  if (typeof dir !== "string" || dir.length === 0) {
    return normalizePath(relativePath);
  }
  return joinPath(dir, relativePath);
}

function parseClapseProjectConfig(rawText, configDir) {
  if (typeof rawText !== "string" || rawText.length === 0) {
    return { moduleSearchDirs: [], pluginDirs: [] };
  }
  let data;
  try {
    data = JSON.parse(rawText);
  } catch {
    return { moduleSearchDirs: [], pluginDirs: [] };
  }
  if (!data || typeof data !== "object" || Array.isArray(data)) {
    return { moduleSearchDirs: [], pluginDirs: [] };
  }
  return {
    moduleSearchDirs: normalizeIncludeDirs(data.include, configDir),
    pluginDirs: normalizePluginDirs(data.plugins, configDir),
  };
}

async function readClapseProjectConfig(startPath = "") {
  const start = normalizePath(startPath || Deno.cwd());
  let dir = pathDir(start);
  if (dir.length === 0 || !start.includes("/")) {
    dir = normalizePath(Deno.cwd());
  }
  if (dir.length === 0) {
    return { moduleSearchDirs: [], pluginDirs: [] };
  }

  const visited = new Set();
  while (true) {
    const candidate = dir;
    if (visited.has(candidate)) {
      break;
    }
    visited.add(candidate);
    const configPath = joinPath(candidate, PROJECT_CONFIG_FILE);
    try {
      const rawText = await Deno.readTextFile(configPath);
      const parsed = parseClapseProjectConfig(rawText, candidate);
      return {
        moduleSearchDirs: parsed.moduleSearchDirs,
        pluginDirs: parsed.pluginDirs,
      };
    } catch {
      // no project config in this directory
    }
    const parent = pathDir(candidate);
    if (parent === candidate) {
      break;
    }
    dir = parent;
  }
  return { moduleSearchDirs: [], pluginDirs: [] };
}

async function collectClapseFilesRecursively(
  rootDir,
  out = [],
  seen = new Set(),
) {
  const normalized = normalizePath(rootDir);
  if (normalized.length === 0 || seen.has(normalized)) {
    return out;
  }
  seen.add(normalized);
  let entries;
  try {
    entries = [];
    for await (const entry of Deno.readDir(normalized)) {
      entries.push(entry);
    }
  } catch {
    return out;
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
  return out;
}

function boolEnvFlag(name, defaultValue = false) {
  const raw = String(Deno.env.get(name) ?? "").trim().toLowerCase();
  if (raw.length === 0) {
    return defaultValue;
  }
  return !(raw === "0" || raw === "false" || raw === "off" || raw === "no");
}

function isCompilerInternalInputPath(inputPath) {
  const normalized = normalizePath(inputPath);
  return normalized === "lib/compiler/kernel.clapse" ||
    normalized.startsWith("lib/compiler/") ||
    normalized.includes("/lib/compiler/");
}

function shouldRunEntrypointDce(inputPath, options = {}) {
  if (options.skipEntrypointReachabilityPrune === true) {
    return false;
  }
  const enabled = boolEnvFlag(CLAPSE_ENTRYPOINT_DCE_ENV, true);
  if (!enabled) {
    return false;
  }
  const forced = boolEnvFlag(CLAPSE_ENTRYPOINT_DCE_FORCE_ENV, false);
  if (!forced && isCompilerInternalInputPath(inputPath)) {
    return false;
  }
  return true;
}

function parseExportNamesFromRaw(raw) {
  return String(raw)
    .split(",")
    .map((name) => name.trim())
    .filter((name) => /^[A-Za-z_][A-Za-z0-9_']*$/u.test(name));
}

function normalizeEntrypointExportRoots(rawRoots) {
  if (!Array.isArray(rawRoots)) {
    return [];
  }
  const out = [];
  const seen = new Set();
  for (const root of rawRoots) {
    const name = String(root ?? "").trim();
    if (!/^[A-Za-z_][A-Za-z0-9_']*$/u.test(name)) {
      continue;
    }
    if (seen.has(name)) {
      continue;
    }
    seen.add(name);
    out.push(name);
  }
  return out;
}

function stripCommentsAndStrings(source) {
  const noStrings = String(source).replace(/"([^"\\]|\\.)*"/gu, " ");
  return noStrings.replace(/--.*$/gmu, " ");
}

function collectIdentifierRefs(source) {
  const refs = new Set();
  const scrubbed = stripCommentsAndStrings(source);
  for (const match of scrubbed.matchAll(IDENTIFIER_REF_RX)) {
    const token = match[0];
    if (token.length === 0) {
      continue;
    }
    const first = token.split(".")[0];
    if (IDENTIFIER_KEYWORDS.has(first)) {
      continue;
    }
    refs.add(token);
  }
  return refs;
}

function parseModuleSourceDescriptor(path, source) {
  const lines = String(source).split("\n");
  let moduleName = "";
  const imports = [];
  const importSeen = new Set();
  const exports = new Set();
  const signatureLinesByName = new Map();
  const functionStarts = [];

  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    if (moduleName.length === 0) {
      const moduleMatch = line.match(TOP_LEVEL_MODULE_RX);
      if (moduleMatch) {
        moduleName = moduleMatch[1];
      }
    }
    const importMatch = line.match(TOP_LEVEL_IMPORT_RX);
    if (importMatch) {
      const importName = importMatch[1];
      if (!importSeen.has(importName)) {
        importSeen.add(importName);
        imports.push(importName);
      }
    }
    const exportMatch = line.match(TOP_LEVEL_EXPORT_RX);
    if (exportMatch) {
      for (const exportName of parseExportNamesFromRaw(exportMatch[1])) {
        exports.add(exportName);
      }
    }
    const startsWithIndent = /^\s/u.test(line);
    if (startsWithIndent) {
      continue;
    }
    const trimmed = line.trim();
    if (trimmed.startsWith("--") || trimmed.length === 0) {
      continue;
    }
    const signatureMatch = line.match(TOP_LEVEL_SIGNATURE_RX);
    if (signatureMatch) {
      const name = signatureMatch[1];
      if (!TOP_LEVEL_DECL_KEYWORDS.has(name)) {
        const rows = signatureLinesByName.get(name) ?? [];
        rows.push(i);
        signatureLinesByName.set(name, rows);
      }
    }
    const fnMatch = line.match(TOP_LEVEL_FUNCTION_RX);
    if (!fnMatch) {
      continue;
    }
    const name = fnMatch[1];
    if (TOP_LEVEL_DECL_KEYWORDS.has(name)) {
      continue;
    }
    functionStarts.push({ name, start: i });
  }

  const functionBlocks = new Map();
  for (let i = 0; i < functionStarts.length; i += 1) {
    const current = functionStarts[i];
    const next = functionStarts[i + 1];
    const end = next ? next.start : lines.length;
    const blockSource = lines.slice(current.start, end).join("\n");
    functionBlocks.set(current.name, {
      start: current.start,
      end,
      refs: collectIdentifierRefs(blockSource),
    });
  }

  return {
    path: toAbsolutePath(path),
    source: String(source),
    moduleName,
    imports,
    exports,
    signatureLinesByName,
    functionBlocks,
    resolvedImports: new Map(),
  };
}

function moduleQualifierCandidates(moduleName) {
  if (typeof moduleName !== "string" || moduleName.length === 0) {
    return [];
  }
  const out = [moduleName];
  const parts = moduleName.split(".");
  if (parts.length > 1) {
    out.push(parts[parts.length - 1]);
  }
  return [...new Set(out)];
}

function functionKey(modulePath, functionName) {
  return `${modulePath}::${functionName}`;
}

function splitFunctionKey(key) {
  const idx = key.lastIndexOf("::");
  if (idx < 0) {
    return { modulePath: "", functionName: key };
  }
  return {
    modulePath: key.slice(0, idx),
    functionName: key.slice(idx + 2),
  };
}

function resolveTokenFunctionDeps(token, moduleInfo, modulesByPath) {
  const deps = [];
  if (token.includes(".")) {
    const parts = token.split(".");
    const functionName = parts.pop() ?? "";
    const qualifier = parts.join(".");
    const candidateModulePaths = new Set();
    for (
      const localQualifier of moduleQualifierCandidates(moduleInfo.moduleName)
    ) {
      if (localQualifier === qualifier) {
        candidateModulePaths.add(moduleInfo.path);
      }
    }
    for (
      const [importName, importPath] of moduleInfo.resolvedImports.entries()
    ) {
      if (typeof importPath !== "string" || importPath.length === 0) {
        continue;
      }
      for (const candidate of moduleQualifierCandidates(importName)) {
        if (candidate === qualifier) {
          candidateModulePaths.add(importPath);
        }
      }
    }
    for (const modulePath of candidateModulePaths) {
      const info = modulesByPath.get(modulePath);
      if (info && info.functionBlocks.has(functionName)) {
        deps.push(functionKey(modulePath, functionName));
      }
    }
    return deps;
  }

  if (moduleInfo.functionBlocks.has(token)) {
    deps.push(functionKey(moduleInfo.path, token));
  }
  for (const [importName, importPath] of moduleInfo.resolvedImports.entries()) {
    if (typeof importPath !== "string" || importPath.length === 0) {
      continue;
    }
    const importedInfo = modulesByPath.get(importPath);
    if (!importedInfo || !importedInfo.functionBlocks.has(token)) {
      continue;
    }
    const exported = importedInfo.exports.size === 0 ||
      importedInfo.exports.has(token);
    if (!exported) {
      continue;
    }
    deps.push(functionKey(importPath, token));
  }
  return deps;
}

function deriveEntrypointRoots(entryModuleInfo, rootExportsOverride = []) {
  const roots = [];
  const addRoot = (name) => {
    if (!entryModuleInfo.functionBlocks.has(name)) {
      return;
    }
    if (!roots.includes(name)) {
      roots.push(name);
    }
  };
  const requestedRoots = normalizeEntrypointExportRoots(rootExportsOverride);
  if (requestedRoots.length > 0) {
    for (const rootName of requestedRoots) {
      addRoot(rootName);
    }
    if (roots.length > 0) {
      return roots;
    }
  }

  if (entryModuleInfo.exports.size > 0) {
    for (const exportName of entryModuleInfo.exports) {
      addRoot(exportName);
    }
  } else if (entryModuleInfo.functionBlocks.has("main")) {
    addRoot("main");
  } else {
    for (const name of entryModuleInfo.functionBlocks.keys()) {
      addRoot(name);
    }
  }
  return roots;
}

function pruneModuleSource(moduleInfo, reachableFunctionNames) {
  const lines = moduleInfo.source.split("\n");
  const keep = Array.from({ length: lines.length }, () => true);
  for (const [name, block] of moduleInfo.functionBlocks.entries()) {
    if (reachableFunctionNames.has(name)) {
      continue;
    }
    for (let i = block.start; i < block.end; i += 1) {
      keep[i] = false;
    }
    const signatureLines = moduleInfo.signatureLinesByName.get(name) ?? [];
    for (const lineIndex of signatureLines) {
      keep[lineIndex] = false;
    }
  }

  for (let i = 0; i < lines.length; i += 1) {
    const match = lines[i].match(TOP_LEVEL_EXPORT_RX);
    if (!match) {
      continue;
    }
    const keptExportNames = parseExportNamesFromRaw(match[1])
      .filter((name) => reachableFunctionNames.has(name));
    if (keptExportNames.length === 0) {
      keep[i] = false;
      continue;
    }
    lines[i] = `export ${keptExportNames.join(", ")}`;
  }

  const filtered = [];
  for (let i = 0; i < lines.length; i += 1) {
    if (keep[i]) {
      filtered.push(lines[i]);
    }
  }
  const joined = filtered.join("\n");
  return moduleInfo.source.endsWith("\n") ? `${joined}\n` : joined;
}

async function resolveImportModulePath(
  moduleName,
  importerPath,
  moduleSearchDirs,
) {
  if (moduleName.startsWith("host.")) {
    return "";
  }
  for (const dir of moduleSearchDirs) {
    const candidate = candidateModulePath(moduleName, dir);
    if (await fileExists(candidate)) {
      return toAbsolutePath(candidate);
    }
  }
  const importerDir = pathDir(importerPath);
  if (importerDir.length > 0) {
    const candidate = candidateModulePath(moduleName, importerDir);
    if (await fileExists(candidate)) {
      return toAbsolutePath(candidate);
    }
  }
  return "";
}

async function buildCompileReachabilityPlan(
  inputPath,
  inputSource,
  projectConfig,
  options = {},
) {
  const entryPath = toAbsolutePath(inputPath);
  const moduleSearchDirs = Array.isArray(projectConfig?.moduleSearchDirs)
    ? projectConfig.moduleSearchDirs
    : [];
  const modulesByPath = new Map();

  const loadModuleByPath = async (path, sourceOverride = null) => {
    const absPath = toAbsolutePath(path);
    if (modulesByPath.has(absPath)) {
      return modulesByPath.get(absPath);
    }
    const sourceText = typeof sourceOverride === "string"
      ? sourceOverride
      : await Deno.readTextFile(absPath);
    const descriptor = parseModuleSourceDescriptor(absPath, sourceText);
    modulesByPath.set(absPath, descriptor);
    for (const importName of descriptor.imports) {
      const importPath = await resolveImportModulePath(
        importName,
        descriptor.path,
        moduleSearchDirs,
      );
      if (importPath.length === 0) {
        continue;
      }
      descriptor.resolvedImports.set(importName, importPath);
      await loadModuleByPath(importPath);
    }
    return descriptor;
  };

  let entryModuleInfo;
  try {
    entryModuleInfo = await loadModuleByPath(entryPath, inputSource);
  } catch {
    return null;
  }
  if (!entryModuleInfo || entryModuleInfo.functionBlocks.size === 0) {
    return null;
  }

  const roots = deriveEntrypointRoots(
    entryModuleInfo,
    options.rootExportsOverride,
  );
  if (roots.length === 0) {
    return null;
  }

  const depsByFunction = new Map();
  for (const moduleInfo of modulesByPath.values()) {
    for (const [name, block] of moduleInfo.functionBlocks.entries()) {
      const key = functionKey(moduleInfo.path, name);
      const deps = new Set();
      for (const token of block.refs) {
        const tokenDeps = resolveTokenFunctionDeps(
          token,
          moduleInfo,
          modulesByPath,
        );
        for (const dep of tokenDeps) {
          deps.add(dep);
        }
      }
      depsByFunction.set(key, deps);
    }
  }

  const reachableFunctionKeys = new Set();
  const queue = [];
  for (const rootName of roots) {
    queue.push(functionKey(entryModuleInfo.path, rootName));
  }
  while (queue.length > 0) {
    const key = queue.pop();
    if (!key || reachableFunctionKeys.has(key)) {
      continue;
    }
    reachableFunctionKeys.add(key);
    const deps = depsByFunction.get(key);
    if (!deps) {
      continue;
    }
    for (const dep of deps) {
      if (!reachableFunctionKeys.has(dep)) {
        queue.push(dep);
      }
    }
  }

  const reachableByModulePath = new Map();
  for (const key of reachableFunctionKeys) {
    const { modulePath, functionName } = splitFunctionKey(key);
    if (!reachableByModulePath.has(modulePath)) {
      reachableByModulePath.set(modulePath, new Set());
    }
    reachableByModulePath.get(modulePath).add(functionName);
  }

  const sourceByPath = new Map();
  let totalPrunedFunctions = 0;
  let totalReachableFunctions = 0;
  for (const moduleInfo of modulesByPath.values()) {
    const reachableNames = reachableByModulePath.get(moduleInfo.path) ??
      new Set();
    totalReachableFunctions += reachableNames.size;
    totalPrunedFunctions += Math.max(
      0,
      moduleInfo.functionBlocks.size - reachableNames.size,
    );
    sourceByPath.set(
      moduleInfo.path,
      pruneModuleSource(moduleInfo, reachableNames),
    );
  }

  const moduleSources = [];
  const sortedModules = [...modulesByPath.values()]
    .sort((a, b) => a.path.localeCompare(b.path, "en"));
  for (const moduleInfo of sortedModules) {
    if (moduleInfo.path === entryModuleInfo.path) {
      continue;
    }
    const reachableNames = reachableByModulePath.get(moduleInfo.path) ??
      new Set();
    if (reachableNames.size === 0) {
      continue;
    }
    moduleSources.push({
      module_name: moduleInfo.moduleName,
      input_path: moduleInfo.path,
      input_source: sourceByPath.get(moduleInfo.path) ?? moduleInfo.source,
      reachable_functions: [...reachableNames].sort((a, b) =>
        a.localeCompare(b, "en")
      ),
    });
  }

  return {
    entryPath: entryModuleInfo.path,
    entryModuleName: entryModuleInfo.moduleName,
    rootExports: roots,
    sourceByPath,
    moduleSources,
    summary: {
      modules_scanned: modulesByPath.size,
      reachable_functions: totalReachableFunctions,
      pruned_functions: totalPrunedFunctions,
    },
  };
}

export async function buildCompileReachabilityPlanForTest(
  inputPath,
  options = {},
) {
  const normalizedInputPath = String(inputPath ?? "").trim();
  if (normalizedInputPath.length === 0) {
    throw new Error("buildCompileReachabilityPlanForTest requires inputPath");
  }
  const inputSource = typeof options.inputSource === "string"
    ? options.inputSource
    : await Deno.readTextFile(normalizedInputPath);
  const projectConfig = options.projectConfig ??
    await readClapseProjectConfig(normalizedInputPath);
  return await buildCompileReachabilityPlan(
    normalizedInputPath,
    inputSource,
    projectConfig,
    {
      rootExportsOverride: normalizeEntrypointExportRoots(
        options.entrypointExports,
      ),
    },
  );
}

async function compilePluginsWasm(wasmPath, inputPath, options = {}) {
  const projectConfig = options.projectConfig ??
    await readClapseProjectConfig(inputPath);
  const { pluginDirs } = projectConfig;
  if (pluginDirs.length === 0) {
    return [];
  }
  const sourceOverrides = options.sourceOverrides instanceof Map
    ? options.sourceOverrides
    : new Map();
  const pluginSources = [];
  const seen = new Set();
  for (const pluginDir of pluginDirs) {
    await collectClapseFilesRecursively(pluginDir, pluginSources, seen);
  }
  const uniqueSources = [...new Set(pluginSources)]
    .sort((a, b) => a.localeCompare(b, "en"));
  const pluginWasmPaths = [];
  for (const sourcePath of uniqueSources) {
    const outputPath = sourcePath.endsWith(".clapse")
      ? sourcePath.replace(/\.clapse$/u, ".wasm")
      : `${sourcePath}.wasm`;
    const overrideSource = sourceOverrides.get(toAbsolutePath(sourcePath));
    await compileViaWasm(wasmPath, sourcePath, outputPath, {
      pluginWasmPaths: [],
      skipPluginCompilation: true,
      skipEntrypointReachabilityPrune: true,
      inputSourceOverride: overrideSource,
    });
    pluginWasmPaths.push(toAbsolutePath(outputPath));
  }
  return pluginWasmPaths;
}

async function fileExists(path) {
  try {
    await Deno.stat(path);
    return true;
  } catch {
    return false;
  }
}

async function materializeEmbeddedCompilerWasm() {
  const embeddedCandidates = [
    new URL("../artifacts/latest/clapse_compiler.wasm", import.meta.url),
    new URL("../out/clapse_compiler.wasm", import.meta.url),
  ];
  for (const candidateUrl of embeddedCandidates) {
    try {
      const wasmBytes = await Deno.readFile(candidateUrl);
      if (wasmBytes.length === 0) continue;
      const tmpPath = await Deno.makeTempFile({
        prefix: "clapse-embedded-compiler-",
        suffix: ".wasm",
      });
      await Deno.writeFile(tmpPath, wasmBytes);
      return tmpPath;
    } catch {
      // keep searching
    }
  }
  return "";
}

function collectCompilerWasmCandidatesFromCwd() {
  const candidates = [];
  const seenDirs = new Set();
  let dir = normalizePath(Deno.cwd());
  if (dir.length === 0) {
    return candidates;
  }
  while (dir.length > 0 && !seenDirs.has(dir)) {
    seenDirs.add(dir);
    candidates.push(joinPath(dir, "artifacts/latest/clapse_compiler.wasm"));
    candidates.push(joinPath(dir, "out/clapse_compiler.wasm"));
    const parent = pathDir(dir);
    if (parent === dir || parent.length === 0) {
      break;
    }
    dir = parent;
  }
  return candidates;
}

async function resolveCompilerWasmPath() {
  const fromEnv = Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "";
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  const candidates = [
    ...collectCompilerWasmCandidatesFromCwd(),
    toPath(new URL("artifacts/latest/clapse_compiler.wasm", REPO_ROOT_URL)),
    toPath(new URL("out/clapse_compiler.wasm", REPO_ROOT_URL)),
  ];
  for (const candidate of candidates) {
    if (await fileExists(candidate)) {
      return candidate;
    }
  }
  return materializeEmbeddedCompilerWasm();
}

function usage() {
  return [
    "Usage:",
    "  deno run -A scripts/run-clapse-compiler-wasm.mjs <clapse-args...>",
    "",
    "Supported commands:",
    "  compile <input.clapse> [output.wasm]",
    "  compile-native <input.clapse> [output.wasm] (alias: compile_native)",
    "  compile-native-debug <input.clapse> [output.wasm] [artifacts-dir] (alias: compile_native_debug)",
    "  compile-debug <input.clapse> [output.wasm] [artifacts-dir] (alias: compile_debug)",
    "  emit-wat <input.clapse> [output.wat]",
    "  selfhost-artifacts <input.clapse> <out-dir>",
    "  format <file>",
    "  format --write <file>",
    "  format --stdin",
    "  lsp [--stdio]",
    "  engine-mode",
    "",
    "Compiler wasm resolution:",
    "  1) CLAPSE_COMPILER_WASM_PATH",
    "  2) artifacts/latest|out/clapse_compiler.wasm from cwd/ancestor dirs",
    "  3) artifacts/latest|out/clapse_compiler.wasm relative to script repo root",
    "  4) embedded compiler wasm bundled in the clapse binary",
    "",
    "Required compiler wasm ABI:",
    "  export memory or __memory",
    "  export clapse_run(request_slice_handle: i32) -> response_slice_handle: i32",
    "  Request and response are UTF-8 JSON in slice descriptors.",
    "",
    "Temporary bootstrap seed mode:",
    "  Set CLAPSE_USE_WASM_BOOTSTRAP_SEED=1 to route compile requests through",
    "  scripts/wasm-bootstrap-seed.mjs using a trusted seed wasm payload.",
    "",
    "Entrypoint reachability pruning:",
    "  compile requests are pruned at compiler ABI dispatch before wasm compile",
    "  (entrypoint exports as roots; fallback root is main).",
    "  CLAPSE_ENTRYPOINT_DCE and CLAPSE_INTERNAL_ENTRYPOINT_DCE are legacy",
    "  compatibility toggles and no longer disable compile dispatch pruning.",
  ].join("\n");
}

const COMMAND_ALIAS_MAP = new Map([
  ["compile_native", "compile-native"],
  ["compile_native_debug", "compile-native-debug"],
  ["compile_debug", "compile-debug"],
]);

function normalizeTopLevelCommand(raw) {
  const command = String(raw ?? "").trim();
  return COMMAND_ALIAS_MAP.get(command) ?? command;
}

const SELFHOST_ARTIFACT_FILES = [
  "lowered_ir.txt",
  "collapsed_ir.txt",
  "compile_response.json",
  "backend.txt",
];
const COMPILE_DEBUG_ARTIFACT_FILES = ["lowered_ir.txt", "collapsed_ir.txt"];

function isPlaceholderSelfhostArtifactsResponse(response) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return true;
  }
  if (response.ok !== true) {
    return true;
  }
  const artifacts = response.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    return true;
  }
  for (const file of SELFHOST_ARTIFACT_FILES) {
    if (file === "compile_response.json" || file === "backend.txt") {
      continue;
    }
    if (typeof artifacts[file] !== "string" || artifacts[file].length === 0) {
      return true;
    }
  }
  return false;
}

function renderTypeScriptBindings(exportsList) {
  if (!Array.isArray(exportsList) || exportsList.length === 0) {
    return "export {}\n";
  }
  const lines = [];
  for (const item of exportsList) {
    if (!item || typeof item.name !== "string") continue;
    const arity = Number(item.arity ?? 0);
    const args = Array.from(
      { length: Math.max(0, arity) },
      (_, i) => `arg${i}: number`,
    ).join(", ");
    lines.push(`export declare function ${item.name}(${args}): number;`);
  }
  return lines.length > 0 ? `${lines.join("\n")}\n` : "export {}\n";
}

function assertObject(value, ctx) {
  if (!value || typeof value !== "object" || Array.isArray(value)) {
    throw new Error(`${ctx}: expected object`);
  }
}

function assertCompileExportEntry(entry, idx) {
  if (!entry || typeof entry !== "object" || Array.isArray(entry)) {
    throw new Error(`compile response: exports[${idx}] must be an object`);
  }
  if (typeof entry.name !== "string" || entry.name.length === 0) {
    throw new Error(
      `compile response: exports[${idx}].name must be a non-empty string`,
    );
  }
  if (!Number.isInteger(entry.arity) || entry.arity < 0) {
    throw new Error(
      `compile response: exports[${idx}].arity must be a non-negative integer`,
    );
  }
}

function decodeCompileResponse(response, inputPath) {
  assertObject(response, "compile response");
  if (typeof response.ok !== "boolean") {
    throw new Error("compile response: missing boolean 'ok'");
  }
  if (response.ok !== true) {
    const mode = String(response.backend ?? "unknown");
    const backend = mode.length > 0 && mode !== "unknown"
      ? ` [backend=${mode}]`
      : "";
    const rawError = response.error;
    const err = rawError === undefined
      ? "unknown compiler error"
      : typeof rawError === "string"
      ? rawError
      : (() => {
        try {
          return JSON.stringify(rawError);
        } catch {
          return String(rawError);
        }
      })();
    throw new Error(`compile${backend} failed for ${inputPath}: ${err}`);
  }
  if (
    typeof response.wasm_base64 !== "string" ||
    response.wasm_base64.length === 0
  ) {
    throw new Error("compile response: missing non-empty 'wasm_base64'");
  }
  if (response.exports !== undefined && !Array.isArray(response.exports)) {
    throw new Error(
      "compile response: 'exports' must be an array when present",
    );
  }
  if (Array.isArray(response.exports)) {
    for (let i = 0; i < response.exports.length; i += 1) {
      assertCompileExportEntry(response.exports[i], i);
    }
  }
  if (response.dts !== undefined && typeof response.dts !== "string") {
    throw new Error("compile response: 'dts' must be a string when present");
  }
  return response;
}

function decodeSelfhostArtifactsResponse(response, inputPath) {
  assertObject(response, "selfhost-artifacts response");
  if (typeof response.ok !== "boolean") {
    throw new Error("selfhost-artifacts response: missing boolean 'ok'");
  }
  if (response.ok !== true) {
    const rawError = response.error;
    const err = rawError === undefined
      ? "unknown compiler error"
      : typeof rawError === "string"
      ? rawError
      : (() => {
        try {
          return JSON.stringify(rawError);
        } catch {
          return String(rawError);
        }
      })();
    throw new Error(`selfhost-artifacts failed for ${inputPath}: ${err}`);
  }
  const artifacts = collectCompileDebugArtifacts(
    response.artifacts,
    "selfhost-artifacts response",
  );
  const backend =
    typeof response.backend === "string" && response.backend.length > 0
      ? response.backend
      : "kernel-selfhost";
  return {
    response: { ...response, backend },
    artifacts,
  };
}

function shouldInjectFuncMapInFixedPointArtifacts() {
  const raw = String(Deno.env.get("CLAPSE_DEBUG_FUNC_MAP") ?? "").trim()
    .toLowerCase();
  return raw === "1" || raw === "true" || raw === "yes";
}

function isCompilerKernelPath(inputPath) {
  const normalized = String(inputPath).replaceAll("\\", "/");
  return normalized === "lib/compiler/kernel.clapse" ||
    normalized.endsWith("/lib/compiler/kernel.clapse");
}

async function writeCompileArtifacts(outputPath, response, options = {}) {
  let wasmBytes = decodeWasmBase64(response.wasm_base64);
  const skipTailCallRewrite = options.skipTailCallRewrite === true;
  if (!skipTailCallRewrite) {
    const tailCallRewrite = rewriteWasmTailCallOpcodes(wasmBytes);
    wasmBytes = tailCallRewrite.wasmBytes;
  }
  if (shouldInjectFuncMapInFixedPointArtifacts()) {
    try {
      wasmBytes = appendClapseFuncMap(wasmBytes);
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      throw new Error(`compile artifact funcmap injection failed: ${msg}`);
    }
  }
  const outputDir = outputPath.includes("/")
    ? outputPath.slice(0, outputPath.lastIndexOf("/"))
    : ".";
  if (outputDir.length > 0 && outputDir !== ".") {
    await Deno.mkdir(outputDir, { recursive: true });
  }
  await Deno.writeFile(outputPath, wasmBytes);
  const dtsPath = outputPath.replace(/\.wasm$/u, ".d.ts");
  const dts = typeof response.dts === "string"
    ? response.dts
    : renderTypeScriptBindings(
      Array.isArray(response.exports) ? response.exports : [],
    );
  await Deno.writeTextFile(dtsPath, dts);
}

async function writeSelfhostArtifacts(outDir, artifacts) {
  await Deno.mkdir(outDir, { recursive: true });
  for (const file of ["lowered_ir.txt", "collapsed_ir.txt"]) {
    const value = artifacts[file];
    if (value === undefined || typeof value !== "string") {
      throw new Error(
        `selfhost-artifacts response: '${file}' must be a string`,
      );
    }
    await Deno.writeTextFile(`${outDir}/${file}`, value);
  }
}

function collectCompileDebugArtifacts(artifacts, contextLabel) {
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    throw new Error(`${contextLabel} must include object 'artifacts'`);
  }
  const out = {};
  const missing = [];
  for (const key of COMPILE_DEBUG_ARTIFACT_FILES) {
    if (typeof artifacts[key] !== "string") {
      missing.push(key);
      continue;
    }
    out[key] = artifacts[key];
  }
  if (missing.length > 0) {
    throw new Error(
      `${contextLabel} missing required debug artifacts. Expected response.artifacts keys: ${
        COMPILE_DEBUG_ARTIFACT_FILES.join(", ")
      }.`,
    );
  }
  return out;
}

function readCompileDebugArtifacts(response) {
  const artifacts = response?.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    return null;
  }
  return collectCompileDebugArtifacts(
    artifacts,
    "compile response for compile_mode debug",
  );
}

function isKnownStubCompileArtifact(wasmBytes, response) {
  if (!(wasmBytes instanceof Uint8Array) || wasmBytes.length !== 122) {
    return false;
  }
  const exportsList = Array.isArray(response?.exports) ? response.exports : [];
  const dts = typeof response?.dts === "string" ? response.dts.trim() : "";
  return exportsList.length === 0 && (dts.length === 0 || dts === "export {}");
}

async function writeCompileDebugArtifacts(artifactsDir, artifacts) {
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    throw new Error("writeCompileDebugArtifacts requires artifact map");
  }
  await Deno.mkdir(artifactsDir, { recursive: true });
  for (const key of COMPILE_DEBUG_ARTIFACT_FILES) {
    await Deno.writeTextFile(`${artifactsDir}/${key}`, artifacts[key]);
  }
}

async function compileViaWasm(wasmPath, inputPath, outputPath, options = {}) {
  const inputSource = typeof options.inputSourceOverride === "string"
    ? options.inputSourceOverride
    : new TextDecoder().decode(await Deno.readFile(inputPath));
  const isKernelCompile = isCompilerKernelPath(inputPath);
  const projectConfig = options.projectConfig ??
    await readClapseProjectConfig(inputPath);
  const pluginWasmPaths = Array.isArray(options.pluginWasmPaths)
    ? options.pluginWasmPaths
    : (!options.skipPluginCompilation && !isKernelCompile
      ? await compilePluginsWasm(wasmPath, inputPath, {
        projectConfig,
      })
      : []);
  const request = {
    command: "compile",
    input_path: inputPath,
    input_source: inputSource,
    plugin_wasm_paths: pluginWasmPaths,
  };
  if (
    typeof options.compileMode === "string" && options.compileMode.length > 0
  ) {
    request.compile_mode = options.compileMode;
  }
  const response = isWasmBootstrapSeedEnabled()
    ? await buildWasmSeedCompileResponse(request, { seedWasmPath: wasmPath })
    : await callCompilerWasm(wasmPath, request);
  const decodedResponse = decodeCompileResponse(response, inputPath);
  const decodedWasmBytes = decodeWasmBase64(decodedResponse.wasm_base64);
  const compileMode = typeof options.compileMode === "string"
    ? options.compileMode
    : "";
  const isNativeCompileMode = compileMode === "kernel-native" ||
    compileMode === "native-debug" ||
    compileMode === "debug";
  if (isNativeCompileMode && decodedResponse.backend !== "kernel-native") {
    throw new Error(
      `compile response for ${inputPath} returned backend=${decodedResponse.backend}; expected backend=kernel-native`,
    );
  }
  if (isNativeCompileMode && isKernelCompile) {
    if (decodedWasmBytes.length < 4096) {
      throw new Error(
        `compile response for ${inputPath} is a stub artifact; configure a native clapse compiler wasm and remove stub fallback mode`,
      );
    }
  }
  if (isNativeCompileMode && isKernelCompile) {
    if (isKnownStubCompileArtifact(decodedWasmBytes, decodedResponse)) {
      throw new Error(
        `compile response for ${inputPath} is a known placeholder stub artifact; configure a native clapse compiler wasm and remove stub fallback mode`,
      );
    }
  }
  let debugArtifacts;
  const hasArtifactsDir = typeof options.artifactsDir === "string" &&
    options.artifactsDir.length > 0;
  const requireCompileArtifacts = options.requireCompileArtifacts === true;
  if (hasArtifactsDir) {
    debugArtifacts = readCompileDebugArtifacts(decodedResponse);
    if (requireCompileArtifacts && debugArtifacts === null) {
      throw new Error(
        `compile response for ${inputPath} missing required debug artifacts object`,
      );
    }
  }
  await writeCompileArtifacts(outputPath, decodedResponse);
  if (debugArtifacts) {
    await writeCompileDebugArtifacts(options.artifactsDir, debugArtifacts);
  }
}

async function writeSelfhostArtifactsViaWasm(wasmPath, inputPath, outDir) {
  const inputSource = await Deno.readTextFile(inputPath);
  const response = await callCompilerWasm(wasmPath, {
    command: "selfhost-artifacts",
    compile_mode: "kernel-native",
    input_path: inputPath,
    input_source: inputSource,
    plugin_wasm_paths: [],
  });
  const decoded = decodeSelfhostArtifactsResponse(response, inputPath);
  const decodedResponse = decoded.response;
  const debugArtifacts = decoded.artifacts;
  const isPlaceholderResponse = decodedResponse.ok === true &&
    isPlaceholderSelfhostArtifactsResponse(decodedResponse);
  if (isPlaceholderResponse) {
    throw new Error(
      `selfhost-artifacts response for ${inputPath} is placeholder/incomplete; expected native kernel artifacts`,
    );
  }
  await writeSelfhostArtifacts(outDir, debugArtifacts);
  await Deno.writeTextFile(
    `${outDir}/compile_response.json`,
    `${JSON.stringify(decodedResponse, null, 2)}\n`,
  );
  await Deno.writeTextFile(
    `${outDir}/backend.txt`,
    `${String(decodedResponse.backend ?? "")}\n`,
  );
}

async function readAllStdin() {
  const chunks = [];
  const buf = new Uint8Array(16 * 1024);
  while (true) {
    const n = await Deno.stdin.read(buf);
    if (n === null) break;
    chunks.push(buf.slice(0, n));
  }
  const total = chunks.reduce((acc, c) => acc + c.length, 0);
  const out = new Uint8Array(total);
  let offset = 0;
  for (const c of chunks) {
    out.set(c, offset);
    offset += c.length;
  }
  return new TextDecoder().decode(out);
}

function decodeFormatResponse(response, ctx) {
  assertObject(response, "format response");
  if (typeof response.ok !== "boolean") {
    throw new Error("format response: missing boolean 'ok'");
  }
  if (!response.ok) {
    const err = typeof response.error === "string"
      ? response.error
      : `format error: ${ctx}`;
    throw new Error(err);
  }
  if (typeof response.formatted !== "string") {
    throw new Error("format response: missing string 'formatted'");
  }
  return response.formatted;
}

function decodeEmitWatResponse(response, ctx) {
  assertObject(response, "emit-wat response");
  if (typeof response.ok !== "boolean") {
    throw new Error("emit-wat response: missing boolean 'ok'");
  }
  if (!response.ok) {
    const err = typeof response.error === "string"
      ? response.error
      : `emit-wat error: ${ctx}`;
    throw new Error(err);
  }
  if (typeof response.wat !== "string") {
    throw new Error("emit-wat response: missing string 'wat'");
  }
  return response.wat;
}

async function emitWatViaWasm(wasmPath, args) {
  if (args.length < 2 || args.length > 3) {
    throw new Error("usage: emit-wat <input.clapse> [output.wat]");
  }
  const inputPath = args[1];
  const inputSource = await Deno.readTextFile(inputPath);
  const response = await callCompilerWasm(wasmPath, {
    command: "emit-wat",
    input_path: inputPath,
    input_source: inputSource,
  });
  const wat = decodeEmitWatResponse(response, inputPath);
  if (args.length === 3) {
    const outputPath = args[2];
    const outputDir = pathDir(outputPath);
    if (outputDir.length > 0 && outputDir !== ".") {
      await Deno.mkdir(outputDir, { recursive: true });
    }
    await Deno.writeTextFile(outputPath, wat);
    return;
  }
  await Deno.stdout.write(new TextEncoder().encode(wat));
}

function stripFrameTokenPunctuation(rawToken) {
  return String(rawToken)
    .replace(/^[\s"'`]+/u, "")
    .replace(/[)\]},"';]+$/u, "");
}

function parseDenoWasmStackOffset(rawToken) {
  const token = stripFrameTokenPunctuation(rawToken);
  const wasmAt = token.indexOf("wasm://");
  if (wasmAt < 0) {
    return null;
  }
  const marker = token.slice(wasmAt);
  if (!marker.startsWith("wasm://")) {
    return null;
  }
  const lastColon = marker.lastIndexOf(":");
  if (lastColon <= 0 || lastColon === marker.length - 1) {
    return null;
  }
  const secondLastColon = marker.lastIndexOf(":", lastColon - 1);
  if (secondLastColon <= 0 || secondLastColon + 1 >= lastColon) {
    return null;
  }
  const lineText = marker.slice(secondLastColon + 1, lastColon);
  const offsetText = marker.slice(lastColon + 1);
  if (!/^\d+$/u.test(lineText) || !/^\d+$/u.test(offsetText)) {
    return null;
  }
  return Number.parseInt(offsetText, 10);
}

function extractWasmOffsetsFromStack(err) {
  const stack = typeof err?.stack === "string" ? err.stack : "";
  if (stack.length === 0) {
    return [];
  }
  const out = [];
  const seen = new Set();
  const pushOffset = (value) => {
    if (!Number.isFinite(value) || value <= 0) {
      return;
    }
    if (seen.has(value)) {
      return;
    }
    seen.add(value);
    out.push(value);
  };
  const framePattern = /wasm-function\[\d+\](?::0x([0-9a-fA-F]+)|\+(\d+))/g;
  for (const match of stack.matchAll(framePattern)) {
    let value = NaN;
    if (typeof match[1] === "string" && match[1].length > 0) {
      value = Number.parseInt(match[1], 16);
    } else if (typeof match[2] === "string" && match[2].length > 0) {
      value = Number.parseInt(match[2], 10);
    }
    pushOffset(value);
    if (out.length >= 24) {
      break;
    }
  }
  if (out.length < 24) {
    const denoWasmFramePattern = /wasm:\/\/[^\s)]+/g;
    for (const match of stack.matchAll(denoWasmFramePattern)) {
      const value = parseDenoWasmStackOffset(match[0]);
      if (value === null) {
        continue;
      }
      pushOffset(value);
      if (out.length >= 24) {
        break;
      }
    }
  }
  return out;
}

function buildFormatterStackMapHint(wasmPath, err) {
  const offsets = extractWasmOffsetsFromStack(err);
  if (offsets.length === 0) {
    return "";
  }
  const offsetCsv = offsets.join(",");
  return [
    "",
    `[clapse] formatter wasm stack offsets: ${offsetCsv}`,
    `[clapse] map with: deno run -A scripts/wasm-stack-map.mjs --wasm ${
      JSON.stringify(wasmPath)
    } --offsets ${offsetCsv}`,
  ].join("\n");
}

function isFormatterStackIdentityFallbackEnabled() {
  const raw = String(
    Deno.env.get("CLAPSE_FORMAT_STACK_IDENTITY_FALLBACK") ?? "",
  )
    .trim()
    .toLowerCase();
  if (raw === "0" || raw === "false" || raw === "off" || raw === "no") {
    return false;
  }
  return true;
}

async function formatRequestWithFallback(wasmPath, request, source, ctx) {
  try {
    const response = await callCompilerWasm(wasmPath, request);
    return decodeFormatResponse(response, ctx);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    const isStackOverflow = message.includes(
      "Maximum call stack size exceeded",
    );
    if (isStackOverflow) {
      const hint = buildFormatterStackMapHint(wasmPath, err);
      if (isFormatterStackIdentityFallbackEnabled()) {
        const lines = [
          `[clapse] formatter stack overflow in ${ctx}; returning input unchanged`,
          hint.trim(),
        ].filter((line) => line.length > 0);
        console.error(lines.join("\n"));
        return source;
      }
      if (err instanceof Error) {
        err.message = `format error: ${ctx}: ${message}${hint}`;
        throw err;
      }
      throw new Error(`format error: ${ctx}: ${message}${hint}`);
    }
    throw err;
  }
}

async function formatViaWasm(wasmPath, args) {
  if (args.length === 2 && args[1] === "--stdin") {
    const src = await readAllStdin();
    const formatted = await formatRequestWithFallback(
      wasmPath,
      {
        command: "format",
        mode: "stdout",
        input_path: "<stdin>",
        source: src,
      },
      src,
      "stdin",
    );
    await Deno.stdout.write(new TextEncoder().encode(formatted));
    return;
  }
  if (args.length === 3 && args[1] === "--write") {
    const path = args[2];
    const src = await Deno.readTextFile(path);
    const formatted = await formatRequestWithFallback(
      wasmPath,
      {
        command: "format",
        mode: "write",
        input_path: path,
        source: src,
      },
      src,
      path,
    );
    await Deno.writeTextFile(path, formatted);
    return;
  }
  if (args.length === 2) {
    const path = args[1];
    const src = await Deno.readTextFile(path);
    const formatted = await formatRequestWithFallback(
      wasmPath,
      {
        command: "format",
        mode: "stdout",
        input_path: path,
        source: src,
      },
      src,
      path,
    );
    await Deno.stdout.write(new TextEncoder().encode(formatted));
    return;
  }
  throw new Error(
    "usage: format <file> | format --write <file> | format --stdin",
  );
}

export async function runWithArgs(rawArgs = cliArgs()) {
  let args = [...rawArgs];
  if (args.length > 0 && args[0] === "--") {
    args = args.slice(1);
  }
  if (args.length > 0) {
    args[0] = normalizeTopLevelCommand(args[0]);
  }
  const wasmPath = await resolveCompilerWasmPath();
  if (args.length === 1 && args[0] === "engine-mode") {
    if (wasmPath.length > 0 && await fileExists(wasmPath)) {
      await validateCompilerWasmAbi(wasmPath);
      console.log("wasm-native");
      return;
    }
    console.log("unwired");
    return;
  }
  if (args.length === 0 || args[0] === "--help" || args[0] === "-h") {
    console.log(usage());
    return;
  }
  if (wasmPath.length === 0) {
    throw new Error(
      "compiler-wasm engine requires CLAPSE_COMPILER_WASM_PATH, embedded compiler wasm, or artifacts/latest|out compiler wasm files",
    );
  }
  if (!(await fileExists(wasmPath))) {
    throw new Error(
      `compiler-wasm path not found: ${wasmPath}. Set CLAPSE_COMPILER_WASM_PATH to an existing artifact.`,
    );
  }
  await validateCompilerWasmAbi(wasmPath);
  if (args[0] === "compile") {
    if (args.length < 2 || args.length > 3) {
      throw new Error("usage: compile <input.clapse> [output.wasm]");
    }
    const inputPath = args[1];
    const outputPath = args[2] ?? inputPath.replace(/\.clapse$/u, ".wasm");
    const compileEngine = String(Deno.env.get("CLAPSE_COMPILE_ENGINE") ?? "")
      .trim()
      .toLowerCase();
    if (
      compileEngine.length > 0 && compileEngine !== "native" &&
      compileEngine !== "kernel-native"
    ) {
      throw new Error(
        `unsupported CLAPSE_COMPILE_ENGINE=${compileEngine}; set kernel-native or unset CLAPSE_COMPILE_ENGINE`,
      );
    }
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "kernel-native",
    });
    return;
  }
  if (args[0] === "compile-native") {
    if (args.length < 2 || args.length > 3) {
      throw new Error("usage: compile-native <input.clapse> [output.wasm]");
    }
    const inputPath = args[1];
    const outputPath = args[2] ?? inputPath.replace(/\.clapse$/u, ".wasm");
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "kernel-native",
    });
    return;
  }
  if (args[0] === "compile-native-debug") {
    if (args.length < 2 || args.length > 4) {
      throw new Error(
        "usage: compile-native-debug <input.clapse> [output.wasm] [artifacts-dir]",
      );
    }
    const inputPath = args[1];
    const outputPath = args[2] ?? inputPath.replace(/\.clapse$/u, ".wasm");
    const defaultArtifactsDir = pathDir(outputPath);
    const artifactsDir = args[3] ??
      (defaultArtifactsDir.length > 0 ? defaultArtifactsDir : ".");
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "native-debug",
      requireCompileArtifacts: true,
      artifactsDir,
    });
    return;
  }
  if (args[0] === "compile-debug") {
    if (args.length < 2 || args.length > 4) {
      throw new Error(
        "usage: compile-debug <input.clapse> [output.wasm] [artifacts-dir]",
      );
    }
    const inputPath = args[1];
    const outputPath = args[2] ?? inputPath.replace(/\.clapse$/u, ".wasm");
    const defaultArtifactsDir = pathDir(outputPath);
    const artifactsDir = args[3] ??
      (defaultArtifactsDir.length > 0 ? defaultArtifactsDir : ".");
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "debug",
      requireCompileArtifacts: true,
      artifactsDir,
    });
    return;
  }
  if (args[0] === "emit-wat") {
    await emitWatViaWasm(wasmPath, args);
    return;
  }
  if (args[0] === "format") {
    await formatViaWasm(wasmPath, args);
    return;
  }
  if (args[0] === "lsp") {
    if (args.length > 2 || (args.length === 2 && args[1] !== "--stdio")) {
      throw new Error("usage: lsp [--stdio]");
    }
    Deno.env.set("CLAPSE_COMPILER_WASM_PATH", wasmPath);
    await runLspServer();
    return;
  }
  if (args[0] === "selfhost-artifacts") {
    if (args.length !== 3) {
      throw new Error("usage: selfhost-artifacts <input.clapse> <out-dir>");
    }
    await writeSelfhostArtifactsViaWasm(wasmPath, args[1], args[2]);
    return;
  }
  throw new Error(
    `unsupported command for wasm compiler runner: ${args.join(" ")}`,
  );
}

if (import.meta.main) {
  await runWithArgs().catch(failWithError);
}
