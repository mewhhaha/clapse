const PROJECT_CONFIG_FILE = "clapse.json";
export const CLAPSE_ENTRYPOINT_DCE_ENV = "CLAPSE_ENTRYPOINT_DCE";
export const CLAPSE_ENTRYPOINT_DCE_FORCE_ENV = "CLAPSE_ENTRYPOINT_DCE_FORCE";
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

function boolEnvFlag(name, defaultValue = false) {
  const raw = String(Deno.env.get(name) ?? "").trim().toLowerCase();
  if (raw.length === 0) {
    return defaultValue;
  }
  return !(raw === "0" || raw === "false" || raw === "off" || raw === "no");
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

export async function readClapseProjectConfig(startPath = "") {
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

async function fileExists(path) {
  try {
    await Deno.stat(path);
    return true;
  } catch {
    return false;
  }
}

function isCompilerInternalInputPath(inputPath) {
  const normalized = normalizePath(inputPath);
  return normalized === "lib/compiler/kernel.clapse" ||
    normalized.startsWith("lib/compiler/") ||
    normalized.includes("/lib/compiler/");
}

export function shouldRunEntrypointDce(inputPath, options = {}) {
  if (options.skipEntrypointReachabilityPrune === true) {
    return false;
  }
  const enabled = options.forceEnable === true
    ? true
    : boolEnvFlag(CLAPSE_ENTRYPOINT_DCE_ENV, true);
  if (!enabled) {
    return false;
  }
  if (options.skipCompilerInternalPathGuard === true) {
    return true;
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
    const keptExportNames = parseExportNamesFromRaw(match[1]).filter((name) =>
      reachableFunctionNames.has(name)
    );
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

export async function buildCompileReachabilityPlan(
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
  const sortedModules = [...modulesByPath.values()].sort((a, b) =>
    a.path.localeCompare(b.path, "en")
  );
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

function compileCommandOf(requestObject) {
  return String(requestObject?.command ?? "").trim().toLowerCase();
}

function hasReachabilityData(requestObject) {
  if (!requestObject || typeof requestObject !== "object") {
    return false;
  }
  if (
    Array.isArray(requestObject.module_sources) &&
    requestObject.module_sources.length > 0
  ) {
    return true;
  }
  return requestObject.__clapse_host_reachability &&
    typeof requestObject.__clapse_host_reachability === "object";
}

export async function applyCompileReachabilityToCompileRequest(
  requestObject,
  options = {},
) {
  if (!requestObject || typeof requestObject !== "object") {
    return { requestObject, reachabilityPlan: null };
  }
  const command = compileCommandOf(requestObject);
  if (command !== "compile" && command !== "compile-debug") {
    return { requestObject, reachabilityPlan: null };
  }
  if (
    options.skipIfReachabilityPresent !== false &&
    hasReachabilityData(requestObject)
  ) {
    return { requestObject, reachabilityPlan: null };
  }
  const inputPath = String(requestObject.input_path ?? "").trim();
  if (inputPath.length === 0) {
    return { requestObject, reachabilityPlan: null };
  }
  if (!shouldRunEntrypointDce(inputPath, options)) {
    return { requestObject, reachabilityPlan: null };
  }
  const inputSource = typeof options.inputSourceOverride === "string"
    ? options.inputSourceOverride
    : (typeof requestObject.input_source === "string"
      ? requestObject.input_source
      : "");
  if (inputSource.length === 0) {
    return { requestObject, reachabilityPlan: null };
  }
  const projectConfig = options.projectConfig ??
    await readClapseProjectConfig(inputPath);
  const requestedRoots = normalizeEntrypointExportRoots(
    requestObject.entrypoint_exports,
  );
  const reachabilityPlan = await buildCompileReachabilityPlan(
    inputPath,
    inputSource,
    projectConfig,
    {
      rootExportsOverride: requestedRoots,
    },
  );
  if (!reachabilityPlan) {
    return { requestObject, reachabilityPlan: null };
  }

  let effectiveInputSource = inputSource;
  if (reachabilityPlan.sourceByPath instanceof Map) {
    const sourceOverride = reachabilityPlan.sourceByPath.get(
      toAbsolutePath(inputPath),
    );
    if (typeof sourceOverride === "string" && sourceOverride.length > 0) {
      effectiveInputSource = sourceOverride;
    }
  }
  const nextRequest = {
    ...requestObject,
    input_source: effectiveInputSource,
  };
  if (Array.isArray(reachabilityPlan.rootExports)) {
    nextRequest.entrypoint_exports = [...reachabilityPlan.rootExports];
  }
  if (
    Array.isArray(reachabilityPlan.moduleSources) &&
    reachabilityPlan.moduleSources.length > 0
  ) {
    nextRequest.module_sources = reachabilityPlan.moduleSources;
  }
  if (reachabilityPlan.summary) {
    nextRequest.__clapse_host_reachability = {
      ...reachabilityPlan.summary,
      entry_module: reachabilityPlan.entryModuleName,
      entry_path: reachabilityPlan.entryPath,
    };
  }
  return { requestObject: nextRequest, reachabilityPlan };
}
