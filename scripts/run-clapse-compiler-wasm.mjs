#!/usr/bin/env -S deno run -A

import { cliArgs, failWithError } from "./runtime-env.mjs";
import { runLspServer } from "./lsp-wasm.mjs";
import {
  appendClapseFuncMap,
  callCompilerWasm,
  decodeWasmBase64,
  validateCompilerWasmAbi,
} from "./wasm-compiler-abi.mjs";
import {
  buildWasmSeedCompileResponse,
  isWasmBootstrapSeedEnabled,
} from "./wasm-bootstrap-seed.mjs";

const REPO_ROOT_URL = new URL("../", import.meta.url);
const PROJECT_CONFIG_FILE = "clapse.json";

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

function normalizeEntrypointExportRoots(rawRoots) {
  if (!Array.isArray(rawRoots)) {
    return [];
  }
  const identifierPattern = /^[A-Za-z_][A-Za-z0-9_']*$/u;
  const operatorPattern =
    /^(?:[!#$%&*+./<>?@\\^|~:][!#$%&*+./<=>?@\\^|~:-]*|=[!#$%&*+./<=?@\\^|~:-][!#$%&*+./<=>?@\\^|~:-]*|-(?:[!#$%&*+./<=?@\\^|~:-][!#$%&*+./<=>?@\\^|~:-]*)?)$/u;
  const out = [];
  const seen = new Set();
  for (const root of rawRoots) {
    const name = String(root ?? "").trim();
    if (!identifierPattern.test(name) && !operatorPattern.test(name)) {
      throw new Error(
        `invalid entrypoint root '${name}' (expected identifier or operator symbol)`,
      );
    }
    if (seen.has(name)) {
      continue;
    }
    seen.add(name);
    out.push(name);
  }
  return out;
}

function parseCompileCommandArgs(rawArgs) {
  const entrypointRootsRaw = [];
  const positionals = [];
  for (let i = 0; i < rawArgs.length; i += 1) {
    const token = rawArgs[i];
    if (token === "--entrypoint-export") {
      const next = rawArgs[i + 1];
      if (typeof next !== "string") {
        throw new Error(
          "usage: --entrypoint-export <name>",
        );
      }
      entrypointRootsRaw.push(next);
      i += 1;
      continue;
    }
    if (token === "--entrypoint-exports") {
      const next = rawArgs[i + 1];
      if (typeof next !== "string") {
        throw new Error(
          "usage: --entrypoint-exports <comma-separated-names>",
        );
      }
      for (const part of next.split(",")) {
        entrypointRootsRaw.push(part);
      }
      i += 1;
      continue;
    }
    positionals.push(token);
  }
  return {
    positionals,
    entrypointExports: normalizeEntrypointExportRoots(entrypointRootsRaw),
  };
}

const IDENTIFIER_NAME_RE = /^[A-Za-z_][A-Za-z0-9_$.']*$/u;
const OPERATOR_NAME_RE =
  /^(?:[!#$%&*+./<>?@\\^|~:][!#$%&*+./<=>?@\\^|~:-]*|=[!#$%&*+./<=?@\\^|~:-][!#$%&*+./<=>?@\\^|~:-]*|-(?:[!#$%&*+./<=>?@\\^|~:-][!#$%&*+./<=>?@\\^|~:-]*)?)$/u;
const QUALIFIED_CALL_RE =
  /([A-Za-z_][A-Za-z0-9_$.']*)\\.([A-Za-z_][A-Za-z0-9_$.']+|[!#$%&*+./<=>?@\\^|~:-]+)/gu;
const NAME_OR_OPERATOR_TOKEN_RE =
  /[A-Za-z_][A-Za-z0-9_$.']+|[!#$%&*+./<=>?@\\^|~:-]+/gu;

function normalizeFunctionName(rawName) {
  const name = String(rawName ?? "").trim();
  if (name.startsWith("(") && name.endsWith(")")) {
    return name.slice(1, -1).trim();
  }
  return name;
}

function isFunctionName(value) {
  const name = normalizeFunctionName(value);
  return IDENTIFIER_NAME_RE.test(name) || OPERATOR_NAME_RE.test(name);
}

function isHostModuleName(rawName) {
  const name = String(rawName ?? "").trim();
  return name.startsWith("host.") || name.startsWith("host/");
}

function stripLineComment(rawLine) {
  const line = String(rawLine);
  const marker = line.indexOf("--");
  return marker >= 0 ? line.slice(0, marker) : line;
}

function parseModuleName(rawLine) {
  const line = stripLineComment(rawLine).trim();
  const quoted = line.match(/^module\s+"([^"]+)"/u);
  if (quoted) {
    return quoted[1].trim();
  }
  const match = line.match(
    /^module\s+([A-Za-z_][A-Za-z0-9_$.']*(?:\.[A-Za-z_][A-Za-z0-9_$.']*)*)/u,
  );
  return match ? match[1] : "";
}

function parseImportBindings(rawBindings) {
  const valueBindings = new Map();
  const typeBindings = new Map();
  const raw = String(rawBindings ?? "").trim();
  if (raw.length === 0) {
    return { valueBindings, typeBindings };
  }
  for (const part of raw.split(",")) {
    const token = part.trim();
    if (token.length === 0) {
      continue;
    }
    let isType = false;
    let bindingText = token;
    if (bindingText.startsWith("type ")) {
      isType = true;
      bindingText = bindingText.slice(5).trim();
    }
    if (bindingText.length === 0) {
      continue;
    }
    let importedName = bindingText;
    let localName = bindingText;
    const asMatch = bindingText.match(/^(.+?)\s+as\s+(.+)$/u);
    if (asMatch) {
      importedName = asMatch[1].trim();
      localName = asMatch[2].trim();
    }
    const imported = normalizeFunctionName(importedName);
    const local = normalizeFunctionName(localName);
    if (!isFunctionName(imported) || !isFunctionName(local)) {
      continue;
    }
    if (isType) {
      if (!typeBindings.has(local)) {
        typeBindings.set(local, imported);
      }
      continue;
    }
    if (!valueBindings.has(local)) {
      valueBindings.set(local, imported);
    }
  }
  return { valueBindings, typeBindings };
}

function parseImportDecl(rawLine) {
  const line = stripLineComment(rawLine).trim();
  if (!line.startsWith("import ")) {
    return null;
  }
  const rest = line.slice("import ".length).trim();
  if (rest.length === 0) {
    return null;
  }

  let isQuoted = false;
  let moduleName = "";
  let specifier = "";
  let remainder = "";
  let alias = "";

  if (rest.startsWith("\"")) {
    const quotedMatch = rest.match(/^"([^"]+)"(.*)$/u);
    if (!quotedMatch) {
      return null;
    }
    isQuoted = true;
    specifier = quotedMatch[1].trim();
    remainder = quotedMatch[2].trim();
  } else {
    const moduleMatch = rest.match(
      /^([A-Za-z_][A-Za-z0-9_$.']*(?:\.[A-Za-z_][A-Za-z0-9_$.']*)*)(.*)$/u,
    );
    if (!moduleMatch) {
      return null;
    }
    moduleName = moduleMatch[1].trim();
    remainder = moduleMatch[2].trim();
  }

  let bindingsRaw = "";
  if (remainder.length > 0) {
    const aliasMatch = remainder.match(/^as\s+([A-Za-z_][A-Za-z0-9_']*)$/u);
    if (aliasMatch) {
      alias = aliasMatch[1];
    } else {
      const bindingsMatch = remainder.match(/^\{(.*)\}$/u);
      if (!bindingsMatch) {
        return null;
      }
      bindingsRaw = bindingsMatch[1];
    }
  }
  if (alias.length > 0 && bindingsRaw.trim().length > 0) {
    return null;
  }

  const { valueBindings, typeBindings } = parseImportBindings(bindingsRaw);
  return {
    isQuoted,
    moduleName,
    specifier,
    alias,
    hasBindingList: bindingsRaw.trim().length > 0,
    valueBindings,
    typeBindings,
    rawLine,
  };
}

function isRelativeOrAbsoluteSpecifier(specifier) {
  const text = String(specifier ?? "");
  return text.startsWith("./") || text.startsWith("../") || text.startsWith("/");
}

function importQualifier(importEntry) {
  const alias = String(importEntry?.alias ?? "").trim();
  if (alias.length > 0) {
    return alias;
  }
  if (importEntry?.isQuoted === true) {
    return "";
  }
  return String(importEntry?.moduleName ?? "").trim();
}

function canonicalImportLine(moduleName) {
  const normalized = String(moduleName ?? "").trim();
  if (normalized.length === 0) {
    return "";
  }
  return `import "${normalized}"`;
}

function escapeRegExp(text) {
  return String(text).replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
}

function rewriteAliasQualifiedRefs(rawLine, aliasMap) {
  if (!(aliasMap instanceof Map) || aliasMap.size === 0) {
    return rawLine;
  }
  const line = String(rawLine);
  const commentAt = line.indexOf("--");
  const codePart = commentAt >= 0 ? line.slice(0, commentAt) : line;
  const commentPart = commentAt >= 0 ? line.slice(commentAt) : "";
  let rewritten = codePart;
  for (const [alias, targetModuleName] of aliasMap.entries()) {
    const aliasName = String(alias).trim();
    const targetName = String(targetModuleName).trim();
    if (aliasName.length === 0 || targetName.length === 0) {
      continue;
    }
    const pattern = new RegExp(`\\b${escapeRegExp(aliasName)}\\.`, "gu");
    rewritten = rewritten.replace(pattern, `${targetName}.`);
  }
  return rewritten + commentPart;
}

const SIMPLE_FUSION_TOKEN_PATTERN =
  "(?:[A-Za-z_][A-Za-z0-9_']*|[!#$%&*+./:<=>?@\\\\^|~-]+)";
const FOLDL_FMAP_LINE_RE = new RegExp(
  `^(\\s*[^=\\n]+?=\\s*)foldl\\s+(${SIMPLE_FUSION_TOKEN_PATTERN})\\s+(${SIMPLE_FUSION_TOKEN_PATTERN})\\s*\\(\\s*fmap\\s+(${SIMPLE_FUSION_TOKEN_PATTERN})\\s+(${SIMPLE_FUSION_TOKEN_PATTERN})\\s*\\)\\s*$`,
  "u",
);

function rewriteFoldlFmapLine(rawLine) {
  const line = String(rawLine);
  const commentAt = line.indexOf("--");
  const codePart = commentAt >= 0 ? line.slice(0, commentAt) : line;
  const commentPart = commentAt >= 0 ? line.slice(commentAt) : "";
  if (/^\s/u.test(codePart)) {
    return line;
  }
  if (parseFunctionDefinitionName(codePart).length === 0) {
    return line;
  }
  const match = codePart.match(FOLDL_FMAP_LINE_RE);
  if (!match) {
    return line;
  }
  const [, lhs, f, z, g, xs] = match;
  return `${lhs}foldl (\\acc x -> ${f} acc (${g} x)) ${z} ${xs}${commentPart}`;
}

function isPrunableImportedDeclarationLine(rawLine) {
  if (/^\s/.test(rawLine)) {
    return false;
  }
  const trimmed = stripLineComment(rawLine).trim();
  if (trimmed.length === 0) {
    return false;
  }
  return trimmed.startsWith("class ") ||
    trimmed.startsWith("instance ") ||
    trimmed.startsWith("law ") ||
    trimmed.startsWith("infix ") ||
    trimmed.startsWith("infixl ") ||
    trimmed.startsWith("infixr ") ||
    trimmed.startsWith("type ");
}

function nextTopLevelBoundaryAfter(lineNo, boundaries, fallbackEnd) {
  for (const boundary of boundaries) {
    if (boundary > lineNo) {
      return boundary;
    }
  }
  return fallbackEnd;
}

function collectNameTokensFromText(text) {
  const out = new Set();
  for (const match of String(text).matchAll(NAME_OR_OPERATOR_TOKEN_RE)) {
    out.add(match[0]);
  }
  return out;
}

function mergeTokenSet(dst, src) {
  let changed = false;
  for (const token of src) {
    if (!dst.has(token)) {
      dst.add(token);
      changed = true;
    }
  }
  return changed;
}

function parseTopLevelDataPrimitiveDefinedNames(rawLine) {
  if (/^\s/.test(rawLine)) {
    return null;
  }
  const trimmed = stripLineComment(rawLine).trim();
  if (trimmed.startsWith("data ")) {
    const typeMatch = trimmed.match(/^data\s+([A-Za-z_][A-Za-z0-9_']*)/u);
    if (!typeMatch) {
      return null;
    }
    const names = new Set([typeMatch[1]]);
    const eqAt = trimmed.indexOf("=");
    if (eqAt >= 0 && eqAt + 1 < trimmed.length) {
      const rhs = trimmed.slice(eqAt + 1);
      for (const ctorPart of rhs.split("|")) {
        const ctorMatch = ctorPart.trim().match(/^([A-Za-z_][A-Za-z0-9_']*)/u);
        if (ctorMatch) {
          names.add(ctorMatch[1]);
        }
      }
    }
    return { kind: "data", names };
  }
  if (trimmed.startsWith("primitive ")) {
    const typeMatch = trimmed.match(/^primitive\s+([A-Za-z_][A-Za-z0-9_']*)/u);
    if (!typeMatch) {
      return null;
    }
    const names = new Set([typeMatch[1]]);
    const eqAt = trimmed.indexOf("=");
    if (eqAt >= 0 && eqAt + 1 < trimmed.length) {
      const rhs = trimmed.slice(eqAt + 1);
      for (const ctorMatch of rhs.matchAll(/([A-Za-z_][A-Za-z0-9_']*)\s*</gu)) {
        names.add(ctorMatch[1]);
      }
    }
    return { kind: "primitive", names };
  }
  return null;
}

function parseExportDecl(rawLine) {
  const line = stripLineComment(rawLine).trim();
  const match = line.match(/^export\b\s+(.*)$/u);
  if (!match) {
    return { names: [], isLegacySyntax: false, hasExport: false };
  }
  const raw = match[1].trim();
  const braceMatch = raw.match(/^\{\s*(.*)\s*\}$/u);
  const listText = braceMatch ? braceMatch[1] : raw;
  const isLegacySyntax = !braceMatch;
  const out = [];
  const seen = new Set();
  for (const part of listText.split(",")) {
    const name = normalizeFunctionName(part);
    if (!isFunctionName(name) || seen.has(name)) {
      continue;
    }
    seen.add(name);
    out.push(name);
  }
  return {
    names: out,
    isLegacySyntax,
    hasExport: out.length > 0,
  };
}

function addExportNamesFromFragment(fragment, names, seen) {
  for (const part of String(fragment ?? "").split(",")) {
    const token = part.trim().replace(/^\{/u, "").replace(/\}$/u, "").trim();
    if (token.length === 0) {
      continue;
    }
    const name = normalizeFunctionName(token);
    if (!isFunctionName(name) || seen.has(name)) {
      continue;
    }
    seen.add(name);
    names.push(name);
  }
}

function parseLayoutExportDecl(lines, startIndex) {
  const rawLine = String(lines[startIndex] ?? "");
  const line = stripLineComment(rawLine).trim();
  if (!line.startsWith("export")) {
    return { names: [], isLegacySyntax: false, hasExport: false, endLine: startIndex };
  }
  const after = line.slice("export".length).trim();
  const names = [];
  const seen = new Set();
  let endLine = startIndex;

  if (after.length === 0) {
    for (let i = startIndex + 1; i < lines.length; i += 1) {
      const blockRaw = String(lines[i] ?? "");
      const blockTrimmed = stripLineComment(blockRaw).trim();
      if (blockTrimmed.length === 0) {
        endLine = i;
        continue;
      }
      if (!/^\s/.test(blockRaw)) {
        break;
      }
      if (blockTrimmed === "{" || blockTrimmed === "}") {
        endLine = i;
        if (blockTrimmed === "}") {
          break;
        }
        continue;
      }
      addExportNamesFromFragment(blockTrimmed, names, seen);
      endLine = i;
    }
    return {
      names,
      isLegacySyntax: false,
      hasExport: names.length > 0,
      endLine,
    };
  }

  if (!after.startsWith("{") || after.includes("}")) {
    return { names: [], isLegacySyntax: false, hasExport: false, endLine: startIndex };
  }

  const firstFragment = after.slice(1).trim();
  if (firstFragment.length > 0) {
    addExportNamesFromFragment(firstFragment, names, seen);
  }
  for (let i = startIndex + 1; i < lines.length; i += 1) {
    const blockRaw = String(lines[i] ?? "");
    const blockTrimmed = stripLineComment(blockRaw).trim();
    if (blockTrimmed.length === 0) {
      endLine = i;
      continue;
    }
    if (!/^\s/.test(blockRaw)) {
      break;
    }
    const closeAt = blockTrimmed.indexOf("}");
    if (closeAt >= 0) {
      const beforeClose = blockTrimmed.slice(0, closeAt).trim();
      if (beforeClose.length > 0) {
        addExportNamesFromFragment(beforeClose, names, seen);
      }
      endLine = i;
      return {
        names,
        isLegacySyntax: false,
        hasExport: names.length > 0,
        endLine,
      };
    }
    addExportNamesFromFragment(blockTrimmed, names, seen);
    endLine = i;
  }
  return {
    names,
    isLegacySyntax: false,
    hasExport: names.length > 0,
    endLine,
  };
}

function parseExportNames(rawLine) {
  return parseExportDecl(rawLine).names;
}

function rewriteLegacyListConstructors(rawSource, sourcePath, warnings) {
  const source = String(rawSource ?? "");
  if (!source.includes("ListNil") && !source.includes("ListCons")) {
    return source;
  }
  const lines = source.split(/\r?\n/);
  let changed = false;
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    if (!line.includes("ListNil") && !line.includes("ListCons")) {
      continue;
    }
    const rewritten = line.replace(/\bListCons\b/gu, "Cons").replace(
      /\bListNil\b/gu,
      "Nil",
    );
    if (rewritten === line) {
      continue;
    }
    lines[i] = rewritten;
    changed = true;
    if (warnings instanceof Map) {
      const warningKey = `${sourcePath}:list-ctor:${i}`;
      if (!warnings.has(warningKey)) {
        warnings.set(
          warningKey,
          `${sourcePath}:${i + 1}: deprecated List constructors ListNil/ListCons rewritten to Nil/Cons`,
        );
      }
    }
  }
  return changed ? lines.join("\n") : source;
}

function parseFunctionDefinitionName(rawLine) {
  const line = stripLineComment(rawLine).trim();
  if (line.length === 0 || /^\s/.test(rawLine)) {
    return "";
  }
  if (
    line.startsWith("module ") ||
    line.startsWith("import ") ||
    line.startsWith("export ") ||
    line.startsWith("type ") ||
    line.startsWith("data ") ||
    line.startsWith("class ") ||
    line.startsWith("instance ") ||
    line.startsWith("law ") ||
    line.startsWith("infix ") ||
    line.startsWith("infixl ") ||
    line.startsWith("infixr ")
  ) {
    return "";
  }
  const sepAt = line.indexOf("=");
  if (sepAt < 0) {
    return "";
  }
  const lhs = line.slice(0, sepAt).trim();
  if (lhs.length === 0) {
    return "";
  }
  const firstToken = lhs.split(/\s+/u)[0];
  if (!isFunctionName(firstToken)) {
    return "";
  }
  return normalizeFunctionName(firstToken);
}

function parseFunctionSignatureName(rawLine) {
  const line = stripLineComment(rawLine).trim();
  if (line.length === 0 || line.includes("=") || /^\s/.test(rawLine)) {
    return "";
  }
  const sepAt = line.indexOf(":");
  if (sepAt < 0) {
    return "";
  }
  const lhs = line.slice(0, sepAt).trim();
  const firstToken = lhs.split(/\s+/u)[0];
  if (!isFunctionName(firstToken)) {
    return "";
  }
  return normalizeFunctionName(firstToken);
}

function isTopLevelBoundaryLine(rawLine) {
  const line = stripLineComment(rawLine).trim();
  if (line.length === 0 || /^\s/.test(rawLine)) {
    return false;
  }
  if (
    line.startsWith("module ") ||
    line.startsWith("import ") ||
    line.startsWith("export ") ||
    line.startsWith("type ") ||
    line.startsWith("data ") ||
    line.startsWith("class ") ||
    line.startsWith("instance ") ||
    line.startsWith("law ") ||
    line.startsWith("infix ") ||
    line.startsWith("infixl ") ||
    line.startsWith("infixr ")
  ) {
    return true;
  }
  return parseFunctionDefinitionName(rawLine).length > 0;
}

async function resolveModuleImport(moduleName, moduleSearchDirs) {
  if (!Array.isArray(moduleSearchDirs) || moduleSearchDirs.length === 0) {
    return "";
  }
  const rawModuleName = String(moduleName ?? "").trim();
  if (rawModuleName.length === 0) {
    return "";
  }
  const moduleNameWithSlash = normalizePath(rawModuleName.replace(/\./gu, "/"));
  const moduleNames = new Set();
  if (moduleNameWithSlash.length > 0) {
    moduleNames.add(moduleNameWithSlash);
  }
  if (rawModuleName !== moduleNameWithSlash) {
    moduleNames.add(rawModuleName);
  }
  for (const dir of moduleSearchDirs) {
    const normalizedDir = normalizePath(dir);
    for (const candidateName of moduleNames) {
      const moduleSuffix = `${candidateName}.clapse`;
      const candidate = toAbsolutePath(
        normalizePath(`${normalizedDir}/${moduleSuffix}`),
      );
      if (await fileExists(candidate)) {
        return candidate;
      }
    }
  }
  return "";
}

async function resolveQuotedImportSpecifier(
  specifier,
  currentSourcePath,
  moduleSearchDirs,
) {
  const spec = normalizePath(String(specifier ?? "").trim());
  if (spec.length === 0) {
    return "";
  }
  const candidates = [];
  const seen = new Set();
  const addCandidate = (path) => {
    const normalized = toAbsolutePath(normalizePath(path));
    if (normalized.length === 0 || seen.has(normalized)) {
      return;
    }
    seen.add(normalized);
    candidates.push(normalized);
  };
  const addMaybeClapseCandidate = (path) => {
    addCandidate(path);
    if (!String(path).endsWith(".clapse")) {
      addCandidate(`${path}.clapse`);
    }
  };

  if (spec.startsWith("./") || spec.startsWith("../") || spec.startsWith("/")) {
    const baseDir = pathDir(currentSourcePath);
    if (spec.startsWith("/")) {
      addMaybeClapseCandidate(spec);
    } else {
      addMaybeClapseCandidate(joinPath(baseDir, spec));
    }
  } else if (Array.isArray(moduleSearchDirs) && moduleSearchDirs.length > 0) {
    for (const dir of moduleSearchDirs) {
      const normalizedDir = normalizePath(dir);
      addMaybeClapseCandidate(`${normalizedDir}/${spec}`);
      const specAsPath = spec.replace(/\./gu, "/");
      if (specAsPath !== spec) {
        addMaybeClapseCandidate(`${normalizedDir}/${specAsPath}`);
      }
    }
  }
  if (!spec.startsWith("./") && !spec.startsWith("../") && !spec.startsWith("/")) {
    const preludeAliases = new Set([
      "prelude",
      "compiler/prelude",
    ]);
    if (preludeAliases.has(spec)) {
      addCandidate(toPath(new URL("lib/compiler/prelude.clapse", REPO_ROOT_URL)));
    }
  }

  for (const candidate of candidates) {
    if (await fileExists(candidate)) {
      return candidate;
    }
  }
  return "";
}

async function resolveImportTarget(importEntry, sourcePath, moduleSearchDirs) {
  if (!importEntry || typeof importEntry !== "object") {
    return "";
  }
  if (importEntry.isHostModule === true) {
    return "";
  }
  if (importEntry.isQuoted === true) {
    return await resolveQuotedImportSpecifier(
      importEntry.specifier,
      sourcePath,
      moduleSearchDirs,
    );
  }
  return await resolveModuleImport(importEntry.moduleName, moduleSearchDirs);
}

function parseModuleSourceInfo(sourceText, sourcePath) {
  const source = String(sourceText);
  const lines = source.split(/\r?\n/);
  let moduleName = "";
  let moduleDeclLine = "";
  const importEntries = [];
  const exportNames = [];
  const exportNameSet = new Set();
  const legacyExportLines = [];
  const functionDefLines = [];
  const boundaryLines = [];
  for (let i = 0; i < lines.length; i += 1) {
    const rawLine = lines[i];
    const trimmed = stripLineComment(rawLine).trim();
    if (trimmed.length === 0) {
      continue;
    }
    if (trimmed.startsWith("module ")) {
      const parsedModuleName = parseModuleName(rawLine);
      if (parsedModuleName.length > 0) {
        moduleName = parsedModuleName;
        if (moduleDeclLine.length === 0) {
          moduleDeclLine = rawLine;
        }
      }
      if (isTopLevelBoundaryLine(rawLine)) boundaryLines.push(i);
      continue;
    }
    const importDecl = parseImportDecl(rawLine);
    if (importDecl !== null) {
      importEntries.push({
        key: `${i}:${importDecl.isQuoted ? `q:${importDecl.specifier}` : `m:${importDecl.moduleName}`}:${importQualifier(importDecl)}`,
        lineNo: i,
        line: rawLine,
        isQuoted: importDecl.isQuoted,
        moduleName: importDecl.moduleName,
        specifier: importDecl.specifier,
        alias: importDecl.alias,
        hasBindingList: importDecl.hasBindingList,
        valueBindings: importDecl.valueBindings,
        typeBindings: importDecl.typeBindings,
        isHostModule: importDecl.isQuoted
          ? isHostModuleName(importDecl.specifier)
          : isHostModuleName(importDecl.moduleName),
      });
      boundaryLines.push(i);
      continue;
    }
    const parsedExportDecl = parseExportDecl(rawLine);
    if (parsedExportDecl.hasExport) {
      if (parsedExportDecl.isLegacySyntax) {
        legacyExportLines.push({
          lineNo: i,
          line: rawLine,
        });
      }
      for (const name of parsedExportDecl.names) {
        if (!exportNameSet.has(name)) {
          exportNameSet.add(name);
          exportNames.push(name);
        }
      }
      boundaryLines.push(i);
      continue;
    }
    if (trimmed === "export" || trimmed.startsWith("export {")) {
      const parsedLayoutExportDecl = parseLayoutExportDecl(lines, i);
      if (parsedLayoutExportDecl.hasExport) {
        for (const name of parsedLayoutExportDecl.names) {
          if (!exportNameSet.has(name)) {
            exportNameSet.add(name);
            exportNames.push(name);
          }
        }
        boundaryLines.push(i);
        i = parsedLayoutExportDecl.endLine;
        continue;
      }
    }
    const functionName = parseFunctionDefinitionName(rawLine);
    if (functionName.length > 0) {
      functionDefLines.push({ name: functionName, line: i });
      boundaryLines.push(i);
      continue;
    }
    if (isTopLevelBoundaryLine(rawLine)) {
      boundaryLines.push(i);
    }
  }
  if (moduleName.length === 0) {
    const base = sourcePath.split("/").pop() ?? "";
    moduleName = base.endsWith(".clapse")
      ? base.slice(0, base.length - ".clapse".length)
      : base;
  }
  const sortedBoundaries = [...new Set(boundaryLines)].sort((a, b) => a - b);
  const functionSpans = new Map();
  for (const def of functionDefLines) {
    let startLine = def.line;
    while (startLine > 0) {
      const previousName = parseFunctionSignatureName(lines[startLine - 1]);
      if (previousName.length > 0 && previousName === def.name) {
        startLine -= 1;
      } else {
        break;
      }
    }
    let endLine = lines.length;
    for (let i = 0; i < sortedBoundaries.length; i += 1) {
      const lineNo = sortedBoundaries[i];
      if (lineNo > def.line) {
        endLine = lineNo;
        break;
      }
    }
    const spanLines = lines.slice(startLine, endLine);
    if (spanLines.length > 0) {
      functionSpans.set(def.name, {
        name: def.name,
        startLine,
        endLine,
        sourceLines: spanLines,
      });
    }
  }
  return {
    sourcePath: toAbsolutePath(sourcePath),
    moduleName,
    moduleDeclLine,
    sourceLines: lines,
    topLevelBoundaries: sortedBoundaries,
    importEntries,
    exportNames,
    exportNameSet,
    legacyExportLines,
    functionSpans,
    functionNames: new Set(functionDefLines.map((entry) => entry.name)),
  };
}

function collectFunctionReferencesFromSpan(spanSource, localFunctionNames) {
  const source = spanSource.map((line) => stripLineComment(line)).join("\n");
  const localRefs = new Set();
  const qualifiedRefs = [];
  const unqualifiedRefs = new Set();
  for (const match of source.matchAll(QUALIFIED_CALL_RE)) {
    const [_, moduleName, symbol] = match;
    const cleanSymbol = normalizeFunctionName(symbol);
    if (moduleName.length > 0 && isFunctionName(cleanSymbol)) {
      qualifiedRefs.push({
        moduleName,
        symbol: cleanSymbol,
      });
    }
  }
  for (const tokenMatch of source.matchAll(NAME_OR_OPERATOR_TOKEN_RE)) {
    const token = tokenMatch[0];
    unqualifiedRefs.add(token);
    if (localFunctionNames.has(token)) {
      localRefs.add(token);
    }
  }
  return { localRefs, qualifiedRefs, unqualifiedRefs };
}

async function buildDemandDrivenCompileInput(
  entryPath,
  entrypointExportsRaw,
  options = {},
) {
  const projectConfig = options.projectConfig ??
    await readClapseProjectConfig(entryPath);
  const moduleSearchDirs = Array.isArray(projectConfig.moduleSearchDirs)
    ? projectConfig.moduleSearchDirs
    : [];
  const includeConfigured = moduleSearchDirs.length > 0;

  const normalizedEntryPath = toAbsolutePath(entryPath);
  const moduleInfos = new Map();
  const moduleOrder = [];
  const queue = [normalizedEntryPath];
  const seen = new Set();
  const deprecationWarnings = new Map();

  while (queue.length > 0) {
    const sourcePath = queue.shift();
    if (seen.has(sourcePath)) {
      continue;
    }
    seen.add(sourcePath);
    const rawSource = await Deno.readTextFile(sourcePath);
    const source = rewriteLegacyListConstructors(
      rawSource,
      sourcePath,
      deprecationWarnings,
    );
    const info = parseModuleSourceInfo(source, sourcePath);
    const resolvedImportEntries = [];
    for (const legacyExport of info.legacyExportLines ?? []) {
      const warningKey = `${sourcePath}:export:${legacyExport.lineNo}`;
      if (!deprecationWarnings.has(warningKey)) {
        const exportText = String(legacyExport.line ?? "").trim();
        deprecationWarnings.set(
          warningKey,
          `${sourcePath}:${legacyExport.lineNo + 1}: deprecated export form '${exportText}' (preferred: export { symbols })`,
        );
      }
    }
    for (const importEntry of info.importEntries) {
      if (importEntry.isQuoted !== true && !importEntry.hasBindingList &&
        String(importEntry.alias ?? "").trim().length === 0) {
        const warningKey = `${sourcePath}:${importEntry.lineNo}`;
        if (!deprecationWarnings.has(warningKey)) {
          const importText = String(importEntry.line).trim();
          deprecationWarnings.set(
            warningKey,
            `${sourcePath}:${importEntry.lineNo + 1}: deprecated import form '${importText}' (preferred: import "mod" { symbols } or import "mod" as alias)`,
          );
        }
      }
      if (importEntry.isHostModule) {
        resolvedImportEntries.push({
          ...importEntry,
          resolvedPath: "",
        });
        continue;
      }
      const resolvedPath = await resolveImportTarget(
        importEntry,
        sourcePath,
        moduleSearchDirs,
      );
      if (resolvedPath.length > 0) {
        resolvedImportEntries.push({
          ...importEntry,
          resolvedPath,
        });
        if (!seen.has(resolvedPath)) {
          queue.push(resolvedPath);
        }
        continue;
      }

      const requiresQuotedResolution = importEntry.isQuoted &&
        (isRelativeOrAbsoluteSpecifier(importEntry.specifier) ||
          importEntry.hasBindingList === true ||
          String(importEntry.alias ?? "").trim().length > 0);
      const shouldFail = importEntry.isQuoted
        ? (requiresQuotedResolution || includeConfigured)
        : includeConfigured;
      if (shouldFail) {
        const importLabel = importEntry.isQuoted
          ? `"${importEntry.specifier}"`
          : importEntry.moduleName;
        throw new Error(
          `unresolved import '${importLabel}' in ${sourcePath}; include '${moduleSearchDirs.join(
            ", ",
          )}' did not resolve a module file`,
        );
      }
      resolvedImportEntries.push({
        ...importEntry,
        resolvedPath: "",
      });
    }
    moduleInfos.set(sourcePath, { ...info, importEntries: resolvedImportEntries });
    moduleOrder.push(sourcePath);
  }
  if (options.emitImportDeprecationWarnings !== false) {
    for (const warning of deprecationWarnings.values()) {
      console.warn(warning);
    }
  }

  const entryInfo = moduleInfos.get(normalizedEntryPath);
  if (!entryInfo) {
    throw new Error(`failed to parse entry module ${normalizedEntryPath}`);
  }
  const explicitRoots = normalizeEntrypointExportRoots(entrypointExportsRaw);
  const rootList = explicitRoots.length > 0
    ? explicitRoots
    : entryInfo.exportNames.length > 0
    ? [...entryInfo.exportNames]
    : ["main"];
  const enableFoldlFmapFusion = explicitRoots.length > 0;

  const moduleRoots = new Map();
  const moduleImportUsage = new Map();
  for (const modulePath of moduleOrder) {
    moduleRoots.set(modulePath, new Set());
    moduleImportUsage.set(modulePath, new Set());
  }
  const entryRoots = moduleRoots.get(normalizedEntryPath);
  for (const root of rootList) {
    entryRoots.add(root);
  }

  let changed = true;
  while (changed) {
    changed = false;
    for (const [modulePath, info] of moduleInfos.entries()) {
      const roots = moduleRoots.get(modulePath);
      if (!roots || roots.size === 0) {
        continue;
      }
      const importUsage = moduleImportUsage.get(modulePath);
      const importEntries = Array.isArray(info.importEntries)
        ? info.importEntries
        : [];
      for (const root of [...roots]) {
        const span = info.functionSpans.get(root);
        if (!span) {
          continue;
        }
        const refs = collectFunctionReferencesFromSpan(
          span.sourceLines,
          info.functionNames,
        );
        for (const localRef of refs.localRefs) {
          if (!roots.has(localRef)) {
            roots.add(localRef);
            changed = true;
          }
        }
        for (const ref of refs.qualifiedRefs) {
          for (const importEntry of importEntries) {
            if (!importEntry || typeof importEntry !== "object") {
              continue;
            }
            const qualifier = importQualifier(importEntry);
            if (qualifier.length === 0 || qualifier !== ref.moduleName) {
              continue;
            }
            importUsage.add(importEntry.key);
            const targetPath = String(importEntry.resolvedPath ?? "");
            if (targetPath.length === 0) {
              continue;
            }
            const targetRoots = moduleRoots.get(targetPath);
            if (!targetRoots) {
              continue;
            }
            if (!targetRoots.has(ref.symbol)) {
              targetRoots.add(ref.symbol);
              changed = true;
            }
          }
        }
        for (const importEntry of importEntries) {
          if (!importEntry || typeof importEntry !== "object") {
            continue;
          }
          if (importEntry.isHostModule === true) {
            continue;
          }
          const targetPath = String(importEntry.resolvedPath ?? "");
          if (targetPath.length === 0) {
            continue;
          }
          const targetRoots = moduleRoots.get(targetPath);
          const targetInfo = moduleInfos.get(targetPath);
          if (!targetRoots || !targetInfo) {
            continue;
          }
          const valueBindingMap = (importEntry.valueBindings instanceof Map &&
              importEntry.valueBindings.size > 0)
            ? importEntry.valueBindings
            : (() => {
              const inferred = new Map();
              if (String(importEntry.alias ?? "").trim().length > 0) {
                return inferred;
              }
              for (const exportName of targetInfo.exportNameSet.values()) {
                inferred.set(exportName, exportName);
              }
              return inferred;
            })();
          if (valueBindingMap.size === 0) {
            continue;
          }
          for (const token of refs.unqualifiedRefs) {
            const targetSymbol = valueBindingMap.get(token);
            if (!targetSymbol) {
              continue;
            }
            importUsage.add(importEntry.key);
            if (!targetRoots.has(targetSymbol)) {
              targetRoots.add(targetSymbol);
              changed = true;
            }
          }
        }
      }
    }
  }

  const mergedSections = [];
  for (const modulePath of moduleOrder) {
    const info = moduleInfos.get(modulePath);
    if (!info) continue;
    const roots = moduleRoots.get(modulePath);
    if (!roots || roots.size === 0) {
      continue;
    }
    const droppedSpans = [...info.functionSpans.values()]
      .filter((span) => !roots.has(span.name))
      .sort((a, b) => a.startLine - b.startLine);
    const prunableDeclSpans = [];
    if (modulePath !== normalizedEntryPath) {
      const boundaries = Array.isArray(info.topLevelBoundaries)
        ? info.topLevelBoundaries
        : [];
      const keptFunctionSpans = [...info.functionSpans.values()]
        .filter((span) => roots.has(span.name));
      const neededTokens = new Set(roots);
      for (const span of keptFunctionSpans) {
        mergeTokenSet(
          neededTokens,
          collectNameTokensFromText(
            span.sourceLines.map((line) => stripLineComment(line)).join("\n"),
          ),
        );
      }
      const selectiveDecls = [];
      for (let i = 0; i < info.sourceLines.length; i += 1) {
        const rawLine = info.sourceLines[i];
        const endLine = nextTopLevelBoundaryAfter(
          i,
          boundaries,
          info.sourceLines.length,
        );
        if (isPrunableImportedDeclarationLine(rawLine)) {
          prunableDeclSpans.push({
            startLine: i,
            endLine,
          });
          continue;
        }
        const declNames = parseTopLevelDataPrimitiveDefinedNames(rawLine);
        if (!declNames) {
          continue;
        }
        const spanLines = info.sourceLines.slice(i, endLine);
        selectiveDecls.push({
          startLine: i,
          endLine,
          names: declNames.names,
          tokens: collectNameTokensFromText(
            spanLines.map((line) => stripLineComment(line)).join("\n"),
          ),
        });
      }
      let selectiveChanged = true;
      const keepDecls = new Set();
      while (selectiveChanged) {
        selectiveChanged = false;
        for (let i = 0; i < selectiveDecls.length; i += 1) {
          if (keepDecls.has(i)) {
            continue;
          }
          const decl = selectiveDecls[i];
          let intersects = false;
          for (const name of decl.names) {
            if (neededTokens.has(name)) {
              intersects = true;
              break;
            }
          }
          if (!intersects) {
            continue;
          }
          keepDecls.add(i);
          if (mergeTokenSet(neededTokens, decl.tokens)) {
            selectiveChanged = true;
          }
        }
      }
      for (let i = 0; i < selectiveDecls.length; i += 1) {
        if (keepDecls.has(i)) {
          continue;
        }
        prunableDeclSpans.push({
          startLine: selectiveDecls[i].startLine,
          endLine: selectiveDecls[i].endLine,
        });
      }
      prunableDeclSpans.sort((a, b) => a.startLine - b.startLine);
    }
    const lines = [];
    const importUsage = moduleImportUsage.get(modulePath);
    const emittedImports = new Set();
    const aliasMap = new Map();
    for (const importEntry of info.importEntries ?? []) {
      const alias = String(importEntry?.alias ?? "").trim();
      const targetPath = String(importEntry?.resolvedPath ?? "");
      if (alias.length === 0 || targetPath.length === 0) {
        continue;
      }
      const targetInfo = moduleInfos.get(targetPath);
      if (!targetInfo || typeof targetInfo.moduleName !== "string" ||
        targetInfo.moduleName.length === 0) {
        continue;
      }
      aliasMap.set(alias, targetInfo.moduleName);
    }
    const exported = info.exportNames.filter((name) => roots.has(name));
    let dropIdx = 0;
    let pruneDeclIdx = 0;
    let emittedModule = false;
    let emittedExport = false;
    for (let lineNo = 0; lineNo < info.sourceLines.length; lineNo += 1) {
      while (
        dropIdx < droppedSpans.length && lineNo >= droppedSpans[dropIdx].endLine
      ) {
        dropIdx += 1;
      }
      if (
        dropIdx < droppedSpans.length &&
        lineNo >= droppedSpans[dropIdx].startLine &&
        lineNo < droppedSpans[dropIdx].endLine
      ) {
        continue;
      }
      while (
        pruneDeclIdx < prunableDeclSpans.length &&
        lineNo >= prunableDeclSpans[pruneDeclIdx].endLine
      ) {
        pruneDeclIdx += 1;
      }
      if (
        pruneDeclIdx < prunableDeclSpans.length &&
        lineNo >= prunableDeclSpans[pruneDeclIdx].startLine &&
        lineNo < prunableDeclSpans[pruneDeclIdx].endLine
      ) {
        continue;
      }
      const rawLine = info.sourceLines[lineNo];
      const importDecl = parseImportDecl(rawLine);
      if (importDecl !== null) {
        const importKey = `${lineNo}:${importDecl.isQuoted ? `q:${importDecl.specifier}` : `m:${importDecl.moduleName}`}:${importQualifier(importDecl)}`;
        const keptImportEntry = (info.importEntries ?? []).find((entry) =>
          entry.key === importKey
        );
        const keepImport = !!keptImportEntry &&
          (importUsage.has(keptImportEntry.key) ||
            keptImportEntry.isHostModule === true);
        if (keepImport && keptImportEntry) {
          let importLine = String(keptImportEntry.line);
          const targetPath = String(keptImportEntry.resolvedPath ?? "");
          if (targetPath.length > 0) {
            const targetInfo = moduleInfos.get(targetPath);
            const targetModuleName = String(targetInfo?.moduleName ?? "");
            const canonical = canonicalImportLine(targetModuleName);
            if (canonical.length > 0) {
              importLine = canonical;
            }
          }
          if (!emittedImports.has(importLine)) {
            emittedImports.add(importLine);
            lines.push(importLine);
          }
        }
        continue;
      }
      const exportNamesFromLine = parseExportNames(rawLine);
      if (exportNamesFromLine.length > 0) {
        if (!emittedExport && exported.length > 0) {
          lines.push(`export { ${exported.join(", ")} }`);
          emittedExport = true;
        }
        continue;
      }
      const moduleNameFromLine = parseModuleName(rawLine);
      if (moduleNameFromLine.length > 0) {
        if (!emittedModule && info.moduleDeclLine.length > 0) {
          lines.push(info.moduleDeclLine);
          emittedModule = true;
        }
        continue;
      }
      const aliasRewritten = rewriteAliasQualifiedRefs(rawLine, aliasMap);
      lines.push(
        enableFoldlFmapFusion
          ? rewriteFoldlFmapLine(aliasRewritten)
          : aliasRewritten,
      );
    }
    if (lines.length > 0) {
      mergedSections.push(lines.join("\n"));
    }
  }
  return {
    entrypointExports: rootList,
    inputSourceOverride: mergedSections.join("\n\n"),
  };
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
    "  compile <input.clapse> [output.wasm] [--entrypoint-export <name>] [--entrypoint-exports <csv>]",
    "  compile-native <input.clapse> [output.wasm] [--entrypoint-export <name>] [--entrypoint-exports <csv>] (alias: compile_native)",
    "  compile-native-debug <input.clapse> [output.wasm] [artifacts-dir] [--entrypoint-export <name>] [--entrypoint-exports <csv>] (alias: compile_native_debug)",
    "  compile-debug <input.clapse> [output.wasm] [artifacts-dir] [--entrypoint-export <name>] [--entrypoint-exports <csv>] (alias: compile_debug)",
    "  emit-wat <input.clapse> [output.wat]",
    "  parse <input.clapse> [out-dir]",
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
    "  kernel-native compile pruning is owned by the native compiler response",
    "  shape. Request payload keeps `entrypoint_exports` if provided and falls",
    "  back to source exports then `main` when unset.",
    "  Unknown explicit roots now fail compile with an error.",
    "  `CLAPSE_ENTRYPOINT_DCE` and `CLAPSE_INTERNAL_ENTRYPOINT_DCE` are",
    "  legacy toggles and do not affect compile request shaping.",
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
  const pluginWasmPaths = Array.isArray(options.pluginWasmPaths)
    ? options.pluginWasmPaths
    : (!options.skipPluginCompilation && !isKernelCompile
      ? await compilePluginsWasm(wasmPath, inputPath, {
        projectConfig: options.projectConfig,
      })
      : []);
  const request = {
    command: "compile",
    input_path: inputPath,
    input_source: inputSource,
    plugin_wasm_paths: pluginWasmPaths,
  };
  const entrypointExports = normalizeEntrypointExportRoots(
    options.entrypointExports,
  );
  if (entrypointExports.length > 0) {
    request.entrypoint_exports = entrypointExports;
  }
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

function decodeParseResponse(response, ctx) {
  assertObject(response, "parse response");
  if (typeof response.ok !== "boolean") {
    throw new Error("parse response: missing boolean 'ok'");
  }
  if (!response.ok) {
    const err = typeof response.error === "string"
      ? response.error
      : `parse error: ${ctx}`;
    throw new Error(err);
  }
  const artifacts = response.artifacts;
  if (artifacts && typeof artifacts === "object" && !Array.isArray(artifacts)) {
    if (typeof artifacts["parsed_cst.txt"] === "string") {
      return artifacts["parsed_cst.txt"];
    }
    if (typeof artifacts.parsed_cst === "string") {
      return artifacts.parsed_cst;
    }
    if (typeof artifacts.parsedCst === "string") {
      return artifacts.parsedCst;
    }
    if (typeof artifacts.cst === "string") {
      return artifacts.cst;
    }
  }
  if (typeof response.parsed_cst === "string") {
    return response.parsed_cst;
  }
  if (typeof response.parsedCst === "string") {
    return response.parsedCst;
  }
  if (typeof response.cst === "string") {
    return response.cst;
  }
  throw new Error(
    "parse response: missing string 'parsed_cst' (expected in response.artifacts.parsed_cst.txt)",
  );
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

async function parseViaWasm(wasmPath, inputPath, outDir) {
  const outputDir = normalizePath(outDir || ".");
  const outputBase = outputDir.length > 0 ? outputDir : ".";
  const inputSource = await Deno.readTextFile(inputPath);
  const response = await callCompilerWasm(wasmPath, {
    command: "parse",
    input_path: inputPath,
    input_source: inputSource,
  });
  const parsedCst = decodeParseResponse(response, inputPath);
  await Deno.mkdir(outputBase, { recursive: true });
  await Deno.writeTextFile(`${outputBase}/parsed_cst.txt`, parsedCst);
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

async function formatRequestWithFallback(wasmPath, request, ctx) {
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
    const parsed = parseCompileCommandArgs(args.slice(1));
    if (parsed.positionals.length < 1 || parsed.positionals.length > 2) {
      throw new Error("usage: compile <input.clapse> [output.wasm]");
    }
    const inputPath = parsed.positionals[0];
    const outputPath = parsed.positionals[1] ??
      inputPath.replace(/\.clapse$/u, ".wasm");
    const projectConfig = await readClapseProjectConfig(inputPath);
    const demandDrivenBase = await buildDemandDrivenCompileInput(
      inputPath,
      parsed.entrypointExports,
      { projectConfig },
    );
    const demandDriven = isCompilerKernelPath(inputPath)
      ? { ...demandDrivenBase, entrypointExports: [] }
      : demandDrivenBase;
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
      projectConfig,
      entrypointExports: demandDriven.entrypointExports,
      inputSourceOverride: demandDriven.inputSourceOverride,
    });
    return;
  }
  if (args[0] === "compile-native") {
    const parsed = parseCompileCommandArgs(args.slice(1));
    if (parsed.positionals.length < 1 || parsed.positionals.length > 2) {
      throw new Error("usage: compile-native <input.clapse> [output.wasm]");
    }
    const inputPath = parsed.positionals[0];
    const outputPath = parsed.positionals[1] ??
      inputPath.replace(/\.clapse$/u, ".wasm");
    const projectConfig = await readClapseProjectConfig(inputPath);
    const demandDrivenBase = await buildDemandDrivenCompileInput(
      inputPath,
      parsed.entrypointExports,
      { projectConfig },
    );
    const demandDriven = isCompilerKernelPath(inputPath)
      ? { ...demandDrivenBase, entrypointExports: [] }
      : demandDrivenBase;
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "kernel-native",
      projectConfig,
      entrypointExports: demandDriven.entrypointExports,
      inputSourceOverride: demandDriven.inputSourceOverride,
    });
    return;
  }
  if (args[0] === "compile-native-debug") {
    const parsed = parseCompileCommandArgs(args.slice(1));
    if (parsed.positionals.length < 1 || parsed.positionals.length > 3) {
      throw new Error(
        "usage: compile-native-debug <input.clapse> [output.wasm] [artifacts-dir]",
      );
    }
    const inputPath = parsed.positionals[0];
    const outputPath = parsed.positionals[1] ??
      inputPath.replace(/\.clapse$/u, ".wasm");
    const defaultArtifactsDir = pathDir(outputPath);
    const artifactsDir = parsed.positionals[2] ??
      (defaultArtifactsDir.length > 0 ? defaultArtifactsDir : ".");
    const projectConfig = await readClapseProjectConfig(inputPath);
    const demandDrivenBase = await buildDemandDrivenCompileInput(
      inputPath,
      parsed.entrypointExports,
      { projectConfig },
    );
    const demandDriven = isCompilerKernelPath(inputPath)
      ? { ...demandDrivenBase, entrypointExports: [] }
      : demandDrivenBase;
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "native-debug",
      projectConfig,
      entrypointExports: demandDriven.entrypointExports,
      inputSourceOverride: demandDriven.inputSourceOverride,
      requireCompileArtifacts: true,
      artifactsDir,
    });
    return;
  }
  if (args[0] === "compile-debug") {
    const parsed = parseCompileCommandArgs(args.slice(1));
    if (parsed.positionals.length < 1 || parsed.positionals.length > 3) {
      throw new Error(
        "usage: compile-debug <input.clapse> [output.wasm] [artifacts-dir]",
      );
    }
    const inputPath = parsed.positionals[0];
    const outputPath = parsed.positionals[1] ??
      inputPath.replace(/\.clapse$/u, ".wasm");
    const defaultArtifactsDir = pathDir(outputPath);
    const artifactsDir = parsed.positionals[2] ??
      (defaultArtifactsDir.length > 0 ? defaultArtifactsDir : ".");
    const projectConfig = await readClapseProjectConfig(inputPath);
    const demandDrivenBase = await buildDemandDrivenCompileInput(
      inputPath,
      parsed.entrypointExports,
      { projectConfig },
    );
    const demandDriven = isCompilerKernelPath(inputPath)
      ? { ...demandDrivenBase, entrypointExports: [] }
      : demandDrivenBase;
    await compileViaWasm(wasmPath, inputPath, outputPath, {
      compileMode: "debug",
      projectConfig,
      entrypointExports: demandDriven.entrypointExports,
      inputSourceOverride: demandDriven.inputSourceOverride,
      requireCompileArtifacts: true,
      artifactsDir,
    });
    return;
  }
  if (args[0] === "emit-wat") {
    await emitWatViaWasm(wasmPath, args);
    return;
  }
  if (args[0] === "parse") {
    if (args.length < 2 || args.length > 3) {
      throw new Error("usage: parse <input.clapse> [out-dir]");
    }
    await parseViaWasm(wasmPath, args[1], args[2] ?? ".");
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
