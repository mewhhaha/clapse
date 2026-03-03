export function buildCompileRequest(
  inputSource,
  compileMode = null,
  options = null,
) {
  const normalizedOptions =
    options && typeof options === "object" ? options : {};
  const inputPath =
    typeof normalizedOptions.inputPath === "string" &&
    normalizedOptions.inputPath.length > 0
      ? normalizedOptions.inputPath
      : "repl/input.clapse";
  const entrypointExports = normalizeEntrypointExports(
    normalizedOptions.entrypointExports,
  );

  const request = {
    command: "compile",
    input_path: inputPath,
    input_source: inputSource,
    plugin_wasm_paths: [],
  };
  if (typeof compileMode === "string" && compileMode.length > 0) {
    request.compile_mode = compileMode;
  }
  if (entrypointExports.length > 0) {
    request.entrypoint_exports = entrypointExports;
  }
  return request;
}

export function buildArtifactsRequest(inputSource) {
  return {
    command: "selfhost-artifacts",
    input_path: "repl/input.clapse",
    input_source: inputSource,
  };
}

export function isCompileResponse(response) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return false;
  }
  if (response.ok !== true) {
    return false;
  }
  if (
    typeof response.wasm_base64 !== "string" ||
    response.wasm_base64.length < 1
  ) {
    return false;
  }
  return true;
}

function normalizeEntrypointExports(value) {
  if (!Array.isArray(value) || value.length === 0) {
    return [];
  }
  const out = [];
  const seen = new Set();
  for (const entry of value) {
    const valueText = String(entry ?? "").trim();
    if (valueText.length === 0 || seen.has(valueText)) {
      continue;
    }
    seen.add(valueText);
    out.push(valueText);
  }
  return out;
}

function isEntrypointExportError(error) {
  if (typeof error !== "string") {
    return false;
  }
  return (
    error.includes("entrypoint_exports") ||
    error.includes("unsupported field") ||
    error.includes("unknown field")
  );
}

function mergeModuleSourcesByPath(moduleSources, paths) {
  const ordered = [];
  for (const path of paths) {
    const source = moduleSources.get(path);
    if (typeof source === "string") {
      ordered.push(source);
    }
  }
  return ordered.join("\n\n");
}

function resolveImportPath(currentPath, moduleName, moduleSources) {
  const candidates = [];
  if (typeof moduleName !== "string" || moduleName.length === 0) {
    return null;
  }

  const normalizedModuleName = moduleName.startsWith("./")
    ? moduleName.slice(2)
    : moduleName;
  candidates.push(normalizedModuleName);
  if (!normalizedModuleName.endsWith(".clapse")) {
    candidates.push(`${normalizedModuleName}.clapse`);
  }
  if (
    !normalizedModuleName.startsWith("/") &&
    !normalizedModuleName.includes("/")
  ) {
    const trimmed = moduleName.trim();
    const parts = currentPath.split("/");
    if (parts.length > 1) {
      parts.pop();
      candidates.push(`${parts.join("/")}/${trimmed}`);
      candidates.push(`${parts.join("/")}/${trimmed}.clapse`);
    }
  }
  for (const candidate of candidates) {
    if (moduleSources.has(candidate)) {
      return candidate;
    }
  }
  return null;
}

function parseImportedModuleRefs(source) {
  const imports = new Set();
  const regex =
    /\bimport\b\s*(?:["']([^"']+)["']|([A-Za-z_][A-Za-z0-9_./-]*))/g;
  let match = regex.exec(source);
  while (match !== null) {
    const target = (match[1] ?? match[2])?.trim();
    if (target.length > 0) {
      imports.add(target);
    }
    match = regex.exec(source);
  }
  return imports;
}

export function tryDebugCompile(session, inputSource, options = null) {
  try {
    const response = session.call(
      buildCompileRequest(inputSource, "debug", options),
    );
    if (isCompileResponse(response)) {
      return { ok: true, response };
    }
    const error =
      response &&
      typeof response === "object" &&
      typeof response.error === "string"
        ? response.error
        : "unexpected compile response";
    return { ok: false, error };
  } catch (err) {
    const error = err instanceof Error ? err.message : String(err);
    return { ok: false, error };
  }
}

export function compileDebugWithLoop({
  session,
  entryPath,
  moduleSources,
  explicitEntrypointExports = [],
}) {
  const sources = moduleSources instanceof Map ? moduleSources : new Map();
  if (!sources.has(entryPath) || typeof sources.get(entryPath) !== "string") {
    return {
      ok: false,
      error: `missing module source: ${entryPath}`,
      passes: 0,
      entryRoots: Array.isArray(explicitEntrypointExports)
        ? explicitEntrypointExports
        : [],
    };
  }

  const rootsByModule = new Map();
  const neededModules = new Set([entryPath]);
  const defaults =
    explicitEntrypointExports.length > 0 ? explicitEntrypointExports : ["main"];
  rootsByModule.set(entryPath, new Set(defaults));

  let pass = 0;
  let changed = true;
  let response = null;
  const MAX_PASSES = 16;
  while (changed && pass < MAX_PASSES) {
    changed = false;
    pass += 1;

    const discoveredModulePaths = Array.from(neededModules);
    for (const modulePath of discoveredModulePaths) {
      const moduleSource = sources.get(modulePath);
      if (typeof moduleSource !== "string") {
        continue;
      }
      const imports = parseImportedModuleRefs(moduleSource);
      for (const imported of imports) {
        const resolved = resolveImportPath(modulePath, imported, sources);
        if (!resolved) {
          continue;
        }
        if (!neededModules.has(resolved)) {
          neededModules.add(resolved);
          changed = true;
        }
        if (!rootsByModule.has(resolved)) {
          rootsByModule.set(resolved, new Set());
        }
      }
    }

    const entryRoots = [...(rootsByModule.get(entryPath) ?? new Set(["main"]))];
    const compileSource = mergeModuleSourcesByPath(sources, neededModules);
    const compileResult = tryDebugCompile(session, compileSource, {
      inputPath: entryPath,
      entrypointExports: entryRoots,
    });
    if (!compileResult.ok) {
      if (
        isEntrypointExportError(compileResult.error) &&
        (entryRoots.length > 0 || compileSource.length > 0)
      ) {
        const fallbackResult = tryDebugCompile(session, compileSource);
        return {
          ...fallbackResult,
          passes: pass,
          entryRoots,
          neededModules,
          usedEntrypointExports: false,
        };
      }
      return {
        ...compileResult,
        passes: pass,
        entryRoots,
        neededModules,
        usedEntrypointExports: true,
      };
    }

    response = compileResult.response;
    const allModulePaths = Array.from(neededModules);
    for (const modulePath of allModulePaths) {
      const roots = rootsByModule.get(modulePath);
      if (!roots) {
        rootsByModule.set(modulePath, new Set());
        changed = true;
      }
    }
  }

  if (!response) {
    return {
      ok: false,
      error: "compile did not produce a response",
      passes: pass,
      entryRoots: [...(rootsByModule.get(entryPath) ?? new Set(["main"]))],
      neededModules,
      usedEntrypointExports: true,
    };
  }

  return {
    ok: true,
    response,
    passes: pass,
    entryRoots: [...(rootsByModule.get(entryPath) ?? new Set(["main"]))],
    neededModules,
    usedEntrypointExports: true,
  };
}

export function runArtifactsPipeline(session, inputSource) {
  let artifactsResponse = null;
  let artifactsError = null;
  try {
    artifactsResponse = session.call(buildArtifactsRequest(inputSource));
  } catch (err) {
    artifactsError = err instanceof Error ? err.message : String(err);
  }
  return { artifactsResponse, artifactsError };
}

export function runCompilePipeline(session, inputSource) {
  const compileResponse = session.call(buildCompileRequest(inputSource));
  const { artifactsResponse, artifactsError } = runArtifactsPipeline(
    session,
    inputSource,
  );
  return { compileResponse, artifactsResponse, artifactsError };
}
