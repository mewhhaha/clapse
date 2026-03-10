#!/usr/bin/env -S deno run -A

import { callCompilerWasmRaw } from "./wasm-compiler-abi.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "").trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/latest/clapse_compiler.wasm";
}

function parseArgs(argv) {
  const out = {
    manifest: "examples/selfhost_behavior_corpus.json",
    compileMode: "debug",
    outPath: "",
    requireNoCompatibility: false,
    requireRawOnly: false,
    requireSuccess: false,
  };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const next = argv[i + 1];
    if (arg === "--manifest" && next) {
      out.manifest = next;
      i += 1;
      continue;
    }
    if (arg === "--compile-mode" && next) {
      out.compileMode = next;
      i += 1;
      continue;
    }
    if (arg === "--out" && next) {
      out.outPath = next;
      i += 1;
      continue;
    }
    if (arg === "--require-no-compatibility" && next) {
      out.requireNoCompatibility = next === "1" || next === "true";
      i += 1;
      continue;
    }
    if (arg === "--require-raw-only" && next) {
      out.requireRawOnly = next === "1" || next === "true";
      i += 1;
      continue;
    }
    if (arg === "--require-success" && next) {
      out.requireSuccess = next === "1" || next === "true";
      i += 1;
      continue;
    }
  }
  return out;
}

function parseManifest(raw) {
  const decoded = JSON.parse(raw);
  assert(decoded && Array.isArray(decoded.scenarios),
    "invalid behavior manifest: expected { scenarios: [...] }");
  return decoded.scenarios;
}

function uniqueCompileCases(scenarios) {
  const seen = new Set();
  const out = [];
  for (const scenario of scenarios) {
    const entry = typeof scenario?.entry === "string" ? scenario.entry.trim() : "";
    const exportName = typeof scenario?.export === "string" && scenario.export.trim().length > 0
      ? scenario.export.trim()
      : "main";
    assert(entry.length > 0, "behavior manifest scenario missing entry");
    assert(exportName.length > 0, `behavior manifest scenario missing export for ${entry}`);
    const key = `${entry}\u0000${exportName}`;
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    out.push({
      entry,
      exportName,
      label: `${entry}#${exportName}`,
    });
  }
  return out;
}

function buildCompileRequest(entry, source, exportName, compileMode) {
  return {
    command: "compile",
    compile_mode: compileMode,
    input_path: entry,
    input_source: source,
    plugin_wasm_paths: [],
    entrypoint_exports: [exportName],
  };
}

function exportNames(response) {
  if (!Array.isArray(response?.public_exports)) {
    return [];
  }
  return response.public_exports
    .map((entry) => entry?.name)
    .filter((name) => typeof name === "string");
}

function incrementCounter(map, key) {
  map.set(key, (map.get(key) ?? 0) + 1);
}

async function main() {
  const args = parseArgs(Deno.args);
  const wasmPath = resolveCompilerWasmPath();
  const scenarios = parseManifest(await Deno.readTextFile(args.manifest));
  const cases = uniqueCompileCases(scenarios);
  const sourceCache = new Map();
  const strategyCounts = new Map();
  const errorCounts = new Map();
  const results = [];
  const failures = [];
  const compatibilityLabels = [];
  const nonRawLabels = [];

  for (const testCase of cases) {
    let source = sourceCache.get(testCase.entry);
    if (typeof source !== "string") {
      source = await Deno.readTextFile(testCase.entry);
      sourceCache.set(testCase.entry, source);
    }
    try {
      const response = await callCompilerWasmRaw(
        wasmPath,
        buildCompileRequest(
          testCase.entry,
          source,
          testCase.exportName,
          args.compileMode,
        ),
        {
          validateCompileContract: true,
          withContractMetadata: true,
        },
      );
      const compileStrategy = typeof response?.compile_strategy === "string" &&
          response.compile_strategy.length > 0
        ? response.compile_strategy
        : "<missing>";
      const compatibilityUsed = response?.compatibility_used === true ||
        compileStrategy === "phase1_compatibility_stub";
      const publicExportNames = exportNames(response);
      const requestedExportPresent = publicExportNames.includes(testCase.exportName);
      const ok = response?.ok === true && requestedExportPresent;
      const failureReason = response?.ok !== true
        ? String(response?.error_code ?? response?.error ?? "compile failed")
        : requestedExportPresent
        ? ""
        : `missing requested export ${testCase.exportName} in ${JSON.stringify(publicExportNames)}`;
      if (response?.ok === true) {
        incrementCounter(strategyCounts, compileStrategy);
        if (compatibilityUsed) {
          compatibilityLabels.push(testCase.label);
        }
        if (compileStrategy !== "compiler_raw") {
          nonRawLabels.push(`${testCase.label}:${compileStrategy}`);
        }
      } else {
        incrementCounter(
          errorCounts,
          String(response?.error_code ?? response?.error ?? "compile failed"),
        );
      }
      const result = {
        ...testCase,
        ok,
        compile_strategy: compileStrategy,
        compatibility_used: compatibilityUsed,
        public_exports: publicExportNames,
      };
      if (!ok) {
        result.failure_reason = failureReason;
        failures.push(`${testCase.label}: ${failureReason}`);
      }
      results.push(result);
    } catch (error) {
      const failureReason = String(error?.message ?? error);
      failures.push(`${testCase.label}: ${failureReason}`);
      results.push({
        ...testCase,
        ok: false,
        compile_strategy: "",
        compatibility_used: false,
        public_exports: [],
        failure_reason: failureReason,
      });
    }
  }

  const strategySummary = Object.fromEntries(
    [...strategyCounts.entries()].sort((a, b) => a[0].localeCompare(b[0])),
  );
  const report = {
    generated_at: new Date().toISOString(),
    wasm_path: wasmPath,
    manifest: args.manifest,
    compile_mode: args.compileMode,
    require_no_compatibility: args.requireNoCompatibility,
    require_raw_only: args.requireRawOnly,
    require_success: args.requireSuccess,
    summary: {
      cases: cases.length,
      ok: results.filter((entry) => entry.ok).length,
      failures: failures.length,
      compatibility_used: compatibilityLabels.length,
      compiler_raw: strategyCounts.get("compiler_raw") ?? 0,
      non_raw: nonRawLabels.length,
      strategy_counts: strategySummary,
      error_counts: Object.fromEntries(
        [...errorCounts.entries()].sort((a, b) => a[0].localeCompare(b[0])),
      ),
    },
    results,
  };

  if (args.outPath.length > 0) {
    const outDir = args.outPath.replace(/\/[^/]*$/, "");
    if (outDir.length > 0 && outDir !== args.outPath) {
      await Deno.mkdir(outDir, { recursive: true });
    }
    await Deno.writeTextFile(args.outPath, JSON.stringify(report, null, 2) + "\n");
  }

  const strategyParts = [...Object.entries(strategySummary)]
    .map(([strategy, count]) => `${strategy}=${count}`)
    .join(",");
  console.log(
    `selfhost-compile-strategy-report: cases=${cases.length} ok=${
      report.summary.ok
    } compatibility=${compatibilityLabels.length} non_raw=${nonRawLabels.length} strategies=[${
      strategyParts
    }]`,
  );
  if (compatibilityLabels.length > 0) {
    console.log(`selfhost-compile-strategy-report: compatibility labels: ${compatibilityLabels.join(", ")}`);
  }
  if (nonRawLabels.length > 0) {
    console.log(`selfhost-compile-strategy-report: non-raw labels: ${nonRawLabels.join(", ")}`);
  }
  if (failures.length > 0) {
    for (const failure of failures) {
      console.error(`selfhost-compile-strategy-report: ${failure}`);
    }
  }

  if (args.requireSuccess && failures.length > 0) {
    throw new Error(`${failures.length} compile cases failed`);
  }
  if (args.requireNoCompatibility && compatibilityLabels.length > 0) {
    throw new Error(`compatibility path used for ${compatibilityLabels.length} compile cases`);
  }
  if (args.requireRawOnly && nonRawLabels.length > 0) {
    throw new Error(`non-raw compile strategy used for ${nonRawLabels.length} compile cases`);
  }
}

await main().catch((error) => {
  console.error(`selfhost-compile-strategy-report: FAIL (${String(error?.message ?? error)})`);
  Deno.exit(1);
});
