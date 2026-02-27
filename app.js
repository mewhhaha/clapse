const GITHUB_OWNER = "mewhhaha";
const GITHUB_REPO = "clapse";
const RELEASES_API =
  `https://api.github.com/repos/${GITHUB_OWNER}/${GITHUB_REPO}/releases?per_page=40`;
const RELEASE_PROBE_LIMIT = 24;
const SUPPORTED_RELEASE_TARGET = 12;
const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();
const TAGGED_INT_MAX = 1073741823;
const TAGGED_INT_MIN = -1073741824;
const WASM_PAGE_SIZE = 65536;
const HOST_ALLOC_GUARD_BYTES = 16 * WASM_PAGE_SIZE;
const SLICE_DESC_SIZE = 8;

const SAMPLE_PROGRAMS = [
  {
    id: "identity",
    label: "Identity",
    source: `identity x = x

main = identity 7
`,
  },
  {
    id: "arithmetic",
    label: "Arithmetic",
    source: `double x = x + x

main = double 21
`,
  },
  {
    id: "manual",
    label: "Custom (keep editor text)",
    source: "",
  },
];

const state = {
  supportedReleases: [],
  compilerByTag: new Map(),
  lastRuns: {
    primary: null,
    compare: null,
  },
  downloadUrls: {
    primary: null,
    compare: null,
  },
  running: false,
};

const elements = {
  primaryRelease: document.getElementById("primary-release"),
  compareRelease: document.getElementById("compare-release"),
  stepPrimaryBack: document.getElementById("step-primary-back"),
  stepPrimaryForward: document.getElementById("step-primary-forward"),
  refreshReleases: document.getElementById("refresh-releases"),
  releaseStatus: document.getElementById("release-status"),
  sampleProgram: document.getElementById("sample-program"),
  inputPath: document.getElementById("input-path"),
  exportName: document.getElementById("export-name"),
  exportArgs: document.getElementById("export-args"),
  sourceCode: document.getElementById("source-code"),
  runPrimary: document.getElementById("run-primary"),
  runCompare: document.getElementById("run-compare"),
  runBoth: document.getElementById("run-both"),
  pipelineStatus: document.getElementById("pipeline-status"),
  compareSummary: document.getElementById("compare-summary"),
};

const panelNodes = {
  primary: {
    version: document.getElementById("primary-version"),
    irLowered: document.getElementById("primary-ir-lowered"),
    irCollapsed: document.getElementById("primary-ir-collapsed"),
    wasm: document.getElementById("primary-wasm"),
    repl: document.getElementById("primary-repl"),
    download: document.getElementById("primary-download"),
  },
  compare: {
    version: document.getElementById("compare-version"),
    irLowered: document.getElementById("compare-ir-lowered"),
    irCollapsed: document.getElementById("compare-ir-collapsed"),
    wasm: document.getElementById("compare-wasm"),
    repl: document.getElementById("compare-repl"),
    download: document.getElementById("compare-download"),
  },
};

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  elements.pipelineStatus.textContent = `Initialization failed: ${message}`;
});

async function main() {
  populateSamples();
  bindEvents();
  resetPanel("primary");
  resetPanel("compare");
  elements.compareSummary.textContent = "Run primary and compare to generate a diff summary.";
  await refreshSupportedReleases();
}

function bindEvents() {
  elements.refreshReleases.addEventListener("click", () => {
    refreshSupportedReleases().catch((err) => {
      setPipelineStatus(err, "Refresh failed");
    });
  });

  elements.sampleProgram.addEventListener("change", () => {
    const selectedId = elements.sampleProgram.value;
    const sample = SAMPLE_PROGRAMS.find((entry) => entry.id === selectedId);
    if (!sample || sample.id === "manual") {
      return;
    }
    elements.sourceCode.value = sample.source;
  });

  elements.stepPrimaryBack.addEventListener("click", () => {
    stepPrimary(-1);
  });
  elements.stepPrimaryForward.addEventListener("click", () => {
    stepPrimary(1);
  });

  elements.runPrimary.addEventListener("click", () => {
    void runOnlyPrimary();
  });
  elements.runCompare.addEventListener("click", () => {
    void runOnlyCompare();
  });
  elements.runBoth.addEventListener("click", () => {
    void runBothSides();
  });
}

function populateSamples() {
  elements.sampleProgram.innerHTML = "";
  for (const sample of SAMPLE_PROGRAMS) {
    const option = document.createElement("option");
    option.value = sample.id;
    option.textContent = sample.label;
    elements.sampleProgram.append(option);
  }
  elements.sampleProgram.value = SAMPLE_PROGRAMS[0].id;
  elements.sourceCode.value = SAMPLE_PROGRAMS[0].source;
}

async function refreshSupportedReleases() {
  toggleReleaseControls(true);
  elements.releaseStatus.textContent = "Fetching release metadata...";
  try {
    const releases = await fetchReleaseMetadata();
    if (releases.length === 0) {
      throw new Error("GitHub releases API returned no tags.");
    }

    const candidates = releases.slice(0, RELEASE_PROBE_LIMIT);
    const supported = [];

    for (let i = 0; i < candidates.length; i += 1) {
      const release = candidates[i];
      elements.releaseStatus.textContent =
        `Probing ${release.tag} (${i + 1}/${candidates.length})...`;
      const result = await probeReleaseSupport(release);
      if (result.supported) {
        supported.push(release);
      }
      if (supported.length >= SUPPORTED_RELEASE_TARGET) {
        break;
      }
    }

    if (supported.length === 0) {
      elements.releaseStatus.textContent =
        "No compatible native compiler release was found in the probed window.";
      return;
    }

    state.supportedReleases = supported;
    renderReleaseSelectors();
    elements.releaseStatus.textContent =
      `Loaded ${supported.length} supported release(s) from GitHub Releases.`;
  } finally {
    toggleReleaseControls(false);
  }
}

function renderReleaseSelectors() {
  const previousPrimary = elements.primaryRelease.value;
  const previousCompare = elements.compareRelease.value;

  elements.primaryRelease.innerHTML = "";
  elements.compareRelease.innerHTML = "";

  const noneOption = document.createElement("option");
  noneOption.value = "";
  noneOption.textContent = "None";
  elements.compareRelease.append(noneOption);

  for (const release of state.supportedReleases) {
    const label = formatReleaseLabel(release);

    const primaryOption = document.createElement("option");
    primaryOption.value = release.tag;
    primaryOption.textContent = label;
    elements.primaryRelease.append(primaryOption);

    const compareOption = document.createElement("option");
    compareOption.value = release.tag;
    compareOption.textContent = label;
    elements.compareRelease.append(compareOption);
  }

  if (hasOption(elements.primaryRelease, previousPrimary)) {
    elements.primaryRelease.value = previousPrimary;
  } else {
    elements.primaryRelease.selectedIndex = 0;
  }

  if (hasOption(elements.compareRelease, previousCompare)) {
    elements.compareRelease.value = previousCompare;
  } else if (state.supportedReleases.length > 1) {
    const primaryTag = elements.primaryRelease.value;
    const fallback = state.supportedReleases.find((release) =>
      release.tag !== primaryTag
    );
    elements.compareRelease.value = fallback ? fallback.tag : "";
  } else {
    elements.compareRelease.value = "";
  }
}

function hasOption(selectEl, value) {
  for (const option of selectEl.options) {
    if (option.value === value) {
      return true;
    }
  }
  return false;
}

function formatReleaseLabel(release) {
  const date = release.publishedAt
    ? new Date(release.publishedAt).toISOString().slice(0, 10)
    : "unknown-date";
  return `${release.tag} (${date})`;
}

function toggleReleaseControls(disabled) {
  elements.primaryRelease.disabled = disabled;
  elements.compareRelease.disabled = disabled;
  elements.refreshReleases.disabled = disabled;
  elements.stepPrimaryBack.disabled = disabled;
  elements.stepPrimaryForward.disabled = disabled;
}

function stepPrimary(delta) {
  const select = elements.primaryRelease;
  if (select.options.length === 0) {
    return;
  }
  const nextIndex = clamp(select.selectedIndex + delta, 0, select.options.length - 1);
  select.selectedIndex = nextIndex;
}

function clamp(n, min, max) {
  return Math.max(min, Math.min(max, n));
}

async function fetchReleaseMetadata() {
  const response = await fetch(RELEASES_API, {
    headers: {
      Accept: "application/vnd.github+json",
    },
  });
  if (!response.ok) {
    throw new Error(`GitHub releases request failed with HTTP ${response.status}`);
  }
  const payload = await response.json();
  if (!Array.isArray(payload)) {
    throw new Error("GitHub releases payload was not an array.");
  }
  const releases = [];
  for (const entry of payload) {
    if (!entry || typeof entry !== "object") {
      continue;
    }
    if (entry.draft === true) {
      continue;
    }
    const tag = String(entry.tag_name ?? "").trim();
    if (tag.length === 0) {
      continue;
    }
    releases.push({
      tag,
      name: String(entry.name ?? tag),
      publishedAt: String(entry.published_at ?? entry.created_at ?? ""),
    });
  }
  return releases;
}

function rawTagFileUrl(tag, path) {
  return `https://raw.githubusercontent.com/${GITHUB_OWNER}/${GITHUB_REPO}/${encodeURIComponent(tag)}/${path}`;
}

async function probeReleaseSupport(release) {
  const tag = release.tag;
  try {
    const session = await createCompilerSession(tag);
    const probeCompileResponse = await session.call({
      command: "compile",
      input_path: "probe/empty.clapse",
      input_source: "",
      plugin_wasm_paths: [],
    });
    if (!hasBoolOk(probeCompileResponse)) {
      return { supported: false, reason: "compile response missing ok field" };
    }
    if (isUnsupportedCommand(probeCompileResponse)) {
      return { supported: false, reason: "compile command unavailable" };
    }

    const probeArtifactsResponse = await session.call({
      command: "selfhost-artifacts",
      input_path: "probe/empty.clapse",
      input_source: "",
    });
    if (!hasBoolOk(probeArtifactsResponse)) {
      return { supported: false, reason: "selfhost-artifacts response missing ok field" };
    }
    if (isUnsupportedCommand(probeArtifactsResponse)) {
      return { supported: false, reason: "selfhost-artifacts command unavailable" };
    }
    return { supported: true };
  } catch (err) {
    return {
      supported: false,
      reason: err instanceof Error ? err.message : String(err),
    };
  }
}

function hasBoolOk(response) {
  return Boolean(
    response &&
      typeof response === "object" &&
      !Array.isArray(response) &&
      typeof response.ok === "boolean",
  );
}

function isUnsupportedCommand(response) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return false;
  }
  const message = String(response.error ?? "").toLowerCase();
  return message.includes("unsupported command");
}

async function runOnlyPrimary() {
  const tag = elements.primaryRelease.value;
  if (!tag) {
    elements.pipelineStatus.textContent = "Select a primary release first.";
    return;
  }
  await withRunLock(async () => {
    await runPipeline("primary", tag);
    updateCompareSummary();
  });
}

async function runOnlyCompare() {
  const tag = elements.compareRelease.value;
  if (!tag) {
    elements.pipelineStatus.textContent = "Select a compare release first.";
    return;
  }
  await withRunLock(async () => {
    await runPipeline("compare", tag);
    updateCompareSummary();
  });
}

async function runBothSides() {
  const primaryTag = elements.primaryRelease.value;
  if (!primaryTag) {
    elements.pipelineStatus.textContent = "Select a primary release first.";
    return;
  }
  const compareTag = elements.compareRelease.value;
  await withRunLock(async () => {
    await runPipeline("primary", primaryTag);
    if (compareTag) {
      await runPipeline("compare", compareTag);
    } else {
      resetPanel("compare");
      state.lastRuns.compare = null;
    }
    updateCompareSummary();
  });
}

async function withRunLock(action) {
  if (state.running) {
    elements.pipelineStatus.textContent = "A run is already in progress.";
    return;
  }
  state.running = true;
  toggleRunButtons(true);
  try {
    await action();
  } catch (err) {
    setPipelineStatus(err, "Pipeline failed");
  } finally {
    state.running = false;
    toggleRunButtons(false);
  }
}

function toggleRunButtons(disabled) {
  elements.runPrimary.disabled = disabled;
  elements.runCompare.disabled = disabled;
  elements.runBoth.disabled = disabled;
}

async function runPipeline(side, tag) {
  const panel = panelNodes[side];
  const source = elements.sourceCode.value;
  const inputPath = elements.inputPath.value.trim() || "repl/input.clapse";
  const exportName = elements.exportName.value.trim() || "main";
  const argsText = elements.exportArgs.value.trim();

  panel.version.textContent = `Version: ${tag}`;
  panel.irLowered.textContent = "Loading IR...";
  panel.irCollapsed.textContent = "Loading IR...";
  panel.wasm.textContent = "Compiling wasm...";
  panel.repl.textContent = "Running REPL...";
  setDownloadLink(side, null, null);

  elements.pipelineStatus.textContent = `Running ${side} pipeline on ${tag}...`;

  const session = await createCompilerSession(tag);
  const artifactsResponse = await session.call({
    command: "selfhost-artifacts",
    input_path: inputPath,
    input_source: source,
  });

  const loweredIr = extractArtifactText(artifactsResponse, "lowered_ir.txt");
  const collapsedIr = extractArtifactText(artifactsResponse, "collapsed_ir.txt");

  const compileResponse = await session.call({
    command: "compile",
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
  });

  let wasmBytes = null;
  let replOutput = "Skipped because compilation did not produce wasm.";
  let wasmSummary = formatCompileError(compileResponse);
  let exportsList = [];

  if (compileResponse && compileResponse.ok === true) {
    if (typeof compileResponse.wasm_base64 === "string" && compileResponse.wasm_base64.length > 0) {
      wasmBytes = decodeWasmBase64(compileResponse.wasm_base64);
      exportsList = normalizeExportsList(compileResponse.exports);
      wasmSummary = formatWasmSummary(tag, wasmBytes, exportsList);
      try {
        replOutput = await executeRepl(wasmBytes, exportName, argsText, exportsList);
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        replOutput = `REPL failed: ${message}`;
      }
      setDownloadLink(side, wasmBytes, tag);
    } else {
      wasmSummary = "Compile succeeded, but wasm_base64 was empty.";
      replOutput = "No wasm emitted.";
    }
  }

  panel.irLowered.textContent = loweredIr;
  panel.irCollapsed.textContent = collapsedIr;
  panel.wasm.textContent = wasmSummary;
  panel.repl.textContent = replOutput;

  state.lastRuns[side] = {
    tag,
    loweredIr,
    collapsedIr,
    wasmBytes,
    exportsList,
    compileOk: compileResponse && compileResponse.ok === true,
    artifactsOk: artifactsResponse && artifactsResponse.ok === true,
    compileError: textOrEmpty(compileResponse?.error),
    artifactsError: textOrEmpty(artifactsResponse?.error),
  };

  elements.pipelineStatus.textContent = `${side} pipeline finished for ${tag}.`;
}

function extractArtifactText(response, key) {
  if (response && response.ok === true && response.artifacts && typeof response.artifacts === "object") {
    const value = response.artifacts[key];
    if (typeof value === "string" && value.length > 0) {
      return value;
    }
    return `(artifact '${key}' was not returned)`;
  }
  return formatCompileError(response);
}

function formatCompileError(response) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return "No structured compiler response.";
  }
  const okLine = `ok: ${String(response.ok === true)}`;
  const errorText = textOrEmpty(response.error);
  if (errorText.length > 0) {
    return `${okLine}\nerror: ${errorText}`;
  }
  return okLine;
}

function normalizeExportsList(value) {
  if (!Array.isArray(value)) {
    return [];
  }
  const out = [];
  for (const entry of value) {
    if (!entry || typeof entry !== "object" || Array.isArray(entry)) {
      continue;
    }
    const name = String(entry.name ?? "").trim();
    const arity = Number(entry.arity);
    if (name.length === 0 || !Number.isInteger(arity) || arity < 0) {
      continue;
    }
    out.push({ name, arity });
  }
  return out;
}

function textOrEmpty(value) {
  return typeof value === "string" ? value : "";
}

function formatWasmSummary(tag, wasmBytes, exportsList) {
  const hash = fnv1aHex(wasmBytes);
  const exportNames = exportsList.map((entry) => `${entry.name}/${entry.arity}`);
  const lines = [
    `release: ${tag}`,
    `bytes: ${wasmBytes.length}`,
    `fnv1a32: ${hash}`,
  ];
  if (exportNames.length > 0) {
    lines.push(`exports: ${exportNames.join(", ")}`);
  } else {
    lines.push("exports: (none reported)");
  }
  return lines.join("\n");
}

async function executeRepl(wasmBytes, exportName, argsText, exportsList) {
  const module = await WebAssembly.compile(wasmBytes);
  const imports = buildStubImports(WebAssembly.Module.imports(module));
  const instance = await WebAssembly.instantiate(module, imports);
  const fn = instance.exports[exportName];
  if (typeof fn !== "function") {
    const available = Object.keys(instance.exports)
      .filter((name) => typeof instance.exports[name] === "function")
      .join(", ");
    return `Export '${exportName}' not found.\nAvailable exports: ${available || "(none)"}`;
  }

  const parsedArgs = parseArgList(argsText);
  const expected = exportsList.find((entry) => entry.name === exportName);
  if (expected && expected.arity !== parsedArgs.length) {
    return `Arity mismatch for ${exportName}: expected ${expected.arity}, got ${parsedArgs.length}.`;
  }

  const encodedArgs = parsedArgs.map((n) => encodeInt(n));
  let resultValue;
  try {
    resultValue = fn(...encodedArgs);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return `Wasm trap while executing ${exportName}: ${message}`;
  }
  const memory = instance.exports.__memory ?? instance.exports.memory;
  const rendered = renderRuntimeValue(resultValue, memory);
  return `result: ${rendered}`;
}

function parseArgList(text) {
  const trimmed = text.trim();
  if (trimmed.length === 0) {
    return [];
  }
  const pieces = trimmed.split(",");
  const out = [];
  for (const piece of pieces) {
    const raw = piece.trim();
    if (raw.length === 0) {
      continue;
    }
    const value = Number(raw);
    if (!Number.isInteger(value)) {
      throw new Error(`Invalid integer argument: ${raw}`);
    }
    out.push(value);
  }
  return out;
}

function encodeInt(n) {
  if (!Number.isInteger(n)) {
    throw new Error(`Non-integer value cannot be encoded: ${n}`);
  }
  if (n < TAGGED_INT_MIN || n > TAGGED_INT_MAX) {
    throw new Error(`Tagged int payload out of range: ${n}`);
  }
  return ((n | 0) << 1) | 1;
}

function decodeInt(v) {
  return v >> 1;
}

function isTaggedInt(v) {
  return (v & 1) === 1;
}

function renderRuntimeValue(value, memoryExport) {
  const signed = value | 0;
  if (isTaggedInt(signed)) {
    return String(decodeInt(signed));
  }
  if (memoryExport instanceof WebAssembly.Memory) {
    try {
      const desc = readSliceDescriptor(memoryExport, value >>> 0);
      const bytes = new Uint8Array(memoryExport.buffer, desc.dataPtr, desc.len);
      if (bytesLikelyText(bytes)) {
        return UTF8_DECODER.decode(bytes);
      }
      return `<slice_u8 len=${desc.len}>`;
    } catch (_err) {
      return `<raw:${value >>> 0}>`;
    }
  }
  return `<raw:${value >>> 0}>`;
}

function readSliceDescriptor(memory, ptr) {
  if (!(memory instanceof WebAssembly.Memory)) {
    throw new Error("memory export is unavailable");
  }
  if (ptr + SLICE_DESC_SIZE > memory.buffer.byteLength) {
    throw new Error("slice descriptor is out of bounds");
  }
  const view = new DataView(memory.buffer);
  const dataPtr = view.getUint32(ptr, true);
  const len = view.getInt32(ptr + 4, true);
  if (len < 0 || dataPtr + len > memory.buffer.byteLength) {
    throw new Error("invalid slice descriptor");
  }
  return { dataPtr, len };
}

function bytesLikelyText(bytes) {
  for (let i = 0; i < bytes.length; i += 1) {
    const b = bytes[i];
    if (b === 0) {
      return false;
    }
    if (b !== 9 && b !== 10 && b !== 13 && (b < 32 || b > 126)) {
      return false;
    }
  }
  return true;
}

function fnv1aHex(bytes) {
  let hash = 0x811c9dc5;
  for (let i = 0; i < bytes.length; i += 1) {
    hash ^= bytes[i];
    hash = Math.imul(hash, 0x01000193);
  }
  return (hash >>> 0).toString(16).padStart(8, "0");
}

function decodeWasmBase64(input) {
  const raw = atob(input);
  const out = new Uint8Array(raw.length);
  for (let i = 0; i < raw.length; i += 1) {
    out[i] = raw.charCodeAt(i);
  }
  return out;
}

function setDownloadLink(side, wasmBytes, tag) {
  const link = panelNodes[side].download;
  const existingUrl = state.downloadUrls[side];
  if (existingUrl) {
    URL.revokeObjectURL(existingUrl);
    state.downloadUrls[side] = null;
  }
  if (!(wasmBytes instanceof Uint8Array) || !tag) {
    link.hidden = true;
    link.href = "#";
    return;
  }
  const blob = new Blob([wasmBytes], { type: "application/wasm" });
  const url = URL.createObjectURL(blob);
  state.downloadUrls[side] = url;
  link.href = url;
  link.download = `${tag}-compiled.wasm`;
  link.hidden = false;
}

function updateCompareSummary() {
  const primary = state.lastRuns.primary;
  const compare = state.lastRuns.compare;
  if (!primary || !compare) {
    elements.compareSummary.textContent = "Run both sides to compare outputs.";
    return;
  }

  const primaryExports = primary.exportsList.map((entry) => entry.name);
  const compareExports = compare.exportsList.map((entry) => entry.name);
  const primaryOnlyExports = primaryExports.filter((name) =>
    !compareExports.includes(name)
  );
  const compareOnlyExports = compareExports.filter((name) =>
    !primaryExports.includes(name)
  );

  const primaryWasmLen = primary.wasmBytes ? primary.wasmBytes.length : 0;
  const compareWasmLen = compare.wasmBytes ? compare.wasmBytes.length : 0;
  const delta = primaryWasmLen - compareWasmLen;

  const loweredEqual = normalizeText(primary.loweredIr) === normalizeText(compare.loweredIr);
  const collapsedEqual = normalizeText(primary.collapsedIr) === normalizeText(compare.collapsedIr);

  const lines = [
    `primary: ${primary.tag}`,
    `compare: ${compare.tag}`,
    `primary compile ok: ${String(primary.compileOk)}`,
    `compare compile ok: ${String(compare.compileOk)}`,
    `primary artifacts ok: ${String(primary.artifactsOk)}`,
    `compare artifacts ok: ${String(compare.artifactsOk)}`,
    `lowered IR equal: ${String(loweredEqual)}`,
    `collapsed IR equal: ${String(collapsedEqual)}`,
    `wasm bytes primary: ${primaryWasmLen}`,
    `wasm bytes compare: ${compareWasmLen}`,
    `wasm byte delta (primary - compare): ${delta}`,
  ];

  if (primary.wasmBytes && compare.wasmBytes) {
    lines.push(`primary wasm hash: ${fnv1aHex(primary.wasmBytes)}`);
    lines.push(`compare wasm hash: ${fnv1aHex(compare.wasmBytes)}`);
  }
  if (primaryOnlyExports.length > 0) {
    lines.push(`exports only in primary: ${primaryOnlyExports.join(", ")}`);
  }
  if (compareOnlyExports.length > 0) {
    lines.push(`exports only in compare: ${compareOnlyExports.join(", ")}`);
  }
  if (primaryOnlyExports.length === 0 && compareOnlyExports.length === 0) {
    lines.push("exports diff: none");
  }

  elements.compareSummary.textContent = lines.join("\n");
}

function normalizeText(value) {
  return String(value ?? "").trim().replace(/\r\n/g, "\n");
}

function setPipelineStatus(err, prefix) {
  const message = err instanceof Error ? err.message : String(err);
  elements.pipelineStatus.textContent = `${prefix}: ${message}`;
}

function resetPanel(side) {
  const panel = panelNodes[side];
  panel.version.textContent = "Version: (none)";
  panel.irLowered.textContent = "No run yet.";
  panel.irCollapsed.textContent = "No run yet.";
  panel.wasm.textContent = "No run yet.";
  panel.repl.textContent = "No run yet.";
  setDownloadLink(side, null, null);
}

async function createCompilerSession(tag) {
  const compiler = await loadCompilerRecord(tag);
  const imports = buildStubImports(WebAssembly.Module.imports(compiler.module));
  const { instance } = await WebAssembly.instantiate(compiler.module, imports);
  const memory = instance.exports.__memory ?? instance.exports.memory;
  if (!(memory instanceof WebAssembly.Memory)) {
    throw new Error(`Release ${tag} is missing memory export.`);
  }
  const run = instance.exports.clapse_run;
  if (typeof run !== "function") {
    throw new Error(`Release ${tag} is missing clapse_run export.`);
  }

  const runtime = makeRuntime();
  runtime.state.memory = memory;
  const heapGlobal = instance.exports.__heap_ptr;
  if (heapGlobal instanceof WebAssembly.Global) {
    runtime.state.heapGlobal = heapGlobal;
  }

  return {
    async call(requestObject) {
      const requestText = JSON.stringify(requestObject);
      const requestBytes = UTF8_ENCODER.encode(requestText);
      const requestHandle = runtime.allocSliceU8(requestBytes);
      const responseHandle = run(requestHandle | 0);
      if (!Number.isInteger(responseHandle) || (responseHandle & 1) === 1) {
        throw new Error(`Release ${tag} returned invalid response handle: ${responseHandle}`);
      }
      const responseBytes = runtime.readSliceU8Copy(responseHandle);
      const responseText = UTF8_DECODER.decode(responseBytes);
      try {
        return JSON.parse(responseText);
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        throw new Error(`Release ${tag} returned invalid JSON: ${message}`);
      }
    },
  };
}

async function loadCompilerRecord(tag) {
  if (state.compilerByTag.has(tag)) {
    return state.compilerByTag.get(tag);
  }

  const wasmUrl = rawTagFileUrl(tag, "artifacts/latest/clapse_compiler.wasm");
  const response = await fetch(wasmUrl, { cache: "no-store" });
  if (!response.ok) {
    throw new Error(`Release ${tag} compiler wasm fetch failed (HTTP ${response.status}).`);
  }
  const wasmBytes = new Uint8Array(await response.arrayBuffer());
  if (wasmBytes.length === 0) {
    throw new Error(`Release ${tag} compiler wasm was empty.`);
  }

  const module = await WebAssembly.compile(wasmBytes);
  const imports = WebAssembly.Module.imports(module);
  const isBridgeMode = imports.some((entry) =>
    entry.module === "host" && entry.name === "clapse_run"
  );
  if (isBridgeMode) {
    throw new Error(`Release ${tag} uses bridge compiler wasm; browser mode requires native compiler wasm.`);
  }
  for (const entry of imports) {
    if (entry.kind !== "function") {
      throw new Error(
        `Release ${tag} imports unsupported ${entry.kind} '${entry.module}.${entry.name}'.`,
      );
    }
  }

  const testImports = buildStubImports(imports);
  const { instance } = await WebAssembly.instantiate(module, testImports);
  const memory = instance.exports.__memory ?? instance.exports.memory;
  if (!(memory instanceof WebAssembly.Memory)) {
    throw new Error(`Release ${tag} compiler wasm missing memory export.`);
  }
  if (typeof instance.exports.clapse_run !== "function") {
    throw new Error(`Release ${tag} compiler wasm missing clapse_run export.`);
  }

  const record = {
    tag,
    wasmUrl,
    module,
  };
  state.compilerByTag.set(tag, record);
  return record;
}

function buildStubImports(imports) {
  const out = {};
  for (const entry of imports) {
    if (entry.kind !== "function") {
      throw new Error(
        `Wasm import '${entry.module}.${entry.name}' has unsupported kind '${entry.kind}'.`,
      );
    }
    if (!out[entry.module]) {
      out[entry.module] = {};
    }
    out[entry.module][entry.name] = (..._args) => 0;
  }
  if (!out.host) {
    out.host = {};
  }
  if (typeof out.host.clapse_run !== "function") {
    out.host.clapse_run = (handle) => handle | 0;
  }
  if (typeof out.host.clapse_host_run !== "function") {
    out.host.clapse_host_run = (handle) => handle | 0;
  }
  if (typeof out.host.read_file !== "function") {
    out.host.read_file = () => 0;
  }
  if (typeof out.host.unix_time_ms !== "function") {
    out.host.unix_time_ms = (seed) => seed | 0;
  }
  return out;
}

function makeRuntime() {
  const localState = {
    memory: null,
    nextAlloc: null,
    heapGlobal: null,
  };

  function ensureMemory() {
    if (!(localState.memory instanceof WebAssembly.Memory)) {
      throw new Error("wasm memory export is unavailable");
    }
    return localState.memory;
  }

  function alignUp(value, align) {
    const rounded = value + (align - 1);
    return rounded - (rounded % align);
  }

  function initLinearAllocator() {
    const memory = ensureMemory();
    if (localState.nextAlloc === null) {
      localState.nextAlloc = memory.buffer.byteLength + HOST_ALLOC_GUARD_BYTES;
    }
    if (localState.heapGlobal instanceof WebAssembly.Global) {
      const floor = localState.heapGlobal.value >>> 0;
      if (floor > localState.nextAlloc) {
        localState.nextAlloc = floor + HOST_ALLOC_GUARD_BYTES;
      }
    }
  }

  function ensureLinearCapacity(requiredEnd) {
    const memory = ensureMemory();
    if (requiredEnd <= memory.buffer.byteLength) {
      return;
    }
    const missing = requiredEnd - memory.buffer.byteLength;
    const pages = Math.ceil(missing / WASM_PAGE_SIZE);
    memory.grow(pages);
  }

  function allocLinear(size, align) {
    initLinearAllocator();
    const start = alignUp(localState.nextAlloc, align);
    const end = start + size;
    ensureLinearCapacity(end);
    localState.nextAlloc = end;
    return start;
  }

  function allocSliceU8(input) {
    const source = toUint8Array(input);
    const descPtr = allocLinear(SLICE_DESC_SIZE, 4);
    const dataPtr = allocLinear(source.length, 1);
    const memory = ensureMemory();
    const view = new DataView(memory.buffer);
    view.setUint32(descPtr, dataPtr >>> 0, true);
    view.setInt32(descPtr + 4, source.length, true);
    const payload = new Uint8Array(memory.buffer, dataPtr, source.length);
    payload.set(source);
    return descPtr;
  }

  function readSliceDescriptor(handle) {
    const descPtr = handle >>> 0;
    const memory = ensureMemory();
    if (descPtr + SLICE_DESC_SIZE > memory.buffer.byteLength) {
      throw new Error(`slice descriptor out of bounds: ${descPtr}`);
    }
    const view = new DataView(memory.buffer);
    const dataPtr = view.getUint32(descPtr, true);
    const len = view.getInt32(descPtr + 4, true);
    if (len < 0 || dataPtr + len > memory.buffer.byteLength) {
      throw new Error(`invalid slice descriptor: ptr=${dataPtr} len=${len}`);
    }
    return { dataPtr, len };
  }

  function readSliceU8Copy(handle) {
    const desc = readSliceDescriptor(handle);
    const memory = ensureMemory();
    const source = new Uint8Array(memory.buffer, desc.dataPtr, desc.len);
    const out = new Uint8Array(source.length);
    out.set(source);
    return out;
  }

  return {
    state: localState,
    allocSliceU8,
    readSliceU8Copy,
  };
}

function toUint8Array(input) {
  if (input instanceof Uint8Array) {
    return input;
  }
  if (ArrayBuffer.isView(input)) {
    return new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
  }
  if (input instanceof ArrayBuffer) {
    return new Uint8Array(input);
  }
  if (Array.isArray(input)) {
    return Uint8Array.from(input);
  }
  throw new Error("expected bytes-like input");
}
