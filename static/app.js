import { extractWasmInstance } from "./wasm_runtime.js";

const ReactGlobal = globalThis.React;
const ReactDomGlobal = globalThis.ReactDOM;

if (!ReactGlobal || !ReactDomGlobal) {
  throw new Error("React runtime failed to load.");
}

const APP_SHELL_HTML = `
<header class="top-strip panel">
  <div class="header-controls">
    <label class="control">
      <input id="auto-run" type="checkbox" checked />
      <span>Auto-run</span>
    </label>
    <button id="run-button" class="toolbar-button" type="button">
      Run (Ctrl + Enter)
    </button>
    <button id="format-button" class="toolbar-button" type="button">
      Format (Ctrl + S)
    </button>
    <label class="control program-control">
      <span>Example</span>
      <select id="program-select">
        <option value="">Example Programs</option>
      </select>
    </label>
    <label class="control release-control">
      <span>Release</span>
      <select id="release-select"></select>
    </label>
    <a
      id="release-link"
      class="header-link"
      href="https://github.com/mewhhaha/clapse/releases"
      target="_blank"
      rel="noopener noreferrer"
    >
      Open Selected Release
    </a>
    <span id="release-status" class="status">
      Loading release metadata...
    </span>
  </div>
</header>

<section class="workspace">
  <article class="panel editor-pane">
    <nav class="tabs source-tabs" role="tablist" aria-label="Source tabs">
      <button
        type="button"
        class="tab-button is-active"
        data-source-tab-target="code"
        aria-selected="true"
      >
        Code
      </button>
      <button
        type="button"
        class="tab-button"
        data-source-tab-target="prelude"
        aria-selected="false"
      >
        Prelude
      </button>
    </nav>

    <section class="tab-panels source-tab-panels">
      <section
        class="tab-panel source-tab-panel is-active"
        data-source-tab-panel="code"
      >
        <div class="pane-body source-editor-wrap">
          <pre id="source-highlight" class="source-highlight" aria-hidden="true"><code id="source-highlight-code"></code></pre>
          <textarea
            id="source-code"
            class="source-input"
            spellcheck="false"
            placeholder="Write Clapse code here..."
          ></textarea>
        </div>
      </section>

      <section class="tab-panel source-tab-panel" data-source-tab-panel="prelude" hidden>
        <pre id="prelude-output" class="result-output prelude-output">
Prelude will load from the selected release.
        </pre>
      </section>
    </section>
  </article>

  <aside class="panel right-pane">
    <nav class="tabs" role="tablist" aria-label="Explorer outputs">
      <button
        type="button"
        class="tab-button"
        data-tab-target="ir"
        aria-selected="false"
      >
        IR
      </button>
      <button
        type="button"
        class="tab-button is-active"
        data-tab-target="compile"
        aria-selected="true"
      >
        Compile
      </button>
      <button
        type="button"
        class="tab-button"
        data-tab-target="problems"
        aria-selected="false"
      >
        Problems
      </button>
      <button
        type="button"
        class="tab-button"
        data-tab-target="settings"
        aria-selected="false"
      >
        Settings
      </button>
    </nav>

    <section class="tab-panels">
      <section class="tab-panel" data-tab-panel="ir" hidden>
        <pre id="ir-output" class="result-output">(IR appears after release load.)</pre>
      </section>

      <section class="tab-panel is-active" data-tab-panel="compile">
        <pre id="compile-output" class="result-output">
(Compile output appears after release load.)
        </pre>
        <a id="wasm-download" href="#" download hidden>
          Download Compiled Wasm
        </a>
      </section>

      <section class="tab-panel" data-tab-panel="problems" hidden>
        <pre id="problems-output" class="result-output">No problems.</pre>
      </section>

      <section class="tab-panel settings-panel" data-tab-panel="settings" hidden>
        <label class="setting-item">
          <input id="settings-auto-run" type="checkbox" checked />
          <span>Auto-run compile on edit</span>
        </label>
        <label class="setting-item">
          <input id="settings-format-on-run" type="checkbox" />
          <span>Format before run</span>
        </label>
        <p id="settings-highlight-note" class="settings-note">
          Release metadata is loaded from GitHub Releases and compiler wasm is fetched from release assets.
        </p>
      </section>
    </section>
  </aside>
</section>
`;

mountShell();

function mountShell() {
  const rootElement = document.getElementById("root");
  if (!rootElement) {
    throw new Error("Missing #root container for React app.");
  }
  const root = ReactDomGlobal.createRoot(rootElement);
  const render = () => {
    root.render(ReactGlobal.createElement(AppShell));
  };
  if (typeof ReactDomGlobal.flushSync === "function") {
    ReactDomGlobal.flushSync(render);
    return;
  }
  render();
}

function AppShell() {
  return ReactGlobal.createElement("main", {
    className: "page",
    dangerouslySetInnerHTML: { __html: APP_SHELL_HTML },
  });
}

const GITHUB_OWNER = "mewhhaha";
const GITHUB_REPO = "clapse";
const RELEASES_API = `https://api.github.com/repos/${GITHUB_OWNER}/${GITHUB_REPO}/releases?per_page=50`;
const RELEASES_PAGE = `https://github.com/${GITHUB_OWNER}/${GITHUB_REPO}/releases`;
const LOCAL_RELEASE_ARTIFACTS_ROOT = "./artifacts/releases";
const MIN_SUPPORTED_RELEASE_TAG = "v0.1.0.12";
const MIN_SUPPORTED_RELEASE_VERSION = parseReleaseVersion(
  MIN_SUPPORTED_RELEASE_TAG,
);
const MIRRORED_RELEASE_TAGS = new Set(["v0.1.0.17"]);
const COMPILER_ASSET_NAME = "clapse_compiler.wasm";
const COMPILER_ASSET_SUFFIX = "/artifacts/latest/clapse_compiler.wasm";
const PRELUDE_ASSET_NAME = "prelude.clapse";
const PRELUDE_ASSET_SUFFIX = "/artifacts/latest/prelude.clapse";
const AUTO_COMPILE_DELAY_MS = 380;
const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();
const WASM_PAGE_SIZE = 65536;
const HOST_ALLOC_GUARD_BYTES = 16 * WASM_PAGE_SIZE;
const SLICE_DESC_SIZE = 8;

const DEFAULT_SOURCE = `identity x = x

main = identity 7
`;

const EXAMPLE_PROGRAMS = [
  {
    id: "identity",
    label: "Identity",
    source: `identity x = x

main = identity 7
`,
  },
  {
    id: "factorial",
    label: "Factorial (recursive)",
    source: `factorial n = case eq n 0 of
  true -> 1
  _ -> mul n (factorial (sub n 1))

main = factorial 8
`,
  },
  {
    id: "fibonacci",
    label: "Fibonacci (recursive)",
    source: `fibonacci n = case eq n 0 of
  true -> 0
  _ -> case eq n 1 of
    true -> 1
    _ -> add (fibonacci (sub n 1)) (fibonacci (sub n 2))

main = fibonacci 10
`,
  },
  {
    id: "gcd",
    label: "GCD (Euclid)",
    source: `gcd a b = case eq b 0 of
  true -> a
  _ -> case eq a b of
    true -> a
    _ -> case lt a b of
      true -> gcd b a
      _ -> gcd (sub a b) b

main = gcd 84 30
`,
  },
  {
    id: "sum_to_n",
    label: "Sum to N",
    source: `sum_to_n n = case eq n 0 of
  true -> 0
  _ -> add n (sum_to_n (sub n 1))

main = sum_to_n 10
`,
  },
  {
    id: "pow",
    label: "Power",
    source: `pow base exp = case eq exp 0 of
  true -> 1
  _ -> mul base (pow base (sub exp 1))

main = pow 3 10
`,
  },
];

const state = {
  releases: [],
  releaseByTag: new Map(),
  compilerByTag: new Map(),
  preludeByTag: new Map(),
  running: false,
  compileQueued: false,
  compileTimer: null,
  preludeLoadTicket: 0,
  autoRun: true,
  formatOnRun: false,
  activeTab: "compile",
  activeSourceTab: "code",
  downloadUrl: null,
};

const elements = {
  programSelect: document.getElementById("program-select"),
  releaseSelect: document.getElementById("release-select"),
  releaseLink: document.getElementById("release-link"),
  releaseStatus: document.getElementById("release-status"),
  sourceCode: document.getElementById("source-code"),
  sourceHighlight: document.getElementById("source-highlight"),
  sourceHighlightCode: document.getElementById("source-highlight-code"),
  preludeOutput: document.getElementById("prelude-output"),
  autoRun: document.getElementById("auto-run"),
  runButton: document.getElementById("run-button"),
  formatButton: document.getElementById("format-button"),
  settingsAutoRun: document.getElementById("settings-auto-run"),
  settingsFormatOnRun: document.getElementById("settings-format-on-run"),
  settingsHighlightNote: document.getElementById("settings-highlight-note"),
  irOutput: document.getElementById("ir-output"),
  compileOutput: document.getElementById("compile-output"),
  problemsOutput: document.getElementById("problems-output"),
  wasmDownload: document.getElementById("wasm-download"),
  sourceTabButtons: [...document.querySelectorAll("[data-source-tab-target]")],
  sourceTabPanels: [...document.querySelectorAll("[data-source-tab-panel]")],
  tabButtons: [...document.querySelectorAll("[data-tab-target]")],
  tabPanels: [...document.querySelectorAll("[data-tab-panel]")],
};

main().catch((err) => {
  setStatus(`Initialization failed: ${errorMessage(err)}`);
});

async function main() {
  elements.sourceCode.value = DEFAULT_SOURCE;
  renderProgramSelect();
  renderSourceHighlight();
  bindEvents();
  setAutoRun(true);
  setFormatOnRun(false);
  setActiveSourceTab("code");
  setActiveTab("compile");
  await loadReleases();
}

function bindEvents() {
  elements.runButton.addEventListener("click", () => {
    cancelScheduledCompile();
    void runCompile({ forceFormat: false });
  });

  elements.formatButton.addEventListener("click", () => {
    void runFormatOnly();
  });

  elements.releaseSelect.addEventListener("change", () => {
    syncReleaseLink();
    updateHighlightAvailability();
    void refreshPreludeForSelectedRelease();
    scheduleCompile(15);
  });

  elements.programSelect.addEventListener("change", () => {
    applySelectedProgram();
  });

  for (const button of elements.sourceTabButtons) {
    button.addEventListener("click", () => {
      const tab = button.dataset.sourceTabTarget ?? "code";
      setActiveSourceTab(tab);
    });
  }

  elements.sourceCode.addEventListener("input", () => {
    renderSourceHighlight();
    if (state.autoRun) {
      scheduleCompile();
    }
  });

  elements.sourceCode.addEventListener("scroll", () => {
    syncSourceHighlightScroll();
  });

  elements.autoRun.addEventListener("change", () => {
    setAutoRun(elements.autoRun.checked);
    if (state.autoRun) {
      scheduleCompile(15);
    }
  });

  elements.settingsAutoRun.addEventListener("change", () => {
    setAutoRun(elements.settingsAutoRun.checked);
    if (state.autoRun) {
      scheduleCompile(15);
    }
  });

  elements.settingsFormatOnRun.addEventListener("change", () => {
    setFormatOnRun(elements.settingsFormatOnRun.checked);
  });

  for (const button of elements.tabButtons) {
    button.addEventListener("click", () => {
      const tab = button.dataset.tabTarget ?? "compile";
      setActiveTab(tab);
    });
  }

  globalThis.addEventListener("keydown", (event) => {
    if (!(event.ctrlKey || event.metaKey)) {
      return;
    }
    if (event.key === "Enter") {
      event.preventDefault();
      cancelScheduledCompile();
      void runCompile({ forceFormat: false });
      return;
    }
    if (event.key.toLowerCase() === "s") {
      event.preventDefault();
      void runFormatOnly();
    }
  });
}

function renderProgramSelect() {
  elements.programSelect.innerHTML = "";

  const placeholder = document.createElement("option");
  placeholder.value = "";
  placeholder.textContent = "Example Programs";
  elements.programSelect.append(placeholder);

  for (const program of EXAMPLE_PROGRAMS) {
    const option = document.createElement("option");
    option.value = program.id;
    option.textContent = program.label;
    elements.programSelect.append(option);
  }

  elements.programSelect.value = "";
}

function applySelectedProgram() {
  const selectedId = elements.programSelect.value;
  if (selectedId.length === 0) {
    return;
  }

  const program = EXAMPLE_PROGRAMS.find((entry) => entry.id === selectedId);
  if (!program) {
    return;
  }

  elements.sourceCode.value = program.source;
  renderSourceHighlight();
  elements.sourceCode.focus();
  setStatus(`Loaded example: ${program.label}.`);
  if (state.autoRun) {
    scheduleCompile(15);
  }
}

function setActiveTab(tab) {
  state.activeTab = tab;
  for (const button of elements.tabButtons) {
    const target = button.dataset.tabTarget;
    button.classList.toggle("is-active", target === tab);
    button.setAttribute("aria-selected", target === tab ? "true" : "false");
  }
  for (const panel of elements.tabPanels) {
    const panelTab = panel.dataset.tabPanel;
    const active = panelTab === tab;
    panel.classList.toggle("is-active", active);
    panel.hidden = !active;
  }
}

function setActiveSourceTab(tab) {
  state.activeSourceTab = tab;
  for (const button of elements.sourceTabButtons) {
    const target = button.dataset.sourceTabTarget;
    button.classList.toggle("is-active", target === tab);
    button.setAttribute("aria-selected", target === tab ? "true" : "false");
  }
  for (const panel of elements.sourceTabPanels) {
    const panelTab = panel.dataset.sourceTabPanel;
    const active = panelTab === tab;
    panel.classList.toggle("is-active", active);
    panel.hidden = !active;
  }
}

function setAutoRun(enabled) {
  state.autoRun = enabled;
  elements.autoRun.checked = enabled;
  elements.settingsAutoRun.checked = enabled;
}

function setFormatOnRun(enabled) {
  state.formatOnRun = enabled;
  elements.settingsFormatOnRun.checked = enabled;
}

function scheduleCompile(delayMs = AUTO_COMPILE_DELAY_MS) {
  if (!state.autoRun) {
    return;
  }
  if (state.compileTimer !== null) {
    clearTimeout(state.compileTimer);
  }
  state.compileTimer = setTimeout(() => {
    state.compileTimer = null;
    void runCompile({ forceFormat: false });
  }, delayMs);
}

function cancelScheduledCompile() {
  if (state.compileTimer !== null) {
    clearTimeout(state.compileTimer);
    state.compileTimer = null;
  }
}

async function loadReleases() {
  setStatus("Loading GitHub releases...");
  setControlsBusy(true);
  try {
    const releases = await fetchReleaseMetadata();
    if (releases.length === 0) {
      throw new Error(
        `No supported mirrored releases were returned by GitHub. Requires tag >= ${MIN_SUPPORTED_RELEASE_TAG} with compiler + prelude assets mirrored in this branch.`,
      );
    }
    state.releases = releases;
    state.releaseByTag = new Map(
      releases.map((release) => [release.tag, release]),
    );
    renderReleaseSelect(releases);
    syncReleaseLink();
    updateHighlightAvailability();
    await refreshPreludeForSelectedRelease();
    setStatus(
      `Loaded ${releases.length} mirrored release(s) (>= ${MIN_SUPPORTED_RELEASE_TAG}).`,
    );
    scheduleCompile(25);
  } catch (err) {
    setStatus(errorMessage(err));
    renderProblems([`Release load failed: ${errorMessage(err)}`]);
    setActiveTab("problems");
  } finally {
    setControlsBusy(false);
  }
}

function renderReleaseSelect(releases) {
  const previous = elements.releaseSelect.value;
  elements.releaseSelect.innerHTML = "";

  for (const release of releases) {
    const option = document.createElement("option");
    option.value = release.tag;
    option.textContent = `${release.tag} (${formatDate(release.publishedAt)})`;
    elements.releaseSelect.append(option);
  }

  if (hasOption(elements.releaseSelect, previous)) {
    elements.releaseSelect.value = previous;
    return;
  }
  if (releases.length > 0) {
    elements.releaseSelect.selectedIndex = 0;
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

function syncReleaseLink() {
  const release = state.releaseByTag.get(elements.releaseSelect.value);
  elements.releaseLink.href = release?.htmlUrl ?? RELEASES_PAGE;
}

function updateHighlightAvailability() {
  const tag = elements.releaseSelect.value;
  const hasTreeSitterAsset = releaseHasTreeSitterAsset(tag);
  if (hasTreeSitterAsset) {
    elements.settingsHighlightNote.textContent =
      "Release includes a Tree-sitter asset; syntax mode can be upgraded once parser integration is added.";
    return;
  }
  elements.settingsHighlightNote.textContent =
    "Current releases do not include a Tree-sitter grammar wasm asset, so editor highlighting uses the built-in tokenizer.";
}

async function fetchReleaseMetadata() {
  const response = await fetch(RELEASES_API, {
    headers: { Accept: "application/vnd.github+json" },
  });
  if (!response.ok) {
    throw new Error(`GitHub releases request failed: HTTP ${response.status}`);
  }

  const payload = await response.json();
  if (!Array.isArray(payload)) {
    throw new Error("GitHub releases payload was not an array.");
  }

  const releases = [];
  for (const entry of payload) {
    if (!entry || typeof entry !== "object" || entry.draft === true) {
      continue;
    }
    const tag = String(entry.tag_name ?? "").trim();
    if (tag.length === 0) {
      continue;
    }
    const assets = Array.isArray(entry.assets) ? entry.assets : [];
    if (!isSupportedRelease(tag, assets)) {
      continue;
    }
    releases.push({
      tag,
      publishedAt: String(entry.published_at ?? entry.created_at ?? ""),
      htmlUrl: String(entry.html_url ?? `${RELEASES_PAGE}/tag/${tag}`),
      assets,
    });
  }

  releases.sort((a, b) => {
    const aDate = Date.parse(a.publishedAt);
    const bDate = Date.parse(b.publishedAt);
    if (Number.isNaN(aDate) && Number.isNaN(bDate)) return 0;
    if (Number.isNaN(aDate)) return 1;
    if (Number.isNaN(bDate)) return -1;
    return bDate - aDate;
  });
  return releases;
}

function formatDate(raw) {
  const parsed = Date.parse(raw);
  if (Number.isNaN(parsed)) {
    return "unknown-date";
  }
  return new Date(parsed).toISOString().slice(0, 10);
}

function releaseHasTreeSitterAsset(tag) {
  const release = state.releaseByTag.get(tag);
  if (!release || !Array.isArray(release.assets)) {
    return false;
  }
  return release.assets.some((asset) => {
    const name = String(asset?.name ?? "").toLowerCase();
    return name.includes("tree-sitter") && name.endsWith(".wasm");
  });
}

function parseReleaseVersion(tag) {
  const normalized = String(tag).trim().replace(/^v/i, "");
  if (!/^\d+(\.\d+)+$/.test(normalized)) {
    return null;
  }
  const parts = normalized.split(".").map((part) => Number(part));
  if (parts.some((part) => !Number.isInteger(part) || part < 0)) {
    return null;
  }
  return parts;
}

function compareReleaseVersions(left, right) {
  const len = Math.max(left.length, right.length);
  for (let i = 0; i < len; i += 1) {
    const a = left[i] ?? 0;
    const b = right[i] ?? 0;
    if (a !== b) {
      return a - b;
    }
  }
  return 0;
}

function hasAsset(assets, name, suffix) {
  return assets.some((asset) => {
    const assetName = String(asset?.name ?? "").trim();
    return assetName === name || assetName.endsWith(suffix);
  });
}

function isSupportedRelease(tag, assets) {
  if (!MIN_SUPPORTED_RELEASE_VERSION) {
    return false;
  }
  if (!MIRRORED_RELEASE_TAGS.has(tag)) {
    return false;
  }
  const releaseVersion = parseReleaseVersion(tag);
  if (!releaseVersion) {
    return false;
  }
  if (
    compareReleaseVersions(releaseVersion, MIN_SUPPORTED_RELEASE_VERSION) < 0
  ) {
    return false;
  }
  const hasCompiler = hasAsset(
    assets,
    COMPILER_ASSET_NAME,
    COMPILER_ASSET_SUFFIX,
  );
  const hasPrelude = hasAsset(assets, PRELUDE_ASSET_NAME, PRELUDE_ASSET_SUFFIX);
  return hasCompiler && hasPrelude;
}

async function runFormatOnly() {
  const tag = elements.releaseSelect.value;
  if (!tag) {
    setStatus("Select a release first.");
    return;
  }

  if (state.running) {
    state.compileQueued = true;
    return;
  }

  state.running = true;
  setControlsBusy(true);
  setStatus(`Formatting with release ${tag}...`);

  try {
    const session = await createCompilerSession(tag);
    const formatResult = callFormat(session, elements.sourceCode.value);
    if (!formatResult.ok) {
      renderProblems([`Format failed: ${formatResult.error}`]);
      setStatus(`Format failed: ${formatResult.error}`);
      setActiveTab("problems");
      return;
    }

    if (formatResult.formatted !== null) {
      elements.sourceCode.value = formatResult.formatted;
      renderSourceHighlight();
    }
    renderProblems(["No problems."]);
    setStatus(`Formatted using ${tag}.`);
    if (state.autoRun) {
      scheduleCompile(20);
    }
  } catch (err) {
    const message = errorMessage(err);
    setStatus(`Format failed: ${message}`);
    renderProblems([`Format failed: ${message}`]);
    setActiveTab("problems");
  } finally {
    state.running = false;
    setControlsBusy(false);
  }
}

async function runCompile({ forceFormat = false } = {}) {
  if (state.running) {
    state.compileQueued = true;
    return;
  }

  const tag = elements.releaseSelect.value;
  if (!tag) {
    setStatus("Select a release first.");
    return;
  }

  state.running = true;
  setControlsBusy(true);
  setStatus(`Compiling with ${tag}...`);
  setDownloadLink(null, null);

  const problems = [];

  try {
    const session = await createCompilerSession(tag);
    let source = elements.sourceCode.value;
    const preludeSource = await loadPreludeSource(tag);
    elements.preludeOutput.textContent = preludeSource;

    if (forceFormat || state.formatOnRun) {
      const formatResult = callFormat(session, source);
      if (formatResult.ok && formatResult.formatted !== null) {
        source = formatResult.formatted;
        elements.sourceCode.value = source;
        renderSourceHighlight();
      } else if (!formatResult.ok) {
        problems.push(`Format failed: ${formatResult.error}`);
      }
    }

    const compileSource = combinePreludeAndSource(preludeSource, source);

    const artifactsResponse = session.call({
      command: "selfhost-artifacts",
      input_path: "repl/input.clapse",
      input_source: compileSource,
    });
    const compileResponse = session.call({
      command: "compile",
      input_path: "repl/input.clapse",
      input_source: compileSource,
      plugin_wasm_paths: [],
    });

    const loweredIr = extractArtifactText(artifactsResponse, "lowered_ir.txt");
    const collapsedIr = extractArtifactText(
      artifactsResponse,
      "collapsed_ir.txt",
    );
    elements.irOutput.textContent = [
      "lowered_ir.txt",
      loweredIr,
      "",
      "collapsed_ir.txt",
      collapsedIr,
    ].join("\n");

    if (artifactsResponse?.ok !== true) {
      problems.push(
        `IR generation failed: ${formatResponseError(artifactsResponse)}`,
      );
    }

    if (compileResponse?.ok === true) {
      const wasmBase64 = String(compileResponse.wasm_base64 ?? "");
      if (wasmBase64.length === 0) {
        elements.compileOutput.textContent =
          "Compile succeeded but wasm_base64 was empty.";
        problems.push("Compile succeeded with empty wasm_base64.");
      } else {
        const wasmBytes = decodeWasmBase64(wasmBase64);
        setDownloadLink(tag, wasmBytes);
        const exportsList = normalizeExports(compileResponse.exports);
        elements.compileOutput.textContent = [
          `ok: true`,
          `release: ${tag}`,
          `prelude_bytes: ${UTF8_ENCODER.encode(preludeSource).length}`,
          `source_bytes: ${UTF8_ENCODER.encode(source).length}`,
          `combined_bytes: ${UTF8_ENCODER.encode(compileSource).length}`,
          `bytes: ${wasmBytes.length}`,
          `fnv1a32: ${fnv1aHex(wasmBytes)}`,
          exportsList.length > 0
            ? `exports: ${exportsList
                .map((entry) => `${entry.name}/${entry.arity}`)
                .join(", ")}`
            : "exports: (none)",
          "",
          "wasm_base64_preview",
          previewText(wasmBase64, 2200),
        ].join("\n");
      }
    } else {
      const compileError = formatResponseError(compileResponse);
      elements.compileOutput.textContent = compileError;
      problems.push(`Compile failed: ${compileError}`);
    }

    if (problems.length === 0) {
      renderProblems(["No problems."]);
      setStatus(`Compiled ${tag} successfully.`);
      if (state.activeTab === "problems") {
        setActiveTab("compile");
      }
    } else {
      renderProblems(problems);
      setStatus(`Finished ${tag} with ${problems.length} problem(s).`);
      setActiveTab("problems");
    }
  } catch (err) {
    const message = errorMessage(err);
    setStatus(`Compile failed: ${message}`);
    elements.compileOutput.textContent = `Compile failed.\n${message}`;
    renderProblems([`Compile failed: ${message}`]);
    setActiveTab("problems");
  } finally {
    state.running = false;
    setControlsBusy(false);
    if (state.compileQueued) {
      state.compileQueued = false;
      void runCompile({ forceFormat: false });
    }
  }
}

function normalizeExports(value) {
  if (!Array.isArray(value)) {
    return [];
  }
  const out = [];
  for (const item of value) {
    if (!item || typeof item !== "object" || Array.isArray(item)) {
      continue;
    }
    const name = String(item.name ?? "").trim();
    const arity = Number(item.arity);
    if (name.length === 0 || !Number.isInteger(arity) || arity < 0) {
      continue;
    }
    out.push({ name, arity });
  }
  return out;
}

function callFormat(session, source) {
  const attempts = [
    {
      command: "format",
      input_path: "repl/input.clapse",
      input_source: source,
      mode: "stdout",
    },
    {
      command: "format",
      input_path: "repl/input.clapse",
      input_source: source,
    },
  ];

  let lastError = "format command failed";
  for (const request of attempts) {
    const response = session.call(request);
    if (!response || typeof response !== "object" || Array.isArray(response)) {
      lastError = "format response was not an object";
      continue;
    }
    if (response.ok !== true) {
      lastError = String(response.error ?? "format failed");
      continue;
    }
    const formatted = extractFormattedText(response);
    if (formatted !== null) {
      return { ok: true, formatted };
    }
    return { ok: true, formatted: source };
  }
  return { ok: false, error: lastError };
}

function extractFormattedText(response) {
  const directKeys = ["formatted", "output_source", "output", "source", "text"];
  for (const key of directKeys) {
    const value = response[key];
    if (typeof value === "string") {
      return value;
    }
  }
  if (response.result && typeof response.result === "object") {
    for (const key of directKeys) {
      const nested = response.result[key];
      if (typeof nested === "string") {
        return nested;
      }
    }
  }
  return null;
}

function renderProblems(lines) {
  const filtered = lines.filter((line) => String(line).trim().length > 0);
  elements.problemsOutput.textContent =
    filtered.length > 0 ? filtered.join("\n\n") : "No problems.";
}

function renderSourceHighlight() {
  const source = elements.sourceCode.value;
  if (source.length === 0) {
    elements.sourceHighlightCode.innerHTML = "\n";
  } else {
    elements.sourceHighlightCode.innerHTML = highlightSourceToHtml(source);
  }
  syncSourceHighlightScroll();
}

function syncSourceHighlightScroll() {
  elements.sourceHighlight.scrollTop = elements.sourceCode.scrollTop;
  elements.sourceHighlight.scrollLeft = elements.sourceCode.scrollLeft;
}

function highlightSourceToHtml(source) {
  const lines = source.split("\n");
  return lines.map((line) => highlightLine(line)).join("\n");
}

function highlightLine(line) {
  const keywords = new Set([
    "module",
    "let",
    "switch",
    "if",
    "then",
    "else",
    "type",
    "data",
    "primitive",
    "where",
    "import",
    "export",
    "match",
    "case",
    "of",
    "do",
    "in",
    "true",
    "false",
  ]);
  const operators = [
    "=>",
    "->",
    "::",
    "==",
    "!=",
    "<=",
    ">=",
    "||",
    "&&",
    ">>=",
    ">>",
    "<$>",
    "<$",
    "<*>",
    "<*",
    "*>",
    "<|>",
    "=",
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "|",
    "&",
    "!",
    ":",
    ",",
    ".",
    "(",
    ")",
    "{",
    "}",
    "[",
    "]",
  ];

  let out = "";
  let i = 0;
  while (i < line.length) {
    if (line.startsWith("//", i) || line.startsWith("--", i)) {
      out += wrapToken("tok-comment", line.slice(i));
      break;
    }

    const ch = line[i];
    if (ch === "'" || ch === '"') {
      const quote = ch;
      let j = i + 1;
      while (j < line.length) {
        if (line[j] === "\\" && j + 1 < line.length) {
          j += 2;
          continue;
        }
        if (line[j] === quote) {
          j += 1;
          break;
        }
        j += 1;
      }
      out += wrapToken("tok-string", line.slice(i, j));
      i = j;
      continue;
    }

    if (/\d/.test(ch)) {
      let j = i + 1;
      while (j < line.length && /[\d_]/.test(line[j])) {
        j += 1;
      }
      out += wrapToken("tok-number", line.slice(i, j));
      i = j;
      continue;
    }

    if (/[A-Za-z_]/.test(ch)) {
      let j = i + 1;
      while (j < line.length && /[A-Za-z0-9_']/.test(line[j])) {
        j += 1;
      }
      const word = line.slice(i, j);
      if (keywords.has(word)) {
        out += wrapToken("tok-keyword", word);
      } else if (/^[A-Z]/.test(word)) {
        out += wrapToken("tok-type", word);
      } else if (looksLikeFunctionName(line, i, j)) {
        out += wrapToken("tok-function", word);
      } else {
        out += escapeHtml(word);
      }
      i = j;
      continue;
    }

    const operator = operators.find((op) => line.startsWith(op, i));
    if (operator) {
      out += wrapToken("tok-operator", operator);
      i += operator.length;
      continue;
    }

    out += escapeHtml(ch);
    i += 1;
  }

  return out;
}

function looksLikeFunctionName(line, start, end) {
  const before = line.slice(0, start).trim();
  if (before.length > 0) {
    return false;
  }
  const after = line.slice(end);
  return /\s*=/.test(after);
}

function wrapToken(className, text) {
  return `<span class="${className}">${escapeHtml(text)}</span>`;
}

function escapeHtml(text) {
  return text
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;");
}

function previewText(text, limit) {
  if (text.length <= limit) {
    return text;
  }
  return `${text.slice(0, limit)}\n... (${
    text.length - limit
  } chars truncated)`;
}

function extractArtifactText(response, name) {
  if (
    response &&
    response.ok === true &&
    response.artifacts &&
    typeof response.artifacts === "object"
  ) {
    const value = response.artifacts[name];
    if (typeof value === "string" && value.length > 0) {
      return value;
    }
    return `(artifact '${name}' missing in response)`;
  }
  return formatResponseError(response);
}

function formatResponseError(response) {
  if (!response || typeof response !== "object" || Array.isArray(response)) {
    return "No structured compiler response.";
  }
  const ok = response.ok === true;
  const error = typeof response.error === "string" ? response.error : "";
  if (error.length > 0) {
    return `ok: ${String(ok)}\nerror: ${error}`;
  }
  return `ok: ${String(ok)}`;
}

function combinePreludeAndSource(preludeSource, source) {
  const prelude = String(preludeSource ?? "").trimEnd();
  const userSource = String(source ?? "");
  if (prelude.length === 0) {
    return userSource;
  }
  if (userSource.length === 0) {
    return `${prelude}\n`;
  }
  return `${prelude}\n\n${userSource}`;
}

async function refreshPreludeForSelectedRelease() {
  const tag = elements.releaseSelect.value;
  const ticket = ++state.preludeLoadTicket;
  if (!tag) {
    elements.preludeOutput.textContent = "Select a release to load prelude.";
    return;
  }

  elements.preludeOutput.textContent = `Loading prelude for ${tag}...`;
  try {
    const prelude = await loadPreludeSource(tag);
    if (ticket !== state.preludeLoadTicket) {
      return;
    }
    elements.preludeOutput.textContent =
      prelude.length > 0 ? prelude : `(Prelude is empty for ${tag}.)`;
  } catch (err) {
    if (ticket !== state.preludeLoadTicket) {
      return;
    }
    elements.preludeOutput.textContent = `Prelude load failed for ${tag}:\n${errorMessage(
      err,
    )}`;
  }
}

async function createCompilerSession(tag) {
  const record = await loadCompilerRecord(tag);
  const imports = buildStubImports(WebAssembly.Module.imports(record.module));
  const instance = extractWasmInstance(
    await WebAssembly.instantiate(record.module, imports),
  );
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

  return {
    call(requestObject) {
      const requestBytes = UTF8_ENCODER.encode(JSON.stringify(requestObject));
      const requestHandle = runtime.allocSliceU8(requestBytes);
      const responseHandle = run(requestHandle | 0);
      if (!Number.isInteger(responseHandle) || (responseHandle & 1) === 1) {
        throw new Error(
          `Release ${tag} returned invalid response handle: ${responseHandle}`,
        );
      }
      const responseBytes = runtime.readSliceU8Copy(responseHandle);
      const responseText = UTF8_DECODER.decode(responseBytes);
      try {
        return JSON.parse(responseText);
      } catch (err) {
        throw new Error(
          `Release ${tag} returned invalid JSON: ${errorMessage(err)}`,
        );
      }
    },
  };
}

async function loadPreludeSource(tag) {
  if (state.preludeByTag.has(tag)) {
    return state.preludeByTag.get(tag).source;
  }

  const candidates = resolvePreludeAssetCandidates(tag);
  let preludeText = null;
  let selectedUrl = "";
  const errors = [];

  for (const candidate of candidates) {
    const url = candidate.url;
    try {
      const response = await fetch(url, {
        cache: "no-store",
        headers: candidate.headers,
      });
      if (!response.ok) {
        errors.push(`${url} -> HTTP ${response.status}`);
        continue;
      }
      const text = await response.text();
      if (text.trim().length === 0) {
        errors.push(`${url} -> empty prelude`);
        continue;
      }
      preludeText = text;
      selectedUrl = url;
      break;
    } catch (err) {
      errors.push(`${url} -> ${errorMessage(err)}`);
    }
  }

  if (typeof preludeText !== "string") {
    throw new Error(
      `Failed to load prelude for ${tag}. Tried:\n${errors.join("\n")}`,
    );
  }

  const record = { tag, source: preludeText, sourceUrl: selectedUrl };
  state.preludeByTag.set(tag, record);
  return preludeText;
}

async function loadCompilerRecord(tag) {
  if (state.compilerByTag.has(tag)) {
    return state.compilerByTag.get(tag);
  }

  const candidates = resolveCompilerAssetCandidates(tag);
  let wasmBytes = null;
  let selectedUrl = "";
  const errors = [];

  for (const candidate of candidates) {
    const url = candidate.url;
    try {
      const response = await fetch(url, {
        cache: "no-store",
        headers: candidate.headers,
      });
      if (!response.ok) {
        errors.push(`${url} -> HTTP ${response.status}`);
        continue;
      }
      const bytes = new Uint8Array(await response.arrayBuffer());
      if (bytes.length === 0) {
        errors.push(`${url} -> empty payload`);
        continue;
      }
      wasmBytes = bytes;
      selectedUrl = url;
      break;
    } catch (err) {
      errors.push(`${url} -> ${errorMessage(err)}`);
    }
  }

  if (!(wasmBytes instanceof Uint8Array)) {
    throw new Error(
      `Failed to load compiler wasm for ${tag}. Tried:\n${errors.join("\n")}`,
    );
  }

  const module = await WebAssembly.compile(wasmBytes);
  const imports = WebAssembly.Module.imports(module);
  for (const entry of imports) {
    if (entry.kind !== "function") {
      throw new Error(
        `Release ${tag} imports unsupported ${entry.kind} '${entry.module}.${entry.name}'.`,
      );
    }
  }

  const testImports = buildStubImports(imports);
  const instance = extractWasmInstance(
    await WebAssembly.instantiate(module, testImports),
  );
  const memory = instance.exports.__memory ?? instance.exports.memory;
  if (!(memory instanceof WebAssembly.Memory)) {
    throw new Error(`Release ${tag} compiler wasm missing memory export.`);
  }
  if (typeof instance.exports.clapse_run !== "function") {
    throw new Error(`Release ${tag} compiler wasm missing clapse_run export.`);
  }

  const record = { tag, wasmUrl: selectedUrl, module };
  state.compilerByTag.set(tag, record);
  return record;
}

function resolveCompilerAssetCandidates(tag) {
  return [
    {
      url: `${LOCAL_RELEASE_ARTIFACTS_ROOT}/${encodeURIComponent(tag)}/${COMPILER_ASSET_NAME}`,
    },
  ];
}

function resolvePreludeAssetCandidates(tag) {
  return [
    {
      url: `${LOCAL_RELEASE_ARTIFACTS_ROOT}/${encodeURIComponent(tag)}/${PRELUDE_ASSET_NAME}`,
    },
  ];
}

function buildStubImports(imports) {
  const out = {};
  for (const entry of imports) {
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

function setControlsBusy(busy) {
  elements.programSelect.disabled = busy;
  elements.releaseSelect.disabled = busy;
  elements.runButton.disabled = busy;
  elements.formatButton.disabled = busy;
}

function setStatus(message) {
  elements.releaseStatus.textContent = message;
}

function setDownloadLink(tag, wasmBytes) {
  if (state.downloadUrl) {
    URL.revokeObjectURL(state.downloadUrl);
    state.downloadUrl = null;
  }

  if (!(wasmBytes instanceof Uint8Array) || !tag) {
    elements.wasmDownload.hidden = true;
    elements.wasmDownload.href = "#";
    return;
  }

  const blob = new Blob([wasmBytes], { type: "application/wasm" });
  const url = URL.createObjectURL(blob);
  elements.wasmDownload.hidden = false;
  elements.wasmDownload.href = url;
  elements.wasmDownload.download = `${tag}-clapse.wasm`;
  state.downloadUrl = url;
}

function errorMessage(err) {
  return err instanceof Error ? err.message : String(err);
}

function decodeWasmBase64(input) {
  const raw = atob(input);
  const out = new Uint8Array(raw.length);
  for (let i = 0; i < raw.length; i += 1) {
    out[i] = raw.charCodeAt(i);
  }
  return out;
}

function fnv1aHex(bytes) {
  let hash = 0x811c9dc5;
  for (let i = 0; i < bytes.length; i += 1) {
    hash ^= bytes[i];
    hash = Math.imul(hash, 0x01000193);
  }
  return (hash >>> 0).toString(16).padStart(8, "0");
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

  function toUint8Array(input) {
    if (input instanceof Uint8Array) return input;
    if (ArrayBuffer.isView(input)) {
      return new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
    }
    if (input instanceof ArrayBuffer) return new Uint8Array(input);
    if (Array.isArray(input)) return Uint8Array.from(input);
    throw new Error("expected bytes-like input");
  }

  function allocSliceU8(input) {
    const source = toUint8Array(input);
    const descPtr = allocLinear(SLICE_DESC_SIZE, 4);
    const dataPtr = allocLinear(source.length, 1);
    const memory = ensureMemory();
    const view = new DataView(memory.buffer);
    view.setUint32(descPtr, dataPtr >>> 0, true);
    view.setInt32(descPtr + 4, source.length, true);
    new Uint8Array(memory.buffer, dataPtr, source.length).set(source);
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
