const GITHUB_OWNER = "mewhhaha";
const GITHUB_REPO = "clapse";
const RELEASES_API =
  `https://api.github.com/repos/${GITHUB_OWNER}/${GITHUB_REPO}/releases?per_page=50`;
const RELEASES_PAGE =
  `https://github.com/${GITHUB_OWNER}/${GITHUB_REPO}/releases`;
const COMPILER_ASSET_NAME = "clapse_compiler.wasm";
const COMPILER_ASSET_SUFFIX = "/artifacts/latest/clapse_compiler.wasm";
const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();
const WASM_PAGE_SIZE = 65536;
const HOST_ALLOC_GUARD_BYTES = 16 * WASM_PAGE_SIZE;
const SLICE_DESC_SIZE = 8;

const DEFAULT_SOURCE = `identity x = x

main = identity 7
`;

const state = {
  releases: [],
  releaseByTag: new Map(),
  compilerByTag: new Map(),
  running: false,
  downloadUrl: null,
};

const elements = {
  releaseSelect: document.getElementById("release-select"),
  compileButton: document.getElementById("compile-button"),
  releaseLink: document.getElementById("release-link"),
  releaseStatus: document.getElementById("release-status"),
  sourceCode: document.getElementById("source-code"),
  irOutput: document.getElementById("ir-output"),
  wasmOutput: document.getElementById("wasm-output"),
  wasmDownload: document.getElementById("wasm-download"),
};

main().catch((err) => {
  setStatus(`Initialization failed: ${errorMessage(err)}`);
});

async function main() {
  elements.sourceCode.value = DEFAULT_SOURCE;
  bindEvents();
  await loadReleases();
}

function bindEvents() {
  elements.compileButton.addEventListener("click", () => {
    void runCompile();
  });

  elements.releaseSelect.addEventListener("change", () => {
    syncReleaseLink();
  });
}

async function loadReleases() {
  setControlsDisabled(true);
  setStatus("Loading GitHub releases...");
  try {
    const releases = await fetchReleaseMetadata();
    if (releases.length === 0) {
      throw new Error("No non-draft releases were returned by GitHub.");
    }
    state.releases = releases;
    state.releaseByTag = new Map(
      releases.map((release) => [release.tag, release]),
    );
    renderReleaseSelect(releases);
    syncReleaseLink();
    setStatus(`Loaded ${releases.length} release(s) from GitHub.`);
  } catch (err) {
    setStatus(errorMessage(err));
  } finally {
    setControlsDisabled(false);
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
  const href = release?.htmlUrl ?? RELEASES_PAGE;
  elements.releaseLink.href = href;
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
      publishedAt: String(entry.published_at ?? entry.created_at ?? ""),
      htmlUrl: String(entry.html_url ?? `${RELEASES_PAGE}/tag/${tag}`),
      assets: Array.isArray(entry.assets) ? entry.assets : [],
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

async function runCompile() {
  if (state.running) {
    setStatus("Compile already in progress.");
    return;
  }

  const tag = elements.releaseSelect.value;
  if (!tag) {
    setStatus("Select a release first.");
    return;
  }

  state.running = true;
  setControlsDisabled(true);
  setStatus(`Compiling with release ${tag}...`);
  elements.irOutput.textContent = "Running selfhost-artifacts...";
  elements.wasmOutput.textContent = "Running compile...";
  setDownloadLink(null, null);

  try {
    const session = await createCompilerSession(tag);
    const source = elements.sourceCode.value;

    const artifactsResponse = await session.call({
      command: "selfhost-artifacts",
      input_path: "repl/input.clapse",
      input_source: source,
    });
    const compileResponse = await session.call({
      command: "compile",
      input_path: "repl/input.clapse",
      input_source: source,
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

    if (compileResponse && compileResponse.ok === true) {
      const wasmBase64 = String(compileResponse.wasm_base64 ?? "");
      if (wasmBase64.length === 0) {
        elements.wasmOutput.textContent =
          "Compile succeeded but wasm_base64 was empty.";
      } else {
        const wasmBytes = decodeWasmBase64(wasmBase64);
        setDownloadLink(tag, wasmBytes);
        elements.wasmOutput.textContent = [
          `ok: true`,
          `release: ${tag}`,
          `bytes: ${wasmBytes.length}`,
          `fnv1a32: ${fnv1aHex(wasmBytes)}`,
          "",
          "wasm_base64_preview",
          previewText(wasmBase64, 2200),
        ].join("\n");
      }
    } else {
      elements.wasmOutput.textContent = formatResponseError(compileResponse);
    }

    const compileOk = Boolean(compileResponse?.ok === true);
    const artifactsOk = Boolean(artifactsResponse?.ok === true);
    setStatus(
      `Done for ${tag} (compile ok: ${String(compileOk)}, artifacts ok: ${
        String(artifactsOk)
      }).`,
    );
  } catch (err) {
    setStatus(`Compile failed: ${errorMessage(err)}`);
    elements.wasmOutput.textContent = "Compile failed. See status above.";
  } finally {
    state.running = false;
    setControlsDisabled(false);
  }
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
    response && response.ok === true && response.artifacts &&
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

async function createCompilerSession(tag) {
  const record = await loadCompilerRecord(tag);
  const imports = buildStubImports(WebAssembly.Module.imports(record.module));
  const { instance } = await WebAssembly.instantiate(record.module, imports);
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

async function loadCompilerRecord(tag) {
  if (state.compilerByTag.has(tag)) {
    return state.compilerByTag.get(tag);
  }

  const candidates = resolveCompilerAssetUrl(tag);
  let wasmBytes = null;
  let selectedUrl = "";
  const errors = [];

  for (const url of candidates) {
    try {
      const response = await fetch(url, { cache: "no-store" });
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
  const { instance } = await WebAssembly.instantiate(module, testImports);
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

function resolveCompilerAssetUrl(tag) {
  const release = state.releaseByTag.get(tag);
  if (!release) {
    throw new Error(`Release ${tag} is not loaded in the dropdown.`);
  }

  const assets = Array.isArray(release.assets) ? release.assets : [];
  const match = assets.find((asset) => {
    const name = String(asset?.name ?? "").trim();
    return name === COMPILER_ASSET_NAME || name.endsWith(COMPILER_ASSET_SUFFIX);
  });
  const urls = [];
  const assetUrl = String(match?.browser_download_url ?? "");
  if (assetUrl.length > 0) {
    urls.push(assetUrl);
  }
  urls.push(
    `https://raw.githubusercontent.com/${GITHUB_OWNER}/${GITHUB_REPO}/${
      encodeURIComponent(
        tag,
      )
    }/artifacts/latest/clapse_compiler.wasm`,
  );
  return urls;
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

function setControlsDisabled(disabled) {
  elements.releaseSelect.disabled = disabled;
  elements.compileButton.disabled = disabled;
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
