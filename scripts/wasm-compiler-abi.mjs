import { decodeInt, encodeInt, makeRuntime } from "./wasm-runtime.mjs";

const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();
const MAX_TAGGED_INT = 1073741823;
const MIN_TAGGED_INT = -1073741824;
const SELFHOST_ARTIFACT_FILES = [
  "merged_module.txt",
  "type_info.txt",
  "type_info_error.txt",
  "lowered_ir.txt",
  "collapsed_ir.txt",
  "exports.txt",
  "wasm_stats.txt",
];

function fromBase64(input) {
  const raw = atob(input);
  const out = new Uint8Array(raw.length);
  for (let i = 0; i < raw.length; i += 1) {
    out[i] = raw.charCodeAt(i);
  }
  return out;
}

function assertFn(instance, name) {
  const fn = instance.exports[name];
  if (typeof fn !== "function") {
    const exportsList = Object.keys(instance.exports).join(", ");
    throw new Error(
      `compiler wasm export '${name}' missing (exports: ${exportsList})`,
    );
  }
  return fn;
}

function assertCompilerExports(instance) {
  const memoryExport = instance.exports.__memory ?? instance.exports.memory;
  if (!(memoryExport instanceof WebAssembly.Memory)) {
    throw new Error("compiler wasm must export __memory or memory");
  }
  assertFn(instance, "clapse_run");
}

function decodeSliceBytes(memory, handle) {
  const ptr = handle >>> 0;
  const view = new DataView(memory.buffer);
  if (ptr + 8 > memory.buffer.byteLength) {
    throw new Error(`request slice descriptor out of bounds: ${ptr}`);
  }
  const dataPtr = view.getUint32(ptr, true);
  const len = view.getInt32(ptr + 4, true);
  if (len < 0 || dataPtr + len > memory.buffer.byteLength) {
    throw new Error(
      `invalid request slice descriptor: ptr=${dataPtr} len=${len}`,
    );
  }
  return new Uint8Array(memory.buffer, dataPtr, len);
}

function runHostCommandSync(args) {
  const proc = new Deno.Command("bash", {
    args: ["-lc", args],
    stdout: "piped",
    stderr: "piped",
  });
  const out = proc.outputSync();
  return {
    ok: out.success,
    code: out.code,
    stdout: UTF8_DECODER.decode(out.stdout),
    stderr: UTF8_DECODER.decode(out.stderr),
  };
}

function shQuote(s) {
  return `'${String(s).replaceAll("'", "'\\''")}'`;
}

function toBase64(bytes) {
  let binary = "";
  for (let i = 0; i < bytes.length; i += 1) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary);
}

function runHostCompile(request) {
  const inputPath = String(request.input_path ?? "");
  return {
    ok: false,
    error:
      `host compile bridge is removed (input=${inputPath}); use native wasm compiler artifact`,
  };
}

function runHostFormat(request) {
  const inputPath = String(request.input_path ?? "");
  const mode = String(request.mode ?? "stdout");
  return {
    ok: false,
    error:
      `host format bridge is removed (input=${inputPath}, mode=${mode}); use native wasm compiler artifact`,
    input_path: inputPath,
    mode,
  };
}

function runHostSelfhostArtifacts(request) {
  const inputPath = String(request.input_path ?? "");
  return {
    ok: false,
    error:
      `host selfhost-artifacts bridge is removed (input=${inputPath}); use native wasm compiler artifact`,
  };
}

function runHostClapseRequest(request) {
  if (!request || typeof request !== "object") {
    return { ok: false, error: "invalid request object" };
  }
  const command = String(request.command ?? "");
  if (command === "compile") {
    return runHostCompile(request);
  }
  if (command === "selfhost-artifacts") {
    return runHostSelfhostArtifacts(request);
  }
  if (command === "format") {
    return runHostFormat(request);
  }
  return { ok: false, error: `unsupported command: ${command}` };
}

function createHostImportObject(runtime, getInstanceRef) {
  const emptySlice = () => runtime.alloc_slice_u8(new Uint8Array(0));

  const runHostRequest = (requestHandle) => {
    const instanceRef = getInstanceRef();
    if (instanceRef === null) {
      throw new Error(
        "compiler wasm host bridge called before instance initialization",
      );
    }
    const memory = instanceRef.exports.__memory ?? instanceRef.exports.memory;
    if (!(memory instanceof WebAssembly.Memory)) {
      throw new Error(
        "compiler wasm missing memory export during host bridge call",
      );
    }
    const requestBytes = decodeSliceBytes(memory, requestHandle | 0);
    const requestText = UTF8_DECODER.decode(requestBytes);
    let requestObj;
    try {
      requestObj = JSON.parse(requestText);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      requestObj = { command: "invalid", parse_error: message };
    }
    const responseObj = runHostClapseRequest(requestObj);
    const responseBytes = UTF8_ENCODER.encode(JSON.stringify(responseObj));
    return runtime.alloc_slice_u8(responseBytes);
  };

  const hostReadFile = (pathHandle) => {
    const instanceRef = getInstanceRef();
    if (instanceRef === null) {
      return emptySlice();
    }
    const memory = instanceRef.exports.__memory ?? instanceRef.exports.memory;
    if (!(memory instanceof WebAssembly.Memory)) {
      return emptySlice();
    }
    let path = "";
    try {
      const pathBytes = decodeSliceBytes(memory, pathHandle | 0);
      path = UTF8_DECODER.decode(pathBytes);
    } catch {
      return emptySlice();
    }
    try {
      const fileBytes = Deno.readFileSync(path);
      return runtime.alloc_slice_u8(fileBytes);
    } catch {
      return emptySlice();
    }
  };

  const hostUnixTimeMs = (seedTagged) => {
    let seed = 0;
    try {
      seed = decodeInt(seedTagged | 0);
    } catch {
      seed = 0;
    }
    const span = MAX_TAGGED_INT - MIN_TAGGED_INT + 1;
    const now = Date.now();
    const payload = ((((now + seed) % span) + span) % span) + MIN_TAGGED_INT;
    return encodeInt(payload);
  };

  return {
    clapse_run: runHostRequest,
    clapse_host_run: runHostRequest,
    read_file: hostReadFile,
    unix_time_ms: hostUnixTimeMs,
  };
}

function allowBridgeFallback() {
  const raw = (Deno.env.get("CLAPSE_ALLOW_BRIDGE") ?? "").toLowerCase();
  return raw === "1" || raw === "true" || raw === "yes";
}

async function loadCompilerWasm(path) {
  const wasmBytes = await Deno.readFile(path);
  const module = await WebAssembly.compile(wasmBytes);
  const imports = WebAssembly.Module.imports(module);
  const isBridge = imports.some((imp) =>
    imp.module === "host" && imp.name === "clapse_run"
  );
  if (isBridge && !allowBridgeFallback()) {
    throw new Error(
      "bridge compiler wasm detected; set CLAPSE_ALLOW_BRIDGE=1 for transitional testing or use a native compiler wasm artifact",
    );
  }
  const runtime = makeRuntime();
  let instanceRef = null;
  const hostImports = {
    host: createHostImportObject(runtime, () => instanceRef),
  };
  const instance = await WebAssembly.instantiate(module, hostImports);
  instanceRef = instance;
  assertCompilerExports(instance);
  const memoryExport = instance.exports.__memory ?? instance.exports.memory;
  runtime.state.memory = memoryExport;
  const heapGlobal = instance.exports.__heap_ptr;
  if (heapGlobal instanceof WebAssembly.Global) {
    runtime.state.heapGlobal = heapGlobal;
  }
  return { instance, runtime };
}

function decodeResponseBytes(runtime, responseHandle) {
  const responseBytes = runtime.read_slice_u8_copy(responseHandle);
  const responseText = UTF8_DECODER.decode(responseBytes);
  try {
    return JSON.parse(responseText);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    throw new Error(`compiler wasm returned invalid JSON: ${message}`);
  }
}

export async function callCompilerWasm(path, requestObject) {
  const { instance, runtime } = await loadCompilerWasm(path);
  const run = assertFn(instance, "clapse_run");
  const requestBytes = UTF8_ENCODER.encode(JSON.stringify(requestObject));
  const requestHandle = runtime.alloc_slice_u8(requestBytes);
  const responseHandle = run(requestHandle);
  if (!Number.isInteger(responseHandle) || (responseHandle & 1) === 1) {
    throw new Error(
      `compiler wasm returned invalid response handle: ${responseHandle}`,
    );
  }
  return decodeResponseBytes(runtime, responseHandle);
}

export async function inspectCompilerWasmAbi(path) {
  const wasmBytes = await Deno.readFile(path);
  const module = await WebAssembly.compile(wasmBytes);
  const imports = WebAssembly.Module.imports(module);
  const isBridge = imports.some((imp) =>
    imp.module === "host" && imp.name === "clapse_run"
  );
  const instance = await WebAssembly.instantiate(module, {
    host: {
      clapse_run: (handle) => handle | 0,
      clapse_host_run: (handle) => handle | 0,
      read_file: () => 0,
      unix_time_ms: (seed) => seed | 0,
    },
  });
  assertCompilerExports(instance);
  return {
    ok: true,
    mode: isBridge ? "bridge" : "native",
  };
}

export async function validateCompilerWasmAbi(path) {
  const info = await inspectCompilerWasmAbi(path);
  if (info.mode === "bridge" && !allowBridgeFallback()) {
    throw new Error(
      "bridge compiler wasm is disabled by default; set CLAPSE_ALLOW_BRIDGE=1 to use transitional bridge mode",
    );
  }
  return true;
}

export function decodeWasmBase64(input) {
  if (typeof input !== "string" || input.length === 0) {
    throw new Error("compiler wasm response missing non-empty 'wasm_base64'");
  }
  return fromBase64(input);
}
