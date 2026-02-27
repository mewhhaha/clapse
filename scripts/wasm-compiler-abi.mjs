import { decodeInt, encodeInt, makeRuntime } from "./wasm-runtime.mjs";

const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();
const MAX_TAGGED_INT = 1073741823;
const MIN_TAGGED_INT = -1073741824;
const HOST_COMPILE_DEPTH_ENV = "CLAPSE_HOST_COMPILE_DEPTH";
const CLAPSE_FUNC_MAP_ENV = "CLAPSE_DEBUG_FUNC_MAP";
const COMPILE_DEBUG_ARTIFACT_FILES = [
  "lowered_ir.txt",
  "collapsed_ir.txt",
];
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

function boolEnvFlag(name, defaultValue = false) {
  const raw = String(Deno.env.get(name) ?? "").trim().toLowerCase();
  if (raw.length === 0) {
    return defaultValue;
  }
  return raw === "1" || raw === "true" || raw === "yes";
}

function encodeVarU32(value) {
  let n = value >>> 0;
  const out = [];
  while (true) {
    const byte = n & 0x7f;
    n >>>= 7;
    if (n === 0) {
      out.push(byte);
      break;
    }
    out.push(byte | 0x80);
  }
  return out;
}

function decodeVarU32(bytes, start, end) {
  let cursor = start;
  let shift = 0;
  let value = 0;
  while (cursor < end) {
    const b = bytes[cursor];
    cursor += 1;
    value |= (b & 0x7f) << shift;
    if ((b & 0x80) === 0) {
      return { value, next: cursor };
    }
    shift += 7;
  }
  throw new Error("unexpected end of wasm section while reading varuint");
}

function readWasmString(bytes, start, end) {
  const lenInfo = decodeVarU32(bytes, start, end);
  const startBytes = lenInfo.next;
  const next = startBytes + lenInfo.value;
  if (next > end) {
    throw new Error("malformed wasm string");
  }
  return {
    value: UTF8_DECODER.decode(bytes.subarray(startBytes, next)),
    next,
  };
}

function decodeLimits(bytes, start, end) {
  const flagsInfo = decodeVarU32(bytes, start, end);
  let cursor = flagsInfo.next;
  if (flagsInfo.value === 0) {
    return decodeVarU32(bytes, cursor, end).next;
  }
  if (flagsInfo.value === 1) {
    cursor = decodeVarU32(bytes, cursor, end).next;
    return decodeVarU32(bytes, cursor, end).next;
  }
  throw new Error("unsupported wasm limits flags");
}

function parseWasmFunctionMetadata(bytes) {
  let cursor = 8;
  let importFunctionCount = 0;
  let functionSectionCount = 0;
  const exportNameByIndex = new Map();
  const wasmNameByIndex = new Map();
  while (cursor < bytes.length) {
    const sectionId = bytes[cursor];
    cursor += 1;
    const sizeInfo = decodeVarU32(bytes, cursor, bytes.length);
    const sectionSize = sizeInfo.value;
    cursor = sizeInfo.next;
    const sectionStart = cursor;
    const sectionEnd = sectionStart + sectionSize;
    if (sectionEnd > bytes.length) {
      throw new Error("malformed wasm section");
    }
    if (sectionId === 2) {
      const importCountInfo = decodeVarU32(bytes, cursor, sectionEnd);
      let iCursor = importCountInfo.next;
      for (let i = 0; i < importCountInfo.value; i += 1) {
        iCursor = readWasmString(bytes, iCursor, sectionEnd).next;
        iCursor = readWasmString(bytes, iCursor, sectionEnd).next;
        const importKind = bytes[iCursor];
        iCursor += 1;
        if (importKind === 0) {
          iCursor = decodeVarU32(bytes, iCursor, sectionEnd).next;
          importFunctionCount += 1;
        } else if (importKind === 1) {
          iCursor += 1;
          iCursor = decodeLimits(bytes, iCursor, sectionEnd);
        } else if (importKind === 2) {
          iCursor = decodeLimits(bytes, iCursor, sectionEnd);
        } else if (importKind === 3) {
          iCursor = decodeVarU32(bytes, iCursor, sectionEnd).next;
          iCursor += 1;
        } else {
          throw new Error(`unsupported wasm import kind: ${importKind}`);
        }
      }
    } else if (sectionId === 3) {
      const functionSectionCountInfo = decodeVarU32(bytes, cursor, sectionEnd);
      functionSectionCount = functionSectionCountInfo.value;
    } else if (sectionId === 7) {
      const exportCountInfo = decodeVarU32(bytes, cursor, sectionEnd);
      let eCursor = exportCountInfo.next;
      for (let i = 0; i < exportCountInfo.value; i += 1) {
        const nameInfo = readWasmString(bytes, eCursor, sectionEnd);
        const kind = bytes[nameInfo.next];
        const indexInfo = decodeVarU32(bytes, nameInfo.next + 1, sectionEnd);
        if (kind === 0 && !exportNameByIndex.has(indexInfo.value)) {
          exportNameByIndex.set(indexInfo.value, nameInfo.value);
        }
        eCursor = indexInfo.next;
      }
    } else if (sectionId === 0) {
      const sectionNameInfo = readWasmString(bytes, cursor, sectionEnd);
      if (sectionNameInfo.value === "name") {
        let nCursor = sectionNameInfo.next;
        while (nCursor < sectionEnd) {
          const subsectionId = bytes[nCursor];
          nCursor += 1;
          const subsectionLenInfo = decodeVarU32(bytes, nCursor, sectionEnd);
          const subsectionStart = subsectionLenInfo.next;
          const subsectionEnd = subsectionStart + subsectionLenInfo.value;
          if (subsectionEnd > sectionEnd) {
            throw new Error("malformed wasm custom name subsection");
          }
          if (subsectionId === 1) {
            const nameCountInfo = decodeVarU32(bytes, subsectionStart, subsectionEnd);
            let nameCursor = nameCountInfo.next;
            for (let i = 0; i < nameCountInfo.value; i += 1) {
              const indexInfo = decodeVarU32(bytes, nameCursor, subsectionEnd);
              const fnIndex = indexInfo.value;
              nameCursor = indexInfo.next;
              const fnNameInfo = readWasmString(bytes, nameCursor, subsectionEnd);
              if (!wasmNameByIndex.has(fnIndex)) {
                wasmNameByIndex.set(fnIndex, fnNameInfo.value);
              }
              nameCursor = fnNameInfo.next;
            }
          }
          nCursor = subsectionEnd;
        }
      }
    }
    cursor = sectionEnd;
  }
  return {
    importFunctionCount,
    functionSectionCount,
    wasmNameByIndex,
    exportNameByIndex,
  };
}

function appendClapseFuncMap(wasmBytes) {
  const metadata = parseWasmFunctionMetadata(wasmBytes);
  const totalFunctionCount = metadata.importFunctionCount + metadata.functionSectionCount;
  const payload = [];
  const sectionNameBytes = UTF8_ENCODER.encode("clapse.funcmap");
  payload.push(...encodeVarU32(sectionNameBytes.length));
  for (const b of sectionNameBytes) {
    payload.push(b);
  }
  payload.push(...encodeVarU32(totalFunctionCount));
  for (let i = 0; i < totalFunctionCount; i += 1) {
    const fnName = metadata.wasmNameByIndex.get(i) ??
      metadata.exportNameByIndex.get(i) ??
      `func_${i}`;
    const fnNameBytes = UTF8_ENCODER.encode(fnName);
    payload.push(...encodeVarU32(i));
    payload.push(...encodeVarU32(fnNameBytes.length));
    for (const b of fnNameBytes) {
      payload.push(b);
    }
  }
  const sectionSize = encodeVarU32(payload.length);
  const out = new Uint8Array(1 + sectionSize.length + payload.length);
  out[0] = 0;
  let outCursor = 1;
  for (const b of sectionSize) {
    out[outCursor] = b;
    outCursor += 1;
  }
  for (let i = 0; i < payload.length; i += 1) {
    out[outCursor + i] = payload[i];
  }
  const final = new Uint8Array(wasmBytes.length + out.length);
  final.set(wasmBytes, 0);
  final.set(out, wasmBytes.length);
  return final;
}

function wantsDebugClapseFuncMap(request) {
  const mode = String(request?.compile_mode ?? "").toLowerCase();
  return boolEnvFlag(CLAPSE_FUNC_MAP_ENV, false)
    || mode === "funcmap"
    || mode === "emit-funcmap"
    || mode === "debug-funcmap"
    || mode === "debug";
}

function wantsCompileDebugArtifacts(request) {
  const mode = String(request?.compile_mode ?? "").toLowerCase().trim();
  return mode === "debug" || mode === "debug-funcmap";
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

function fileExistsSync(path) {
  try {
    const stat = Deno.statSync(path);
    return stat.isFile;
  } catch {
    return false;
  }
}

function isBridgeWasmPath(path) {
  const fileName = String(path).toLowerCase();
  return fileName.includes("bridge") && fileName.endsWith(".wasm");
}

function resolveHostNativeCompilerWasmPath() {
  const envPath = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "").trim();
  if (envPath.length > 0 && !isBridgeWasmPath(envPath) && fileExistsSync(envPath)) {
    return envPath;
  }
  const fallbackCandidates = [
    "out/clapse_compiler.wasm",
    "artifacts/latest/clapse_compiler.wasm",
  ];
  for (const candidate of fallbackCandidates) {
    if (fileExistsSync(candidate)) {
      return candidate;
    }
  }
  return "";
}

function coerceStringArray(value) {
  if (!Array.isArray(value)) return [];
  const out = [];
  for (const item of value) {
    if (typeof item === "string" && item.length > 0) {
      out.push(item);
    }
  }
  return out;
}

function parseRequestString(request, key) {
  const value = request[key];
  return typeof value === "string" ? value : "";
}

function currentHostCompileDepth() {
  const raw = String(Deno.env.get(HOST_COMPILE_DEPTH_ENV) ?? "0").trim();
  const parsed = Number.parseInt(raw, 10);
  if (!Number.isFinite(parsed) || parsed < 0) {
    return 0;
  }
  return parsed;
}

function runHostCompileNative(request) {
  const depth = currentHostCompileDepth();
  if (depth > 1) {
    return {
      ok: false,
      error:
        "host compile recursion detected; compiler artifact still requires host compile backend",
    };
  }
  const compilerPath = resolveHostNativeCompilerWasmPath();
  if (!compilerPath) {
    return {
      ok: false,
      error: "host compile bridge requires a native compiler artifact; set CLAPSE_COMPILER_WASM_PATH",
    };
  }
  const requestHandle = {
    command: "compile",
    input_path: request.input_path,
    input_source: request.input_source,
    plugin_wasm_paths: coerceStringArray(request.plugin_wasm_paths),
  };
  const procEnv = { ...Deno.env.toObject() };
  procEnv[HOST_COMPILE_DEPTH_ENV] = String(depth + 1);
  const runScript = new URL("./run-clapse-compiler-wasm.mjs", import.meta.url).pathname;
  const sourcePath = Deno.makeTempFileSync({
    prefix: "clapse-host-compile-source-",
    suffix: ".clapse",
  });
  const outPath = Deno.makeTempFileSync({
    prefix: "clapse-host-compile-",
    suffix: ".wasm",
  });
  const outDtsPath = outPath.replace(/\.wasm$/u, ".d.ts");
  const debugArtifactsDir = Deno.makeTempDirSync({
    prefix: "clapse-host-compile-debug-artifacts-",
  });
  try {
    try {
      Deno.writeTextFileSync(sourcePath, requestHandle.input_source);
    } catch {
      return {
        ok: false,
        error: `host compile bridge failed to write temporary source for ${String(requestHandle.input_path ?? "<unknown>")}`,
      };
    }
    const proc = new Deno.Command("deno", {
      args: ["run", "-A", runScript, "compile", sourcePath, outPath],
      env: procEnv,
      stdout: "piped",
      stderr: "piped",
    });
    const procOut = proc.outputSync();
    if (!procOut.success) {
      const stderrTail = UTF8_DECODER.decode(procOut.stderr).trim();
      const stdoutTail = UTF8_DECODER.decode(procOut.stdout).trim();
      return {
        ok: false,
        error: `host compile command failed (exit ${procOut.code}): ${stderrTail || stdoutTail || "empty output"}`,
      };
    }
    let wasmBytes;
    try {
      wasmBytes = Deno.readFileSync(outPath);
    } catch {
      return {
        ok: false,
        error: `host compile command did not produce ${outPath}`,
      };
    }
    let dts = "export {}";
    try {
      dts = Deno.readTextFileSync(outDtsPath);
    } catch {
      // keep export {} fallback
    }
    let outputWasmBase64;
    try {
      outputWasmBase64 = toBase64(wasmBytes);
    } catch {
      return {
        ok: false,
        error: "host compile command produced malformed wasm artifact",
      };
    }
    if (outputWasmBase64.length === 0) {
      return {
        ok: false,
        error: "host compile command produced empty wasm artifact",
      };
    }
    if (wantsDebugClapseFuncMap(request)) {
      try {
        const responseBytes = fromBase64(outputWasmBase64);
        const withFuncMap = appendClapseFuncMap(responseBytes);
        outputWasmBase64 = toBase64(withFuncMap);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        return {
          ok: false,
          error: `host compile funcmap injection failed: ${msg}`,
        };
      }
    }
    let debugArtifacts = null;
    if (wantsCompileDebugArtifacts(request)) {
      const selfhostProc = new Deno.Command("deno", {
        args: [
          "run",
          "-A",
          runScript,
          "selfhost-artifacts",
          sourcePath,
          debugArtifactsDir,
        ],
        env: procEnv,
        stdout: "piped",
        stderr: "piped",
      });
      const selfhostOut = selfhostProc.outputSync();
      if (!selfhostOut.success) {
        const stderrTail = UTF8_DECODER.decode(selfhostOut.stderr).trim();
        const stdoutTail = UTF8_DECODER.decode(selfhostOut.stdout).trim();
        return {
          ok: false,
          error:
            `host debug-artifacts command failed (exit ${selfhostOut.code}): ${stderrTail || stdoutTail || "empty output"}`,
        };
      }
      const collected = {};
      for (const key of COMPILE_DEBUG_ARTIFACT_FILES) {
        const filePath = `${debugArtifactsDir}/${key}`;
        try {
          collected[key] = Deno.readTextFileSync(filePath);
        } catch {
          return {
            ok: false,
            error:
              `host debug-artifacts command did not produce ${key}`,
          };
        }
      }
      debugArtifacts = collected;
    }
    return {
      ok: true,
      wasm_base64: outputWasmBase64,
      exports: [],
      dts,
      ...(debugArtifacts === null ? {} : { artifacts: debugArtifacts }),
    };
  } finally {
    try {
      Deno.removeSync(sourcePath);
    } catch {
      // best effort cleanup
    }
    try {
      Deno.removeSync(outPath);
    } catch {
      // best effort cleanup
    }
    try {
      Deno.removeSync(outDtsPath);
    } catch {
      // best effort cleanup
    }
    try {
      Deno.removeSync(debugArtifactsDir, { recursive: true });
    } catch {
      // best effort cleanup
    }
  }
}

function toBase64(bytes) {
  let binary = "";
  for (let i = 0; i < bytes.length; i += 1) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary);
}

function runHostCompile(request) {
  const inputPath = parseRequestString(request, "input_path");
  if (typeof request.input_source !== "string") {
    return {
      ok: false,
      error: `host compile bridge requires string input_source (input=${inputPath})`,
    };
  }
  const inputSource = request.input_source;
  return runHostCompileNative({
    ...request,
    input_path: inputPath,
    input_source: inputSource,
  });
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

async function loadCompilerWasm(path) {
  const wasmBytes = await Deno.readFile(path);
  const module = await WebAssembly.compile(wasmBytes);
  const imports = WebAssembly.Module.imports(module);
  const isBridge = imports.some((imp) =>
    imp.module === "host" && imp.name === "clapse_run"
  );
  if (isBridge) {
    throw new Error(
      "bridge compiler wasm detected; use clapse_compiler.wasm without host bridge support",
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
  if (info.mode === "bridge") {
    throw new Error(
      "bridge compiler wasm is disabled; use a native clapse_compiler.wasm artifact",
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

export { appendClapseFuncMap };
