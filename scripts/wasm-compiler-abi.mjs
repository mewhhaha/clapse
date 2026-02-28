import { makeRuntime } from "./wasm-runtime.mjs";

const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();
const CLAPSE_WASM_TAIL_CALL_ENV = "CLAPSE_EMIT_WASM_TAIL_CALLS";
const COMPILE_DEBUG_ARTIFACT_FILES = [
  "lowered_ir.txt",
  "collapsed_ir.txt",
];

function fromBase64(input) {
  const raw = atob(input);
  const out = new Uint8Array(raw.length);
  for (let i = 0; i < raw.length; i += 1) {
    out[i] = raw.charCodeAt(i);
  }
  return out;
}

function toBase64(bytes) {
  const chunkSize = 0x8000;
  const chunks = [];
  for (let i = 0; i < bytes.length; i += chunkSize) {
    const end = Math.min(i + chunkSize, bytes.length);
    chunks.push(String.fromCharCode(...bytes.subarray(i, end)));
  }
  return btoa(chunks.join(""));
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

function parseWasmExportEntries(bytes, sectionStart, sectionEnd) {
  const countInfo = decodeVarU32(bytes, sectionStart, sectionEnd);
  let cursor = countInfo.next;
  const entries = [];
  for (let i = 0; i < countInfo.value; i += 1) {
    const entryStart = cursor;
    const nameInfo = readWasmString(bytes, cursor, sectionEnd);
    cursor = nameInfo.next;
    if (cursor >= sectionEnd) {
      throw new Error("malformed wasm export entry");
    }
    const kind = bytes[cursor];
    cursor += 1;
    const indexInfo = decodeVarU32(bytes, cursor, sectionEnd);
    cursor = indexInfo.next;
    entries.push({
      name: nameInfo.value,
      kind,
      index: indexInfo.value,
      start: entryStart,
      end: cursor,
    });
  }
  return {
    count: countInfo.value,
    entries,
  };
}

function appendFunctionExportAlias(wasmBytes, sourceExport, aliasExport) {
  let cursor = 8;
  while (cursor < wasmBytes.length) {
    const sectionIdPos = cursor;
    const sectionId = wasmBytes[cursor];
    cursor += 1;
    const sizeInfo = decodeVarU32(wasmBytes, cursor, wasmBytes.length);
    const sectionStart = sizeInfo.next;
    const sectionEnd = sectionStart + sizeInfo.value;
    if (sectionEnd > wasmBytes.length) {
      throw new Error("malformed wasm section");
    }
    cursor = sectionEnd;
    if (sectionId !== 7) {
      continue;
    }
    const parsed = parseWasmExportEntries(wasmBytes, sectionStart, sectionEnd);
    const hasAlias = parsed.entries.some((entry) =>
      entry.kind === 0 && entry.name === aliasExport
    );
    if (hasAlias) {
      return wasmBytes;
    }
    const sourceFn = parsed.entries.find((entry) =>
      entry.kind === 0 && entry.name === sourceExport
    );
    if (!sourceFn) {
      return wasmBytes;
    }
    const aliasBytes = UTF8_ENCODER.encode(aliasExport);
    const aliasEntry = [];
    aliasEntry.push(...encodeVarU32(aliasBytes.length));
    for (const b of aliasBytes) {
      aliasEntry.push(b);
    }
    aliasEntry.push(0); // function export
    aliasEntry.push(...encodeVarU32(sourceFn.index));

    const payload = [];
    payload.push(...encodeVarU32(parsed.count + 1));
    for (const entry of parsed.entries) {
      const bytes = wasmBytes.subarray(entry.start, entry.end);
      for (const b of bytes) {
        payload.push(b);
      }
    }
    payload.push(...aliasEntry);

    const payloadSize = encodeVarU32(payload.length);
    const sectionLength = 1 + payloadSize.length + payload.length;
    const oldSectionLength = sectionEnd - sectionIdPos;
    const out = new Uint8Array(
      wasmBytes.length - oldSectionLength + sectionLength,
    );
    out.set(wasmBytes.subarray(0, sectionIdPos), 0);
    let outCursor = sectionIdPos;
    out[outCursor] = 7;
    outCursor += 1;
    for (const b of payloadSize) {
      out[outCursor] = b;
      outCursor += 1;
    }
    for (let i = 0; i < payload.length; i += 1) {
      out[outCursor + i] = payload[i];
    }
    outCursor += payload.length;
    out.set(wasmBytes.subarray(sectionEnd), outCursor);
    return out;
  }
  return wasmBytes;
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
            const nameCountInfo = decodeVarU32(
              bytes,
              subsectionStart,
              subsectionEnd,
            );
            let nameCursor = nameCountInfo.next;
            for (let i = 0; i < nameCountInfo.value; i += 1) {
              const indexInfo = decodeVarU32(bytes, nameCursor, subsectionEnd);
              const fnIndex = indexInfo.value;
              nameCursor = indexInfo.next;
              const fnNameInfo = readWasmString(
                bytes,
                nameCursor,
                subsectionEnd,
              );
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
  const totalFunctionCount = metadata.importFunctionCount +
    metadata.functionSectionCount;
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

function tryDecodeTailCallSuffix(bytes, opIndex, suffixEndExclusive) {
  const opcode = bytes[opIndex];
  if (opcode !== 0x10 && opcode !== 0x11) {
    return null;
  }
  let cursor = opIndex + 1;
  const firstImmediate = decodeVarU32(bytes, cursor, suffixEndExclusive);
  cursor = firstImmediate.next;
  if (opcode === 0x11) {
    const secondImmediate = decodeVarU32(bytes, cursor, suffixEndExclusive);
    cursor = secondImmediate.next;
  }
  if (cursor !== suffixEndExclusive) {
    return null;
  }
  return opcode;
}

function rewriteTailCallSuffixInBody(bytes, exprStart, funcEndIndex) {
  if (funcEndIndex <= exprStart) {
    return 0;
  }
  if (bytes[funcEndIndex] !== 0x0b) {
    return 0;
  }
  const suffixCandidates = [funcEndIndex];
  if (funcEndIndex > exprStart && bytes[funcEndIndex - 1] === 0x0f) {
    suffixCandidates.push(funcEndIndex - 1);
  }
  for (const suffixEnd of suffixCandidates) {
    const minScanIndex = Math.max(exprStart, suffixEnd - 12);
    for (let opIndex = suffixEnd - 1; opIndex >= minScanIndex; opIndex -= 1) {
      let opcode;
      try {
        opcode = tryDecodeTailCallSuffix(bytes, opIndex, suffixEnd);
      } catch {
        opcode = null;
      }
      if (opcode === null) {
        continue;
      }
      if (opcode === 0x10) {
        bytes[opIndex] = 0x12; // return_call
      } else {
        bytes[opIndex] = 0x13; // return_call_indirect
      }
      return 1;
    }
  }
  return 0;
}

function rewriteWasmTailCallOpcodesUnsafe(wasmBytes) {
  const rewritten = new Uint8Array(wasmBytes);
  let rewrites = 0;
  let cursor = 8;
  while (cursor < rewritten.length) {
    const sectionId = rewritten[cursor];
    cursor += 1;
    const sizeInfo = decodeVarU32(rewritten, cursor, rewritten.length);
    const sectionStart = sizeInfo.next;
    const sectionEnd = sectionStart + sizeInfo.value;
    if (sectionEnd > rewritten.length) {
      throw new Error("malformed wasm section");
    }
    if (sectionId === 10) {
      const functionCountInfo = decodeVarU32(
        rewritten,
        sectionStart,
        sectionEnd,
      );
      let codeCursor = functionCountInfo.next;
      for (let i = 0; i < functionCountInfo.value; i += 1) {
        const bodySizeInfo = decodeVarU32(rewritten, codeCursor, sectionEnd);
        const bodyStart = bodySizeInfo.next;
        const bodyEnd = bodyStart + bodySizeInfo.value;
        if (bodyEnd > sectionEnd) {
          throw new Error("malformed wasm function body");
        }
        const localDeclCountInfo = decodeVarU32(rewritten, bodyStart, bodyEnd);
        let exprStart = localDeclCountInfo.next;
        for (let j = 0; j < localDeclCountInfo.value; j += 1) {
          const localCountInfo = decodeVarU32(rewritten, exprStart, bodyEnd);
          exprStart = localCountInfo.next;
          if (exprStart >= bodyEnd) {
            throw new Error("malformed wasm local declaration");
          }
          exprStart += 1; // value type byte
        }
        rewrites += rewriteTailCallSuffixInBody(
          rewritten,
          exprStart,
          bodyEnd - 1,
        );
        codeCursor = bodyEnd;
      }
    }
    cursor = sectionEnd;
  }
  return { wasmBytes: rewritten, rewrites };
}

export function rewriteWasmTailCallOpcodes(wasmBytes) {
  if (!boolEnvFlag(CLAPSE_WASM_TAIL_CALL_ENV, true)) {
    return { wasmBytes, rewrites: 0 };
  }
  try {
    return rewriteWasmTailCallOpcodesUnsafe(wasmBytes);
  } catch {
    return { wasmBytes, rewrites: 0 };
  }
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

async function loadCompilerWasm(path) {
  const wasmBytes = await Deno.readFile(path);
  const module = await WebAssembly.compile(wasmBytes);
  const imports = WebAssembly.Module.imports(module);
  const hostImports = imports.filter((imp) => imp.module === "host");
  if (hostImports.length > 0) {
    const hostImportList = hostImports.map((imp) => imp.name).join(", ");
    throw new Error(
      `bridge compiler wasm detected (host imports: ${hostImportList}); use clapse_compiler.wasm without host bridge support`,
    );
  }
  const runtime = makeRuntime();
  const instance = await WebAssembly.instantiate(module, {});
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

function assertObject(value, context) {
  if (!value || typeof value !== "object" || Array.isArray(value)) {
    throw new Error(`${context}: expected object`);
  }
}

function requestCommand(requestObject) {
  return String(requestObject?.command ?? "").trim().toLowerCase();
}

function compileMode(requestObject) {
  return String(requestObject?.compile_mode ?? "").trim().toLowerCase();
}

function isCompileLikeRequest(requestObject) {
  if (!requestObject || typeof requestObject !== "object") {
    return false;
  }
  const cmd = requestCommand(requestObject);
  return cmd === "compile" || cmd === "compile-debug" ||
    cmd === "selfhost-artifacts";
}

function isEmitWatRequest(requestObject) {
  if (!requestObject || typeof requestObject !== "object") {
    return false;
  }
  return requestCommand(requestObject) === "emit-wat";
}

function compileRequestNeedsDebugArtifacts(requestObject) {
  const command = requestCommand(requestObject);
  if (command === "compile-debug") {
    return true;
  }
  const mode = compileMode(requestObject);
  return mode === "debug" || mode === "kernel-debug" ||
    mode === "native-debug" || mode === "debug-funcmap";
}

function normalizeContractPath(path) {
  return String(path ?? "").trim().replaceAll("\\", "/");
}

function isCompilerKernelInputPath(requestObject) {
  const inputPath = normalizeContractPath(requestObject?.input_path);
  if (inputPath.length === 0) {
    return false;
  }
  return inputPath === "lib/compiler/kernel.clapse" ||
    inputPath.endsWith("/lib/compiler/kernel.clapse");
}

function compileRequestNeedsCompilerAbiOutput(requestObject) {
  const mode = compileMode(requestObject);
  return mode === "kernel-native" && isCompilerKernelInputPath(requestObject);
}

function assertCompileArtifactsContract(responseObject) {
  const artifacts = responseObject.artifacts;
  assertObject(artifacts, "compile response.artifacts");
  const missing = [];
  for (const file of COMPILE_DEBUG_ARTIFACT_FILES) {
    if (typeof artifacts[file] !== "string") {
      missing.push(file);
    }
  }
  if (missing.length > 0) {
    throw new Error(
      `compile response.artifacts missing debug keys: ${missing.join(", ")}`,
    );
  }
}

function assertCompilerAbiOutputContract(responseObject) {
  let wasmBytes;
  try {
    wasmBytes = decodeWasmBase64(responseObject.wasm_base64);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    throw new Error(`compile response wasm_base64 decode failed: ${msg}`);
  }
  let module;
  try {
    module = new WebAssembly.Module(wasmBytes);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    throw new Error(`compile response wasm_base64 is not valid wasm: ${msg}`);
  }
  const exportNames = WebAssembly.Module.exports(module).map((entry) =>
    entry.name
  );
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  const hasRun = exportNames.includes("clapse_run");
  if (hasMemory && hasRun) {
    return responseObject;
  }
  const hasMain = exportNames.includes("main");
  if (hasMemory && hasMain && !hasRun) {
    const aliased = appendFunctionExportAlias(wasmBytes, "main", "clapse_run");
    let aliasedModule;
    try {
      aliasedModule = new WebAssembly.Module(aliased);
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      throw new Error(
        `compile response alias patch produced invalid wasm: ${msg}`,
      );
    }
    const aliasedExports = WebAssembly.Module.exports(aliasedModule).map((
      entry,
    ) => entry.name);
    if (!aliasedExports.includes("clapse_run")) {
      throw new Error(
        `compile response for kernel path must emit compiler ABI exports (required: memory + clapse_run; got: ${
          exportNames.join(", ")
        })`,
      );
    }
    const next = {
      ...responseObject,
      wasm_base64: toBase64(aliased),
    };
    const exportsList = Array.isArray(responseObject.exports)
      ? [...responseObject.exports]
      : [];
    const hasDeclaredRun = exportsList.some((entry) =>
      entry && entry.name === "clapse_run"
    );
    if (!hasDeclaredRun) {
      exportsList.push({ name: "clapse_run", arity: 1 });
    }
    if (exportsList.length > 0) {
      next.exports = exportsList;
    }
    if (
      typeof responseObject.dts === "string" &&
      !responseObject.dts.includes("clapse_run")
    ) {
      const suffix = responseObject.dts.endsWith("\n") ? "" : "\n";
      next.dts =
        `${responseObject.dts}${suffix}export declare function clapse_run(request_handle: number): number;\n`;
    }
    return next;
  }
  throw new Error(
    `compile response for kernel path must emit compiler ABI exports (required: memory + clapse_run; got: ${
      exportNames.join(", ")
    })`,
  );
}

function validateCompileResponseContract(requestObject, responseObject) {
  assertObject(responseObject, "compile response");
  if (typeof responseObject.ok !== "boolean") {
    throw new Error("compile response: missing boolean 'ok'");
  }
  if (responseObject.ok !== true) {
    return responseObject;
  }
  if (
    typeof responseObject.backend !== "string" ||
    responseObject.backend.length === 0
  ) {
    throw new Error("compile response: missing non-empty string 'backend'");
  }
  if (responseObject.backend !== "kernel-native") {
    throw new Error(
      `compile response: unsupported backend '${responseObject.backend}' (expected kernel-native)`,
    );
  }
  if (
    typeof responseObject.wasm_base64 !== "string" ||
    responseObject.wasm_base64.length === 0
  ) {
    throw new Error("compile response: missing non-empty string 'wasm_base64'");
  }
  let normalizedResponse = responseObject;
  if (compileRequestNeedsCompilerAbiOutput(requestObject)) {
    normalizedResponse = assertCompilerAbiOutputContract(normalizedResponse);
  }
  if (compileRequestNeedsDebugArtifacts(requestObject)) {
    assertCompileArtifactsContract(normalizedResponse);
  }
  return normalizedResponse;
}

function validateEmitWatResponseContract(responseObject) {
  assertObject(responseObject, "emit-wat response");
  if (typeof responseObject.ok !== "boolean") {
    throw new Error("emit-wat response: missing boolean 'ok'");
  }
  if (responseObject.ok !== true) {
    return responseObject;
  }
  if (
    typeof responseObject.wat !== "string" || responseObject.wat.length === 0
  ) {
    throw new Error("emit-wat response: missing non-empty string 'wat'");
  }
  return responseObject;
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
  const response = decodeResponseBytes(runtime, responseHandle);
  if (isCompileLikeRequest(requestObject)) {
    return validateCompileResponseContract(requestObject, response);
  }
  if (isEmitWatRequest(requestObject)) {
    return validateEmitWatResponseContract(response);
  }
  return response;
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
