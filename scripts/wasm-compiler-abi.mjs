import { makeRuntime } from "./wasm-runtime.mjs";
import {
  buildWasmSeedCompileResponse,
  isWasmBootstrapSeedEnabled,
} from "./wasm-bootstrap-seed.mjs";

const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder();
const MIN_STABLE_KERNEL_COMPILER_BYTES = 16 * 1024;
const COMPILE_DEBUG_ARTIFACT_FILES = [
  "lowered_ir.txt",
  "collapsed_ir.txt",
];
const KNOWN_PLACEHOLDER_WASM_BYTES = 122;
const KNOWN_PLACEHOLDER_ERROR_CODE = "compile_placeholder_response";

function fromBase64(input) {
  const raw = atob(input);
  const out = new Uint8Array(raw.length);
  for (let i = 0; i < raw.length; i += 1) {
    out[i] = raw.charCodeAt(i);
  }
  return out;
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
  return { instance, runtime, wasmBytes };
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
  return cmd === "compile" || cmd === "compile-debug";
}

function normalizePlaceholderSourceText(value) {
  return String(value ?? "").replace(/\r\n/g, "\n");
}

function sourceEchoArtifactPayload(artifactText, label, sourceText) {
  const marker = `(${label}) `;
  if (typeof artifactText !== "string") {
    return null;
  }
  const text = normalizePlaceholderSourceText(artifactText);
  if (!text.startsWith(marker)) {
    return null;
  }
  const payload = text.slice(marker.length);
  const source = normalizePlaceholderSourceText(sourceText);
  if (source.length === 0) {
    return null;
  }
  return payload === source ||
    payload.startsWith(source) ||
    source.startsWith(payload)
    ? payload
    : null;
}

function sourceEchoArtifactMatches(artifactText, label, sourceText) {
  return sourceEchoArtifactPayload(artifactText, label, sourceText) !== null;
}

function isSourceEchoCompileResponse(requestObject, responseObject) {
  const artifacts = responseObject?.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    return false;
  }
  const lowered = artifacts["lowered_ir.txt"];
  const collapsed = artifacts["collapsed_ir.txt"];
  if (typeof lowered !== "string" || typeof collapsed !== "string") {
    return false;
  }
  const sourceText = requestObject?.input_source;
  return sourceEchoArtifactMatches(lowered, "lowered_ir", sourceText) &&
    sourceEchoArtifactMatches(collapsed, "collapsed_ir", sourceText);
}

function normalizeSourceEchoCompileResponse(requestObject, responseObject) {
  if (
    typeof responseObject !== "object" ||
    responseObject === null ||
    Array.isArray(responseObject)
  ) {
    return responseObject;
  }
  const artifacts = responseObject.artifacts;
  if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
    return responseObject;
  }
  const sourceText = requestObject?.input_source;
  const loweredPayload = sourceEchoArtifactPayload(
    artifacts["lowered_ir.txt"],
    "lowered_ir",
    sourceText,
  );
  const collapsedPayload = sourceEchoArtifactPayload(
    artifacts["collapsed_ir.txt"],
    "collapsed_ir",
    sourceText,
  );
  if (loweredPayload === null || collapsedPayload === null) {
    return responseObject;
  }
  return {
    ...responseObject,
    artifacts: {
      ...artifacts,
      "lowered_ir.txt": loweredPayload,
      "collapsed_ir.txt": collapsedPayload,
    },
  };
}

function detectPlaceholderCompileShape(responseObject) {
  if (
    typeof responseObject !== "object" ||
    responseObject === null ||
    Array.isArray(responseObject)
  ) {
    return false;
  }
  if (typeof responseObject.wasm_base64 !== "string" ||
    responseObject.wasm_base64.length === 0
  ) {
    return false;
  }
  let wasmBytes;
  try {
    wasmBytes = decodeWasmBase64(responseObject.wasm_base64);
  } catch {
    return false;
  }
  if (wasmBytes.length !== KNOWN_PLACEHOLDER_WASM_BYTES) {
    return false;
  }
  const publicExports = responseObject.public_exports;
  const abiExports = responseObject.abi_exports;
  const legacyExports = responseObject.exports;
  if (
    (Array.isArray(publicExports) && publicExports.length > 0) ||
    (Array.isArray(abiExports) && abiExports.length > 0) ||
    (Array.isArray(legacyExports) && legacyExports.length > 0)
  ) {
    return false;
  }
  const dts = typeof responseObject.dts === "string"
    ? responseObject.dts.trim()
    : "";
  return dts.length === 0 || dts === "export {}";
}

function buildPlaceholderCompileError(responseObject, errorCode, message, meta) {
  const base = {
    ok: false,
    error_code: errorCode,
    error: message,
  };
  if (typeof responseObject?.backend === "string" && responseObject.backend.length > 0) {
    base.backend = responseObject.backend;
  }
  if (meta && typeof meta === "object" && Object.keys(meta).length > 0) {
    base.meta = meta;
  }
  return base;
}

function findLegacyExportDecl(inputSource) {
  if (typeof inputSource !== "string" || inputSource.length === 0) {
    return null;
  }
  const lines = inputSource.split(/\r?\n/u);
  for (let i = 0; i < lines.length; i += 1) {
    const line = String(lines[i] ?? "");
    const commentAt = line.indexOf("--");
    const code = (commentAt >= 0 ? line.slice(0, commentAt) : line).trim();
    if (!code.startsWith("export")) {
      continue;
    }
    if (/^export\s*\{/u.test(code)) {
      continue;
    }
    return {
      line: i + 1,
      text: code,
    };
  }
  return null;
}

function assertNoLegacyExportSyntax(requestObject) {
  if (!isCompileLikeRequest(requestObject)) {
    return;
  }
  const legacy = findLegacyExportDecl(requestObject?.input_source);
  if (legacy === null) {
    return;
  }
  throw new Error(
    `unsupported export declaration in compile input at line ${legacy.line}: '${legacy.text}' (use export { ... })`,
  );
}

function isSelfhostArtifactsRequest(requestObject) {
  if (!requestObject || typeof requestObject !== "object") {
    return false;
  }
  return requestCommand(requestObject) === "selfhost-artifacts";
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

function isKernelNativeCompileRequest(requestObject) {
  if (!isCompileLikeRequest(requestObject)) {
    return false;
  }
  return compileMode(requestObject) === "kernel-native";
}

function shouldFailClosedPlaceholderCompileResponse(requestObject) {
  return !isCompilerKernelInputPath(requestObject);
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

function hasCompilerAbiExports(exportNames) {
  const hasMemory = exportNames.includes("memory") ||
    exportNames.includes("__memory");
  const hasRun = exportNames.includes("clapse_run");
  return hasMemory && hasRun;
}

function assertCompileExportEntry(entry, idx, fieldLabel) {
  const label = fieldLabel ?? "exports";
  if (!entry || typeof entry !== "object" || Array.isArray(entry)) {
    throw new Error(`compile response: ${label}[${idx}] must be an object`);
  }
  if (typeof entry.name !== "string" || entry.name.length === 0) {
    throw new Error(
      `compile response: ${label}[${idx}].name must be a non-empty string`,
    );
  }
  if (!Number.isInteger(entry.arity) || entry.arity < 0) {
    throw new Error(
      `compile response: ${label}[${idx}].arity must be a non-negative integer`,
    );
  }
}

function parseCompileExportList(responseObject, fieldLabel) {
  const hasField = Object.prototype.hasOwnProperty.call(
    responseObject,
    fieldLabel,
  );
  if (!hasField) {
    return null;
  }
  const raw = responseObject[fieldLabel];
  if (!Array.isArray(raw)) {
    throw new Error(`compile response: '${fieldLabel}' must be an array`);
  }
  for (let i = 0; i < raw.length; i += 1) {
    assertCompileExportEntry(raw[i], i, fieldLabel);
  }
  return raw;
}

function attachCompileContractMetadata(
  responseObject,
  contractMeta,
  options = {},
) {
  if (options.withContractMetadata !== true) {
    return responseObject;
  }
  if (!contractMeta || typeof contractMeta !== "object") {
    return responseObject;
  }
  if (Object.keys(contractMeta).length === 0) {
    return responseObject;
  }
  return {
    ...responseObject,
    __clapse_contract: contractMeta,
  };
}

function assertCompilerAbiOutputContract(responseObject) {
  const contractMeta = {};
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
  if (!hasCompilerAbiExports(exportNames)) {
    throw new Error(
      `compile response for kernel path must emit compiler ABI exports (required: memory + clapse_run; got: ${
        exportNames.join(", ")
      })`,
    );
  }
  if (wasmBytes.length < MIN_STABLE_KERNEL_COMPILER_BYTES) {
    throw new Error(
      `compile response for kernel path is too small (${wasmBytes.length} bytes); strict ABI contract rejects tiny-output fallback`,
    );
  }
  return {
    responseObject,
    contractMeta,
  };
}

function validateCompileResponseContract(
  requestObject,
  responseObject,
  options = {},
) {
  const boundaryResponse = normalizeSourceEchoCompileResponse(
    requestObject,
    responseObject,
  );
  assertObject(boundaryResponse, "compile response");
  if (typeof boundaryResponse.ok !== "boolean") {
    throw new Error("compile response: missing boolean 'ok'");
  }
  if (boundaryResponse.ok !== true) {
    return boundaryResponse;
  }
  if (shouldFailClosedPlaceholderCompileResponse(requestObject)) {
    if (isSourceEchoCompileResponse(requestObject, boundaryResponse)) {
      return buildPlaceholderCompileError(
        boundaryResponse,
        KNOWN_PLACEHOLDER_ERROR_CODE,
        "compile response appears to contain source-echo placeholder artifacts",
        {
          reason: "source_echo_artifacts",
        },
      );
    }
    if (detectPlaceholderCompileShape(boundaryResponse)) {
      return buildPlaceholderCompileError(
        boundaryResponse,
        KNOWN_PLACEHOLDER_ERROR_CODE,
        "compile response appears to be a known tiny placeholder artifact",
        {
          reason: "tiny_placeholder_shape",
          wasm_bytes: boundaryResponse.wasm_base64
            ? decodeWasmBase64(boundaryResponse.wasm_base64).length
            : 0,
        },
      );
    }
  }
  if (
    typeof boundaryResponse.backend !== "string" ||
    boundaryResponse.backend.length === 0
  ) {
    throw new Error("compile response: missing non-empty string 'backend'");
  }
  if (boundaryResponse.backend !== "kernel-native") {
    throw new Error(
      `compile response: unsupported backend '${boundaryResponse.backend}' (expected kernel-native)`,
    );
  }
  if (
    typeof boundaryResponse.wasm_base64 !== "string" ||
    boundaryResponse.wasm_base64.length === 0
  ) {
    throw new Error("compile response: missing non-empty string 'wasm_base64'");
  }
  const publicExports = parseCompileExportList(boundaryResponse, "public_exports");
  const abiExports = parseCompileExportList(boundaryResponse, "abi_exports");
  const legacyExports = parseCompileExportList(boundaryResponse, "exports");
  if (publicExports === null && legacyExports === null && abiExports === null) {
    throw new Error(
      "compile response: missing export lists; expected public_exports, abi_exports, or legacy exports",
    );
  }
  let normalizedResponse = {
    ...boundaryResponse,
    ...(legacyExports !== null && { exports: legacyExports }),
    ...(publicExports !== null && { public_exports: publicExports }),
    ...(abiExports !== null && { abi_exports: abiExports }),
  };
  if (publicExports === null && legacyExports !== null) {
    normalizedResponse.public_exports = legacyExports;
  }
  let contractMeta = {};
  if (compileRequestNeedsCompilerAbiOutput(requestObject)) {
    const abiResult = assertCompilerAbiOutputContract(normalizedResponse);
    normalizedResponse = abiResult.responseObject;
    contractMeta = abiResult.contractMeta;
  }
  if (compileRequestNeedsDebugArtifacts(requestObject)) {
    assertCompileArtifactsContract(normalizedResponse);
  }
  return attachCompileContractMetadata(
    normalizedResponse,
    contractMeta,
    options,
  );
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

function validateSelfhostArtifactsResponseContract(responseObject) {
  assertObject(responseObject, "selfhost-artifacts response");
  if (typeof responseObject.ok !== "boolean") {
    throw new Error("selfhost-artifacts response: missing boolean 'ok'");
  }
  if (responseObject.ok !== true) {
    return responseObject;
  }
  assertCompileArtifactsContract(responseObject);
  return responseObject;
}

export async function callCompilerWasm(path, requestObject, options = {}) {
  const { instance, runtime, wasmBytes } = await loadCompilerWasm(path);
  const requestForWire = requestObject;
  assertNoLegacyExportSyntax(requestForWire);
  if (isCompileLikeRequest(requestForWire) && isWasmBootstrapSeedEnabled()) {
    if (isKernelNativeCompileRequest(requestForWire)) {
      throw new Error(
        "kernel-native compile rejects CLAPSE_USE_WASM_BOOTSTRAP_SEED=1; disable seed mode for strict native requests",
      );
    }
    const seededResponse = await buildWasmSeedCompileResponse(requestForWire, {
      seedWasmBytes: wasmBytes,
    });
    return validateCompileResponseContract(requestForWire, seededResponse, {
      compilerWasmBytes: wasmBytes,
      withContractMetadata: options.withContractMetadata === true,
    });
  }
  const run = assertFn(instance, "clapse_run");
  const requestBytes = UTF8_ENCODER.encode(JSON.stringify(requestForWire));
  const requestHandle = runtime.alloc_slice_u8(requestBytes);
  const responseHandle = run(requestHandle);
  if (!Number.isInteger(responseHandle) || (responseHandle & 1) === 1) {
    throw new Error(
      `compiler wasm returned invalid response handle: ${responseHandle}`,
    );
  }
  let response = decodeResponseBytes(runtime, responseHandle);
  if (isSelfhostArtifactsRequest(requestForWire)) {
    return validateSelfhostArtifactsResponseContract(response);
  }
  if (isCompileLikeRequest(requestForWire)) {
    return validateCompileResponseContract(requestForWire, response, {
      compilerWasmBytes: wasmBytes,
      withContractMetadata: options.withContractMetadata === true,
    });
  }
  if (isEmitWatRequest(requestForWire)) {
    return validateEmitWatResponseContract(response);
  }
  return response;
}

export async function callCompilerWasmRaw(path, requestObject) {
  const { instance, runtime, wasmBytes } = await loadCompilerWasm(path);
  const requestForWire = requestObject;
  assertNoLegacyExportSyntax(requestForWire);
  if (isCompileLikeRequest(requestForWire) && isWasmBootstrapSeedEnabled()) {
    if (isKernelNativeCompileRequest(requestForWire)) {
      throw new Error(
        "kernel-native compile rejects CLAPSE_USE_WASM_BOOTSTRAP_SEED=1; disable seed mode for strict native requests",
      );
    }
    return await buildWasmSeedCompileResponse(requestForWire, {
      seedWasmBytes: wasmBytes,
    });
  }
  const run = assertFn(instance, "clapse_run");
  const requestBytes = UTF8_ENCODER.encode(JSON.stringify(requestForWire));
  const requestHandle = runtime.alloc_slice_u8(requestBytes);
  const responseHandle = run(requestHandle);
  if (!Number.isInteger(responseHandle) || (responseHandle & 1) === 1) {
    throw new Error(
      `compiler wasm returned invalid response handle: ${responseHandle}`,
    );
  }
  const response = decodeResponseBytes(runtime, responseHandle);
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
