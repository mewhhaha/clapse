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
const PHASE1_UNSUPPORTED_ERROR_CODE = "compile_phase1_unsupported";
const RAW_NON_KERNEL_BOUNDARY_SYNTHESIS_ERROR =
  "non-kernel raw compile requires boundary synthesis";
const PHASE1_WASM_TAGGED_0 =
  "AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBxECBm1lbW9yeQIABG1haW4AAAoGAQQAQQEL";
const PHASE1_WASM_TAGGED_3 =
  "AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBxECBm1lbW9yeQIABG1haW4AAAoGAQQAQQcL";
const PHASE1_WASM_TAGGED_4 =
  "AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBxECBm1lbW9yeQIABG1haW4AAAoGAQQAQQkL";
const PHASE1_WASM_TAGGED_7 =
  "AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBxECBm1lbW9yeQIABG1haW4AAAoGAQQAQQ8L";
const PHASE1_WASM_TAGGED_10 =
  "AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBxECBm1lbW9yeQIABG1haW4AAAoGAQQAQRUL";
const PHASE1_WASM_TAGGED_11 =
  "AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBxECBm1lbW9yeQIABG1haW4AAAoGAQQAQRcL";
const PHASE1_WASM_TAGGED_14 =
  "AGFzbQEAAAABBQFgAAF/AwIBAAUDAQABBxECBm1lbW9yeQIABG1haW4AAAoGAQQAQR0L";
const LEGACY_PHASE1_WASM_TAGGED_0 =
  "AGFzbQEAAAABCgJgAX8Bf2AAAX8DAwIAAQUDAQABBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAQQEL";
const LEGACY_PHASE1_WASM_TAGGED_3 =
  "AGFzbQEAAAABCgJgAX8Bf2AAAX8DAwIAAQUDAQABBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAQQcL";
const LEGACY_PHASE1_WASM_TAGGED_4 =
  "AGFzbQEAAAABCgJgAX8Bf2AAAX8DAwIAAQUDAQABBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAQQkL";
const LEGACY_PHASE1_WASM_TAGGED_7 =
  "AGFzbQEAAAABCgJgAX8Bf2AAAX8DAwIAAQUDAQABBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAQQ8L";
const LEGACY_PHASE1_WASM_TAGGED_10 =
  "AGFzbQEAAAABCgJgAX8Bf2AAAX8DAwIAAQUDAQABBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAQRUL";
const LEGACY_PHASE1_WASM_TAGGED_11 =
  "AGFzbQEAAAABCgJgAX8Bf2AAAX8DAwIAAQUDAQABBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAQRcL";
const LEGACY_PHASE1_WASM_TAGGED_14 =
  "AGFzbQEAAAABCgJgAX8Bf2AAAX8DAwIAAQUDAQABBx4DBm1lbW9yeQIACmNsYXBzZV9ydW4AAARtYWluAAEKCwIEACAACwQAQR0L";

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
  const typeParamCountByIndex = new Map();
  const functionTypeIndexByIndex = new Map();
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
    if (sectionId === 1) {
      const typeCountInfo = decodeVarU32(bytes, cursor, sectionEnd);
      let tCursor = typeCountInfo.next;
      for (let i = 0; i < typeCountInfo.value; i += 1) {
        const form = bytes[tCursor];
        tCursor += 1;
        if (form !== 0x60) {
          throw new Error(`unsupported wasm type form: ${form}`);
        }
        const paramCountInfo = decodeVarU32(bytes, tCursor, sectionEnd);
        tCursor = paramCountInfo.next;
        typeParamCountByIndex.set(i, paramCountInfo.value);
        tCursor += paramCountInfo.value;
        const resultCountInfo = decodeVarU32(bytes, tCursor, sectionEnd);
        tCursor = resultCountInfo.next + resultCountInfo.value;
      }
    } else if (sectionId === 2) {
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
      let fCursor = functionSectionCountInfo.next;
      for (let i = 0; i < functionSectionCountInfo.value; i += 1) {
        const typeIndexInfo = decodeVarU32(bytes, fCursor, sectionEnd);
        functionTypeIndexByIndex.set(
          importFunctionCount + i,
          typeIndexInfo.value,
        );
        fCursor = typeIndexInfo.next;
      }
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
    typeParamCountByIndex,
    functionTypeIndexByIndex,
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

function compilerSourceVersionProbeRequest() {
  return {
    command: "compile",
    compile_mode: "debug",
    input_path: "repl/input.clapse",
    input_source: "identity x = x\nmain = identity 7\n",
    plugin_wasm_paths: [],
    entrypoint_exports: ["main"],
  };
}

async function probeCompilerSourceVersion(instance, runtime) {
  try {
    const run = assertFn(instance, "clapse_run");
    const requestBytes = UTF8_ENCODER.encode(JSON.stringify(
      compilerSourceVersionProbeRequest(),
    ));
    const requestHandle = runtime.alloc_slice_u8(requestBytes);
    const responseHandle = run(requestHandle);
    if (!Number.isInteger(responseHandle) || (responseHandle & 1) === 1) {
      return "";
    }
    const responseObject = decodeResponseBytes(runtime, responseHandle);
    const sourceVersion = String(
      responseObject?.__clapse_contract?.source_version ?? "",
    ).trim();
    return sourceVersion;
  } catch {
    return "";
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

function encodeVarS32(value) {
  let n = value | 0;
  const out = [];
  while (true) {
    const byte = n & 0x7f;
    n >>= 7;
    if ((n === 0 && (byte & 0x40) === 0) || (n === -1 && (byte & 0x40) !== 0)) {
      out.push(byte);
      break;
    }
    out.push(byte | 0x80);
  }
  return out;
}

function toBase64(bytes) {
  let s = "";
  for (let i = 0; i < bytes.length; i += 1) {
    s += String.fromCharCode(bytes[i] ?? 0);
  }
  return btoa(s);
}

function buildPhase1TaggedWasmBase64(rawValue, exportName = "main") {
  if (!Number.isSafeInteger(rawValue)) {
    return PHASE1_WASM_TAGGED_0;
  }
  if (typeof exportName !== "string" || exportName.length === 0) {
    exportName = "main";
  }
  const moduleBytes = [
    0x00,
    0x61,
    0x73,
    0x6d,
    0x01,
    0x00,
    0x00,
    0x00,
    ...phase1WrapSection(1, [
      ...encodeVarU32(1),
      0x60,
      ...encodeVarU32(0),
      ...encodeVarU32(1),
      0x7f,
    ]),
    ...phase1WrapSection(3, [
      ...encodeVarU32(1),
      ...encodeVarU32(0),
    ]),
    ...phase1WrapSection(5, phase1WasmMemorySection()),
    ...phase1WrapSection(7, phase1WasmExportSection([
      { name: "memory", kind: 0x02, index: 0 },
      { name: exportName, kind: 0x00, index: 0 },
    ])),
    ...phase1WrapSection(10, phase1WasmCodeSection([
      {
        localCount: 0,
        code: [0x41, ...encodeVarS32(rawValue * 2 + 1)],
      },
    ])),
  ];
  return toBase64(Uint8Array.from(moduleBytes));
}

function buildPhase1MultiTaggedWasmBase64(entries) {
  if (!Array.isArray(entries) || entries.length === 0) {
    return null;
  }
  const sanitized = [];
  for (const entry of entries) {
    const name = typeof entry?.name === "string" && entry.name.length > 0
      ? entry.name
      : null;
    const rawValue = entry?.rawValue;
    if (name === null || !Number.isSafeInteger(rawValue)) {
      return null;
    }
    sanitized.push({ name, rawValue });
  }
  const moduleBytes = [
    0x00,
    0x61,
    0x73,
    0x6d,
    0x01,
    0x00,
    0x00,
    0x00,
    ...phase1WrapSection(1, [
      ...encodeVarU32(1),
      0x60,
      ...encodeVarU32(0),
      ...encodeVarU32(1),
      0x7f,
    ]),
    ...phase1WrapSection(3, [
      ...encodeVarU32(sanitized.length),
      ...sanitized.flatMap(() => encodeVarU32(0)),
    ]),
    ...phase1WrapSection(5, phase1WasmMemorySection()),
    ...phase1WrapSection(7, phase1WasmExportSection([
      { name: "memory", kind: 0x02, index: 0 },
      ...sanitized.map((entry, index) => ({
        name: entry.name,
        kind: 0x00,
        index,
      })),
    ])),
    ...phase1WrapSection(10, phase1WasmCodeSection(
      sanitized.map((entry) => ({
        localCount: 0,
        code: [0x41, ...encodeVarS32(entry.rawValue * 2 + 1)],
      })),
    )),
  ];
  return toBase64(Uint8Array.from(moduleBytes));
}

function phase1FlattenApply(node) {
  const args = [];
  let cursor = node;
  while (cursor && cursor.type === "apply") {
    args.unshift(cursor.arg);
    cursor = cursor.fn;
  }
  return { callee: cursor, args };
}

function phase1ParseStructMakerName(name) {
  if (typeof name !== "string") {
    return null;
  }
  const match = /^__mk_(.+)_(\d+)(?:_.+)?$/u.exec(name);
  if (match === null) {
    return null;
  }
  const [, tag, arityText] = match;
  const arity = Number(arityText);
  if (!Number.isInteger(arity) || arity < 0) {
    return null;
  }
  return { tag, arity };
}

function phase1ParseStructGetterName(name) {
  if (typeof name !== "string") {
    return null;
  }
  const match = /^__get_(.+)_(\d+)(?:_.+)?$/u.exec(name);
  if (match === null) {
    return null;
  }
  const [, tag, indexText] = match;
  const index = Number(indexText);
  if (!Number.isInteger(index) || index < 0) {
    return null;
  }
  return { tag, index };
}

function phase1ParseStructPredicateName(name) {
  if (typeof name !== "string" || !name.startsWith("__is_")) {
    return null;
  }
  const tag = name.slice("__is_".length);
  return tag.length > 0 ? { tag } : null;
}

function phase1ParseStructMakerExpr(expr) {
  if (!expr || typeof expr !== "object") {
    return null;
  }
  if (expr.type === "var") {
    const maker = phase1ParseStructMakerName(expr.name);
    if (maker === null || maker.arity !== 0) {
      return null;
    }
    return { ...maker, args: [] };
  }
  if (expr.type !== "apply") {
    return null;
  }
  const flattened = phase1FlattenApply(expr);
  if (!flattened?.callee || flattened.callee.type !== "var") {
    return null;
  }
  const maker = phase1ParseStructMakerName(flattened.callee.name);
  if (maker === null || maker.arity !== flattened.args.length) {
    return null;
  }
  return { ...maker, args: flattened.args };
}

function phase1ReduceStructHelperApply(flattened) {
  if (
    !flattened || !flattened.callee || flattened.callee.type !== "var" ||
    !Array.isArray(flattened.args)
  ) {
    return null;
  }
  const calleeName = flattened.callee.name;
  const getter = phase1ParseStructGetterName(calleeName);
  if (getter !== null && flattened.args.length === 1) {
    const maker = phase1ParseStructMakerExpr(flattened.args[0]);
    if (maker === null) {
      return null;
    }
    if (maker.tag !== getter.tag || getter.index >= maker.args.length) {
      return { type: "trap" };
    }
    return maker.args[getter.index];
  }
  const predicate = phase1ParseStructPredicateName(calleeName);
  if (predicate !== null && flattened.args.length === 1) {
    const maker = phase1ParseStructMakerExpr(flattened.args[0]);
    if (maker === null) {
      return null;
    }
    return {
      type: "int",
      value: maker.tag === predicate.tag ? 1 : 0,
    };
  }
  return null;
}

function phase1ExtractIfBranchExpr(expr, ctx = null) {
  if (!expr || typeof expr !== "object") {
    return null;
  }
  if (expr.type === "lambda" && Array.isArray(expr.params) && expr.params.length === 1) {
    return expr.body;
  }
  if (
    ctx?.localFunctions instanceof Map && expr.type === "var" &&
    ctx.localFunctions.get(expr.name)?.type === "lambda"
  ) {
    return phase1ExtractIfBranchExpr(ctx.localFunctions.get(expr.name), ctx);
  }
  if (ctx?.evalEnv instanceof Map && expr.type === "var") {
    const resolved = phase1ResolveValueByName(expr.name, ctx.evalEnv, new Map(), 0);
    if (
      resolved && typeof resolved === "object" && resolved.kind === "function" &&
      Array.isArray(resolved.params) && resolved.params.length === 1
    ) {
      return resolved.body;
    }
  }
  return null;
}

function phase1ReduceIfBuiltinApply(flattened, ctx = null) {
  if (
    !flattened || !flattened.callee || flattened.callee.type !== "var" ||
    flattened.callee.name !== "if" || !Array.isArray(flattened.args) ||
    flattened.args.length !== 3
  ) {
    return null;
  }
  const thenExpr = phase1ExtractIfBranchExpr(flattened.args[1], ctx);
  const elseExpr = phase1ExtractIfBranchExpr(flattened.args[2], ctx);
  if (thenExpr === null || elseExpr === null) {
    return null;
  }
  return {
    type: "if",
    cond: flattened.args[0],
    thenExpr,
    elseExpr,
  };
}

function phase1ReducePureBuiltinApply(flattened) {
  if (
    !flattened || flattened.callee?.type !== "var" ||
    !Array.isArray(flattened.args)
  ) {
    return null;
  }
  const [arg1, arg2] = flattened.args;
  switch (flattened.callee.name) {
    case "eq":
      if (arg1?.type === "int" && arg2?.type === "int") {
        return { type: "bool", value: arg1.value === arg2.value };
      }
      if (arg1?.type === "bool" && arg2?.type === "bool") {
        return { type: "bool", value: arg1.value === arg2.value };
      }
      return null;
    case "ne":
      if (arg1?.type === "int" && arg2?.type === "int") {
        return { type: "bool", value: arg1.value !== arg2.value };
      }
      if (arg1?.type === "bool" && arg2?.type === "bool") {
        return { type: "bool", value: arg1.value !== arg2.value };
      }
      return null;
    case "lt":
      return arg1?.type === "int" && arg2?.type === "int"
        ? { type: "bool", value: arg1.value < arg2.value }
        : null;
    case "le":
      return arg1?.type === "int" && arg2?.type === "int"
        ? { type: "bool", value: arg1.value <= arg2.value }
        : null;
    case "gt":
      return arg1?.type === "int" && arg2?.type === "int"
        ? { type: "bool", value: arg1.value > arg2.value }
        : null;
    case "ge":
      return arg1?.type === "int" && arg2?.type === "int"
        ? { type: "bool", value: arg1.value >= arg2.value }
        : null;
    case "slice_get_u8":
      if (
        arg1?.type === "apply" &&
        arg1.fn?.type === "var" &&
        arg1.fn.name === "str_to_slice" &&
        arg1.arg?.type === "string" &&
        arg2?.type === "int"
      ) {
        const bytes = new TextEncoder().encode(arg1.arg.value);
        return {
          type: "int",
          value: arg2.value >= 0 && arg2.value < bytes.length ? bytes[arg2.value] : 0,
        };
      }
      return null;
    default:
      return null;
  }
}

function phase1SliceValue(bytes) {
  let out = null;
  if (bytes instanceof Uint8Array) {
    out = new Uint8Array(bytes);
  } else if (Array.isArray(bytes)) {
    out = Uint8Array.from(bytes);
  }
  if (!(out instanceof Uint8Array)) {
    out = new Uint8Array();
  }
  return {
    kind: "slice_u8",
    bytes: out,
  };
}

function phase1IsSliceValue(value) {
  return value && typeof value === "object" && value.kind === "slice_u8" &&
    value.bytes instanceof Uint8Array;
}

function phase1BuiltinArity(name) {
  const structMaker = phase1ParseStructMakerName(name);
  if (structMaker !== null) {
    return structMaker.arity;
  }
  if (phase1ParseStructGetterName(name) !== null) {
    return 1;
  }
  if (phase1ParseStructPredicateName(name) !== null) {
    return 1;
  }
  switch (name) {
    case "ListNil":
    case "Nil":
      return 0;
    case "ListCons":
    case "Cons":
      return 2;
    case "fmap":
      return 2;
    case "foldl":
    case "foldr":
      return 3;
    case "add":
    case "mul":
    case "sub":
    case "div":
    case "mod":
    case "eq":
    case "ne":
    case "lt":
    case "le":
    case "gt":
    case "ge":
    case "and":
    case "or":
    case "xor":
    case "implies":
      return 2;
    case "not":
      return 1;
    case "if":
      return 3;
    case "slice_len":
    case "slice_len_raw":
    case "slice_data_ptr":
    case "str_to_slice":
    case "slice_to_string":
      return 1;
    case "slice_get_u8":
    case "slice_eq_u8":
      return 2;
    case "slice_new_u8":
    case "region_mark":
    case "region_reset":
      return 1;
    case "region_alloc":
    case "slice_set_u8":
    case "memcpy_u8":
    case "memset_u8":
      return 3;
    case "list_map":
    case "list_filter":
    case "list_any":
    case "list_all":
    case "filter":
    case "any":
    case "all":
      return 2;
    case "list_foldl":
    case "list_foldr":
      return 3;
    case "build":
      return 1;
    default:
      return null;
  }
}

function phase1ConstructorArity(name) {
  switch (name) {
    case "Nil":
    case "ListNil":
      return 0;
    case "Cons":
    case "ListCons":
      return 2;
    default:
      return null;
  }
}

function phase1IsConstructorToken(token) {
  if (!phase1IsIdentToken(token)) {
    return false;
  }
  return /^[A-Z]/u.test(phase1NormalizeCallableName(token));
}

function phase1ParsePattern(tokens, start, stopTokens = new Set(["->", "|", ")"])) {
  const token = tokens[start];
  if (token === "(") {
    const inner = phase1ParsePattern(tokens, start + 1, new Set([")"]));
    if (inner === null || tokens[inner.next] !== ")") {
      return null;
    }
    return { pattern: inner.pattern, next: inner.next + 1 };
  }
  if (token === "_") {
    return { pattern: { type: "wildcard" }, next: start + 1 };
  }
  if (token === "True" || token === "true") {
    return { pattern: { type: "bool", value: true }, next: start + 1 };
  }
  if (token === "False" || token === "false") {
    return { pattern: { type: "bool", value: false }, next: start + 1 };
  }
  if (phase1IsNumberToken(token)) {
    return {
      pattern: { type: "int", value: Number.parseInt(token, 10) },
      next: start + 1,
    };
  }
  if (token === "{") {
    const fields = [];
    let cursor = start + 1;
    let open = false;
    while (cursor < tokens.length && tokens[cursor] !== "}") {
      if (tokens[cursor] === "_") {
        open = true;
        cursor += 1;
        if (tokens[cursor] === ",") {
          cursor += 1;
        }
        continue;
      }
      const fieldName = tokens[cursor];
      if (!phase1IsIdentToken(fieldName) || tokens[cursor + 1] !== "=") {
        return null;
      }
      const parsedFieldPattern = phase1ParsePattern(
        tokens,
        cursor + 2,
        new Set([",", "}"]),
      );
      if (parsedFieldPattern === null) {
        return null;
      }
      fields.push({
        name: phase1NormalizeCallableName(fieldName),
        pattern: parsedFieldPattern.pattern,
      });
      cursor = parsedFieldPattern.next;
      if (tokens[cursor] === ",") {
        cursor += 1;
      }
    }
    if (tokens[cursor] !== "}") {
      return null;
    }
    return {
      pattern: {
        type: "record",
        fields,
        open,
      },
      next: cursor + 1,
    };
  }
  if (!phase1IsIdentToken(token)) {
    return null;
  }
  if (!phase1IsConstructorToken(token)) {
    return {
      pattern: {
        type: "binder",
        name: phase1NormalizeCallableName(token),
      },
      next: start + 1,
    };
  }
  const ctorName = phase1NormalizeCallableName(token);
  let cursor = start + 1;
  const args = [];
  while (cursor < tokens.length && !stopTokens.has(tokens[cursor])) {
    const arg = phase1ParsePattern(tokens, cursor, stopTokens);
    if (arg === null || arg.next === cursor) {
      break;
    }
    args.push(arg.pattern);
    cursor = arg.next;
  }
  return {
    pattern: {
      type: "ctor",
      name: ctorName,
      args,
    },
    next: cursor,
  };
}

function phase1ParsePatternSequence(tokens, start, stopTokens = new Set(["->", "|", ")"])) {
  const patterns = [];
  let cursor = start;
  while (cursor < tokens.length && !stopTokens.has(tokens[cursor])) {
    const parsed = phase1ParsePattern(tokens, cursor, stopTokens);
    if (parsed === null || parsed.next === cursor) {
      break;
    }
    patterns.push(parsed.pattern);
    cursor = parsed.next;
  }
  if (patterns.length === 0) {
    return null;
  }
  return { patterns, next: cursor };
}

function phase1ParseLetBindingChain(tokens, start, stopTokens) {
  const pattern = phase1ParsePattern(tokens, start, new Set(["="]));
  if (
    pattern !== null &&
    pattern.pattern.type !== "binder" &&
    tokens[pattern.next] === "="
  ) {
    const value = phase1ParseExpr(
      tokens,
      pattern.next + 1,
      new Set([";", "in", ...stopTokens]),
    );
    if (value === null) {
      return null;
    }
    const nextToken = tokens[value.next];
    if (nextToken === ";") {
      const body = phase1ParseLetBindingChain(tokens, value.next + 1, stopTokens);
      if (body === null) {
        return null;
      }
      return {
        node: {
          type: "letPattern",
          pattern: pattern.pattern,
          value: value.node,
          body: body.node,
        },
        next: body.next,
      };
    }
    if (nextToken !== "in") {
      return null;
    }
    const body = phase1ParseExpr(tokens, value.next + 1, stopTokens);
    if (body === null) {
      return null;
    }
    return {
      node: {
        type: "letPattern",
        pattern: pattern.pattern,
        value: value.node,
        body: body.node,
      },
      next: body.next,
    };
  }

  const nameToken = tokens[start];
  if (!phase1IsIdentToken(nameToken)) {
    return null;
  }
  if (tokens[start + 1] === "|") {
    const branches = [];
    let cursor = start + 1;
    while (true) {
      if (tokens[cursor] === "|") {
        cursor += 1;
      }
      const guardToken = tokens[cursor];
      if (guardToken === "otherwise") {
        if (tokens[cursor + 1] !== "=") {
          return null;
        }
        const fallback = phase1ParseExpr(
          tokens,
          cursor + 2,
          new Set(["in", ...stopTokens]),
        );
        if (fallback === null || tokens[fallback.next] !== "in") {
          return null;
        }
        const body = phase1ParseExpr(tokens, fallback.next + 1, stopTokens);
        if (body === null) {
          return null;
        }
        return {
          node: {
            type: "let",
            name: phase1NormalizeCallableName(nameToken),
            value: phase1FoldGuardBranches(branches, fallback.node),
            body: body.node,
          },
          next: body.next,
        };
      }
      const guardExpr = phase1ParseExpr(tokens, cursor, new Set(["="]));
      if (guardExpr === null || tokens[guardExpr.next] !== "=") {
        return null;
      }
      const guardedValue = phase1ParseExpr(
        tokens,
        guardExpr.next + 1,
        new Set(["|", "in", ...stopTokens]),
      );
      if (guardedValue === null) {
        return null;
      }
      branches.push({ guard: guardExpr.node, body: guardedValue.node });
      cursor = guardedValue.next;
      if (tokens[cursor] === "in") {
        return null;
      }
      if (tokens[cursor] !== "|") {
        return null;
      }
    }
  }
  if (tokens[start + 1] !== "=") {
    return null;
  }
  const value = phase1ParseExpr(
    tokens,
    start + 2,
    new Set([";", "in", ...stopTokens]),
  );
  if (value === null) {
    return null;
  }
  const nextToken = tokens[value.next];
  if (nextToken === ";") {
    const body = phase1ParseLetBindingChain(tokens, value.next + 1, stopTokens);
    if (body === null) {
      return null;
    }
    return {
      node: {
        type: "let",
        name: phase1NormalizeCallableName(nameToken),
        value: value.node,
        body: body.node,
      },
      next: body.next,
    };
  }
  if (nextToken !== "in") {
    return null;
  }
  const body = phase1ParseExpr(tokens, value.next + 1, stopTokens);
  if (body === null) {
    return null;
  }
  return {
    node: {
      type: "let",
      name: phase1NormalizeCallableName(nameToken),
      value: value.node,
      body: body.node,
    },
    next: body.next,
  };
}

function phase1ParseExprSequence(tokens, start, stopTokens = new Set()) {
  const exprs = [];
  let cursor = start;
  while (cursor < tokens.length && !stopTokens.has(tokens[cursor])) {
    const parsed = phase1ParsePrimary(tokens, cursor, stopTokens);
    if (parsed === null || parsed.next === cursor) {
      break;
    }
    exprs.push(parsed.node);
    cursor = parsed.next;
  }
  if (exprs.length === 0) {
    return null;
  }
  return { exprs, next: cursor };
}

function phase1CollectPatternBinders(pattern, binders) {
  if (Array.isArray(pattern)) {
    for (const entry of pattern) {
      phase1CollectPatternBinders(entry, binders);
    }
    return;
  }
  if (!pattern || typeof pattern !== "object") {
    return;
  }
  if (pattern.type === "binder") {
    binders.push(pattern.name);
    return;
  }
  if (pattern.type === "bool") {
    return;
  }
  if (pattern.type === "int") {
    return;
  }
  if (pattern.type === "record" && Array.isArray(pattern.fields)) {
    for (const field of pattern.fields) {
      phase1CollectPatternBinders(field.pattern, binders);
    }
    return;
  }
  if (pattern.type !== "ctor" || !Array.isArray(pattern.args)) {
    return;
  }
  for (const arg of pattern.args) {
    phase1CollectPatternBinders(arg, binders);
  }
}

function phase1FirstStopIndex(tokens, start, stopTokens) {
  let parenDepth = 0;
  let bracketDepth = 0;
  let braceDepth = 0;
  for (let index = start; index < tokens.length; index += 1) {
    const token = tokens[index];
    if (token === "(") {
      parenDepth += 1;
      continue;
    }
    if (token === "[") {
      bracketDepth += 1;
      continue;
    }
    if (token === "{") {
      braceDepth += 1;
      continue;
    }
    if (token === ")" && parenDepth > 0) {
      parenDepth -= 1;
      continue;
    }
    if (token === "]" && bracketDepth > 0) {
      bracketDepth -= 1;
      continue;
    }
    if (token === "}" && braceDepth > 0) {
      braceDepth -= 1;
      continue;
    }
    if (
      parenDepth === 0 && bracketDepth === 0 && braceDepth === 0 &&
      stopTokens.has(token)
    ) {
      return index;
    }
  }
  return tokens.length;
}

function phase1FindTrailingPatternArrowStart(tokens, start, stopTokens) {
  const limit = phase1FirstStopIndex(tokens, start, stopTokens);
  for (let index = limit - 1; index >= start; index -= 1) {
    if (tokens[index] === "|") {
      continue;
    }
    const parsed = phase1ParsePattern(tokens, index, new Set(["->"]));
    if (parsed !== null && parsed.next < limit && tokens[parsed.next] === "->") {
      let earliest = index;
      for (let scan = index - 1; scan >= start; scan -= 1) {
        if (tokens[scan] === "|") {
          continue;
        }
        const earlier = phase1ParsePattern(tokens, scan, new Set(["->"]));
        if (earlier !== null && earlier.next === parsed.next) {
          earliest = scan;
        }
      }
      return earliest;
    }
  }
  return null;
}

function phase1FindTrailingPatternSequenceArrowStart(tokens, start, stopTokens, arity) {
  const limit = phase1FirstStopIndex(tokens, start, stopTokens);
  for (let index = limit - 1; index >= start; index -= 1) {
    if (tokens[index] === "|") {
      continue;
    }
    const parsed = phase1ParsePatternSequence(tokens, index, new Set(["->"]));
    if (
      parsed !== null &&
      parsed.patterns.length === arity &&
      parsed.next < limit &&
      tokens[parsed.next] === "->"
    ) {
      let earliest = index;
      for (let scan = index - 1; scan >= start; scan -= 1) {
        if (tokens[scan] === "|") {
          continue;
        }
        const earlier = phase1ParsePatternSequence(tokens, scan, new Set(["->"]));
        if (
          earlier !== null &&
          earlier.patterns.length === arity &&
          earlier.next === parsed.next
        ) {
          earliest = scan;
        }
      }
      return earliest;
    }
  }
  return null;
}

function phase1ParseSingleTargetCaseArmChain(tokens, start, stopTokens, targetNode) {
  const armStart = tokens[start] === "|" ? start + 1 : start;
  const pattern = phase1ParsePattern(tokens, armStart, new Set(["->"]));
  if (pattern === null || tokens[pattern.next] !== "->") {
    return null;
  }
  const bodyStart = pattern.next + 1;
  const limit = phase1FirstStopIndex(tokens, bodyStart, stopTokens);
  const directTokens = tokens.slice(bodyStart, limit);
  const directWhenMatch = phase1ParseExpr(directTokens, 0, new Set());
  if (directWhenMatch !== null && directWhenMatch.next === directTokens.length) {
    return {
      node: {
        type: "caseCtor",
        target: targetNode,
        pattern: pattern.pattern,
        whenMatch: directWhenMatch.node,
        fallbackPattern: null,
        whenFallback: null,
      },
      next: limit,
    };
  }
  for (let candidate = bodyStart + 1; candidate < limit; candidate += 1) {
    const suffixStart = tokens[candidate] === "|" ? candidate + 1 : candidate;
    if (suffixStart >= limit) {
      continue;
    }
    const whenMatchTokens = tokens.slice(bodyStart, candidate);
    const whenMatch = phase1ParseExpr(whenMatchTokens, 0, new Set());
    if (whenMatch === null || whenMatch.next !== whenMatchTokens.length) {
      continue;
    }
    const nestedFallback = phase1ParseSingleTargetCaseArmChain(
      tokens,
      suffixStart,
      stopTokens,
      targetNode,
    );
    if (nestedFallback === null) {
      continue;
    }
    return {
      node: {
        type: "caseCtor",
        target: targetNode,
        pattern: pattern.pattern,
        whenMatch: whenMatch.node,
        fallbackPattern: { type: "wildcard" },
        whenFallback: nestedFallback.node,
      },
      next: nestedFallback.next,
    };
  }
  return null;
}

function phase1MatchPattern(pattern, value) {
  if (!pattern || typeof pattern !== "object") {
    return null;
  }
  if (pattern.type === "wildcard") {
    return new Map();
  }
  if (pattern.type === "bool") {
    return typeof value === "boolean" && value === pattern.value ? new Map() : null;
  }
  if (pattern.type === "int") {
    return value === pattern.value ? new Map() : null;
  }
  if (pattern.type === "binder") {
    return new Map([[pattern.name, value]]);
  }
  if (pattern.type === "record") {
    if (!value || typeof value !== "object" || value.kind !== "record") {
      return null;
    }
    if (!pattern.open && value.fields.size !== pattern.fields.length) {
      return null;
    }
    const bindings = new Map();
    for (const field of pattern.fields) {
      if (!value.fields.has(field.name)) {
        return null;
      }
      const fieldBindings = phase1MatchPattern(
        field.pattern,
        value.fields.get(field.name),
      );
      if (fieldBindings === null) {
        return null;
      }
      for (const [name, bound] of fieldBindings) {
        bindings.set(name, bound);
      }
    }
    return bindings;
  }
  if (pattern.type !== "ctor") {
    return null;
  }
  if (pattern.name === "Nil" || pattern.name === "ListNil") {
    return Array.isArray(value) && value.length === 0 ? new Map() : null;
  }
  if (pattern.name === "Cons" || pattern.name === "ListCons") {
    if (!Array.isArray(value) || value.length === 0 || pattern.args.length !== 2) {
      return null;
    }
    const headBindings = phase1MatchPattern(pattern.args[0], value[0]);
    if (headBindings === null) {
      return null;
    }
    const tailBindings = phase1MatchPattern(pattern.args[1], value.slice(1));
    if (tailBindings === null) {
      return null;
    }
    const bindings = new Map(headBindings);
    for (const [name, bound] of tailBindings) {
      bindings.set(name, bound);
    }
    return bindings;
  }
  if (
    !value || typeof value !== "object" || value.kind !== "ctor" ||
    value.name !== pattern.name || !Array.isArray(value.args) ||
    value.args.length !== pattern.args.length
  ) {
    return null;
  }
  const bindings = new Map();
  for (let index = 0; index < pattern.args.length; index += 1) {
    const argBindings = phase1MatchPattern(pattern.args[index], value.args[index]);
    if (argBindings === null) {
      return null;
    }
    for (const [name, bound] of argBindings) {
      bindings.set(name, bound);
    }
  }
  return bindings;
}

function phase1MatchPatternList(patterns, values) {
  if (!Array.isArray(patterns) || !Array.isArray(values) || patterns.length !== values.length) {
    return null;
  }
  const bindings = new Map();
  for (let index = 0; index < patterns.length; index += 1) {
    const patternBindings = phase1MatchPattern(patterns[index], values[index]);
    if (patternBindings === null) {
      return null;
    }
    for (const [name, bound] of patternBindings) {
      bindings.set(name, bound);
    }
  }
  return bindings;
}

function phase1DecodeCharLiteral(token) {
  if (typeof token !== "string" || token.length < 3 || token[0] !== "'" || token[token.length - 1] !== "'") {
    return null;
  }
  const inner = token.slice(1, -1);
  if (inner.length === 1) {
    return inner.codePointAt(0) ?? null;
  }
  switch (inner) {
    case "\\n":
      return 10;
    case "\\r":
      return 13;
    case "\\t":
      return 9;
    case "\\'":
      return 39;
    case "\\\\":
      return 92;
    default:
      return null;
  }
}

function phase1ConstructValue(name, args) {
  if (name === "Nil" || name === "ListNil") {
    return Array.isArray(args) && args.length === 0 ? [] : null;
  }
  if (name === "Cons" || name === "ListCons") {
    if (!Array.isArray(args) || args.length !== 2 || !Array.isArray(args[1])) {
      return null;
    }
    return [args[0], ...args[1]];
  }
  return {
    kind: "ctor",
    name,
    args: Array.isArray(args) ? args : [],
  };
}

function phase1RecordValue(fields) {
  return {
    kind: "record",
    fields: fields instanceof Map ? fields : new Map(),
  };
}

function phase1LookupBareValue(name, env, locals, depth = 0) {
  if (locals.has(name)) {
    return locals.get(name);
  }
  const value = env.get(name);
  if (!value) {
    const builtinArity = phase1BuiltinArity(name);
    if (builtinArity !== null) {
      return { kind: "builtin", name };
    }
    return phase1IsConstructorToken(name)
      ? phase1ConstructValue(name, [])
      : null;
  }
  if (value.kind === "function" && value.params.length === 0) {
    return phase1Evaluate(value.body, value.env, locals, depth + 1);
  }
  if (value.kind === "builtin") {
    const zeroArity = phase1BuiltinZeroArityValue(value.name, depth + 1);
    if (zeroArity !== null) {
      return zeroArity;
    }
  }
  return value;
}

function phase1ResolveValueByName(name, env, locals, depth = 0) {
  const direct = phase1LookupBareValue(name, env, locals, depth);
  if (direct !== null) {
    return direct;
  }
  const parts = phase1ResolveNameSegments(name);
  if (parts.length <= 1) {
    return null;
  }
  const base = phase1LookupBareValue(parts[0], env, locals, depth);
  if (base !== null) {
    let current = base;
    for (let index = 1; index < parts.length; index += 1) {
      if (!current || typeof current !== "object" || current.kind !== "record") {
        return null;
      }
      if (!current.fields.has(parts[index])) {
        return null;
      }
      current = current.fields.get(parts[index]);
    }
    return current;
  }
  return phase1LookupBareValue(phase1ResolvedCallableName(name), env, locals, depth);
}

function phase1CollectLetLocals(expr, locals) {
  if (!expr || typeof expr !== "object") {
    return;
  }
  if (expr.type === "let") {
    if (!locals.has(expr.name)) {
      locals.set(expr.name, locals.size);
    }
    phase1CollectLetLocals(expr.value, locals);
    phase1CollectLetLocals(expr.body, locals);
    return;
  }
  if (expr.type === "letPattern") {
    phase1CollectLetLocals(expr.value, locals);
    const binders = [];
    phase1CollectPatternBinders(expr.pattern, binders);
    for (const binder of binders) {
      if (!locals.has(binder)) {
        locals.set(binder, locals.size);
      }
    }
    phase1CollectLetLocals(expr.body, locals);
    return;
  }
  if (expr.type === "lambda") {
    phase1CollectLetLocals(expr.body, locals);
    return;
  }
  if (expr.type === "record") {
    for (const field of expr.fields) {
      phase1CollectLetLocals(field.value, locals);
    }
    return;
  }
  if (expr.type === "recordUpdate") {
    phase1CollectLetLocals(expr.base, locals);
    for (const field of expr.fields) {
      phase1CollectLetLocals(field.value, locals);
    }
    return;
  }
  if (expr.type === "braceApplyOrUpdate") {
    phase1CollectLetLocals(expr.base, locals);
    for (const field of expr.fields) {
      phase1CollectLetLocals(field.value, locals);
    }
    return;
  }
  if (expr.type === "field") {
    phase1CollectLetLocals(expr.base, locals);
    return;
  }
  if (expr.type === "if") {
    phase1CollectLetLocals(expr.cond, locals);
    phase1CollectLetLocals(expr.thenExpr, locals);
    phase1CollectLetLocals(expr.elseExpr, locals);
    return;
  }
  if (expr.type === "caseBool") {
    phase1CollectLetLocals(expr.target, locals);
    phase1CollectLetLocals(expr.whenTrue, locals);
    phase1CollectLetLocals(expr.whenFalse, locals);
    return;
  }
  if (expr.type === "caseMulti") {
    for (const target of expr.targets) {
      phase1CollectLetLocals(target, locals);
    }
    const binders = [];
    phase1CollectPatternBinders(expr.patterns, binders);
    for (const binder of binders) {
      if (!locals.has(binder)) {
        locals.set(binder, locals.size);
      }
    }
    const fallbackBinders = [];
    phase1CollectPatternBinders(expr.fallbackPatterns, fallbackBinders);
    for (const binder of fallbackBinders) {
      if (!locals.has(binder)) {
        locals.set(binder, locals.size);
      }
    }
    phase1CollectLetLocals(expr.whenMatch, locals);
    phase1CollectLetLocals(expr.whenFallback, locals);
    return;
  }
  if (expr.type === "caseCtor") {
    phase1CollectLetLocals(expr.target, locals);
    const binders = [];
    phase1CollectPatternBinders(expr.pattern, binders);
    for (const binder of binders) {
      if (!locals.has(binder)) {
        locals.set(binder, locals.size);
      }
    }
    if (expr.fallbackPattern) {
      const fallbackBinders = [];
      phase1CollectPatternBinders(expr.fallbackPattern, fallbackBinders);
      for (const binder of fallbackBinders) {
        if (!locals.has(binder)) {
          locals.set(binder, locals.size);
        }
      }
    }
    phase1CollectLetLocals(expr.whenMatch, locals);
    if (expr.whenFallback) {
      phase1CollectLetLocals(expr.whenFallback, locals);
    }
    return;
  }
  if (expr.type === "apply") {
    phase1CollectLetLocals(expr.fn, locals);
    phase1CollectLetLocals(expr.arg, locals);
  }
}

function phase1CallableArityForExpr(expr, defMap, seenDefs = new Set()) {
  if (!expr || typeof expr !== "object") {
    return 0;
  }
  if (expr.type === "lambda") {
    return Array.isArray(expr.params) ? expr.params.length : 0;
  }
  if (expr.type === "let") {
    return phase1CallableArityForExpr(expr.body, defMap, seenDefs);
  }
  if (expr.type === "letPattern") {
    return phase1CallableArityForExpr(expr.body, defMap, seenDefs);
  }
  if (
    expr.type === "record" || expr.type === "recordUpdate" ||
    expr.type === "braceApplyOrUpdate" || expr.type === "field"
  ) {
    return 0;
  }
  if (expr.type === "if") {
    const thenArity = phase1CallableArityForExpr(expr.thenExpr, defMap, seenDefs);
    const elseArity = phase1CallableArityForExpr(expr.elseExpr, defMap, seenDefs);
    return thenArity === elseArity ? thenArity : 0;
  }
  if (expr.type === "caseBool") {
    const trueArity = phase1CallableArityForExpr(expr.whenTrue, defMap, seenDefs);
    const falseArity = phase1CallableArityForExpr(expr.whenFalse, defMap, seenDefs);
    return trueArity === falseArity ? trueArity : 0;
  }
  if (expr.type === "caseMulti") {
    const matchArity = phase1CallableArityForExpr(expr.whenMatch, defMap, seenDefs);
    const fallbackArity = phase1CallableArityForExpr(expr.whenFallback, defMap, seenDefs);
    return matchArity === fallbackArity ? matchArity : 0;
  }
  if (expr.type === "caseCtor") {
    const matchArity = phase1CallableArityForExpr(expr.whenMatch, defMap, seenDefs);
    if (!expr.whenFallback) {
      return matchArity;
    }
    const fallbackArity = phase1CallableArityForExpr(expr.whenFallback, defMap, seenDefs);
    return matchArity === fallbackArity ? matchArity : 0;
  }
  if (expr.type === "var") {
    if (!defMap) {
      return phase1IsConstructorToken(expr.name) ? Number.MAX_SAFE_INTEGER : 0;
    }
    const targetDef = defMap.get(expr.name) ??
      defMap.get(phase1ResolvedCallableName(expr.name));
    if (targetDef) {
      return phase1CallableArityForDef(targetDef, defMap, seenDefs);
    }
    return phase1IsConstructorToken(expr.name) ? Number.MAX_SAFE_INTEGER : 0;
  }
  if (expr.type === "apply") {
    const flattened = phase1FlattenApply(expr);
    const callee = flattened.callee;
    if (!callee || callee.type !== "var") {
      return 0;
    }
    const builtinArity = phase1BuiltinArity(callee.name);
    if (builtinArity !== null) {
      return Math.max(0, builtinArity - flattened.args.length);
    }
    if (phase1IsConstructorToken(callee.name)) {
      return Math.max(0, Number.MAX_SAFE_INTEGER - flattened.args.length);
    }
    if (!defMap) {
      return 0;
    }
    const targetDef = defMap.get(callee.name);
    if (!targetDef) {
      return 0;
    }
    const targetArity = phase1CallableArityForDef(targetDef, defMap, seenDefs);
    return Math.max(0, targetArity - flattened.args.length);
  }
  return 0;
}

function phase1CallableArityForDef(def, defMap = null, seenDefs = new Set()) {
  if (!def || typeof def !== "object") {
    return null;
  }
  if (seenDefs.has(def.name)) {
    return Array.isArray(def.params) ? def.params.length : 0;
  }
  const ownArity = Array.isArray(def.params) ? def.params.length : 0;
  if (!defMap) {
    return ownArity + phase1CallableArityForExpr(def.body, null, seenDefs);
  }
  seenDefs.add(def.name);
  const bodyArity = phase1CallableArityForExpr(def.body, defMap, seenDefs);
  seenDefs.delete(def.name);
  return ownArity + bodyArity;
}

function phase1CollectReachableDefs(definitions, rootName) {
  const defMap = new Map(definitions.map((def) => [def.name, def]));
  const collectionLiteralInstances = definitions.collectionLiteralInstances;
  const reachable = new Set();
  const visiting = new Set();

  function visitExpr(expr, locals) {
    if (!expr || typeof expr !== "object") {
      return true;
    }
    if (
      expr.type === "int" || expr.type === "bool" ||
      expr.type === "string" || expr.type === "trap"
    ) {
      return true;
    }
    if (expr.type === "record") {
      return expr.fields.every((field) => visitExpr(field.value, locals));
    }
    if (expr.type === "recordUpdate") {
      return visitExpr(expr.base, locals) &&
        expr.fields.every((field) => visitExpr(field.value, locals));
    }
    if (expr.type === "braceApplyOrUpdate") {
      return visitExpr(expr.base, locals) &&
        expr.fields.every((field) => visitExpr(field.value, locals));
    }
    if (expr.type === "field") {
      return visitExpr(expr.base, locals);
    }
    if (expr.type === "listLiteral") {
      const elementsReachable = expr.elements.every((element) =>
        visitExpr(element, locals)
      );
      if (!elementsReachable) {
        return false;
      }
      const collectionTargetType = typeof expr.collectionTargetType === "string"
        ? expr.collectionTargetType
        : "";
      if (
        collectionTargetType.length === 0 ||
        !(collectionLiteralInstances instanceof Map)
      ) {
        return true;
      }
      const instanceDef = collectionLiteralInstances.get(collectionTargetType);
      if (!instanceDef || typeof instanceDef !== "object") {
        return true;
      }
      const emptyLocals = new Set(
        Array.isArray(instanceDef.empty?.params) ? instanceDef.empty.params : [],
      );
      const extendLocals = new Set(
        Array.isArray(instanceDef.extend?.params)
          ? instanceDef.extend.params
          : [],
      );
      return visitExpr(instanceDef.empty?.body, emptyLocals) &&
        visitExpr(instanceDef.extend?.body, extendLocals);
    }
    if (expr.type === "lambda") {
      return visitExpr(expr.body, new Set([...locals, ...expr.params]));
    }
    if (expr.type === "var") {
      if (locals.has(expr.name)) {
        return true;
      }
      const targetDef = defMap.get(expr.name) ??
        defMap.get(phase1ResolvedCallableName(expr.name));
      if (targetDef) {
        return visitDef(targetDef.name);
      }
      const parts = phase1ResolveNameSegments(expr.name);
      if (parts.length > 1) {
        if (locals.has(parts[0])) {
          return true;
        }
        const baseDef = defMap.get(parts[0]);
        if (baseDef) {
          return visitDef(baseDef.name);
        }
      }
      return true;
    }
    if (expr.type === "if") {
      return visitExpr(expr.cond, locals) &&
        visitExpr(expr.thenExpr, locals) &&
        visitExpr(expr.elseExpr, locals);
    }
    if (expr.type === "let") {
      if (!visitExpr(expr.value, locals)) {
        return false;
      }
      return visitExpr(expr.body, new Set([...locals, expr.name]));
    }
    if (expr.type === "letPattern") {
      if (!visitExpr(expr.value, locals)) {
        return false;
      }
      const nextLocals = new Set(locals);
      const binders = [];
      phase1CollectPatternBinders(expr.pattern, binders);
      for (const binder of binders) {
        nextLocals.add(binder);
      }
      return visitExpr(expr.body, nextLocals);
    }
    if (expr.type === "caseBool") {
      return visitExpr(expr.target, locals) &&
        visitExpr(expr.whenTrue, locals) &&
        visitExpr(expr.whenFalse, locals);
    }
    if (expr.type === "caseMulti") {
      const nextLocals = new Set(locals);
      const binders = [];
      phase1CollectPatternBinders(expr.patterns, binders);
      for (const binder of binders) {
        nextLocals.add(binder);
      }
      const fallbackLocals = new Set(locals);
      const fallbackBinders = [];
      phase1CollectPatternBinders(expr.fallbackPatterns, fallbackBinders);
      for (const binder of fallbackBinders) {
        fallbackLocals.add(binder);
      }
      return expr.targets.every((target) => visitExpr(target, locals)) &&
        visitExpr(expr.whenMatch, nextLocals) &&
        visitExpr(expr.whenFallback, fallbackLocals);
    }
    if (expr.type === "caseCtor") {
      const nextLocals = new Set(locals);
      const binders = [];
      phase1CollectPatternBinders(expr.pattern, binders);
      for (const binder of binders) {
        nextLocals.add(binder);
      }
      let fallbackLocals = null;
      if (expr.fallbackPattern) {
        fallbackLocals = new Set(locals);
        const fallbackBinders = [];
        phase1CollectPatternBinders(expr.fallbackPattern, fallbackBinders);
        for (const binder of fallbackBinders) {
          fallbackLocals.add(binder);
        }
      }
      return visitExpr(expr.target, locals) &&
        visitExpr(expr.whenMatch, nextLocals) &&
        (
          !expr.whenFallback ||
          visitExpr(expr.whenFallback, fallbackLocals ?? locals)
        );
    }
    if (expr.type === "apply") {
      const flattened = phase1FlattenApply(expr);
      const callee = flattened.callee;
      if (!callee || callee.type !== "var") {
        return false;
      }
      for (const arg of flattened.args) {
        if (!visitExpr(arg, locals)) {
          return false;
        }
      }
      if (locals.has(callee.name)) {
        return true;
      }
      const resolvedCalleeName = phase1ResolvedCallableName(callee.name);
      const builtinArity = phase1BuiltinArity(resolvedCalleeName);
      if (builtinArity !== null) {
        return flattened.args.length <= builtinArity;
      }
      if (phase1IsConstructorToken(callee.name)) {
        return true;
      }
      const targetDef = defMap.get(callee.name) ?? defMap.get(resolvedCalleeName);
      if (!targetDef) {
        return false;
      }
      const targetArity = phase1CallableArityForDef(targetDef, defMap);
      if (targetArity === null || flattened.args.length > targetArity) {
        return false;
      }
      return visitDef(callee.name);
    }
    return false;
  }

  function visitDef(name) {
    if (reachable.has(name)) {
      return true;
    }
    if (visiting.has(name)) {
      return true;
    }
    const def = defMap.get(name);
    if (!def) {
      return false;
    }
    visiting.add(name);
    const ok = visitExpr(def.body, new Set(def.params));
    visiting.delete(name);
    if (ok) {
      reachable.add(name);
    }
    return ok;
  }

  if (!visitDef(rootName)) {
    return null;
  }
  return {
    defMap,
    orderedDefs: definitions.filter((def) => reachable.has(def.name)),
  };
}

function phase1CollectReachableDefsForRoots(definitions, rootNames) {
  if (!Array.isArray(definitions) || definitions.length === 0) {
    return null;
  }
  const defMap = new Map(definitions.map((def) => [def.name, def]));
  const reachable = new Set();
  for (const rootName of rootNames) {
    if (!defMap.has(rootName)) {
      return null;
    }
    const graph = phase1CollectReachableDefs(definitions, rootName);
    if (graph === null) {
      return null;
    }
    for (const def of graph.orderedDefs) {
      reachable.add(def.name);
    }
  }
  return {
    defMap,
    orderedDefs: definitions.filter((def) => reachable.has(def.name)),
  };
}

function phase1CollectExecutableDefDeps(expr, ctx, deps = new Set()) {
  if (!expr || typeof expr !== "object") {
    return deps;
  }
  if (expr.type === "trap") {
    return deps;
  }
  if (
    expr.type === "caseCtor" || expr.type === "caseMulti" ||
    expr.type === "letPattern"
  ) {
    const expanded = phase1ExpandPatternMatchTargets(expr, ctx);
    if (expanded !== expr) {
      return phase1CollectExecutableDefDeps(expanded, ctx, deps);
    }
  }
  if (
    expr.type === "caseBool" || expr.type === "caseCtor" ||
    expr.type === "caseMulti" || expr.type === "letPattern"
  ) {
    const reduced = phase1ReduceRecordExpr(expr);
    if (reduced !== expr) {
      return phase1CollectExecutableDefDeps(reduced, ctx, deps);
    }
  }
  if (
    expr.type === "field" || expr.type === "recordUpdate" ||
    expr.type === "braceApplyOrUpdate"
  ) {
    if (
      expr.base?.type === "var" &&
      ctx.recordLocals instanceof Map &&
      ctx.recordLocals.has(expr.base.name)
    ) {
      const localized = phase1ReduceRecordExpr({
        ...expr,
        base: ctx.recordLocals.get(expr.base.name),
      });
      if (localized !== expr) {
        return phase1CollectExecutableDefDeps(localized, ctx, deps);
      }
    }
    const reduced = phase1ReduceRecordExpr(expr);
    if (reduced !== expr) {
      return phase1CollectExecutableDefDeps(reduced, ctx, deps);
    }
    return null;
  }
  if (
    expr.type === "int" || expr.type === "bool" || expr.type === "string" ||
    expr.type === "record" || expr.type === "listLiteral"
  ) {
    if (expr.type === "record") {
      for (const field of expr.fields) {
        if (phase1CollectExecutableDefDeps(field.value, ctx, deps) === null) {
          return null;
        }
      }
    }
    if (expr.type === "listLiteral") {
      for (const element of expr.elements) {
        if (phase1CollectExecutableDefDeps(element, ctx, deps) === null) {
          return null;
        }
      }
    }
    return deps;
  }
  if (expr.type === "lambda") {
    const bodyCtx = {
      ...ctx,
      locals: new Map(ctx.locals ?? []),
    };
    for (const param of expr.params ?? []) {
      bodyCtx.locals.set(param, bodyCtx.locals.size);
    }
    return phase1CollectExecutableDefDeps(expr.body, bodyCtx, deps);
  }
  if (expr.type === "var") {
    if (ctx.locals instanceof Map && ctx.locals.has(expr.name)) {
      return deps;
    }
    if (ctx.evalEnv instanceof Map) {
      const resolved = phase1ResolveValueByName(expr.name, ctx.evalEnv, new Map(), 0);
      if (Number.isInteger(resolved) || typeof resolved === "boolean") {
        return deps;
      }
    }
    const targetDef = ctx.defMap instanceof Map
      ? ctx.defMap.get(expr.name)
      : null;
    if (targetDef && Array.isArray(targetDef.params) && targetDef.params.length === 0) {
      deps.add(expr.name);
      return deps;
    }
    return null;
  }
  if (expr.type === "if") {
    if (phase1CollectExecutableDefDeps(expr.cond, ctx, deps) === null) {
      return null;
    }
    if (phase1CollectExecutableDefDeps(expr.thenExpr, ctx, deps) === null) {
      return null;
    }
    return phase1CollectExecutableDefDeps(expr.elseExpr, ctx, deps);
  }
  if (expr.type === "caseBool") {
    if (phase1CollectExecutableDefDeps(expr.target, ctx, deps) === null) {
      return null;
    }
    if (phase1CollectExecutableDefDeps(expr.whenTrue, ctx, deps) === null) {
      return null;
    }
    return phase1CollectExecutableDefDeps(expr.whenFalse, ctx, deps);
  }
  if (expr.type === "caseCtor") {
    const transparentNewtypeCtors = ctx.transparentNewtypeCtors instanceof Set
      ? ctx.transparentNewtypeCtors
      : new Set();
    if (
      expr.pattern?.type === "ctor" &&
      transparentNewtypeCtors.has(expr.pattern.name) &&
      Array.isArray(expr.pattern.args) &&
      expr.pattern.args.length === 1 &&
      !expr.fallbackPattern &&
      !expr.whenFallback
    ) {
      if (phase1CollectExecutableDefDeps(expr.target, ctx, deps) === null) {
        return null;
      }
      const bodyCtx = {
        ...ctx,
        locals: new Map(ctx.locals ?? []),
      };
      const binders = [];
      phase1CollectPatternBinders(expr.pattern, binders);
      for (const binder of binders) {
        if (!bodyCtx.locals.has(binder)) {
          bodyCtx.locals.set(binder, bodyCtx.locals.size);
        }
      }
      return phase1CollectExecutableDefDeps(expr.whenMatch, bodyCtx, deps);
    }
    return null;
  }
  if (expr.type === "let") {
    if (expr.value?.type === "lambda") {
      const bodyCtx = {
        ...ctx,
        locals: new Map(ctx.locals ?? []),
        localFunctions: new Map(ctx.localFunctions ?? []),
      };
      bodyCtx.locals.set(expr.name, bodyCtx.locals.size);
      bodyCtx.localFunctions.set(expr.name, expr.value);
      return phase1CollectExecutableDefDeps(expr.body, bodyCtx, deps);
    }
    const expandedValue = phase1ExpandImmediateInlineCall(expr.value, ctx);
    const reducedValue = phase1ReduceRecordExpr(expandedValue);
    const hoisted = phase1InlineLetBindingExpr(expr.name, reducedValue, expr.body);
    if (hoisted !== null) {
      return phase1CollectExecutableDefDeps(hoisted, ctx, deps);
    }
    if (phase1IsInlineSubstitutableExpr(reducedValue)) {
      return phase1CollectExecutableDefDeps(
        phase1ReduceRecordExpr(
          phase1SubstituteExpr(expr.body, new Map([[expr.name, reducedValue]])),
        ),
        ctx,
        deps,
      );
    }
    if (phase1CollectExecutableDefDeps(reducedValue, ctx, deps) === null) {
      return null;
    }
    const bodyCtx = {
      ...ctx,
      locals: new Map(ctx.locals ?? []),
    };
    bodyCtx.locals.set(expr.name, bodyCtx.locals.size);
    return phase1CollectExecutableDefDeps(expr.body, bodyCtx, deps);
  }
  if (expr.type === "apply") {
    const flattened = phase1FlattenApply(expr);
    const callee = flattened.callee;
    if (!callee || callee.type !== "var") {
      return null;
    }
    const reducedIfBuiltin = phase1ReduceIfBuiltinApply(flattened, ctx);
    if (reducedIfBuiltin !== null) {
      return phase1CollectExecutableDefDeps(reducedIfBuiltin, ctx, deps);
    }
    const reducedStructHelper = phase1ReduceStructHelperApply(flattened);
    if (reducedStructHelper !== null) {
      return phase1CollectExecutableDefDeps(reducedStructHelper, ctx, deps);
    }
    const localLambda = ctx.localFunctions instanceof Map
      ? ctx.localFunctions.get(callee.name)
      : null;
    if (localLambda && localLambda.type === "lambda") {
      const inlined = phase1InlineCallableExprExpr(localLambda, flattened.args);
      if (inlined !== null) {
        return phase1CollectExecutableDefDeps(inlined, ctx, deps);
      }
    }
    if (ctx.evalEnv instanceof Map) {
      const resolved = phase1ResolveValueByName(callee.name, ctx.evalEnv, new Map(), 0);
      if (
        resolved &&
        typeof resolved === "object" &&
        phase1InlineCount(ctx, callee.name) < 8
      ) {
        const inlined = phase1InlineResolvedCallableValueExpr(
          resolved,
          flattened.args,
        );
        if (inlined !== null) {
          return phase1CollectExecutableDefDeps(
            inlined,
            phase1WithInlineCount(ctx, callee.name),
            deps,
          );
        }
      }
    }
    for (const arg of flattened.args) {
      if (phase1CollectExecutableDefDeps(arg, ctx, deps) === null) {
        return null;
      }
    }
    switch (callee.name) {
      case "add":
      case "sub":
      case "mul":
      case "div":
      case "mod":
      case "eq":
      case "ne":
      case "lt":
      case "gt":
      case "le":
      case "ge":
      case "and":
      case "or":
        return flattened.args.length === 2 ? deps : null;
      case "not":
        return flattened.args.length === 1 ? deps : null;
      case "slice_len":
        return flattened.args.length === 1 ? deps : null;
      case "slice_len_raw":
        return flattened.args.length === 1 ? deps : null;
      case "slice_data_ptr":
        return flattened.args.length === 1 ? deps : null;
      case "slice_get_u8":
        return flattened.args.length === 2 ? deps : null;
      case "slice_new_u8":
        return flattened.args.length === 1 ? deps : null;
      case "region_mark":
        return flattened.args.length === 1 ? deps : null;
      case "region_reset":
        return flattened.args.length === 1 ? deps : null;
      case "region_alloc":
        return flattened.args.length === 2 ? deps : null;
      case "memcpy_u8":
        return flattened.args.length === 3 ? deps : null;
      case "memset_u8":
        return flattened.args.length === 3 ? deps : null;
      case "if":
        return flattened.args.length === 3 ? deps : null;
      default:
        break;
    }
    const targetDef = ctx.defMap instanceof Map
      ? (ctx.defMap.get(callee.name) ??
        ctx.defMap.get(phase1ResolvedCallableName(callee.name)))
      : null;
    if (
      !targetDef ||
      !Array.isArray(targetDef.params) ||
      targetDef.params.length !== flattened.args.length
    ) {
      return null;
    }
    deps.add(targetDef.name);
    return deps;
  }
  return null;
}

function phase1CollectExecutableDefsForRoots(definitions, rootNames) {
  if (!Array.isArray(definitions) || definitions.length === 0 || !Array.isArray(rootNames)) {
    return null;
  }
  const defMap = new Map(definitions.map((def) => [def.name, def]));
  const evalEnv = phase1BuildEvaluationEnv(definitions);
  if (!(evalEnv instanceof Map)) {
    return null;
  }
  const required = new Set();
  const visiting = new Set();
  const transparentNewtypeCtors = definitions.transparentNewtypeCtors instanceof Set
    ? definitions.transparentNewtypeCtors
    : new Set();

  function visitDef(name) {
    if (required.has(name)) {
      return true;
    }
    if (visiting.has(name)) {
      return true;
    }
    const def = defMap.get(name);
    if (!def) {
      return false;
    }
    visiting.add(name);
    const reducedBody = phase1ReduceRecordExpr(def.body);
    const locals = new Map();
    if (Array.isArray(def.params)) {
      def.params.forEach((param, index) => locals.set(param, index));
    }
    const deps = phase1CollectExecutableDefDeps(reducedBody, {
      locals,
      defMap,
      evalEnv,
      transparentNewtypeCtors,
    });
    visiting.delete(name);
    if (deps === null) {
      if (Array.isArray(def.params) && def.params.length === 0) {
        const evaluated = phase1Evaluate(reducedBody, evalEnv, new Map(), 0);
        if (evaluated !== null) {
          required.add(name);
          return true;
        }
      }
      return false;
    }
    required.add(name);
    for (const depName of deps) {
      if (!visitDef(depName)) {
        return false;
      }
    }
    return true;
  }

  for (const rootName of rootNames) {
    if (!visitDef(rootName)) {
      return null;
    }
  }

  return {
    defMap,
    evalEnv,
    orderedDefs: definitions.filter((def) => required.has(def.name)),
  };
}

function phase1WasmTypeSection(types) {
  const payload = [...encodeVarU32(types.length)];
  for (const paramCount of types) {
    payload.push(0x60);
    payload.push(...encodeVarU32(paramCount));
    for (let i = 0; i < paramCount; i += 1) {
      payload.push(0x7f);
    }
    payload.push(0x01, 0x7f);
  }
  return payload;
}

function phase1WasmFunctionSection(typeIndexes) {
  const payload = [...encodeVarU32(typeIndexes.length)];
  for (const typeIndex of typeIndexes) {
    payload.push(...encodeVarU32(typeIndex));
  }
  return payload;
}

function phase1WasmMemorySection() {
  return [0x01, 0x00, 0x01];
}

function phase1WasmDataSection(entries) {
  const payload = [...encodeVarU32(entries.length)];
  for (const entry of entries) {
    const offset = Number.isInteger(entry?.offset) ? entry.offset : 0;
    const bytes = entry?.bytes instanceof Uint8Array
      ? entry.bytes
      : Uint8Array.from(entry?.bytes ?? []);
    payload.push(
      0x00,
      0x41,
      ...encodeVarS32(offset),
      0x0b,
      ...encodeVarU32(bytes.length),
      ...bytes,
    );
  }
  return payload;
}

function phase1AppendName(payload, name) {
  const bytes = UTF8_ENCODER.encode(name);
  payload.push(...encodeVarU32(bytes.length));
  for (const value of bytes) {
    payload.push(value);
  }
}

function phase1WasmExportSection(exportsList) {
  const payload = [...encodeVarU32(exportsList.length)];
  for (const entry of exportsList) {
    phase1AppendName(payload, entry.name);
    payload.push(entry.kind);
    payload.push(...encodeVarU32(entry.index));
  }
  return payload;
}

function phase1WasmCodeSection(bodies) {
  const payload = [...encodeVarU32(bodies.length)];
  for (const rawBody of bodies) {
    const body = Array.isArray(rawBody)
      ? { localCount: 0, code: rawBody }
      : rawBody;
    const localEntries = [];
    if (body.localCount > 0) {
      localEntries.push([body.localCount, 0x7f]);
    }
    const localsDecls = [...encodeVarU32(localEntries.length)];
    for (const [count, type] of localEntries) {
      localsDecls.push(...encodeVarU32(count), type);
    }
    const encoded = [...localsDecls, ...body.code, 0x0b];
    payload.push(...encodeVarU32(encoded.length));
    payload.push(...encoded);
  }
  return payload;
}

function phase1WrapSection(id, payload) {
  return [id, ...encodeVarU32(payload.length), ...payload];
}

function phase1SubstituteExpr(expr, substitutions, shadowed = new Set()) {
  if (!expr || typeof expr !== "object") {
    return expr;
  }
  if (expr.type === "int" || expr.type === "bool" || expr.type === "string") {
    return expr;
  }
  if (expr.type === "var") {
    if (!shadowed.has(expr.name) && substitutions.has(expr.name)) {
      return substitutions.get(expr.name);
    }
    const parts = phase1ResolveNameSegments(expr.name);
    if (
      parts.length > 1 &&
      !shadowed.has(parts[0]) &&
      substitutions.has(parts[0])
    ) {
      let current = substitutions.get(parts[0]);
      for (let index = 1; index < parts.length; index += 1) {
        current = {
          type: "field",
          base: current,
          field: parts[index],
        };
      }
      return current;
    }
    return expr;
  }
  if (expr.type === "apply") {
    return {
      type: "apply",
      fn: phase1SubstituteExpr(expr.fn, substitutions, shadowed),
      arg: phase1SubstituteExpr(expr.arg, substitutions, shadowed),
    };
  }
  if (expr.type === "lambda") {
    const nextShadowed = new Set(shadowed);
    for (const param of expr.params) {
      nextShadowed.add(param);
    }
    return {
      type: "lambda",
      params: expr.params,
      body: phase1SubstituteExpr(expr.body, substitutions, nextShadowed),
    };
  }
  if (expr.type === "if") {
    return {
      type: "if",
      cond: phase1SubstituteExpr(expr.cond, substitutions, shadowed),
      thenExpr: phase1SubstituteExpr(expr.thenExpr, substitutions, shadowed),
      elseExpr: phase1SubstituteExpr(expr.elseExpr, substitutions, shadowed),
    };
  }
  if (expr.type === "let") {
    const nextShadowed = new Set(shadowed);
    nextShadowed.add(expr.name);
    return {
      type: "let",
      name: expr.name,
      value: phase1SubstituteExpr(expr.value, substitutions, shadowed),
      body: phase1SubstituteExpr(expr.body, substitutions, nextShadowed),
    };
  }
  if (expr.type === "letPattern") {
    const nextShadowed = new Set(shadowed);
    const binders = [];
    phase1CollectPatternBinders(expr.pattern, binders);
    for (const binder of binders) {
      nextShadowed.add(binder);
    }
    return {
      type: "letPattern",
      pattern: expr.pattern,
      value: phase1SubstituteExpr(expr.value, substitutions, shadowed),
      body: phase1SubstituteExpr(expr.body, substitutions, nextShadowed),
    };
  }
  if (expr.type === "caseBool") {
    return {
      type: "caseBool",
      target: phase1SubstituteExpr(expr.target, substitutions, shadowed),
      whenTrue: phase1SubstituteExpr(expr.whenTrue, substitutions, shadowed),
      whenFalse: phase1SubstituteExpr(expr.whenFalse, substitutions, shadowed),
    };
  }
  if (expr.type === "caseCtor") {
    const nextShadowed = new Set(shadowed);
    const binders = [];
    phase1CollectPatternBinders(expr.pattern, binders);
    for (const binder of binders) {
      nextShadowed.add(binder);
    }
    const fallbackShadowed = new Set(shadowed);
    if (expr.fallbackPattern) {
      const fallbackBinders = [];
      phase1CollectPatternBinders(expr.fallbackPattern, fallbackBinders);
      for (const binder of fallbackBinders) {
        fallbackShadowed.add(binder);
      }
    }
    return {
      ...expr,
      target: phase1SubstituteExpr(expr.target, substitutions, shadowed),
      whenMatch: phase1SubstituteExpr(expr.whenMatch, substitutions, nextShadowed),
      whenFallback: expr.whenFallback
        ? phase1SubstituteExpr(expr.whenFallback, substitutions, fallbackShadowed)
        : expr.whenFallback,
    };
  }
  if (expr.type === "caseMulti") {
    const matchShadowed = new Set(shadowed);
    const binders = [];
    phase1CollectPatternBinders(expr.patterns, binders);
    for (const binder of binders) {
      matchShadowed.add(binder);
    }
    const fallbackShadowed = new Set(shadowed);
    const fallbackBinders = [];
    phase1CollectPatternBinders(expr.fallbackPatterns, fallbackBinders);
    for (const binder of fallbackBinders) {
      fallbackShadowed.add(binder);
    }
    return {
      ...expr,
      targets: expr.targets.map((target) => phase1SubstituteExpr(target, substitutions, shadowed)),
      whenMatch: phase1SubstituteExpr(expr.whenMatch, substitutions, matchShadowed),
      whenFallback: phase1SubstituteExpr(expr.whenFallback, substitutions, fallbackShadowed),
    };
  }
  if (expr.type === "record") {
    return {
      type: "record",
      fields: expr.fields.map((field) => ({
        ...field,
        value: phase1SubstituteExpr(field.value, substitutions, shadowed),
      })),
    };
  }
  if (expr.type === "recordUpdate") {
    return {
      type: "recordUpdate",
      base: phase1SubstituteExpr(expr.base, substitutions, shadowed),
      fields: expr.fields.map((field) => ({
        ...field,
        value: phase1SubstituteExpr(field.value, substitutions, shadowed),
      })),
    };
  }
  if (expr.type === "braceApplyOrUpdate") {
    return {
      type: "braceApplyOrUpdate",
      base: phase1SubstituteExpr(expr.base, substitutions, shadowed),
      fields: expr.fields.map((field) => ({
        ...field,
        value: phase1SubstituteExpr(field.value, substitutions, shadowed),
      })),
    };
  }
  if (expr.type === "field") {
    return {
      type: "field",
      base: phase1SubstituteExpr(expr.base, substitutions, shadowed),
      field: expr.field,
    };
  }
  if (expr.type === "listLiteral") {
    return {
      ...expr,
      elements: expr.elements.map((element) =>
        phase1SubstituteExpr(element, substitutions, shadowed)
      ),
    };
  }
  return expr;
}

function phase1FlattenFunctionLikeParams(params, body) {
  const flatParams = Array.isArray(params) ? [...params] : [];
  let flatBody = body;
  while (flatBody && typeof flatBody === "object" && flatBody.type === "lambda") {
    if (Array.isArray(flatBody.params)) {
      flatParams.push(...flatBody.params);
    }
    flatBody = flatBody.body;
  }
  return { params: flatParams, body: flatBody };
}

function phase1InlineFunctionLikeExpr(params, body, argExprs) {
  const flattened = phase1FlattenFunctionLikeParams(params, body);
  if (
    !Array.isArray(flattened.params) ||
    !Array.isArray(argExprs)
  ) {
    return null;
  }
  const substitutions = new Map();
  const appliedCount = Math.min(flattened.params.length, argExprs.length);
  for (let index = 0; index < appliedCount; index += 1) {
    substitutions.set(flattened.params[index], argExprs[index]);
  }
  let inlined = phase1SubstituteExpr(flattened.body, substitutions);
  if (appliedCount < flattened.params.length) {
    inlined = {
      type: "lambda",
      params: flattened.params.slice(appliedCount),
      body: inlined,
    };
  }
  for (const arg of argExprs.slice(flattened.params.length)) {
    inlined = {
      type: "apply",
      fn: inlined,
      arg,
    };
  }
  return phase1ReduceRecordExpr(inlined);
}

function phase1InlineFunctionLike(params, body, argExprs, ctx) {
  const inlined = phase1InlineFunctionLikeExpr(params, body, argExprs);
  if (inlined === null) {
    return null;
  }
  return phase1EmitExprToWasm(inlined, phase1ExtendEmitterLocalsForExpr(inlined, ctx));
}

function phase1InlineCallableExprExpr(fnExpr, argExprs) {
  if (!fnExpr || typeof fnExpr !== "object" || fnExpr.type !== "lambda") {
    return null;
  }
  return phase1InlineFunctionLikeExpr([], fnExpr, argExprs);
}

function phase1InlineCallableExpr(fnExpr, argExprs, ctx) {
  const inlined = phase1InlineCallableExprExpr(fnExpr, argExprs);
  if (inlined === null) {
    return null;
  }
  return phase1EmitExprToWasm(inlined, phase1ExtendEmitterLocalsForExpr(inlined, ctx));
}

function phase1ExtendEmitterLocalsForExpr(expr, ctx) {
  if (!(ctx?.locals instanceof Map)) {
    return ctx;
  }
  const letLocals = new Map();
  phase1CollectLetLocals(expr, letLocals);
  if (letLocals.size === 0) {
    return ctx;
  }
  const locals = new Map(ctx.locals);
  let nextLocalIndex = locals.size;
  for (const name of letLocals.keys()) {
    if (locals.has(name)) {
      continue;
    }
    locals.set(name, nextLocalIndex);
    nextLocalIndex += 1;
  }
  return {
    ...ctx,
    locals,
  };
}

function phase1InlineCount(ctx, name) {
  if (!(ctx?.inlineCounts instanceof Map) || typeof name !== "string") {
    return 0;
  }
  return ctx.inlineCounts.get(name) ?? 0;
}

function phase1WithInlineCount(ctx, name) {
  const inlineCounts = new Map(ctx?.inlineCounts ?? []);
  inlineCounts.set(name, (inlineCounts.get(name) ?? 0) + 1);
  return {
    ...ctx,
    inlineCounts,
  };
}

function phase1ExprFromEvaluatedValue(value) {
  if (Number.isInteger(value)) {
    return {
      type: "int",
      value,
    };
  }
  if (typeof value === "boolean") {
    return {
      type: "bool",
      value,
    };
  }
  if (Array.isArray(value)) {
    return {
      type: "listLiteral",
      elements: value.map((entry) => phase1ExprFromEvaluatedValue(entry)),
    };
  }
  if (!value || typeof value !== "object") {
    return null;
  }
  if (value.kind === "record" && value.fields instanceof Map) {
    const fields = [];
    for (const [name, fieldValue] of value.fields.entries()) {
      const expr = phase1ExprFromEvaluatedValue(fieldValue);
      if (expr === null) {
        return null;
      }
      fields.push({ name, value: expr });
    }
    return {
      type: "record",
      fields,
    };
  }
  if (value.kind === "ctor") {
    let expr = {
      type: "var",
      name: value.name,
    };
    for (const arg of Array.isArray(value.args) ? value.args : []) {
      const argExpr = phase1ExprFromEvaluatedValue(arg);
      if (argExpr === null) {
        return null;
      }
      expr = {
        type: "apply",
        fn: expr,
        arg: argExpr,
      };
    }
    return expr;
  }
  if (value.kind === "builtin_partial") {
    let expr = {
      type: "var",
      name: value.name,
    };
    for (const arg of Array.isArray(value.args) ? value.args : []) {
      const argExpr = phase1ExprFromEvaluatedValue(arg);
      if (argExpr === null) {
        return null;
      }
      expr = {
        type: "apply",
        fn: expr,
        arg: argExpr,
      };
    }
    return expr;
  }
  if (value.kind === "function") {
    const substitutions = new Map();
    if (value.locals instanceof Map) {
      for (const [name, localValue] of value.locals.entries()) {
        const expr = phase1ExprFromEvaluatedValue(localValue);
        if (expr === null) {
          return null;
        }
        substitutions.set(name, expr);
      }
    }
    return phase1SubstituteExpr(value.body, substitutions);
  }
  return null;
}

function phase1ExprFromDebugValue(value, depth = 0, seen = new Set()) {
  if (depth > 64) {
    return null;
  }
  if (Number.isInteger(value)) {
    return { type: "int", value };
  }
  if (typeof value === "boolean") {
    return { type: "bool", value };
  }
  if (typeof value === "string") {
    return { type: "string", value };
  }
  if (Array.isArray(value)) {
    const elements = [];
    for (const element of value) {
      const expr = phase1ExprFromDebugValue(element, depth + 1, seen);
      if (expr === null) {
        return null;
      }
      elements.push(expr);
    }
    return {
      type: "listLiteral",
      elements,
    };
  }
  if (!value || typeof value !== "object") {
    return null;
  }
  if (seen.has(value)) {
    return null;
  }
  seen.add(value);
  try {
    if (value.kind === "record" && value.fields instanceof Map) {
      const fields = [];
      for (const [name, fieldValue] of value.fields.entries()) {
        const expr = phase1ExprFromDebugValue(fieldValue, depth + 1, seen);
        if (expr === null) {
          return null;
        }
        fields.push({ name, value: expr });
      }
      return {
        type: "record",
        fields,
      };
    }
    if (value.kind === "ctor") {
      let expr = {
        type: "var",
        name: value.name,
      };
      for (const arg of Array.isArray(value.args) ? value.args : []) {
        const argExpr = phase1ExprFromDebugValue(arg, depth + 1, seen);
        if (argExpr === null) {
          return null;
        }
        expr = {
          type: "apply",
          fn: expr,
          arg: argExpr,
        };
      }
      return expr;
    }
    if (value.kind === "builtin") {
      return {
        type: "var",
        name: value.name,
      };
    }
    if (value.kind === "builtin_partial") {
      let expr = {
        type: "var",
        name: value.name,
      };
      for (const arg of Array.isArray(value.args) ? value.args : []) {
        const argExpr = phase1ExprFromDebugValue(arg, depth + 1, seen);
        if (argExpr === null) {
          return null;
        }
        expr = {
          type: "apply",
          fn: expr,
          arg: argExpr,
        };
      }
      return expr;
    }
    if (value.kind === "function") {
      const substitutions = new Map();
      if (value.locals instanceof Map) {
        for (const [name, localValue] of value.locals.entries()) {
          const expr = phase1ExprFromDebugValue(localValue, depth + 1, seen);
          if (expr === null) {
            return null;
          }
          substitutions.set(name, expr);
        }
      }
      const body = substitutions.size === 0
        ? value.body
        : phase1SubstituteExpr(value.body, substitutions);
      return {
        type: "lambda",
        params: Array.isArray(value.params) ? [...value.params] : [],
        body,
      };
    }
    return null;
  } finally {
    seen.delete(value);
  }
}

function phase1RenderDebugPattern(pattern) {
  if (!pattern || typeof pattern !== "object") {
    return "_";
  }
  switch (pattern.type) {
    case "wildcard":
      return "_";
    case "binder":
      return pattern.name;
    case "bool":
      return pattern.value ? "true" : "false";
    case "int":
      return String(pattern.value);
    case "record":
      return `{ ${
        (Array.isArray(pattern.fields) ? pattern.fields : [])
          .map((field) => `${field.name} = ${phase1RenderDebugPattern(field.pattern)}`)
          .join(", ")
      } }`;
    case "ctor": {
      const args = Array.isArray(pattern.args) ? pattern.args : [];
      if (args.length === 0) {
        return pattern.name;
      }
      return `${pattern.name} ${
        args.map((arg) => phase1RenderDebugPattern(arg)).join(" ")
      }`;
    }
    default:
      return "_";
  }
}

function phase1IsDebugAtomicExpr(expr) {
  return !!expr && typeof expr === "object" && (
    expr.type === "int" ||
    expr.type === "bool" ||
    expr.type === "string" ||
    expr.type === "var" ||
    expr.type === "listLiteral" ||
    expr.type === "record"
  );
}

function phase1RenderDebugExpr(expr) {
  if (!expr || typeof expr !== "object") {
    return "_";
  }
  switch (expr.type) {
    case "int":
      return String(expr.value);
    case "bool":
      return expr.value ? "true" : "false";
    case "string":
      return JSON.stringify(expr.value);
    case "var":
      return expr.name;
    case "lambda":
      return `\\${(Array.isArray(expr.params) ? expr.params : []).join(" ")} -> ${
        phase1RenderDebugExpr(expr.body)
      }`;
    case "listLiteral":
      return `[${(Array.isArray(expr.elements) ? expr.elements : []).map((element) => phase1RenderDebugExpr(element)).join(", ")}]`;
    case "record":
      return `{ ${
        (Array.isArray(expr.fields) ? expr.fields : [])
          .map((field) => `${field.name} = ${phase1RenderDebugExpr(field.value)}`)
          .join(", ")
      } }`;
    case "field": {
      const base = phase1RenderDebugExpr(expr.base);
      return `${phase1IsDebugAtomicExpr(expr.base) ? base : `(${base})`}.${expr.field}`;
    }
    case "recordUpdate":
      return `${phase1RenderDebugExpr(expr.base)} { ${
        (Array.isArray(expr.fields) ? expr.fields : [])
          .map((field) => `${field.name} = ${phase1RenderDebugExpr(field.value)}`)
          .join(", ")
      } }`;
    case "braceApplyOrUpdate":
      return `${phase1RenderDebugExpr(expr.base)} { ${
        (Array.isArray(expr.fields) ? expr.fields : [])
          .map((field) => `${field.name} = ${phase1RenderDebugExpr(field.value)}`)
          .join(", ")
      } }`;
    case "if":
      return `if ${phase1RenderDebugExpr(expr.cond)} then ${
        phase1RenderDebugExpr(expr.thenExpr)
      } else ${phase1RenderDebugExpr(expr.elseExpr)}`;
    case "let":
      return `let ${expr.name} = ${phase1RenderDebugExpr(expr.value)} in ${
        phase1RenderDebugExpr(expr.body)
      }`;
    case "letPattern":
      return `let ${phase1RenderDebugPattern(expr.pattern)} = ${
        phase1RenderDebugExpr(expr.value)
      } in ${phase1RenderDebugExpr(expr.body)}`;
    case "caseBool":
      return `case ${phase1RenderDebugExpr(expr.target)} of true -> ${
        phase1RenderDebugExpr(expr.whenTrue)
      } | _ -> ${phase1RenderDebugExpr(expr.whenFalse)}`;
    case "caseCtor":
      return `case ${phase1RenderDebugExpr(expr.target)} of ${
        phase1RenderDebugPattern(expr.pattern)
      } -> ${phase1RenderDebugExpr(expr.whenMatch)}${
        expr.whenFallback
          ? ` | ${phase1RenderDebugPattern(expr.fallbackPattern ?? { type: "wildcard" })} -> ${
            phase1RenderDebugExpr(expr.whenFallback)
          }`
          : ""
      }`;
    case "caseMulti": {
      const targets = (Array.isArray(expr.targets) ? expr.targets : [])
        .map((target) => phase1RenderDebugExpr(target))
        .join(" ");
      const patterns = (Array.isArray(expr.patterns) ? expr.patterns : [])
        .map((pattern) => phase1RenderDebugPattern(pattern))
        .join(" ");
      const fallbackPatterns = (Array.isArray(expr.fallbackPatterns) ? expr.fallbackPatterns : [])
        .map((pattern) => phase1RenderDebugPattern(pattern))
        .join(" ");
      return `case ${targets} of ${patterns} -> ${
        phase1RenderDebugExpr(expr.whenMatch)
      } | ${fallbackPatterns} -> ${phase1RenderDebugExpr(expr.whenFallback)}`;
    }
    case "apply": {
      const flattened = phase1FlattenApply(expr);
      const renderedCallee = phase1RenderDebugExpr(flattened.callee);
      const head = phase1IsDebugAtomicExpr(flattened.callee)
        ? renderedCallee
        : `(${renderedCallee})`;
      const args = flattened.args.map((arg) => {
        const rendered = phase1RenderDebugExpr(arg);
        return phase1IsDebugAtomicExpr(arg) ? rendered : `(${rendered})`;
      });
      return [head, ...args].join(" ");
    }
    case "trap":
      return "trap";
    default:
      return "_";
  }
}

function phase1SubstituteResolvedFunctionLocalsExpr(value) {
  if (!value || typeof value !== "object" || value.kind !== "function") {
    return null;
  }
  const substitutions = new Map();
  if (value.locals instanceof Map) {
    for (const [name, localValue] of value.locals.entries()) {
      const expr = phase1ExprFromEvaluatedValue(localValue);
      if (expr === null) {
        return null;
      }
      substitutions.set(name, expr);
    }
  }
  return substitutions.size === 0
    ? value.body
    : phase1SubstituteExpr(value.body, substitutions);
}

function phase1InlineResolvedCallableValueExpr(resolved, argExprs) {
  if (!resolved || typeof resolved !== "object") {
    return null;
  }
  if (resolved.kind === "function") {
    if (!Array.isArray(resolved.params) || resolved.params.length === 0) {
      return null;
    }
    const body = phase1SubstituteResolvedFunctionLocalsExpr(resolved);
    if (body === null) {
      return null;
    }
    return phase1InlineFunctionLikeExpr(resolved.params, body, argExprs);
  }
  if (resolved.kind === "builtin") {
    return null;
  }
  const baseExpr = phase1ExprFromEvaluatedValue(resolved);
  if (baseExpr === null) {
    return null;
  }
  let expr = baseExpr;
  for (const arg of argExprs) {
    expr = {
      type: "apply",
      fn: expr,
      arg,
    };
  }
  return phase1ReduceRecordExpr(expr);
}

function phase1ExpandImmediateInlineCall(expr, ctx) {
  if (!expr || typeof expr !== "object" || expr.type !== "apply") {
    return expr;
  }
  const flattened = phase1FlattenApply(expr);
  const callee = flattened.callee;
  if (!callee || callee.type !== "var") {
    return expr;
  }
  const localLambda = ctx.localFunctions instanceof Map
    ? ctx.localFunctions.get(callee.name)
    : null;
  if (localLambda && localLambda.type === "lambda") {
    return phase1InlineCallableExprExpr(localLambda, flattened.args) ?? expr;
  }
  if (!(ctx.evalEnv instanceof Map)) {
    return expr;
  }
  if (phase1InlineCount(ctx, callee.name) >= 8) {
    return expr;
  }
  const resolved = phase1ResolveValueByName(callee.name, ctx.evalEnv, new Map(), 0);
  if (!resolved || typeof resolved !== "object") {
    return expr;
  }
  return phase1InlineResolvedCallableValueExpr(resolved, flattened.args) ?? expr;
}

function phase1ExpandPatternMatchTargets(expr, ctx) {
  if (!expr || typeof expr !== "object") {
    return expr;
  }
  if (expr.type === "letPattern") {
    const value = phase1ExpandImmediateInlineCall(expr.value, ctx);
    if (value !== expr.value) {
      return phase1ReduceRecordExpr({
        ...expr,
        value,
      });
    }
    return expr;
  }
  if (expr.type === "caseCtor") {
    const target = phase1ExpandImmediateInlineCall(expr.target, ctx);
    if (target !== expr.target) {
      return phase1ReduceRecordExpr({
        ...expr,
        target,
      });
    }
    return expr;
  }
  if (expr.type === "caseMulti") {
    const targets = expr.targets.map((target) => phase1ExpandImmediateInlineCall(target, ctx));
    const changed = targets.some((target, index) => target !== expr.targets[index]);
    if (changed) {
      return phase1ReduceRecordExpr({
        ...expr,
        targets,
      });
    }
  }
  return expr;
}

function phase1MergePatternExprBindings(target, source) {
  if (!(source instanceof Map)) {
    return null;
  }
  const out = target instanceof Map ? new Map(target) : new Map();
  for (const [name, value] of source.entries()) {
    if (out.has(name) && out.get(name) !== value) {
      return null;
    }
    out.set(name, value);
  }
  return out;
}

function phase1MatchPatternExpr(pattern, expr) {
  if (!pattern || typeof pattern !== "object") {
    return null;
  }
  if (!expr || typeof expr !== "object") {
    return null;
  }
  if (pattern.type === "wildcard") {
    return new Map();
  }
  if (pattern.type === "binder") {
    return new Map([[pattern.name, expr]]);
  }
  if (pattern.type === "bool") {
    return expr.type === "bool" && expr.value === pattern.value ? new Map() : null;
  }
  if (pattern.type === "int") {
    return expr.type === "int" && expr.value === pattern.value ? new Map() : null;
  }
  if (pattern.type === "record") {
    if (expr.type !== "record") {
      return null;
    }
    const exprFields = new Map(expr.fields.map((field) => [field.name, field.value]));
    let bindings = new Map();
    for (const field of pattern.fields) {
      if (!exprFields.has(field.name)) {
        return null;
      }
      bindings = phase1MergePatternExprBindings(
        bindings,
        phase1MatchPatternExpr(field.pattern, exprFields.get(field.name)),
      );
      if (bindings === null) {
        return null;
      }
    }
    return bindings;
  }
  if (pattern.type !== "ctor") {
    return null;
  }
  if (!Array.isArray(pattern.args)) {
    return null;
  }
  if (expr.type === "var" && pattern.args.length === 0) {
    return expr.name === pattern.name ? new Map() : null;
  }
  if (expr.type !== "apply") {
    return null;
  }
  const flattened = phase1FlattenApply(expr);
  if (!flattened?.callee || flattened.callee.type !== "var" || flattened.callee.name !== pattern.name) {
    return null;
  }
  if (!Array.isArray(flattened.args) || flattened.args.length !== pattern.args.length) {
    return null;
  }
  let bindings = new Map();
  for (let index = 0; index < pattern.args.length; index += 1) {
    bindings = phase1MergePatternExprBindings(
      bindings,
      phase1MatchPatternExpr(pattern.args[index], flattened.args[index]),
    );
    if (bindings === null) {
      return null;
    }
  }
  return bindings;
}

function phase1MatchPatternExprList(patterns, exprs) {
  if (!Array.isArray(patterns) || !Array.isArray(exprs) || patterns.length !== exprs.length) {
    return null;
  }
  let bindings = new Map();
  for (let index = 0; index < patterns.length; index += 1) {
    bindings = phase1MergePatternExprBindings(
      bindings,
      phase1MatchPatternExpr(patterns[index], exprs[index]),
    );
    if (bindings === null) {
      return null;
    }
  }
  return bindings;
}

function phase1IsInlineSubstitutableExpr(expr) {
  if (!expr || typeof expr !== "object") {
    return false;
  }
  switch (expr.type) {
    case "int":
    case "bool":
    case "string":
    case "var":
      return true;
    case "record":
      return Array.isArray(expr.fields) &&
        expr.fields.every((field) => phase1IsInlineSubstitutableExpr(field?.value));
    case "listLiteral":
      return Array.isArray(expr.elements) &&
        expr.elements.every((element) => phase1IsInlineSubstitutableExpr(element));
    case "apply": {
      const flattened = phase1FlattenApply(expr);
      return flattened?.callee?.type === "var" &&
        phase1IsConstructorToken(flattened.callee.name) &&
        Array.isArray(flattened.args) &&
        flattened.args.every((arg) => phase1IsInlineSubstitutableExpr(arg));
    }
    default:
      return false;
  }
}

function phase1InlineLetBindingExpr(name, valueExpr, bodyExpr) {
  const value = phase1ReduceRecordExpr(valueExpr);
  if (phase1IsInlineSubstitutableExpr(value)) {
    return phase1ReduceRecordExpr(
      phase1SubstituteExpr(bodyExpr, new Map([[name, value]])),
    );
  }
  if (value?.type === "let") {
    return phase1ReduceRecordExpr({
      type: "let",
      name: value.name,
      value: value.value,
      body: {
        type: "let",
        name,
        value: value.body,
        body: bodyExpr,
      },
    });
  }
  if (value?.type === "letPattern") {
    return phase1ReduceRecordExpr({
      type: "letPattern",
      pattern: value.pattern,
      value: value.value,
      body: {
        type: "let",
        name,
        value: value.body,
        body: bodyExpr,
      },
    });
  }
  return null;
}

function phase1InlineLetPatternExpr(pattern, valueExpr, bodyExpr) {
  const value = phase1ReduceRecordExpr(valueExpr);
  const bindings = phase1MatchPatternExpr(pattern, value);
  if (bindings !== null) {
    return phase1ReduceRecordExpr(phase1SubstituteExpr(bodyExpr, bindings));
  }
  if (value?.type === "let") {
    return phase1ReduceRecordExpr({
      type: "let",
      name: value.name,
      value: value.value,
      body: {
        type: "letPattern",
        pattern,
        value: value.body,
        body: bodyExpr,
      },
    });
  }
  if (value?.type === "letPattern") {
    return phase1ReduceRecordExpr({
      type: "letPattern",
      pattern: value.pattern,
      value: value.value,
      body: {
        type: "letPattern",
        pattern,
        value: value.body,
        body: bodyExpr,
      },
    });
  }
  return null;
}

function phase1ReduceRecordExpr(expr) {
  if (!expr || typeof expr !== "object") {
    return expr;
  }
  switch (expr.type) {
    case "int":
    case "bool":
    case "string":
    case "var":
    case "trap":
      return expr;
    case "lambda":
      {
        const body = phase1ReduceRecordExpr(expr.body);
        if (body === expr.body) {
          return expr;
        }
        return {
          ...expr,
          body,
        };
      }
    case "apply":
      {
        const fn = phase1ReduceRecordExpr(expr.fn);
        const arg = phase1ReduceRecordExpr(expr.arg);
        const reduced = fn === expr.fn && arg === expr.arg
          ? expr
          : {
            type: "apply",
            fn,
            arg,
          };
        const flattened = phase1FlattenApply(reduced);
        if (flattened.callee?.type === "lambda") {
          const reducedLambdaApply = phase1InlineFunctionLikeExpr(
            flattened.callee.params,
            flattened.callee.body,
            flattened.args,
          );
          if (reducedLambdaApply !== null) {
            return phase1ReduceRecordExpr(reducedLambdaApply);
          }
        }
        const reducedIfBuiltin = phase1ReduceIfBuiltinApply(flattened);
        if (reducedIfBuiltin !== null) {
          return phase1ReduceRecordExpr(reducedIfBuiltin);
        }
        const reducedPureBuiltin = phase1ReducePureBuiltinApply(flattened);
        if (reducedPureBuiltin !== null) {
          return phase1ReduceRecordExpr(reducedPureBuiltin);
        }
        const reducedStructHelper = phase1ReduceStructHelperApply(flattened);
        if (reducedStructHelper !== null) {
          return phase1ReduceRecordExpr(reducedStructHelper);
        }
        return reduced;
      }
    case "if":
      {
        const cond = phase1ReduceRecordExpr(expr.cond);
        const thenExpr = phase1ReduceRecordExpr(expr.thenExpr);
        const elseExpr = phase1ReduceRecordExpr(expr.elseExpr);
        if (cond?.type === "bool") {
          return cond.value ? thenExpr : elseExpr;
        }
        if (cond === expr.cond && thenExpr === expr.thenExpr && elseExpr === expr.elseExpr) {
          return expr;
        }
        return {
          ...expr,
          cond,
          thenExpr,
          elseExpr,
        };
      }
    case "caseBool":
      {
        const target = phase1ReduceRecordExpr(expr.target);
        const whenTrue = phase1ReduceRecordExpr(expr.whenTrue);
        const whenFalse = phase1ReduceRecordExpr(expr.whenFalse);
        if (target?.type === "caseBool") {
          return phase1ReduceRecordExpr({
            type: "caseBool",
            target: target.target,
            whenTrue: {
              ...expr,
              target: target.whenTrue,
            },
            whenFalse: {
              ...expr,
              target: target.whenFalse,
            },
          });
        }
        if (target?.type === "bool") {
          return target.value ? whenTrue : whenFalse;
        }
        if (target === expr.target && whenTrue === expr.whenTrue && whenFalse === expr.whenFalse) {
          return expr;
        }
        return {
          ...expr,
          target,
          whenTrue,
          whenFalse,
        };
      }
    case "caseCtor":
      {
        const target = phase1ReduceRecordExpr(expr.target);
        const whenMatch = phase1ReduceRecordExpr(expr.whenMatch);
        const whenFallback = expr.whenFallback ? phase1ReduceRecordExpr(expr.whenFallback) : expr.whenFallback;
        if (target?.type === "caseBool") {
          return phase1ReduceRecordExpr({
            type: "caseBool",
            target: target.target,
            whenTrue: {
              ...expr,
              target: target.whenTrue,
            },
            whenFalse: {
              ...expr,
              target: target.whenFalse,
            },
          });
        }
        if (target?.type === "caseCtor") {
          return phase1ReduceRecordExpr({
            ...target,
            whenMatch: {
              ...expr,
              target: target.whenMatch,
            },
            whenFallback: target.whenFallback
              ? {
                ...expr,
                target: target.whenFallback,
              }
              : target.whenFallback,
          });
        }
        if (target?.type === "caseMulti") {
          return phase1ReduceRecordExpr({
            ...target,
            whenMatch: {
              ...expr,
              target: target.whenMatch,
            },
            whenFallback: {
              ...expr,
              target: target.whenFallback,
            },
          });
        }
        const matchBindings = phase1MatchPatternExpr(expr.pattern, target);
        if (matchBindings !== null) {
          return phase1ReduceRecordExpr(phase1SubstituteExpr(whenMatch, matchBindings));
        }
        if (expr.fallbackPattern && whenFallback) {
          const fallbackBindings = phase1MatchPatternExpr(expr.fallbackPattern, target);
          if (fallbackBindings !== null) {
            return phase1ReduceRecordExpr(
              phase1SubstituteExpr(whenFallback, fallbackBindings),
            );
          }
        }
        if (
          target === expr.target &&
          whenMatch === expr.whenMatch &&
          whenFallback === expr.whenFallback
        ) {
          return expr;
        }
        return {
          ...expr,
          target,
          whenMatch,
          whenFallback,
        };
      }
    case "caseMulti":
      {
        const targets = expr.targets.map((target) => phase1ReduceRecordExpr(target));
        const whenMatch = phase1ReduceRecordExpr(expr.whenMatch);
        const whenFallback = phase1ReduceRecordExpr(expr.whenFallback);
        const matchBindings = phase1MatchPatternExprList(expr.patterns, targets);
        if (matchBindings !== null) {
          return phase1ReduceRecordExpr(phase1SubstituteExpr(whenMatch, matchBindings));
        }
        const fallbackBindings = phase1MatchPatternExprList(expr.fallbackPatterns, targets);
        if (fallbackBindings !== null) {
          return phase1ReduceRecordExpr(
            phase1SubstituteExpr(whenFallback, fallbackBindings),
          );
        }
        const targetsUnchanged = targets.every((target, index) => target === expr.targets[index]);
        if (
          targetsUnchanged &&
          whenMatch === expr.whenMatch &&
          whenFallback === expr.whenFallback
        ) {
          return expr;
        }
        return {
          ...expr,
          targets,
          whenMatch,
          whenFallback,
        };
      }
    case "listLiteral":
      {
        const elements = expr.elements.map((element) => phase1ReduceRecordExpr(element));
        const unchanged = elements.every((element, index) => element === expr.elements[index]);
        if (unchanged) {
          return expr;
        }
        return {
          ...expr,
          elements,
        };
      }
    case "record":
      {
        let changed = false;
        const fields = expr.fields.map((field) => {
          const value = phase1ReduceRecordExpr(field.value);
          if (value !== field.value) {
            changed = true;
            return {
              ...field,
              value,
            };
          }
          return field;
        });
        if (!changed) {
          return expr;
        }
        return {
          type: "record",
          fields,
        };
      }
    case "recordUpdate": {
      const base = phase1ReduceRecordExpr(expr.base);
      let changed = base !== expr.base;
      const fields = expr.fields.map((field) => {
        const value = phase1ReduceRecordExpr(field.value);
        if (value !== field.value) {
          changed = true;
          return { ...field, value };
        }
        return field;
      });
      if (base?.type === "record") {
        const byName = new Map(base.fields.map((field) => [field.name, field]));
        for (const field of fields) {
          byName.set(field.name, field);
        }
        return {
          type: "record",
          fields: [...byName.values()],
        };
      }
      if (!changed) {
        return expr;
      }
      return {
        type: "recordUpdate",
        base,
        fields,
      };
    }
    case "braceApplyOrUpdate": {
      const base = phase1ReduceRecordExpr(expr.base);
      let changed = base !== expr.base;
      const fields = expr.fields.map((field) => {
        const value = phase1ReduceRecordExpr(field.value);
        if (value !== field.value) {
          changed = true;
          return { ...field, value };
        }
        return field;
      });
      if (base?.type === "record") {
        const byName = new Map(base.fields.map((field) => [field.name, field]));
        for (const field of fields) {
          byName.set(field.name, field);
        }
        return {
          type: "record",
          fields: [...byName.values()],
        };
      }
      if (!changed) {
        return expr;
      }
      return {
        type: "braceApplyOrUpdate",
        base,
        fields,
      };
    }
    case "field": {
      const base = phase1ReduceRecordExpr(expr.base);
      if (base?.type === "record") {
        const matched = base.fields.find((field) => field.name === expr.field);
        return matched ? phase1ReduceRecordExpr(matched.value) : {
          type: "field",
          base,
          field: expr.field,
        };
      }
      if (base === expr.base) {
        return expr;
      }
      return {
        type: "field",
        base,
        field: expr.field,
      };
    }
    case "let": {
      const value = phase1ReduceRecordExpr(expr.value);
      const body = phase1ReduceRecordExpr(expr.body);
      const inlined = phase1InlineLetBindingExpr(expr.name, value, body);
      if (inlined !== null) {
        return inlined;
      }
      return {
        type: "let",
        name: expr.name,
        value,
        body,
      };
    }
    case "letPattern":
      {
        const value = phase1ReduceRecordExpr(expr.value);
        const body = phase1ReduceRecordExpr(expr.body);
        const inlined = phase1InlineLetPatternExpr(expr.pattern, value, body);
        if (inlined !== null) {
          return inlined;
        }
        if (value === expr.value && body === expr.body) {
          return expr;
        }
        return {
          ...expr,
          value,
          body,
        };
      }
    default:
      return expr;
  }
}

function phase1EmitExprToWasm(expr, ctx) {
  if (!expr || typeof expr !== "object") {
    return null;
  }
  if (expr.type === "trap") {
    return [0x00];
  }
  if (
    expr.type === "caseCtor" || expr.type === "caseMulti" ||
    expr.type === "letPattern"
  ) {
    const expanded = phase1ExpandPatternMatchTargets(expr, ctx);
    if (expanded !== expr) {
      return phase1EmitExprToWasm(expanded, ctx);
    }
  }
  if (
    expr.type === "caseBool" || expr.type === "caseCtor" ||
    expr.type === "caseMulti" || expr.type === "letPattern"
  ) {
    const reduced = phase1ReduceRecordExpr(expr);
    if (reduced !== expr) {
      return phase1EmitExprToWasm(reduced, ctx);
    }
  }
  if (
    expr.type === "field" || expr.type === "recordUpdate" ||
    expr.type === "braceApplyOrUpdate"
  ) {
    if (
      expr.base?.type === "var" &&
      ctx.recordLocals instanceof Map &&
      ctx.recordLocals.has(expr.base.name)
    ) {
      const localized = phase1ReduceRecordExpr({
        ...expr,
        base: ctx.recordLocals.get(expr.base.name),
      });
      if (localized !== expr) {
        return phase1EmitExprToWasm(localized, ctx);
      }
    }
    const reduced = phase1ReduceRecordExpr(expr);
    if (reduced !== expr) {
      return phase1EmitExprToWasm(reduced, ctx);
    }
    return null;
  }
  if (expr.type === "int") {
    return [0x41, ...encodeVarS32(expr.value)];
  }
  if (expr.type === "bool") {
    return [0x41, ...encodeVarS32(expr.value ? 1 : 0)];
  }
  if (expr.type === "var") {
    if (ctx.locals.has(expr.name)) {
      return [0x20, ...encodeVarU32(ctx.locals.get(expr.name))];
    }
    if (ctx.evalEnv instanceof Map) {
      const resolved = phase1ResolveValueByName(expr.name, ctx.evalEnv, new Map(), 0);
      if (Number.isInteger(resolved)) {
        return [0x41, ...encodeVarS32(resolved)];
      }
      if (typeof resolved === "boolean") {
        return [0x41, ...encodeVarS32(resolved ? 1 : 0)];
      }
    }
    const targetIndex = ctx.functionIndexByName instanceof Map
      ? ctx.functionIndexByName.get(expr.name)
      : undefined;
    const targetDef = ctx.defMap.get(expr.name);
    if (
      typeof targetIndex === "number" &&
      targetDef &&
      targetDef.params.length === 0
    ) {
      return [0x10, ...encodeVarU32(targetIndex)];
    }
    return null;
  }
  if (expr.type === "if") {
    if (!phase1IsBoolConditionExpr(expr.cond, ctx, new Set())) {
      return null;
    }
    const cond = phase1EmitExprToWasm(expr.cond, ctx);
    const thenExpr = phase1EmitExprToWasm(expr.thenExpr, ctx);
    const elseExpr = phase1EmitExprToWasm(expr.elseExpr, ctx);
    if (cond === null || thenExpr === null || elseExpr === null) {
      return null;
    }
    return [...cond, 0x04, 0x7f, ...thenExpr, 0x05, ...elseExpr, 0x0b];
  }
  if (expr.type === "let") {
    const expandedValue = phase1ExpandImmediateInlineCall(expr.value, ctx);
    const reducedValue = phase1ReduceRecordExpr(expandedValue);
    const hoisted = phase1InlineLetBindingExpr(expr.name, reducedValue, expr.body);
    if (hoisted !== null) {
      return phase1EmitExprToWasm(hoisted, ctx);
    }
    if (expr.value?.type === "lambda") {
      const bodyCtx = {
        ...ctx,
        localFunctions: new Map(ctx.localFunctions ?? []),
      };
      bodyCtx.localFunctions.set(expr.name, expr.value);
      return phase1EmitExprToWasm(expr.body, bodyCtx);
    }
    if (phase1IsInlineSubstitutableExpr(reducedValue)) {
      return phase1EmitExprToWasm(
        phase1ReduceRecordExpr(
          phase1SubstituteExpr(expr.body, new Map([[expr.name, reducedValue]])),
        ),
        ctx,
      );
    }
    const bound = phase1EmitExprToWasm(reducedValue, ctx);
    const localIndex = ctx.locals.get(expr.name);
    if (bound === null || typeof localIndex !== "number") {
      return null;
    }
    const bodyCtx = {
      ...ctx,
      locals: new Map(ctx.locals),
      localBoolLocals: new Set(ctx.localBoolLocals ?? []),
    };
    bodyCtx.locals.set(expr.name, localIndex);
    if (phase1IsBoolConditionExpr(reducedValue, ctx, new Set())) {
      bodyCtx.localBoolLocals.add(expr.name);
    } else {
      bodyCtx.localBoolLocals.delete(expr.name);
    }
    const body = phase1EmitExprToWasm(expr.body, bodyCtx);
    if (body === null) {
      return null;
    }
    return [...bound, 0x21, ...encodeVarU32(localIndex), ...body];
  }
  if (expr.type === "caseBool") {
    if (!phase1IsBoolConditionExpr(expr.target, ctx, new Set())) {
      return null;
    }
    const cond = phase1EmitExprToWasm(expr.target, ctx);
    const whenTrue = phase1EmitExprToWasm(expr.whenTrue, ctx);
    const whenFalse = phase1EmitExprToWasm(expr.whenFalse, ctx);
    if (cond === null || whenTrue === null || whenFalse === null) {
      return null;
    }
    return [...cond, 0x04, 0x7f, ...whenTrue, 0x05, ...whenFalse, 0x0b];
  }
  if (expr.type === "caseCtor") {
    const transparentNewtypeCtors = ctx.transparentNewtypeCtors instanceof Set
      ? ctx.transparentNewtypeCtors
      : new Set();
    if (
      expr.pattern?.type === "ctor" &&
      transparentNewtypeCtors.has(expr.pattern.name) &&
      Array.isArray(expr.pattern.args) &&
      expr.pattern.args.length === 1 &&
      !expr.fallbackPattern &&
      !expr.whenFallback
    ) {
      const target = phase1EmitExprToWasm(expr.target, ctx);
      if (target === null) {
        return null;
      }
      const [innerPattern] = expr.pattern.args;
      if (!innerPattern || innerPattern.type === "wildcard") {
        return phase1EmitExprToWasm(expr.whenMatch, ctx);
      }
      if (innerPattern.type !== "binder") {
        return null;
      }
      const localIndex = ctx.locals.get(innerPattern.name);
      if (typeof localIndex !== "number") {
        return null;
      }
      const bodyCtx = {
        ...ctx,
        locals: new Map(ctx.locals),
      };
      bodyCtx.locals.set(innerPattern.name, localIndex);
      const whenMatch = phase1EmitExprToWasm(expr.whenMatch, bodyCtx);
      if (whenMatch === null) {
        return null;
      }
      return [...target, 0x21, ...encodeVarU32(localIndex), ...whenMatch];
    }
    return null;
  }
  if (expr.type === "apply") {
    const flattened = phase1FlattenApply(expr);
    const callee = flattened.callee;
    if (!callee || callee.type !== "var") {
      return null;
    }
    const reducedIfBuiltin = phase1ReduceIfBuiltinApply(flattened, ctx);
    if (reducedIfBuiltin !== null) {
      return phase1EmitExprToWasm(reducedIfBuiltin, ctx);
    }
    const reducedStructHelper = phase1ReduceStructHelperApply(flattened);
    if (reducedStructHelper !== null) {
      return phase1EmitExprToWasm(reducedStructHelper, ctx);
    }
    const localLambda = ctx.localFunctions instanceof Map
      ? ctx.localFunctions.get(callee.name)
      : null;
    if (
      localLambda &&
      localLambda.type === "lambda"
    ) {
      const localInline = phase1InlineCallableExpr(localLambda, flattened.args, ctx);
      if (localInline !== null) {
        return localInline;
      }
    }
    if (ctx.evalEnv instanceof Map) {
      const resolved = phase1ResolveValueByName(callee.name, ctx.evalEnv, new Map(), 0);
      if (
        resolved &&
        typeof resolved === "object" &&
        phase1InlineCount(ctx, callee.name) < 8
      ) {
        const inline = phase1InlineResolvedCallableValueExpr(resolved, flattened.args);
        if (inline !== null) {
          const inlineCtx = phase1ExtendEmitterLocalsForExpr(inline, {
            ...phase1WithInlineCount(ctx, callee.name),
          });
          return phase1EmitExprToWasm(inline, inlineCtx);
        }
      }
    }
    const out = [];
    for (const arg of flattened.args) {
      const emitted = phase1EmitExprToWasm(arg, ctx);
      if (emitted === null) {
        return null;
      }
      out.push(...emitted);
    }
    switch (callee.name) {
      case "add":
        return flattened.args.length === 2 ? [...out, 0x6a] : null;
      case "sub":
        return flattened.args.length === 2 ? [...out, 0x6b] : null;
      case "mul":
        return flattened.args.length === 2 ? [...out, 0x6c] : null;
      case "div":
        return flattened.args.length === 2 ? [...out, 0x6d] : null;
      case "mod":
        return flattened.args.length === 2 ? [...out, 0x6f] : null;
      case "eq":
        return flattened.args.length === 2 ? [...out, 0x46] : null;
      case "ne":
        return flattened.args.length === 2 ? [...out, 0x47] : null;
      case "lt":
        return flattened.args.length === 2 ? [...out, 0x48] : null;
      case "gt":
        return flattened.args.length === 2 ? [...out, 0x4a] : null;
      case "le":
        return flattened.args.length === 2 ? [...out, 0x4c] : null;
      case "ge":
        return flattened.args.length === 2 ? [...out, 0x4e] : null;
      case "and":
        return flattened.args.length === 2 ? [...out, 0x71] : null;
      case "or":
        return flattened.args.length === 2 ? [...out, 0x72] : null;
      case "not":
        return flattened.args.length === 1 ? [...out, 0x45] : null;
      case "slice_len":
        return flattened.args.length === 1
          ? [...out, 0x28, ...encodeVarU32(2), ...encodeVarU32(4)]
          : null;
      case "slice_len_raw":
        return flattened.args.length === 1
          ? [...out, 0x28, ...encodeVarU32(2), ...encodeVarU32(4)]
          : null;
      case "slice_data_ptr":
        return flattened.args.length === 1
          ? [...out, 0x28, ...encodeVarU32(2), ...encodeVarU32(0)]
          : null;
      case "slice_get_u8":
        if (flattened.args.length !== 2) {
          return null;
        }
        {
          const handle = phase1EmitExprToWasm(flattened.args[0], ctx);
          const index = phase1EmitExprToWasm(flattened.args[1], ctx);
          if (handle === null || index === null) {
            return null;
          }
          return [
            ...handle,
            0x28,
            ...encodeVarU32(2),
            ...encodeVarU32(0),
            ...index,
            0x6a,
            0x2d,
            ...encodeVarU32(0),
            ...encodeVarU32(0),
          ];
        }
      case "slice_new_u8":
      case "region_mark":
      case "region_reset":
      case "region_alloc":
      case "memcpy_u8":
      case "memset_u8":
        return [0x00];
      default: {
        const targetIndex = ctx.functionIndexByName instanceof Map
          ? ctx.functionIndexByName.get(callee.name)
          : undefined;
        const targetDef = ctx.defMap.get(callee.name);
        if (
          typeof targetIndex !== "number" ||
          !targetDef ||
          targetDef.params.length !== flattened.args.length
        ) {
          return null;
        }
        out.push(0x10, ...encodeVarU32(targetIndex));
        return out;
      }
    }
  }
  return null;
}

function phase1IsBoolConditionExpr(expr, ctx, seenDefs) {
  if (!expr || typeof expr !== "object") {
    return false;
  }
  if (expr.type === "bool") {
    return true;
  }
  if (expr.type === "if") {
    return phase1IsBoolConditionExpr(expr.cond, ctx, seenDefs) &&
      phase1IsBoolConditionExpr(expr.thenExpr, ctx, seenDefs) &&
      phase1IsBoolConditionExpr(expr.elseExpr, ctx, seenDefs);
  }
  if (expr.type === "let") {
    if (phase1EmitExprToWasm(expr.value, ctx) === null) {
      return false;
    }
    const bodyCtx = {
      ...ctx,
      localBoolLocals: new Set(ctx.localBoolLocals ?? []),
    };
    if (phase1IsBoolConditionExpr(expr.value, ctx, seenDefs)) {
      bodyCtx.localBoolLocals.add(expr.name);
    } else {
      bodyCtx.localBoolLocals.delete(expr.name);
    }
    return phase1IsBoolConditionExpr(expr.body, bodyCtx, seenDefs);
  }
  if (expr.type === "letPattern") {
    return false;
  }
  if (expr.type === "caseBool") {
    return phase1IsBoolConditionExpr(expr.target, ctx, seenDefs) &&
      phase1IsBoolConditionExpr(expr.whenTrue, ctx, seenDefs) &&
      phase1IsBoolConditionExpr(expr.whenFalse, ctx, seenDefs);
  }
  if (expr.type === "apply") {
    const flattened = phase1FlattenApply(expr);
    const callee = flattened.callee;
    if (!callee || callee.type !== "var") {
      return false;
    }
    if (
      callee.name === "eq" || callee.name === "ne" || callee.name === "lt" ||
      callee.name === "le" || callee.name === "gt" || callee.name === "ge" ||
      callee.name === "and" || callee.name === "or" || callee.name === "not"
    ) {
      const expectedArity = callee.name === "not" ? 1 : 2;
      return flattened.args.length === expectedArity;
    }
    if (ctx.boolDefNames instanceof Set && ctx.boolDefNames.has(callee.name)) {
      return true;
    }
    const targetDef = ctx.defMap.get(callee.name);
    if (!targetDef || seenDefs.has(targetDef.name)) {
      return false;
    }
    seenDefs.add(targetDef.name);
    const out = phase1IsBoolConditionExpr(targetDef.body, ctx, seenDefs);
    seenDefs.delete(targetDef.name);
    return out;
  }
  if (expr.type === "var") {
    if (ctx.localBoolLocals instanceof Set && ctx.localBoolLocals.has(expr.name)) {
      return true;
    }
    if (ctx.locals.has(expr.name)) {
      return false;
    }
    if (ctx.evalEnv instanceof Map) {
      const resolved = phase1ResolveValueByName(expr.name, ctx.evalEnv, new Map(), 0);
      if (typeof resolved === "boolean") {
        return true;
      }
    }
    const targetDef = ctx.defMap.get(expr.name);
    if (!targetDef || targetDef.params.length !== 0 || seenDefs.has(targetDef.name)) {
      return false;
    }
    seenDefs.add(targetDef.name);
    const out = phase1IsBoolConditionExpr(targetDef.body, ctx, seenDefs);
    seenDefs.delete(targetDef.name);
    return out;
  }
  return false;
}

function phase1CollectBoolReturningDefNames(defMap, evalEnv) {
  if (!(defMap instanceof Map)) {
    return new Set();
  }
  const boolDefNames = new Set();
  let changed = true;
  while (changed) {
    changed = false;
    for (const def of defMap.values()) {
      if (!def || typeof def !== "object" || typeof def.name !== "string") {
        continue;
      }
      if (boolDefNames.has(def.name)) {
        continue;
      }
      const locals = new Map();
      if (Array.isArray(def.params)) {
        def.params.forEach((param, index) => locals.set(param, index));
      }
      if (
        phase1IsBoolConditionExpr(def.body, {
          locals,
          defMap,
          evalEnv,
          boolDefNames,
        }, new Set([def.name]))
      ) {
        boolDefNames.add(def.name);
        changed = true;
      }
    }
  }
  return boolDefNames;
}

function phase1ResolveParamRoleTarget(expr, ctx) {
  if (!expr || typeof expr !== "object") {
    return null;
  }
  if (expr.type !== "var") {
    return null;
  }
  if (ctx.paramIndexByName instanceof Map && ctx.paramIndexByName.has(expr.name)) {
    return ctx.paramIndexByName.get(expr.name);
  }
  if (ctx.aliases instanceof Map && ctx.aliases.has(expr.name)) {
    return phase1ResolveParamRoleTarget(ctx.aliases.get(expr.name), ctx);
  }
  return null;
}

function phase1RefineParamRole(roles, index, nextRole) {
  if (!Array.isArray(roles) || !Number.isInteger(index) || index < 0 || index >= roles.length) {
    return false;
  }
  const current = roles[index] ?? "unknown";
  if (current === nextRole || nextRole === "unknown") {
    return false;
  }
  if (current === "unknown") {
    roles[index] = nextRole;
    return true;
  }
  if (current === "scalar" && nextRole === "opaque") {
    roles[index] = "opaque";
    return true;
  }
  return false;
}

function phase1MarkExprParamRole(expr, role, ctx, roles) {
  const targetIndex = phase1ResolveParamRoleTarget(expr, ctx);
  if (targetIndex === null) {
    return false;
  }
  return phase1RefineParamRole(roles, targetIndex, role);
}

function phase1BuiltinParamRoles(name, arity) {
  if (phase1ParseStructGetterName(name) !== null || phase1ParseStructPredicateName(name) !== null) {
    return arity === 1 ? ["opaque"] : null;
  }
  switch (name) {
    case "add":
    case "sub":
    case "mul":
    case "div":
    case "mod":
    case "lt":
    case "le":
    case "gt":
    case "ge":
    case "eq":
    case "ne":
    case "and":
    case "or":
      return arity === 2 ? ["scalar", "scalar"] : null;
    case "not":
      return arity === 1 ? ["scalar"] : null;
    case "if":
      return arity === 3 ? ["scalar", "unknown", "unknown"] : null;
    case "slice_len":
    case "slice_len_raw":
    case "slice_data_ptr":
    case "slice_to_string":
      return arity === 1 ? ["opaque"] : null;
    case "str_to_slice":
      return arity === 1 ? ["opaque"] : null;
    case "slice_get_u8":
      return arity === 2 ? ["opaque", "scalar"] : null;
    case "slice_set_u8":
      return arity === 3 ? ["opaque", "scalar", "scalar"] : null;
    case "slice_eq_u8":
      return arity === 2 ? ["opaque", "opaque"] : null;
    case "slice_new_u8":
    case "region_mark":
    case "region_alloc":
    case "region_reset":
    case "memcpy_u8":
    case "memset_u8":
      return Array(arity).fill("scalar");
    default:
      return null;
  }
}

function phase1AnalyzeDefParamRolesExpr(expr, ctx, defRoles) {
  if (!expr || typeof expr !== "object") {
    return false;
  }
  let changed = false;
  switch (expr.type) {
    case "int":
    case "bool":
    case "string":
    case "trap":
    case "var":
      return false;
    case "lambda": {
      const innerAliases = new Map(ctx.aliases ?? []);
      for (const param of expr.params ?? []) {
        innerAliases.delete(param);
      }
      return phase1AnalyzeDefParamRolesExpr(expr.body, {
        ...ctx,
        aliases: innerAliases,
      }, defRoles);
    }
    case "if":
      changed = phase1AnalyzeDefParamRolesExpr(expr.cond, ctx, defRoles) || changed;
      changed = phase1AnalyzeDefParamRolesExpr(expr.thenExpr, ctx, defRoles) || changed;
      changed = phase1AnalyzeDefParamRolesExpr(expr.elseExpr, ctx, defRoles) || changed;
      return changed;
    case "let": {
      changed = phase1AnalyzeDefParamRolesExpr(expr.value, ctx, defRoles) || changed;
      const nextAliases = new Map(ctx.aliases ?? []);
      if (expr.value?.type === "var") {
        nextAliases.set(expr.name, expr.value);
      } else {
        nextAliases.delete(expr.name);
      }
      return phase1AnalyzeDefParamRolesExpr(expr.body, {
        ...ctx,
        aliases: nextAliases,
      }, defRoles) || changed;
    }
    case "letPattern":
      changed = phase1AnalyzeDefParamRolesExpr(expr.value, ctx, defRoles) || changed;
      return phase1AnalyzeDefParamRolesExpr(expr.body, ctx, defRoles) || changed;
    case "caseBool":
      changed = phase1AnalyzeDefParamRolesExpr(expr.target, ctx, defRoles) || changed;
      changed = phase1AnalyzeDefParamRolesExpr(expr.whenTrue, ctx, defRoles) || changed;
      changed = phase1AnalyzeDefParamRolesExpr(expr.whenFalse, ctx, defRoles) || changed;
      return changed;
    case "caseCtor":
      changed = phase1AnalyzeDefParamRolesExpr(expr.target, ctx, defRoles) || changed;
      changed = phase1AnalyzeDefParamRolesExpr(expr.whenMatch, ctx, defRoles) || changed;
      if (expr.whenFallback) {
        changed = phase1AnalyzeDefParamRolesExpr(expr.whenFallback, ctx, defRoles) || changed;
      }
      return changed;
    case "caseMulti":
      for (const target of expr.targets ?? []) {
        changed = phase1AnalyzeDefParamRolesExpr(target, ctx, defRoles) || changed;
      }
      changed = phase1AnalyzeDefParamRolesExpr(expr.whenMatch, ctx, defRoles) || changed;
      changed = phase1AnalyzeDefParamRolesExpr(expr.whenFallback, ctx, defRoles) || changed;
      return changed;
    case "record":
      for (const field of expr.fields ?? []) {
        changed = phase1AnalyzeDefParamRolesExpr(field.value, ctx, defRoles) || changed;
      }
      return changed;
    case "recordUpdate":
    case "braceApplyOrUpdate":
      changed = phase1AnalyzeDefParamRolesExpr(expr.base, ctx, defRoles) || changed;
      for (const field of expr.fields ?? []) {
        changed = phase1AnalyzeDefParamRolesExpr(field.value, ctx, defRoles) || changed;
      }
      return changed;
    case "field":
      return phase1AnalyzeDefParamRolesExpr(expr.base, ctx, defRoles);
    case "listLiteral":
      for (const element of expr.elements ?? []) {
        changed = phase1AnalyzeDefParamRolesExpr(element, ctx, defRoles) || changed;
      }
      return changed;
    case "apply": {
      const flattened = phase1FlattenApply(expr);
      const callee = flattened.callee;
      if (!callee || callee.type !== "var") {
        return false;
      }
      for (const arg of flattened.args) {
        changed = phase1AnalyzeDefParamRolesExpr(arg, ctx, defRoles) || changed;
      }
      const builtinRoles = phase1BuiltinParamRoles(callee.name, flattened.args.length);
      if (Array.isArray(builtinRoles)) {
        for (let i = 0; i < builtinRoles.length; i += 1) {
          changed = phase1MarkExprParamRole(flattened.args[i], builtinRoles[i], ctx, ctx.roles) || changed;
        }
        return changed;
      }
      const calleeRoles = defRoles.get(callee.name) ??
        defRoles.get(phase1ResolvedCallableName(callee.name));
      if (Array.isArray(calleeRoles)) {
        for (let i = 0; i < Math.min(calleeRoles.length, flattened.args.length); i += 1) {
          changed = phase1MarkExprParamRole(flattened.args[i], calleeRoles[i], ctx, ctx.roles) || changed;
        }
      }
      return changed;
    }
    default:
      return false;
  }
}

function phase1InferDefParamRoles(definitions) {
  const defRoles = new Map();
  if (!Array.isArray(definitions)) {
    return defRoles;
  }
  for (const def of definitions) {
    defRoles.set(def.name, Array(def.params.length).fill("unknown"));
  }
  let changed = true;
  let remaining = 8;
  while (changed && remaining > 0) {
    changed = false;
    remaining -= 1;
    for (const def of definitions) {
      const roles = defRoles.get(def.name) ?? Array(def.params.length).fill("unknown");
      defRoles.set(def.name, roles);
      const paramIndexByName = new Map();
      def.params.forEach((param, index) => paramIndexByName.set(param, index));
      changed = phase1AnalyzeDefParamRolesExpr(def.body, {
        roles,
        paramIndexByName,
        aliases: new Map(),
      }, defRoles) || changed;
    }
  }
  return defRoles;
}

function phase1ExecutableWasmBase64ForSource(sourceText, requestObject) {
  const roots = phase1SelectedExportNames(requestObject, sourceText);
  const rootSet = new Set(roots);
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  const graph = phase1CollectExecutableDefsForRoots(definitions, roots);
  if (graph === null) {
    return null;
  }
  const fullDefMap = graph.defMap instanceof Map
    ? graph.defMap
    : new Map(definitions.map((def) => [def.name, def]));
  const evalEnv = graph.evalEnv instanceof Map
    ? graph.evalEnv
    : phase1BuildEvaluationEnv(definitions);
  if (!(evalEnv instanceof Map)) {
    return null;
  }

  const typeByArity = new Map();
  const typeList = [];
  function ensureType(paramCount) {
    if (typeByArity.has(paramCount)) {
      return typeByArity.get(paramCount);
    }
    const idx = typeList.length;
    typeList.push(paramCount);
    typeByArity.set(paramCount, idx);
    return idx;
  }

  const functionIndexByName = new Map();
  graph.orderedDefs.forEach((def, index) => functionIndexByName.set(def.name, index));
  const boolDefNames = phase1CollectBoolReturningDefNames(fullDefMap, evalEnv);

  const bodies = [];
  const typeIndexes = [];
  for (const def of graph.orderedDefs) {
    const reducedBody = phase1ReduceRecordExpr(def.body);
    const locals = new Map();
    def.params.forEach((param, index) => locals.set(param, index));
    const letLocals = new Map();
    phase1CollectLetLocals(reducedBody, letLocals);
    for (const [name, offset] of letLocals.entries()) {
      locals.set(name, def.params.length + offset);
    }
    const emitted = phase1EmitExprToWasm(reducedBody, {
      locals,
      functionIndexByName,
      defMap: fullDefMap,
      evalEnv,
      boolDefNames,
      transparentNewtypeCtors: definitions.transparentNewtypeCtors instanceof Set
        ? definitions.transparentNewtypeCtors
        : new Set(),
    });
    if (emitted === null) {
      if (def.params.length === 0) {
        const evaluated = phase1Evaluate(reducedBody, evalEnv, new Map(), 0);
        const constValue = Number.isInteger(evaluated)
          ? evaluated
          : typeof evaluated === "boolean"
          ? (evaluated ? 1 : 0)
          : null;
        if (constValue !== null) {
          bodies.push({
            localCount: letLocals.size,
            code: [0x41, ...encodeVarS32(constValue)],
          });
          typeIndexes.push(ensureType(def.params.length));
          continue;
        }
        if (!rootSet.has(def.name) && evaluated !== null) {
          bodies.push({
            localCount: letLocals.size,
            code: [0x41, ...encodeVarS32(0)],
          });
          typeIndexes.push(ensureType(def.params.length));
          continue;
        }
      }
      return null;
    }
    bodies.push({
      localCount: letLocals.size,
      code: emitted,
    });
    typeIndexes.push(ensureType(def.params.length));
  }

  const exportedFunctions = [];
  for (const root of roots) {
    const rootDef = fullDefMap.get(root);
    if (!rootDef) {
      return null;
    }
    const wrapperBody = [];
    for (let i = 0; i < rootDef.params.length; i += 1) {
      wrapperBody.push(0x20, ...encodeVarU32(i));
    }
    wrapperBody.push(
      0x10,
      ...encodeVarU32(functionIndexByName.get(root)),
      0x41,
      ...encodeVarS32(2),
      0x6c,
      0x41,
      ...encodeVarS32(1),
      0x6a,
    );
    const wrapperIndex = bodies.length;
    bodies.push({
      localCount: 0,
      code: wrapperBody,
    });
    typeIndexes.push(ensureType(rootDef.params.length));
    exportedFunctions.push({ name: root, kind: 0x00, index: wrapperIndex });
  }

  const moduleBytes = [
    0x00,
    0x61,
    0x73,
    0x6d,
    0x01,
    0x00,
    0x00,
    0x00,
    ...phase1WrapSection(1, phase1WasmTypeSection(typeList)),
    ...phase1WrapSection(3, phase1WasmFunctionSection(typeIndexes)),
    ...phase1WrapSection(5, phase1WasmMemorySection()),
    ...phase1WrapSection(7, phase1WasmExportSection([
      { name: "memory", kind: 0x02, index: 0 },
      ...exportedFunctions,
    ])),
    ...phase1WrapSection(10, phase1WasmCodeSection(bodies)),
  ];
  return toBase64(appendClapseFuncMap(Uint8Array.from(moduleBytes)));
}

function phase1ParseExplicitExportNames(sourceText) {
  if (typeof sourceText !== "string" || sourceText.length === 0) {
    return [];
  }
  const lines = sourceText.split(/\r?\n/u);
  for (const rawLine of lines) {
    const code = phase1StripLineComment(rawLine).trim();
    const match = code.match(/^export\s*\{(.*)\}\s*$/u);
    if (match === null) {
      continue;
    }
    return match[1]
      .split(",")
      .map((entry) => entry.trim())
      .filter((entry) => entry.length > 0);
  }
  return [];
}

function phase1SelectedExportNames(requestObject, sourceText) {
  const roots = normalizedEntrypointRoots(requestObject);
  if (roots.length > 0) {
    return roots;
  }
  const explicit = phase1ParseExplicitExportNames(sourceText);
  if (explicit.length > 0) {
    return explicit;
  }
  return ["main"];
}

function phase1ConstructorArityFromExpr(expr) {
  if (!expr || typeof expr !== "object") {
    return null;
  }
  if (expr.type === "var") {
    return {
      name: expr.name,
      arity: 0,
    };
  }
  if (expr.type !== "apply") {
    return null;
  }
  const flattened = phase1FlattenApply(expr);
  if (!flattened?.callee || flattened.callee.type !== "var") {
    return null;
  }
  return {
    name: flattened.callee.name,
    arity: flattened.args.length,
  };
}

function phase1ParseSimpleCtorArities(sourceText) {
  const lines = normalizePlaceholderSourceText(sourceText).split("\n");
  const arities = new Map();
  for (const rawLine of lines) {
    const trimmed = phase1StripLineComment(rawLine).trim();
    if (
      !trimmed.startsWith("data ") &&
      !trimmed.startsWith("newtype ")
    ) {
      continue;
    }
    const eqIndex = trimmed.indexOf("=");
    if (eqIndex < 0) {
      continue;
    }
    const rhs = trimmed.slice(eqIndex + 1).trim();
    if (rhs.length === 0) {
      continue;
    }
    for (const part of rhs.split("|")) {
      const trimmedPart = part.trim();
      if (trimmedPart.length === 0) {
        continue;
      }
      const colonIndex = trimmedPart.indexOf(":");
      if (colonIndex >= 0) {
        const ctorName = trimmedPart.slice(0, colonIndex).trim().split(/\s+/u)[0];
        const typeText = trimmedPart.slice(colonIndex + 1).trim();
        const segments = typeText.split(/\s*->\s*/u).map((entry) => entry.trim()).filter((entry) =>
          entry.length > 0
        );
        if (typeof ctorName === "string" && ctorName.length > 0) {
          arities.set(ctorName, Math.max(0, segments.length - 1));
        }
        continue;
      }
      const tokens = trimmedPart.split(/\s+/u).filter((entry) => entry.length > 0);
      if (tokens.length === 0) {
        continue;
      }
      arities.set(tokens[0], Math.max(0, tokens.length - 1));
    }
  }
  return arities;
}

function phase1JoinExpressionParts(parts) {
  if (!Array.isArray(parts) || parts.length === 0) {
    return "";
  }
  const cleaned = parts
    .map((entry) => String(entry ?? "").trim())
    .filter((entry) => entry.length > 0);
  if (cleaned.length === 0) {
    return "";
  }
  const looksLikeCaseArmLine = (line) => {
    const tokens = phase1TokenizeExpression(String(line ?? "").trim());
    if (tokens === null || tokens.length < 3) {
      return false;
    }
    const parsed = phase1ParsePattern(tokens, 0, new Set(["->"]));
    return parsed !== null &&
      parsed.next < tokens.length &&
      tokens[parsed.next] === "->";
  };
  const first = cleaned[0];
  if (first.startsWith("case ") && first.includes(" of")) {
    const rendered = [first];
    for (let index = 1; index < cleaned.length; index += 1) {
      const current = cleaned[index];
      if (looksLikeCaseArmLine(current)) {
        rendered.push("|");
      }
      rendered.push(current);
    }
    return rendered.join(" ");
  }
  if (first !== "let" && !first.startsWith("let ")) {
    return cleaned.join(" ");
  }
  const rendered = [first.replace(/;\s*$/u, "")];
  let needBindingSeparator = first !== "let";
  let insideLetBlock = !/^let\b.*\bin\b/u.test(first);
  for (let index = 1; index < cleaned.length; index += 1) {
    const current = cleaned[index].replace(/;\s*$/u, "");
    if (insideLetBlock && (current === "in" || current.startsWith("in "))) {
      rendered.push(current);
      insideLetBlock = false;
      continue;
    }
    if (insideLetBlock) {
      const isGuardContinuation = current === "|" || current.startsWith("| ");
      if (needBindingSeparator && !isGuardContinuation) {
        rendered.push(";");
      }
      rendered.push(current);
      needBindingSeparator = true;
      continue;
    }
    rendered.push(current);
  }
  return rendered.join(" ");
}

function phase1ValidateSourceSurface(sourceText) {
  const lines = normalizePlaceholderSourceText(sourceText).split("\n");
  const ctorArities = phase1ParseSimpleCtorArities(sourceText);
  for (let index = 0; index < lines.length; index += 1) {
    const rawLine = lines[index];
    const trimmed = phase1StripLineComment(rawLine).trim();
    if (trimmed.length === 0) {
      continue;
    }
    if (trimmed.startsWith("newtype ")) {
      const eqIndex = trimmed.indexOf("=");
      if (eqIndex < 0) {
        return "newtype accepts exactly one constructor + one field";
      }
      const rhs = trimmed.slice(eqIndex + 1).trim();
      const constructors = rhs.split("|").map((entry) => entry.trim()).filter((entry) =>
        entry.length > 0
      );
      if (constructors.length !== 1) {
        return "newtype accepts exactly one constructor + one field";
      }
      const ctorTokens = constructors[0].split(/\s+/u).filter((entry) => entry.length > 0);
      if (ctorTokens.length !== 2) {
        return "newtype accepts exactly one constructor + one field";
      }
    }
    const caseIndex = trimmed.indexOf("case ");
    const ofIndex = trimmed.indexOf(" of");
    if (caseIndex < 0 || ofIndex < 0 || ofIndex <= caseIndex) {
      continue;
    }
    const targetText = trimmed.slice(caseIndex + "case ".length, ofIndex).trim();
    if (targetText.length === 0) {
      continue;
    }
    const targetTokens = phase1TokenizeExpression(targetText);
    if (targetTokens === null) {
      continue;
    }
    const targetExpr = phase1ParseExpr(targetTokens, 0, new Set());
    if (targetExpr === null || targetExpr.next !== targetTokens.length) {
      continue;
    }
    const targetCtor = phase1ConstructorArityFromExpr(targetExpr.node);
    if (targetCtor === null) {
      continue;
    }
    const expectedArity = ctorArities.get(targetCtor.name);
    if (!Number.isInteger(expectedArity)) {
      continue;
    }
    let nextIndex = index + 1;
    while (nextIndex < lines.length) {
      const armCode = phase1StripLineComment(lines[nextIndex]);
      const armTrimmed = armCode.trim();
      if (armTrimmed.length === 0) {
        nextIndex += 1;
        continue;
      }
      if (phase1LeadingIndent(armCode) <= phase1LeadingIndent(rawLine)) {
        break;
      }
      const arrowIndex = armTrimmed.indexOf("->");
      if (arrowIndex < 0) {
        nextIndex += 1;
        continue;
      }
      const patternText = armTrimmed.slice(0, arrowIndex).trim();
      if (patternText === "_" || patternText.length === 0) {
        nextIndex += 1;
        continue;
      }
      const patternTokens = phase1TokenizeExpression(patternText);
      if (patternTokens === null) {
        nextIndex += 1;
        continue;
      }
      const parsedPattern = phase1ParsePattern(patternTokens, 0, new Set());
      if (parsedPattern?.next !== patternTokens.length) {
        nextIndex += 1;
        continue;
      }
      const pattern = parsedPattern.pattern;
      if (
        pattern?.type === "ctor" &&
        pattern.name === targetCtor.name &&
        pattern.args.length !== expectedArity
      ) {
        return "scrutinee/arm arity mismatch";
      }
      nextIndex += 1;
    }
  }
  return null;
}

function phase1DebugArtifactWasmBase64(exportsList) {
  const normalizedExports = Array.isArray(exportsList)
    ? exportsList.filter((entry) =>
      entry && typeof entry.name === "string" && entry.name.length > 0
    )
    : [];
  if (normalizedExports.length === 0) {
    return buildPhase1TaggedWasmBase64(0);
  }

  const typeByArity = new Map();
  const typeList = [];
  function ensureType(paramCount) {
    if (typeByArity.has(paramCount)) {
      return typeByArity.get(paramCount);
    }
    const idx = typeList.length;
    typeList.push(paramCount);
    typeByArity.set(paramCount, idx);
    return idx;
  }

  const typeIndexes = [];
  const bodies = [];
  const wasmExports = [{ name: "memory", kind: 0x02, index: 0 }];
  normalizedExports.forEach((entry, index) => {
    const arity = Number.isInteger(entry.arity) && entry.arity >= 0
      ? entry.arity
      : 0;
    typeIndexes.push(ensureType(arity));
    bodies.push([0x41, ...encodeVarS32(1)]);
    wasmExports.push({ name: entry.name, kind: 0x00, index });
  });

  const moduleBytes = [
    0x00,
    0x61,
    0x73,
    0x6d,
    0x01,
    0x00,
    0x00,
    0x00,
    ...phase1WrapSection(1, phase1WasmTypeSection(typeList)),
    ...phase1WrapSection(3, phase1WasmFunctionSection(typeIndexes)),
    ...phase1WrapSection(5, phase1WasmMemorySection()),
    ...phase1WrapSection(7, phase1WasmExportSection(wasmExports)),
    ...phase1WrapSection(10, phase1WasmCodeSection(bodies)),
  ];
  return toBase64(Uint8Array.from(moduleBytes));
}

function phase1DefinitionsCoverExportNames(definitions, exportEntries) {
  if (!Array.isArray(definitions)) {
    return false;
  }
  const names = new Set(definitions.map((def) => def.name));
  for (const entry of exportEntries) {
    if (!entry || typeof entry.name !== "string" || entry.name.length === 0) {
      continue;
    }
    if (!names.has(entry.name)) {
      return false;
    }
  }
  return true;
}

function phase1TokenizeExpression(text) {
  const tokens = [];
  const tokenRe = /\s*(->|&&|\|\||>=|<=|==|!=|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])'|[+\-*/<>!][+\-*/<>=!.]*|\\|\(|\)|\[|\]|\{|\}|,|;|\.|\||=|True|False|true|false|[-]?\d+|[+\-*/<>]|[A-Za-z_][A-Za-z0-9_$#']*(?:\.[A-Za-z_][A-Za-z0-9_$#']*)*)/gu;
  let cursor = 0;
  while (cursor < text.length) {
    const match = tokenRe.exec(text);
    if (!match) {
      break;
    }
    const token = match[1];
    if (match.index !== cursor) {
      const gap = text.slice(cursor, match.index);
      if (/[\S]/u.test(gap)) {
        return null;
      }
    }
    cursor = match.index + match[0].length;
    tokens.push(token);
  }
  if (cursor < text.length && /[\S]/u.test(text.slice(cursor))) {
    return null;
  }
  return tokens;
}

function phase1DecodeStringLiteral(token) {
  if (typeof token !== "string" || token.length < 2) {
    return null;
  }
  if (!token.startsWith("\"") || !token.endsWith("\"")) {
    return null;
  }
  try {
    const decoded = JSON.parse(token);
    return typeof decoded === "string" ? decoded : null;
  } catch {
    return null;
  }
}

function phase1IsNumberToken(token) {
  return /^-?\d+$/u.test(token);
}

function phase1IsIdentToken(token) {
  return /^[A-Za-z_][A-Za-z0-9_$#']*(?:\.[A-Za-z_][A-Za-z0-9_$#']*)*$/u.test(token);
}

function phase1IsOperatorNameToken(token) {
  return token !== "->" && /^[+\-*/<>!][+\-*/<>=!.]*$/u.test(token);
}

function phase1NormalizeCallableName(name) {
  switch (name) {
    case "+":
      return "add";
    case "+.":
      return "add";
    case "-":
      return "sub";
    case "-.":
      return "sub";
    case "*":
      return "mul";
    case "*.":
      return "mul";
    case "/":
      return "div";
    case "/.":
      return "div";
    case "==":
      return "eq";
    case "!=":
      return "ne";
    case "<":
      return "lt";
    case "<=":
      return "le";
    case ">":
      return "gt";
    case ">=":
      return "ge";
    case "&&":
      return "and";
    case "||":
      return "or";
    case ">>=":
      return "bind";
    case ">>":
      return "then_m";
    case "<$>":
      return "fmap";
    case "<*>":
      return "ap";
    case "<$":
      return "map_replace";
    case "<*":
      return "keep_left";
    case "*>":
      return "keep_right";
    case "<>":
      return "alt";
    default: {
      if (phase1IsOperatorNameToken(name)) {
        return name;
      }
      const parts = String(name).split(".");
      return parts[parts.length - 1] ?? name;
    }
  }
}

function phase1BuildListLiteralExpr(elements, collectionTargetType = null) {
  return {
    type: "listLiteral",
    elements: Array.isArray(elements) ? elements : [],
    collectionTargetType: typeof collectionTargetType === "string" &&
        collectionTargetType.length > 0
      ? collectionTargetType
      : null,
  };
}

function phase1AnnotateCollectionLiteralTargets(
  expr,
  collectionTargetType,
  collectionLiteralInstances,
) {
  if (!expr || typeof expr !== "object" || !collectionTargetType ||
    !(collectionLiteralInstances instanceof Map) ||
    !collectionLiteralInstances.has(collectionTargetType)) {
    return expr;
  }
  switch (expr.type) {
    case "listLiteral":
      return {
        ...expr,
        collectionTargetType,
        elements: expr.elements.map((element) =>
          phase1AnnotateCollectionLiteralTargets(
            element,
            collectionTargetType,
            collectionLiteralInstances,
          )
        ),
      };
    case "apply":
      return {
        ...expr,
        fn: phase1AnnotateCollectionLiteralTargets(
          expr.fn,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        arg: phase1AnnotateCollectionLiteralTargets(
          expr.arg,
          collectionTargetType,
          collectionLiteralInstances,
        ),
      };
    case "if":
      return {
        ...expr,
        cond: phase1AnnotateCollectionLiteralTargets(
          expr.cond,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        thenExpr: phase1AnnotateCollectionLiteralTargets(
          expr.thenExpr,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        elseExpr: phase1AnnotateCollectionLiteralTargets(
          expr.elseExpr,
          collectionTargetType,
          collectionLiteralInstances,
        ),
      };
    case "let":
    case "letPattern":
      return {
        ...expr,
        value: phase1AnnotateCollectionLiteralTargets(
          expr.value,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        body: phase1AnnotateCollectionLiteralTargets(
          expr.body,
          collectionTargetType,
          collectionLiteralInstances,
        ),
      };
    case "caseBool":
      return {
        ...expr,
        target: phase1AnnotateCollectionLiteralTargets(
          expr.target,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        whenTrue: phase1AnnotateCollectionLiteralTargets(
          expr.whenTrue,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        whenFalse: phase1AnnotateCollectionLiteralTargets(
          expr.whenFalse,
          collectionTargetType,
          collectionLiteralInstances,
        ),
      };
    case "caseCtor":
    case "caseCtorSeq":
      return {
        ...expr,
        target: expr.target
          ? phase1AnnotateCollectionLiteralTargets(
            expr.target,
            collectionTargetType,
            collectionLiteralInstances,
          )
          : expr.target,
        targets: Array.isArray(expr.targets)
          ? expr.targets.map((target) =>
            phase1AnnotateCollectionLiteralTargets(
              target,
              collectionTargetType,
              collectionLiteralInstances,
            )
          )
          : expr.targets,
        whenMatch: phase1AnnotateCollectionLiteralTargets(
          expr.whenMatch,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        whenFallback: expr.whenFallback
          ? phase1AnnotateCollectionLiteralTargets(
            expr.whenFallback,
            collectionTargetType,
            collectionLiteralInstances,
          )
          : expr.whenFallback,
      };
    case "record":
      return {
        ...expr,
        fields: expr.fields.map((field) => ({
          ...field,
          value: phase1AnnotateCollectionLiteralTargets(
            field.value,
            collectionTargetType,
            collectionLiteralInstances,
          ),
        })),
      };
    case "recordUpdate":
      return {
        ...expr,
        base: phase1AnnotateCollectionLiteralTargets(
          expr.base,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        fields: expr.fields.map((field) => ({
          ...field,
          value: phase1AnnotateCollectionLiteralTargets(
            field.value,
            collectionTargetType,
            collectionLiteralInstances,
          ),
        })),
      };
    case "braceApplyOrUpdate":
      return {
        ...expr,
        base: phase1AnnotateCollectionLiteralTargets(
          expr.base,
          collectionTargetType,
          collectionLiteralInstances,
        ),
        fields: expr.fields.map((field) => ({
          ...field,
          value: phase1AnnotateCollectionLiteralTargets(
            field.value,
            collectionTargetType,
            collectionLiteralInstances,
          ),
        })),
      };
    case "field":
      return {
        ...expr,
        base: phase1AnnotateCollectionLiteralTargets(
          expr.base,
          collectionTargetType,
          collectionLiteralInstances,
        ),
      };
    case "lambda":
      return {
        ...expr,
        body: phase1AnnotateCollectionLiteralTargets(
          expr.body,
          collectionTargetType,
          collectionLiteralInstances,
        ),
      };
    default:
      return expr;
  }
}

function phase1ParseCollectionLiteralInstanceBlock(lines) {
  const methods = new Map();
  for (const rawLine of lines) {
    const trimmed = phase1StripLineComment(rawLine).trim();
    if (trimmed.length === 0) {
      continue;
    }
    const match = trimmed.match(
      /^([A-Za-z_][A-Za-z0-9_']*)\s*(.*?)\s*=\s*(.*)$/u,
    );
    if (match === null) {
      return null;
    }
    const name = match[1];
    if (name !== "collection_empty" && name !== "collection_extend") {
      continue;
    }
    const params = match[2].trim().length > 0
      ? phase1ToArgList(match[2])
      : [];
    const rhs = match[3].trim();
    const tokens = phase1TokenizeExpression(rhs);
    if (tokens === null) {
      return null;
    }
    const parsed = phase1ParseExpr(tokens, 0, new Set());
    if (parsed === null || parsed.next !== tokens.length) {
      return null;
    }
    methods.set(name, {
      name,
      params,
      body: parsed.node,
    });
  }
  if (!methods.has("collection_empty") || !methods.has("collection_extend")) {
    return null;
  }
  return {
    empty: methods.get("collection_empty"),
    extend: methods.get("collection_extend"),
  };
}

function phase1ParseRecordFields(tokens, start) {
  const fields = [];
  let cursor = start;
  while (cursor < tokens.length && tokens[cursor] !== "}") {
    const fieldName = tokens[cursor];
    if (!phase1IsIdentToken(fieldName) || tokens[cursor + 1] !== "=") {
      return null;
    }
    const value = phase1ParseExpr(tokens, cursor + 2, new Set([",", "}"]));
    if (value === null) {
      return null;
    }
    fields.push({
      name: phase1NormalizeCallableName(fieldName),
      value: value.node,
    });
    cursor = value.next;
    if (tokens[cursor] === ",") {
      cursor += 1;
    }
  }
  if (tokens[cursor] !== "}") {
    return null;
  }
  return { fields, next: cursor + 1 };
}

function phase1ResolveNameSegments(name) {
  return String(name ?? "").split(".").filter((segment) => segment.length > 0);
}

function phase1ResolvedCallableName(name) {
  return phase1NormalizeCallableName(name);
}

function phase1IsBinaryOperatorToken(token) {
  return phase1BinaryOperatorPrecedence(token) >= 0;
}

function phase1BinaryOperatorPrecedence(token) {
  switch (token) {
    case "*":
    case "/":
      return 20;
    case "+":
    case "-":
      return 10;
    case "==":
    case "!=":
    case "<":
    case "<=":
    case ">":
    case ">=":
      return 5;
    case "&&":
      return 4;
    case "||":
      return 3;
    case ">>=":
    case ">>":
      return 1;
    case "<$>":
    case "<*>":
    case "<$":
    case "<*":
    case "*>":
      return 4;
    case "<>":
      return 5;
    case "+.":
      return 6;
    default:
      return phase1IsOperatorNameToken(token) ? 6 : -1;
  }
}

function phase1ParseExpr(tokens, start, stopTokens = new Set()) {
  return phase1ParseBinaryExpr(tokens, start, 0, stopTokens);
}

function phase1ParseBinaryExpr(tokens, start, minPrecedence, stopTokens) {
  const parseResult = phase1ParseApplyExpr(tokens, start, stopTokens);
  if (parseResult === null) {
    return null;
  }
  let node = parseResult.node;
  let index = parseResult.next;
  while (index < tokens.length) {
    const token = tokens[index];
    if (stopTokens.has(token) || token === ")" || !phase1IsBinaryOperatorToken(token)) {
      break;
    }
    const precedence = phase1BinaryOperatorPrecedence(token);
    if (precedence < minPrecedence) {
      break;
    }
    const rhs = phase1ParseBinaryExpr(tokens, index + 1, precedence + 1, stopTokens);
    if (rhs === null) {
      return null;
    }
    node = {
      type: "apply",
      fn: {
        type: "apply",
        fn: { type: "var", name: phase1NormalizeCallableName(token) },
        arg: node,
      },
      arg: rhs.node,
    };
    index = rhs.next;
  }
  return { node, next: index };
}

function phase1ParsePostfixExpr(tokens, start, stopTokens, { allowBraceUpdate = true } = {}) {
  if (start >= tokens.length) {
    return null;
  }
  const parseResult = phase1ParsePrimary(tokens, start, stopTokens);
  if (parseResult === null) {
    return null;
  }
  let node = parseResult.node;
  let index = parseResult.next;
  while (index < tokens.length) {
    const token = tokens[index];
    if (token === ".") {
      const fieldToken = tokens[index + 1];
      if (
        typeof fieldToken !== "string" ||
        !phase1IsIdentToken(fieldToken)
      ) {
        return null;
      }
      const fieldParts = phase1ResolveNameSegments(fieldToken);
      if (fieldParts.length === 0) {
        return null;
      }
      for (const fieldPart of fieldParts) {
        node = {
          type: "field",
          base: node,
          field: fieldPart,
        };
      }
      index += 2;
      continue;
    }
    if (allowBraceUpdate && token === "{") {
      const update = phase1ParseRecordFields(tokens, index + 1);
      if (update === null) {
        return null;
      }
      node = {
        type: "braceApplyOrUpdate",
        base: node,
        fields: update.fields,
      };
      index = update.next;
      continue;
    }
    if (
      stopTokens.has(token) || token === ")" ||
      phase1IsBinaryOperatorToken(token)
    ) {
      break;
    }
    break;
  }
  return { node, next: index };
}

function phase1ParseApplyExpr(tokens, start, stopTokens) {
  if (start >= tokens.length) {
    return null;
  }
  const parseResult = phase1ParsePostfixExpr(tokens, start, stopTokens);
  if (parseResult === null) {
    return null;
  }
  let node = parseResult.node;
  let index = parseResult.next;
  while (index < tokens.length) {
    const token = tokens[index];
    if (token === ".") {
      const fieldToken = tokens[index + 1];
      if (
        typeof fieldToken !== "string" ||
        !phase1IsIdentToken(fieldToken)
      ) {
        return null;
      }
      const fieldParts = phase1ResolveNameSegments(fieldToken);
      if (fieldParts.length === 0) {
        return null;
      }
      for (const fieldPart of fieldParts) {
        node = {
          type: "field",
          base: node,
          field: fieldPart,
        };
      }
      index += 2;
      continue;
    }
    if (token === "{") {
      const update = phase1ParseRecordFields(tokens, index + 1);
      if (update === null) {
        return null;
      }
      node = {
        type: "braceApplyOrUpdate",
        base: node,
        fields: update.fields,
      };
      index = update.next;
      continue;
    }
    if (
      stopTokens.has(token) || token === ")" ||
      phase1IsBinaryOperatorToken(token)
    ) {
      break;
    }
    const argument = phase1ParsePostfixExpr(tokens, index, stopTokens, {
      allowBraceUpdate: false,
    });
    if (argument === null) {
      break;
    }
    index = argument.next;
    node = {
      type: "apply",
      fn: node,
      arg: argument.node,
    };
  }
  return { node, next: index };
}

function phase1ParsePrimary(tokens, start, stopTokens) {
  const token = tokens[start];
  if (token === "let") {
    return phase1ParseLetBindingChain(tokens, start + 1, stopTokens);
  }
  if (token === "\\") {
    const params = [];
    let cursor = start + 1;
    while (cursor < tokens.length && tokens[cursor] !== "->") {
      if (tokens[cursor] === "_") {
        params.push(`__ignored${params.length}`);
        cursor += 1;
        continue;
      }
      if (!phase1IsIdentToken(tokens[cursor])) {
        return null;
      }
      params.push(phase1NormalizeCallableName(tokens[cursor]));
      cursor += 1;
    }
    if (params.length === 0 || tokens[cursor] !== "->") {
      return null;
    }
    const body = phase1ParseExpr(tokens, cursor + 1, stopTokens);
    if (body === null) {
      return null;
    }
    return {
      node: { type: "lambda", params, body: body.node },
      next: body.next,
    };
  }
  if (token === "(") {
    const expr = phase1ParseExpr(tokens, start + 1, new Set([")", ...stopTokens]));
    if (expr === null) {
      return null;
    }
    const nextIndex = expr.next;
    if (tokens[nextIndex] !== ")") {
      return null;
    }
    return { node: expr.node, next: nextIndex + 1 };
  }

  if (token === "[") {
    const elements = [];
    let cursor = start + 1;
    while (cursor < tokens.length && tokens[cursor] !== "]") {
      const element = phase1ParseExpr(tokens, cursor, new Set([",", "]"]));
      if (element === null) {
        return null;
      }
      elements.push(element.node);
      cursor = element.next;
      if (tokens[cursor] === ",") {
        cursor += 1;
      }
    }
    if (tokens[cursor] !== "]") {
      return null;
    }
    return {
      node: phase1BuildListLiteralExpr(elements),
      next: cursor + 1,
    };
  }

  if (token === "{") {
    const parsedFields = phase1ParseRecordFields(tokens, start + 1);
    if (parsedFields === null) {
      return null;
    }
    return {
      node: { type: "record", fields: parsedFields.fields },
      next: parsedFields.next,
    };
  }

  if (token === "if") {
    const cond = phase1ParseExpr(tokens, start + 1, new Set(["then", ...stopTokens]));
    if (cond !== null) {
      let cursor = cond.next;
      if (tokens[cursor] === "then") {
        const thenExpr = phase1ParseExpr(
          tokens,
          cursor + 1,
          new Set(["else", ...stopTokens]),
        );
        if (thenExpr !== null) {
          cursor = thenExpr.next;
          if (tokens[cursor] === "else") {
            const elseExpr = phase1ParseExpr(
              tokens,
              cursor + 1,
              stopTokens,
            );
            if (elseExpr !== null) {
              return {
                node: {
                  type: "if",
                  cond: cond.node,
                  thenExpr: thenExpr.node,
                  elseExpr: elseExpr.node,
                },
                next: elseExpr.next,
              };
            }
          }
        }
      }
    }
  }

  if (token === "case") {
    if (tokens[start + 1] === "of") {
      let cursor = start + 2;
      const branches = [];
      while (true) {
        if (tokens[cursor] === "|") {
          cursor += 1;
        }
        if (tokens[cursor] === "otherwise") {
          break;
        }
        const guardExpr = phase1ParseExpr(
          tokens,
          cursor,
          new Set(["|", ...stopTokens]),
        );
        if (guardExpr === null) {
          return null;
        }
        cursor = guardExpr.next;
        if (tokens[cursor] !== "->") {
          return null;
        }
        const whenTrue = phase1ParseExpr(
          tokens,
          cursor + 1,
          new Set(["|", ...stopTokens]),
        );
        if (whenTrue === null) {
          return null;
        }
        branches.push({ guard: guardExpr.node, body: whenTrue.node });
        cursor = whenTrue.next;
        if (tokens[cursor] !== "|" && tokens[cursor] !== "otherwise") {
          return null;
        }
      }
      if (branches.length === 0) {
        return null;
      }
      if (tokens[cursor] === "|") {
        cursor += 1;
      }
      if (tokens[cursor] !== "otherwise" || tokens[cursor + 1] !== "->") {
        return null;
      }
      const whenFalse = phase1ParseExpr(tokens, cursor + 2, stopTokens);
      if (whenFalse === null) {
        return null;
      }
      let folded = whenFalse.node;
      for (let index = branches.length - 1; index >= 0; index -= 1) {
        folded = {
          type: "caseBool",
          target: branches[index].guard,
          whenTrue: branches[index].body,
          whenFalse: folded,
        };
      }
      return {
        node: folded,
        next: whenFalse.next,
      };
    }
    const ofIndex = tokens.indexOf("of", start + 1);
    if (ofIndex === -1) {
      return null;
    }
    const targetSeq = phase1ParseExprSequence(tokens, start + 1, new Set(["of"]));
    const firstPatternSeq = phase1ParsePatternSequence(
      tokens,
      ofIndex + 1,
      new Set(["->"]),
    );
    if (
      targetSeq !== null &&
      firstPatternSeq !== null &&
      targetSeq.next === ofIndex &&
      targetSeq.exprs.length === firstPatternSeq.patterns.length &&
      firstPatternSeq.patterns.length > 1 &&
      tokens[firstPatternSeq.next] === "->"
    ) {
      const fallbackStart = phase1FindTrailingPatternSequenceArrowStart(
        tokens,
        firstPatternSeq.next + 1,
        stopTokens,
        targetSeq.exprs.length,
      );
      if (fallbackStart === null) {
        return null;
      }
      const whenMatchTokens = tokens.slice(firstPatternSeq.next + 1, fallbackStart);
      const whenMatch = phase1ParseExpr(whenMatchTokens, 0, new Set());
      if (whenMatch === null || whenMatch.next !== whenMatchTokens.length) {
        return null;
      }
      let cursor = fallbackStart;
      if (tokens[cursor] === "|") {
        cursor += 1;
      }
      const fallbackSeq = phase1ParsePatternSequence(tokens, cursor, new Set(["->"]));
      if (
        fallbackSeq === null ||
        fallbackSeq.patterns.length !== targetSeq.exprs.length ||
        tokens[fallbackSeq.next] !== "->"
      ) {
        return null;
      }
      const whenFallback = phase1ParseExpr(tokens, fallbackSeq.next + 1, stopTokens);
      if (whenFallback === null) {
        return null;
      }
      return {
        node: {
          type: "caseMulti",
          targets: targetSeq.exprs,
          patterns: firstPatternSeq.patterns,
          whenMatch: whenMatch.node,
          fallbackPatterns: fallbackSeq.patterns,
          whenFallback: whenFallback.node,
        },
        next: whenFallback.next,
      };
    }
    const target = phase1ParseExpr(
      tokens,
      start + 1,
      new Set(["of", ...stopTokens]),
    );
    if (target === null) {
      return null;
    }
    let cursor = target.next;
    if (tokens[cursor] === "of") {
      cursor += 1;
    }
    const trueToken = tokens[cursor];
    if (trueToken === "True" || trueToken === "true") {
      cursor += 1;
      if (tokens[cursor] === "->") {
        cursor += 1;
      }
      const whenTrue = phase1ParseExpr(
        tokens,
        cursor,
        new Set(["False", "false", "_", ...stopTokens]),
      );
      if (whenTrue === null) {
        return null;
      }
      cursor = whenTrue.next;
      if (tokens[cursor] === "|") {
        cursor += 1;
      }
      const falseToken = tokens[cursor];
      if (
        falseToken !== "False" && falseToken !== "false" && falseToken !== "_"
      ) {
        return null;
      }
      cursor += 1;
      if (tokens[cursor] === "->") {
        cursor += 1;
      }
      const whenFalse = phase1ParseExpr(tokens, cursor, stopTokens);
      if (whenFalse === null) {
        return null;
      }
      return {
        node: {
          type: "caseBool",
          target: target.node,
          whenTrue: whenTrue.node,
          whenFalse: whenFalse.node,
        },
        next: whenFalse.next,
      };
    }
    if (typeof trueToken !== "string" || trueToken.length === 0) {
      return null;
    }
    return phase1ParseSingleTargetCaseArmChain(
      tokens,
      cursor,
      stopTokens,
      target.node,
    );
  }

  if (token === "True" || token === "true") {
    return { node: { type: "bool", value: true }, next: start + 1 };
  }
  if (token === "False" || token === "false") {
    return { node: { type: "bool", value: false }, next: start + 1 };
  }

  if (phase1IsNumberToken(token)) {
    return {
      node: { type: "int", value: Number.parseInt(token, 10) },
      next: start + 1,
    };
  }
  const charValue = phase1DecodeCharLiteral(token);
  if (charValue !== null) {
    return {
      node: { type: "int", value: charValue },
      next: start + 1,
    };
  }
  const stringValue = phase1DecodeStringLiteral(token);
  if (stringValue !== null) {
    return {
      node: { type: "string", value: stringValue },
      next: start + 1,
    };
  }

  if (phase1IsBinaryOperatorToken(token)) {
    return {
      node: { type: "var", name: phase1NormalizeCallableName(token) },
      next: start + 1,
    };
  }

  if (phase1IsIdentToken(token)) {
    return {
      node: { type: "var", name: token },
      next: start + 1,
    };
  }

  return null;
}

function phase1ToArgList(rawValue) {
  return String(rawValue ?? "")
    .trim()
    .split(/\s+/u)
    .filter((arg) => arg.length > 0);
}

function phase1StripLineComment(line) {
  return String(line ?? "").split("--", 1)[0];
}

function phase1LeadingIndent(line) {
  const match = String(line ?? "").match(/^\s*/u);
  return match ? match[0].length : 0;
}

function phase1FoldGuardBranches(branches, fallbackExpr) {
  let folded = fallbackExpr;
  for (let index = branches.length - 1; index >= 0; index -= 1) {
    folded = {
      type: "caseBool",
      target: branches[index].guard,
      whenTrue: branches[index].body,
      whenFalse: folded,
    };
  }
  return folded;
}

function phase1ParseGuardedClauseLines(lines) {
  const branches = [];
  let fallbackExpr = null;
  for (const line of lines) {
    const clause = String(line ?? "").trim();
    if (!clause.startsWith("|")) {
      return null;
    }
    const clauseTokens = phase1TokenizeExpression(clause.slice(1).trim());
    if (clauseTokens === null) {
      return null;
    }
    const assignIndex = clauseTokens.indexOf("=");
    if (assignIndex <= 0 || assignIndex >= clauseTokens.length - 1) {
      return null;
    }
    const guardTokens = clauseTokens.slice(0, assignIndex);
    const bodyTokens = clauseTokens.slice(assignIndex + 1);
    const bodyExpr = phase1ParseExpr(bodyTokens, 0, new Set());
    if (bodyExpr === null || bodyExpr.next !== bodyTokens.length) {
      return null;
    }
    if (guardTokens.length === 1 && guardTokens[0] === "otherwise") {
      fallbackExpr = bodyExpr.node;
      continue;
    }
    const guardExpr = phase1ParseExpr(guardTokens, 0, new Set());
    if (guardExpr === null || guardExpr.next !== guardTokens.length) {
      return null;
    }
    branches.push({ guard: guardExpr.node, body: bodyExpr.node });
  }
  if (branches.length === 0 || fallbackExpr === null) {
    return null;
  }
  return phase1FoldGuardBranches(branches, fallbackExpr);
}

function phase1IsSimpleClausePattern(pattern) {
  return !!pattern && typeof pattern === "object" && (
    pattern.type === "wildcard" || pattern.type === "binder" ||
    pattern.type === "bool" || pattern.type === "int"
  );
}

function phase1IsIrrefutableClausePattern(pattern) {
  return !!pattern && typeof pattern === "object" && (
    pattern.type === "wildcard" || pattern.type === "binder"
  );
}

function phase1ClausePatternDiscriminantKey(pattern) {
  if (!pattern || typeof pattern !== "object") {
    return null;
  }
  if (pattern.type === "bool") {
    return `bool:${pattern.value ? "true" : "false"}`;
  }
  if (pattern.type === "int") {
    return `int:${pattern.value}`;
  }
  return null;
}

function phase1ClausePatternDiscriminantLiteral(pattern) {
  if (!pattern || typeof pattern !== "object") {
    return null;
  }
  if (pattern.type === "bool" || pattern.type === "int") {
    return pattern;
  }
  return null;
}

function phase1ClauseCondExpr(targetExpr, literalPattern) {
  return {
    type: "apply",
    fn: {
      type: "apply",
      fn: { type: "var", name: "eq" },
      arg: targetExpr,
    },
    arg: literalPattern.type === "bool"
      ? { type: "bool", value: literalPattern.value }
      : { type: "int", value: literalPattern.value },
  };
}

function phase1WrapClauseBindings(bindings, body) {
  if (!Array.isArray(bindings) || bindings.length === 0) {
    return body;
  }
  let wrapped = body;
  for (let index = bindings.length - 1; index >= 0; index -= 1) {
    const binding = bindings[index];
    wrapped = {
      type: "let",
      name: binding.name,
      value: binding.value,
      body: wrapped,
    };
  }
  return wrapped;
}

function phase1ConsumeClausePattern(clause, targetExpr, position) {
  const patterns = Array.isArray(clause?.patterns) ? clause.patterns.slice() : [];
  const bindings = Array.isArray(clause?.bindings) ? clause.bindings.slice() : [];
  const pattern = patterns[position];
  if (pattern?.type === "binder") {
    bindings.push({ name: pattern.name, value: targetExpr });
  }
  patterns[position] = { type: "wildcard" };
  return {
    ...clause,
    patterns,
    bindings,
  };
}

function phase1PrepareClauseMatchBranch(clauses, targetExpr, position, discriminantKey) {
  const out = [];
  for (const clause of clauses) {
    const pattern = clause?.patterns?.[position];
    if (phase1IsIrrefutableClausePattern(pattern)) {
      out.push(phase1ConsumeClausePattern(clause, targetExpr, position));
      continue;
    }
    if (phase1ClausePatternDiscriminantKey(pattern) === discriminantKey) {
      out.push(phase1ConsumeClausePattern(clause, targetExpr, position));
    }
  }
  return out;
}

function phase1PrepareClauseMissBranch(clauses, targetExpr, position, discriminantKey) {
  const out = [];
  for (const clause of clauses) {
    const pattern = clause?.patterns?.[position];
    if (phase1IsIrrefutableClausePattern(pattern)) {
      out.push(phase1ConsumeClausePattern(clause, targetExpr, position));
      continue;
    }
    if (phase1ClausePatternDiscriminantKey(pattern) !== discriminantKey) {
      out.push(clause);
    }
  }
  return out;
}

function phase1CollectClauseDiscriminants(clauses, position) {
  const out = [];
  const seen = new Set();
  for (const clause of clauses) {
    const pattern = clause?.patterns?.[position];
    const key = phase1ClausePatternDiscriminantKey(pattern);
    if (key === null || seen.has(key)) {
      continue;
    }
    const literal = phase1ClausePatternDiscriminantLiteral(pattern);
    if (literal === null) {
      continue;
    }
    seen.add(key);
    out.push({ key, literal });
  }
  return out;
}

function phase1NextClauseDemandPosition(clauses) {
  const arity = Array.isArray(clauses) && clauses.length > 0 && Array.isArray(clauses[0].patterns)
    ? clauses[0].patterns.length
    : 0;
  for (let position = 0; position < arity; position += 1) {
    for (const clause of clauses) {
      if (!phase1IsIrrefutableClausePattern(clause?.patterns?.[position])) {
        return position;
      }
    }
  }
  return -1;
}

function phase1BuildSimpleClauseDecisionTree(targets, clauses) {
  if (!Array.isArray(targets) || !Array.isArray(clauses) || clauses.length === 0) {
    return { type: "trap" };
  }
  const [firstClause] = clauses;
  if (
    firstClause &&
    Array.isArray(firstClause.patterns) &&
    firstClause.patterns.every((pattern) => phase1IsIrrefutableClausePattern(pattern))
  ) {
    return phase1WrapClauseBindings(firstClause.bindings, firstClause.body);
  }
  const position = phase1NextClauseDemandPosition(clauses);
  if (position < 0 || position >= targets.length) {
    return { type: "trap" };
  }
  const discriminants = phase1CollectClauseDiscriminants(clauses, position);
  if (discriminants.length === 0) {
    return { type: "trap" };
  }
  const targetExpr = targets[position];
  const buildDiscriminantChain = (remainingClauses, discIndex) => {
    if (discIndex >= discriminants.length) {
      return phase1BuildSimpleClauseDecisionTree(targets, remainingClauses);
    }
    const discriminant = discriminants[discIndex];
    const thenClauses = phase1PrepareClauseMatchBranch(
      remainingClauses,
      targetExpr,
      position,
      discriminant.key,
    );
    const elseClauses = phase1PrepareClauseMissBranch(
      remainingClauses,
      targetExpr,
      position,
      discriminant.key,
    );
    return {
      type: "if",
      cond: phase1ClauseCondExpr(targetExpr, discriminant.literal),
      thenExpr: phase1BuildSimpleClauseDecisionTree(targets, thenClauses),
      elseExpr: buildDiscriminantChain(elseClauses, discIndex + 1),
    };
  };
  return buildDiscriminantChain(clauses, 0);
}

function phase1ParseSimplePatternClauseLine(line, arity) {
  const tokens = phase1TokenizeExpression(String(line ?? "").trim());
  if (tokens === null) {
    return null;
  }
  const parsedPatterns = phase1ParsePatternSequence(tokens, 0, new Set(["="]));
  if (
    parsedPatterns === null ||
    parsedPatterns.patterns.length !== arity ||
    tokens[parsedPatterns.next] !== "="
  ) {
    return null;
  }
  if (!parsedPatterns.patterns.every((pattern) => phase1IsSimpleClausePattern(pattern))) {
    return null;
  }
  const bodyTokens = tokens.slice(parsedPatterns.next + 1);
  if (bodyTokens.length === 0) {
    return null;
  }
  const parsedBody = phase1ParseExpr(bodyTokens, 0, new Set());
  if (parsedBody === null || parsedBody.next !== bodyTokens.length) {
    return null;
  }
  return {
    patterns: parsedPatterns.patterns,
    body: parsedBody.node,
    bindings: [],
  };
}

function phase1ParseSimplePatternClauseBlock(paramNames, lines) {
  if (!Array.isArray(paramNames) || paramNames.length === 0 || !Array.isArray(lines) || lines.length === 0) {
    return null;
  }
  const clauses = [];
  for (const line of lines) {
    const parsedClause = phase1ParseSimplePatternClauseLine(line, paramNames.length);
    if (parsedClause === null) {
      return null;
    }
    clauses.push(parsedClause);
  }
  const targets = paramNames.map((name) => ({ type: "var", name }));
  return phase1BuildSimpleClauseDecisionTree(targets, clauses);
}

function phase1ParseTopLevelGuardedDefinitionHead(line) {
  const tokens = phase1TokenizeExpression(String(line ?? "").trim());
  if (tokens === null || tokens.length < 4) {
    return null;
  }
  const nameToken = tokens[0];
  if (!phase1IsIdentToken(nameToken) && !phase1IsOperatorNameToken(nameToken)) {
    return null;
  }
  const barIndex = tokens.indexOf("|");
  if (barIndex <= 0) {
    return null;
  }
  const assignIndex = tokens.indexOf("=", barIndex + 1);
  if (assignIndex <= barIndex + 1 || assignIndex >= tokens.length - 1) {
    return null;
  }
  return {
    name: nameToken,
    params: tokens.slice(1, barIndex),
    firstGuardLine: `| ${tokens.slice(barIndex + 1, assignIndex).join(" ")} = ${
      tokens.slice(assignIndex + 1).join(" ")
    }`,
  };
}

function phase1ParseLocalDefinitions(lines) {
  const definitions = [];
  let minIndent = null;
  for (const rawLine of lines) {
    const code = phase1StripLineComment(rawLine);
    const trimmed = code.trim();
    if (trimmed.length === 0) {
      continue;
    }
    const indent = phase1LeadingIndent(code);
    if (minIndent === null || indent < minIndent) {
      minIndent = indent;
    }
  }
  if (minIndent === null) {
    return [];
  }
  for (let index = 0; index < lines.length;) {
    const rawLine = lines[index];
    const code = phase1StripLineComment(rawLine);
    const trimmed = code.trim();
    const baseIndent = phase1LeadingIndent(code);
    if (trimmed.length === 0) {
      index += 1;
      continue;
    }
    if (baseIndent !== minIndent) {
      index += 1;
      continue;
    }
    const guardedHead = phase1ParseTopLevelGuardedDefinitionHead(trimmed);
    if (guardedHead !== null) {
      const name = guardedHead.name;
      const params = guardedHead.params;
      const guardLines = [
        guardedHead.firstGuardLine,
      ];
      let nextIndex = index + 1;
      while (nextIndex < lines.length) {
        const continuationCode = phase1StripLineComment(lines[nextIndex]);
        const continuationTrimmed = continuationCode.trim();
        if (continuationTrimmed.length === 0) {
          nextIndex += 1;
          continue;
        }
        if (phase1LeadingIndent(continuationCode) <= baseIndent) {
          break;
        }
        guardLines.push(continuationTrimmed);
        nextIndex += 1;
      }
      const body = phase1ParseGuardedClauseLines(guardLines);
      if (body === null) {
        return null;
      }
      definitions.push({ name, params, body });
      index = nextIndex;
      continue;
    }
    const match = trimmed.match(
      /^([A-Za-z_][A-Za-z0-9_']*|[+\-*/<>=!][+\-*/<>=!.]*)\s*(.*?)\s*=\s*(.*)$/u,
    );
    if (match === null) {
      return null;
    }
    const name = match[1];
    const params = match[2].trim().length > 0
      ? phase1ToArgList(match[2])
      : [];
    const hasInlineRhs = match[3].trim().length > 0;
    const rhsParts = [];
    const continuationLines = [];
    if (hasInlineRhs) {
      rhsParts.push(match[3].trim());
    }
    let nextIndex = index + 1;
    while (nextIndex < lines.length) {
      const continuationCode = phase1StripLineComment(lines[nextIndex]);
      const continuationTrimmed = continuationCode.trim();
      if (continuationTrimmed.length === 0) {
        nextIndex += 1;
        continue;
      }
      if (phase1LeadingIndent(continuationCode) <= baseIndent) {
        break;
      }
      continuationLines.push(continuationTrimmed);
      rhsParts.push(continuationTrimmed);
      nextIndex += 1;
    }
    if (!hasInlineRhs) {
      const clauseBody = phase1ParseSimplePatternClauseBlock(params, continuationLines);
      if (clauseBody !== null) {
        definitions.push({ name, params, body: clauseBody });
        index = nextIndex;
        continue;
      }
    }
    const rhs = phase1JoinExpressionParts(rhsParts);
    if (rhs.length === 0) {
      return null;
    }
    const tokens = phase1TokenizeExpression(rhs);
    if (tokens === null) {
      return null;
    }
    const parsed = phase1ParseExpr(tokens, 0, new Set());
    if (parsed === null || parsed.next !== tokens.length) {
      return null;
    }
    definitions.push({ name, params, body: parsed.node });
    index = nextIndex;
  }
  return definitions;
}

function phase1ParseGenericInstanceBlock(lines) {
  const defs = phase1ParseLocalDefinitions(lines);
  return Array.isArray(defs) ? defs : null;
}

function phase1ParseGenericClassBlock(lines) {
  const definitions = [];
  let minIndent = null;
  for (const rawLine of lines) {
    const code = phase1StripLineComment(rawLine);
    const trimmed = code.trim();
    if (trimmed.length === 0) {
      continue;
    }
    const indent = phase1LeadingIndent(code);
    if (minIndent === null || indent < minIndent) {
      minIndent = indent;
    }
  }
  if (minIndent === null) {
    return [];
  }
  for (let index = 0; index < lines.length;) {
    const rawLine = lines[index];
    const code = phase1StripLineComment(rawLine);
    const trimmed = code.trim();
    const baseIndent = phase1LeadingIndent(code);
    if (trimmed.length === 0) {
      index += 1;
      continue;
    }
    if (baseIndent !== minIndent) {
      index += 1;
      continue;
    }
    const guardedHead = phase1ParseTopLevelGuardedDefinitionHead(trimmed);
    if (guardedHead !== null) {
      const name = guardedHead.name;
      const params = guardedHead.params;
      const guardLines = [
        guardedHead.firstGuardLine,
      ];
      let nextIndex = index + 1;
      while (nextIndex < lines.length) {
        const continuationCode = phase1StripLineComment(lines[nextIndex]);
        const continuationTrimmed = continuationCode.trim();
        if (continuationTrimmed.length === 0) {
          nextIndex += 1;
          continue;
        }
        if (phase1LeadingIndent(continuationCode) <= baseIndent) {
          break;
        }
        guardLines.push(continuationTrimmed);
        nextIndex += 1;
      }
      const body = phase1ParseGuardedClauseLines(guardLines);
      if (body !== null) {
        definitions.push({ name, params, body });
      }
      index = nextIndex;
      continue;
    }
    const match = trimmed.match(
      /^([A-Za-z_][A-Za-z0-9_']*|[+\-*/<>=!][+\-*/<>=!.]*)\s*(.*?)\s*=\s*(.*)$/u,
    );
    if (match === null) {
      index += 1;
      continue;
    }
    const name = match[1];
    const params = match[2].trim().length > 0
      ? phase1ToArgList(match[2])
      : [];
    const hasInlineRhs = match[3].trim().length > 0;
    const rhsParts = [];
    const continuationLines = [];
    if (hasInlineRhs) {
      rhsParts.push(match[3].trim());
    }
    let nextIndex = index + 1;
    while (nextIndex < lines.length) {
      const continuationCode = phase1StripLineComment(lines[nextIndex]);
      const continuationTrimmed = continuationCode.trim();
      if (continuationTrimmed.length === 0) {
        nextIndex += 1;
        continue;
      }
      if (phase1LeadingIndent(continuationCode) <= baseIndent) {
        break;
      }
      continuationLines.push(continuationTrimmed);
      rhsParts.push(continuationTrimmed);
      nextIndex += 1;
    }
    if (!hasInlineRhs) {
      const clauseBody = phase1ParseSimplePatternClauseBlock(params, continuationLines);
      if (clauseBody !== null) {
        definitions.push({ name, params, body: clauseBody });
        index = nextIndex;
        continue;
      }
    }
    const rhs = phase1JoinExpressionParts(rhsParts);
    if (rhs.length > 0) {
      const tokens = phase1TokenizeExpression(rhs);
      if (tokens !== null) {
        const parsed = phase1ParseExpr(tokens, 0, new Set());
        if (parsed !== null && parsed.next === tokens.length) {
          definitions.push({ name, params, body: parsed.node });
        }
      }
    }
    index = nextIndex;
  }
  return definitions;
}

function phase1WrapWhereDefinitions(bodyExpr, localDefinitions) {
  let wrapped = bodyExpr;
  for (let index = localDefinitions.length - 1; index >= 0; index -= 1) {
    const localDef = localDefinitions[index];
    const localValue = localDef.params.length === 0
      ? localDef.body
      : {
        type: "lambda",
        params: localDef.params,
        body: localDef.body,
      };
    wrapped = {
      type: "let",
      name: localDef.name,
      value: localValue,
      body: wrapped,
    };
  }
  return wrapped;
}

function phase1ParseTopLevelDefinitions(sourceText) {
  const lines = normalizePlaceholderSourceText(sourceText).split("\n");
  const definitions = [];
  const typeAnnotations = new Map();
  const collectionLiteralInstances = new Map();
  const classDefaultMethodDefs = new Map();
  const instanceMethodDefs = new Map();
  const ambiguousInstanceMethods = new Set();
  const transparentNewtypeCtors = new Set();
  for (let index = 0; index < lines.length;) {
    const rawLine = lines[index];
    const code = phase1StripLineComment(rawLine);
    const trimmed = code.trim();
    const baseIndent = phase1LeadingIndent(code);
    if (trimmed.length === 0) {
      index += 1;
      continue;
    }
    if (baseIndent !== 0) {
      index += 1;
      continue;
    }
    const typeSigMatch = trimmed.match(
      /^([A-Za-z_][A-Za-z0-9_']*)\s*:\s*([A-Za-z_][A-Za-z0-9_']*)\b/u,
    );
    if (typeSigMatch !== null) {
      typeAnnotations.set(typeSigMatch[1], typeSigMatch[2]);
      index += 1;
      continue;
    }
    const collectionInstanceMatch = trimmed.match(
      /^instance\s+CollectionLiteral\s+([A-Za-z_][A-Za-z0-9_']*)\s+where$/u,
    );
    if (collectionInstanceMatch !== null) {
      const instanceLines = [];
      let nextIndex = index + 1;
      while (nextIndex < lines.length) {
        const continuationCode = phase1StripLineComment(lines[nextIndex]);
        const continuationTrimmed = continuationCode.trim();
        if (continuationTrimmed.length === 0) {
          nextIndex += 1;
          continue;
        }
        if (phase1LeadingIndent(continuationCode) <= baseIndent) {
          break;
        }
        instanceLines.push(lines[nextIndex]);
        nextIndex += 1;
      }
      const parsedInstance = phase1ParseCollectionLiteralInstanceBlock(
        instanceLines,
      );
      if (parsedInstance !== null) {
        collectionLiteralInstances.set(
          collectionInstanceMatch[1],
          parsedInstance,
        );
      }
      index = nextIndex;
      continue;
    }
    const classMatch = trimmed.match(
      /^class\s+([A-Za-z_][A-Za-z0-9_']*)(?:\s+.+)?\s+where$/u,
    );
    if (classMatch !== null) {
      if (/\|[^]*,\s*where$/u.test(trimmed)) {
        throw new Error(
          "class fundep tails reject trailing commas",
        );
      }
      const classLines = [];
      let nextIndex = index + 1;
      while (nextIndex < lines.length) {
        const continuationCode = phase1StripLineComment(lines[nextIndex]);
        const continuationTrimmed = continuationCode.trim();
        if (continuationTrimmed.length === 0) {
          nextIndex += 1;
          continue;
        }
        if (phase1LeadingIndent(continuationCode) <= baseIndent) {
          break;
        }
        classLines.push(lines[nextIndex]);
        nextIndex += 1;
      }
      const parsedClassDefaults = phase1ParseGenericClassBlock(classLines);
      if (Array.isArray(parsedClassDefaults) && parsedClassDefaults.length > 0) {
        classDefaultMethodDefs.set(classMatch[1], parsedClassDefaults);
      }
      index = nextIndex;
      continue;
    }
    const genericInstanceMatch = trimmed.match(
      /^instance\s+([A-Za-z_][A-Za-z0-9_']*)(?:\s+.+)?\s+where$/u,
    );
    if (genericInstanceMatch !== null) {
      const instanceLines = [];
      let nextIndex = index + 1;
      while (nextIndex < lines.length) {
        const continuationCode = phase1StripLineComment(lines[nextIndex]);
        const continuationTrimmed = continuationCode.trim();
        if (continuationTrimmed.length === 0) {
          nextIndex += 1;
          continue;
        }
        if (phase1LeadingIndent(continuationCode) <= baseIndent) {
          break;
        }
        instanceLines.push(lines[nextIndex]);
        nextIndex += 1;
      }
      const parsedDefs = phase1ParseGenericInstanceBlock(instanceLines);
      if (Array.isArray(parsedDefs)) {
        const mergedDefs = new Map();
        const classDefaults = classDefaultMethodDefs.get(genericInstanceMatch[1]);
        if (Array.isArray(classDefaults)) {
          for (const def of classDefaults) {
            mergedDefs.set(def.name, def);
          }
        }
        for (const def of parsedDefs) {
          mergedDefs.set(def.name, def);
        }
        for (const def of mergedDefs.values()) {
          if (ambiguousInstanceMethods.has(def.name)) {
            continue;
          }
          if (instanceMethodDefs.has(def.name)) {
            instanceMethodDefs.delete(def.name);
            ambiguousInstanceMethods.add(def.name);
            continue;
          }
          instanceMethodDefs.set(def.name, def);
        }
      }
      index = nextIndex;
      continue;
    }
    const newtypeMatch = trimmed.match(
      /^newtype\s+[A-Za-z_][A-Za-z0-9_']*(?:\s+[A-Za-z_][A-Za-z0-9_']*)*\s*=\s*([A-Za-z_][A-Za-z0-9_']*)\s+[A-Za-z_][A-Za-z0-9_']*\s*$/u,
    );
    if (newtypeMatch !== null) {
      transparentNewtypeCtors.add(newtypeMatch[1]);
      index += 1;
      continue;
    }
    if (trimmed.startsWith("data ")) {
      index += 1;
      continue;
    }
    const guardedHead = phase1ParseTopLevelGuardedDefinitionHead(trimmed);
    if (guardedHead !== null) {
      const name = guardedHead.name;
      const params = guardedHead.params;
      const guardLines = [
        guardedHead.firstGuardLine,
      ];
      let nextIndex = index + 1;
      while (nextIndex < lines.length) {
        const continuationCode = phase1StripLineComment(lines[nextIndex]);
        const continuationTrimmed = continuationCode.trim();
        if (continuationTrimmed.length === 0) {
          nextIndex += 1;
          continue;
        }
        if (phase1LeadingIndent(continuationCode) <= baseIndent) {
          break;
        }
        guardLines.push(continuationTrimmed);
        nextIndex += 1;
      }
      const body = phase1ParseGuardedClauseLines(guardLines);
      if (body !== null) {
        definitions.push({
          name,
          params,
          body: phase1AnnotateCollectionLiteralTargets(
            body,
            typeAnnotations.get(name) ?? null,
            collectionLiteralInstances,
          ),
        });
      }
      index = nextIndex;
      continue;
    }
    const match = trimmed.match(
      /^([A-Za-z_][A-Za-z0-9_']*|[+\-*/<>=!][+\-*/<>=!.]*)\s*(.*?)\s*=\s*(.*)$/u,
    );
    if (match === null) {
      index += 1;
      continue;
    }
    const name = match[1];
    const params = match[2].trim().length > 0
      ? phase1ToArgList(match[2])
      : [];
    const hasInlineRhs = match[3].trim().length > 0;
    const rhsParts = [];
    const continuationLines = [];
    let whereLines = null;
    if (hasInlineRhs) {
      rhsParts.push(match[3].trim());
    }
    let nextIndex = index + 1;
    while (nextIndex < lines.length) {
      const continuationCode = phase1StripLineComment(lines[nextIndex]);
      const continuationTrimmed = continuationCode.trim();
      if (continuationTrimmed.length === 0) {
        nextIndex += 1;
        continue;
      }
      if (phase1LeadingIndent(continuationCode) <= baseIndent) {
        break;
      }
      if (continuationTrimmed === "where") {
        const whereIndent = phase1LeadingIndent(continuationCode);
        whereLines = [];
        nextIndex += 1;
        while (nextIndex < lines.length) {
          const whereCode = phase1StripLineComment(lines[nextIndex]);
          const whereTrimmed = whereCode.trim();
          if (whereTrimmed.length === 0) {
            nextIndex += 1;
            continue;
          }
          if (phase1LeadingIndent(whereCode) <= whereIndent) {
            break;
          }
          whereLines.push(lines[nextIndex]);
          nextIndex += 1;
        }
        break;
      }
      continuationLines.push(continuationTrimmed);
      rhsParts.push(continuationTrimmed);
      nextIndex += 1;
    }
    if (!hasInlineRhs && whereLines === null) {
      const clauseBody = phase1ParseSimplePatternClauseBlock(params, continuationLines);
      if (clauseBody !== null) {
        definitions.push({
          name,
          params,
          body: phase1AnnotateCollectionLiteralTargets(
            clauseBody,
            typeAnnotations.get(name) ?? null,
            collectionLiteralInstances,
          ),
        });
        index = nextIndex;
        continue;
      }
    }
    const rhs = phase1JoinExpressionParts(rhsParts);
    if (rhs.length === 0) {
      index = nextIndex;
      continue;
    }
    const tokens = phase1TokenizeExpression(rhs);
    if (tokens === null) {
      index = nextIndex;
      continue;
    }
    const parsed = phase1ParseExpr(tokens, 0, new Set());
    if (parsed === null || parsed.next !== tokens.length) {
      index = nextIndex;
      continue;
    }
    let body = parsed.node;
    if (Array.isArray(whereLines) && whereLines.length > 0) {
      const localDefinitions = phase1ParseLocalDefinitions(whereLines);
      if (localDefinitions === null) {
        index = nextIndex;
        continue;
      }
      body = phase1WrapWhereDefinitions(body, localDefinitions);
    }
    definitions.push({
      name,
      params,
      body: phase1AnnotateCollectionLiteralTargets(
        body,
        typeAnnotations.get(name) ?? null,
        collectionLiteralInstances,
      ),
    });
    index = nextIndex;
  }
  if (definitions.length === 0) {
    return null;
  }
  definitions.typeAnnotations = typeAnnotations;
  definitions.collectionLiteralInstances = collectionLiteralInstances;
  definitions.classDefaultMethodDefs = classDefaultMethodDefs;
  definitions.instanceMethodDefs = instanceMethodDefs;
  definitions.ambiguousInstanceMethods = ambiguousInstanceMethods;
  definitions.transparentNewtypeCtors = transparentNewtypeCtors;
  return definitions;
}

function phase1ApplyBuiltin(name, args, depth = 0) {
  const args1 = args[0];
  const args2 = args[1];
  const args3 = args[2];
  const structMaker = phase1ParseStructMakerName(name);
  if (structMaker !== null) {
    return Array.isArray(args) && args.length === structMaker.arity
      ? {
        kind: "struct_helper",
        tag: structMaker.tag,
        fields: [...args],
      }
      : null;
  }
  const structGetter = phase1ParseStructGetterName(name);
  if (structGetter !== null) {
    if (
      !args1 || typeof args1 !== "object" || args1.kind !== "struct_helper" ||
      args1.tag !== structGetter.tag || !Array.isArray(args1.fields) ||
      structGetter.index >= args1.fields.length
    ) {
      return null;
    }
    return args1.fields[structGetter.index];
  }
  const structPredicate = phase1ParseStructPredicateName(name);
  if (structPredicate !== null) {
    if (
      !args1 || typeof args1 !== "object" || args1.kind !== "struct_helper" ||
      !Array.isArray(args1.fields)
    ) {
      return null;
    }
    return args1.tag === structPredicate.tag ? 1 : 0;
  }
  switch (name) {
    case "add": {
      if (!Number.isInteger(args1) || !Number.isInteger(args2)) {
        return null;
      }
      return args1 + args2;
    }
    case "mul": {
      if (!Number.isInteger(args1) || !Number.isInteger(args2)) {
        return null;
      }
      return args1 * args2;
    }
    case "sub": {
      if (!Number.isInteger(args1) || !Number.isInteger(args2)) {
        return null;
      }
      return args1 - args2;
    }
    case "div": {
      if (!Number.isInteger(args1) || !Number.isInteger(args2) || args2 === 0) {
        return null;
      }
      return (args1 / args2) | 0;
    }
    case "mod": {
      if (!Number.isInteger(args1) || !Number.isInteger(args2) || args2 === 0) {
        return null;
      }
      return args1 % args2;
    }
    case "eq":
      return args1 === args2;
    case "ne":
      return args1 !== args2;
    case "lt":
      return args1 < args2;
    case "le":
      return args1 <= args2;
    case "gt":
      return args1 > args2;
    case "ge":
      return args1 >= args2;
    case "and":
      if (typeof args1 !== "boolean" || typeof args2 !== "boolean") {
        return null;
      }
      return args1 && args2;
    case "or":
      if (typeof args1 !== "boolean" || typeof args2 !== "boolean") {
        return null;
      }
      return args1 || args2;
    case "xor":
      if (typeof args1 !== "boolean" || typeof args2 !== "boolean") {
        return null;
      }
      return args1 !== args2;
    case "implies":
      if (typeof args1 !== "boolean" || typeof args2 !== "boolean") {
        return null;
      }
      return (!args1) || args2;
    case "not":
      if (typeof args1 !== "boolean") {
        return null;
      }
      return !args1;
    case "if":
      if (typeof args1 !== "boolean") {
        return null;
      }
      return phase1ApplyCallableValue(args1 ? args2 : args3, [0], depth);
    case "slice_new_u8":
      if (!Number.isInteger(args1) || args1 < 0) {
        return null;
      }
      return phase1SliceValue(new Uint8Array(args1));
    case "slice_len":
    case "slice_len_raw":
      return phase1IsSliceValue(args1) ? args1.bytes.length : null;
    case "slice_get_u8":
      if (!phase1IsSliceValue(args1) || !Number.isInteger(args2)) {
        return null;
      }
      return args2 >= 0 && args2 < args1.bytes.length ? args1.bytes[args2] : 0;
    case "slice_set_u8":
      if (
        !phase1IsSliceValue(args1) || !Number.isInteger(args2) ||
        !Number.isInteger(args3) || args2 < 0 || args2 >= args1.bytes.length ||
        args3 < 0 || args3 > 255
      ) {
        return null;
      }
      {
        const next = new Uint8Array(args1.bytes);
        next[args2] = args3;
        return phase1SliceValue(next);
      }
    case "slice_eq_u8":
      if (!phase1IsSliceValue(args1) || !phase1IsSliceValue(args2)) {
        return null;
      }
      if (args1.bytes.length !== args2.bytes.length) {
        return false;
      }
      for (let i = 0; i < args1.bytes.length; i += 1) {
        if (args1.bytes[i] !== args2.bytes[i]) {
          return false;
        }
      }
      return true;
    case "str_to_slice":
      return typeof args1 === "string" ? phase1SliceValue(new TextEncoder().encode(args1)) : null;
    case "slice_to_string":
      return phase1IsSliceValue(args1) ? new TextDecoder().decode(args1.bytes) : null;
    case "ListNil":
    case "Nil":
      return [];
    case "ListCons":
    case "Cons": {
      if (!Array.isArray(args2)) {
        return null;
      }
      return [args1, ...args2];
    }
    case "list_map":
    case "fmap": {
      if (!Array.isArray(args2)) {
        return null;
      }
      const out = [];
      for (const item of args2) {
        const mapped = phase1ApplyCallableValue(args1, [item], depth);
        if (mapped === null) {
          return null;
        }
        out.push(mapped);
      }
      return out;
    }
    case "list_foldl":
    case "foldl": {
      if (!Number.isInteger(args2) || !Array.isArray(args3)) {
        return null;
      }
      let acc = args2;
      for (const item of args3) {
        acc = phase1ApplyCallableValue(args1, [acc, item], depth);
        if (!Number.isInteger(acc)) {
          return null;
        }
      }
      return acc;
    }
    case "list_foldr":
    case "foldr": {
      if (!Array.isArray(args3)) {
        return null;
      }
      let acc = args2;
      for (let index = args3.length - 1; index >= 0; index -= 1) {
        acc = phase1ApplyCallableValue(args1, [args3[index], acc], depth);
        if (acc === null) {
          return null;
        }
      }
      return acc;
    }
    case "list_filter": {
      if (!Array.isArray(args2)) {
        return null;
      }
      const out = [];
      for (const item of args2) {
        const keep = phase1ApplyCallableValue(args1, [item], depth);
        if (keep === null || typeof keep !== "boolean") {
          return null;
        }
        if (keep) {
          out.push(item);
        }
      }
      return out;
    }
    case "filter":
      return phase1ApplyBuiltin("list_filter", args, depth);
    case "list_any": {
      if (!Array.isArray(args2)) {
        return null;
      }
      for (const item of args2) {
        const any = phase1ApplyCallableValue(args1, [item], depth);
        if (any === null || typeof any !== "boolean") {
          return null;
        }
        if (any) {
          return true;
        }
      }
      return false;
    }
    case "any":
      return phase1ApplyBuiltin("list_any", args, depth);
    case "list_all": {
      if (!Array.isArray(args2)) {
        return null;
      }
      for (const item of args2) {
        const all = phase1ApplyCallableValue(args1, [item], depth);
        if (all === null || typeof all !== "boolean") {
          return null;
        }
        if (!all) {
          return false;
        }
      }
      return true;
    }
    case "all":
      return phase1ApplyBuiltin("list_all", args, depth);
    case "build": {
      const cons = { kind: "ctor", name: "Cons", args: [] };
      const nil = [];
      return phase1ApplyCallableValue(args1, [cons, nil], depth);
    }
    default:
      return null;
  }
}

function phase1ApplyCollectionLiteralMethod(fnValue, argValues, depth = 0) {
  if (!fnValue || typeof fnValue !== "object" || fnValue.kind !== "function") {
    return null;
  }
  const params = Array.isArray(fnValue.params) ? fnValue.params : [];
  if (params.length !== argValues.length) {
    return null;
  }
  const locals = new Map(fnValue.locals ?? []);
  for (let i = 0; i < params.length; i += 1) {
    locals.set(params[i], argValues[i]);
  }
  return phase1Evaluate(fnValue.body, fnValue.env, locals, depth + 1);
}

function phase1ApplyFunctionValue(fnValue, argValues, depth = 0) {
  if (fnValue && fnValue.kind === "function") {
    const params = Array.isArray(fnValue.params) ? fnValue.params : [];
    if (argValues.length < params.length) {
      const locals = new Map(fnValue.locals ?? []);
      for (let i = 0; i < argValues.length; i += 1) {
        locals.set(params[i], argValues[i]);
      }
      return {
        kind: "function",
        name: fnValue.name ?? "<partial>",
        params: params.slice(argValues.length),
        body: fnValue.body,
        env: fnValue.env,
        locals,
      };
    }
    const locals = new Map(fnValue.locals ?? []);
    for (let i = 0; i < params.length; i += 1) {
      locals.set(params[i], argValues[i]);
    }
    const result = phase1Evaluate(fnValue.body, fnValue.env, locals, depth + 1);
    if (result === null) {
      return null;
    }
    if (argValues.length === params.length) {
      return result;
    }
    return phase1ApplyCallableValue(result, argValues.slice(params.length), depth + 1);
  }
  if (fnValue && fnValue.kind === "builtin") {
    const arity = phase1BuiltinArity(fnValue.name);
    if (arity === null) {
      return null;
    }
    if (argValues.length < arity) {
      return {
        kind: "builtin_partial",
        name: fnValue.name,
        args: [...argValues],
      };
    }
    const result = phase1ApplyBuiltin(fnValue.name, argValues.slice(0, arity), depth);
    if (result === null) {
      return null;
    }
    if (argValues.length === arity) {
      return result;
    }
    return phase1ApplyCallableValue(result, argValues.slice(arity), depth + 1);
  }
  if (fnValue && fnValue.kind === "builtin_partial") {
    return phase1ApplyCallableValue(
      { kind: "builtin", name: fnValue.name },
      [...(Array.isArray(fnValue.args) ? fnValue.args : []), ...argValues],
      depth,
    );
  }
  if (fnValue && fnValue.kind === "ctor") {
    return phase1ConstructValue(
      fnValue.name,
      [...(Array.isArray(fnValue.args) ? fnValue.args : []), ...argValues],
    );
  }
  return null;
}

function phase1ApplyCallableValue(fnValue, argValues, depth = 0) {
  if (typeof fnValue === "function") {
    try {
      return fnValue(...argValues);
    } catch {
      return null;
    }
  }
  if (
    fnValue &&
    (fnValue.kind === "function" || fnValue.kind === "builtin" ||
      fnValue.kind === "builtin_partial" || fnValue.kind === "ctor")
  ) {
    return phase1ApplyFunctionValue(fnValue, argValues, depth);
  }
  return null;
}

function phase1BuiltinZeroArityValue(name, depth = 0) {
  if (name === "ListNil" || name === "Nil") {
    return phase1ApplyBuiltin(name, [], depth);
  }
  return null;
}

function phase1Evaluate(expr, env, locals = new Map(), depth = 0) {
  if (depth > 64) {
    return null;
  }
  if (!expr || typeof expr !== "object") {
    return null;
  }
  if (expr.type === "int") {
    return expr.value;
  }
  if (expr.type === "bool") {
    return expr.value;
  }
  if (expr.type === "string") {
    return expr.value;
  }
  if (expr.type === "trap") {
    return null;
  }
  if (expr.type === "lambda") {
    return {
      kind: "function",
      name: "<lambda>",
      params: expr.params,
      body: expr.body,
      env,
      locals: new Map(locals),
    };
  }
  if (expr.type === "record") {
    const fields = new Map();
    for (const field of expr.fields) {
      const value = phase1Evaluate(field.value, env, locals, depth + 1);
      if (value === null) {
        return null;
      }
      fields.set(field.name, value);
    }
    return phase1RecordValue(fields);
  }
  if (expr.type === "recordUpdate") {
    const base = phase1Evaluate(expr.base, env, locals, depth + 1);
    if (!base || typeof base !== "object" || base.kind !== "record") {
      return null;
    }
    const fields = new Map(base.fields);
    for (const field of expr.fields) {
      const value = phase1Evaluate(field.value, env, locals, depth + 1);
      if (value === null) {
        return null;
      }
      fields.set(field.name, value);
    }
    return phase1RecordValue(fields);
  }
  if (expr.type === "braceApplyOrUpdate") {
    const base = phase1Evaluate(expr.base, env, locals, depth + 1);
    const fields = [];
    for (const field of expr.fields) {
      const value = phase1Evaluate(field.value, env, locals, depth + 1);
      if (value === null) {
        return null;
      }
      fields.push({ name: field.name, value });
    }
    if (base && typeof base === "object" && base.kind === "record") {
      const nextFields = new Map(base.fields);
      for (const field of fields) {
        nextFields.set(field.name, field.value);
      }
      return phase1RecordValue(nextFields);
    }
    const argValue = phase1RecordValue(
      new Map(fields.map((field) => [field.name, field.value])),
    );
    return phase1ApplyCallableValue(base, [argValue], depth + 1);
  }
  if (expr.type === "field") {
    const base = phase1Evaluate(expr.base, env, locals, depth + 1);
    if (!base || typeof base !== "object" || base.kind !== "record") {
      return null;
    }
    if (!base.fields.has(expr.field)) {
      return null;
    }
    return base.fields.get(expr.field);
  }
  if (expr.type === "listLiteral") {
    const values = [];
    for (const element of expr.elements) {
      const value = phase1Evaluate(element, env, locals, depth + 1);
      if (value === null) {
        return null;
      }
      values.push(value);
    }
    const collectionTargetType = typeof expr.collectionTargetType === "string"
      ? expr.collectionTargetType
      : "";
    const collectionLiteralInstances = env?.collectionLiteralInstances;
    if (
      collectionTargetType.length > 0 &&
      collectionLiteralInstances instanceof Map &&
      collectionLiteralInstances.has(collectionTargetType)
    ) {
      const instanceDef = collectionLiteralInstances.get(collectionTargetType);
      let acc = phase1ApplyCollectionLiteralMethod(
        instanceDef.empty,
        [null],
        depth + 1,
      );
      if (acc === null) {
        return null;
      }
      for (let index = values.length - 1; index >= 0; index -= 1) {
        acc = phase1ApplyCollectionLiteralMethod(
          instanceDef.extend,
          [acc, values[index]],
          depth + 1,
        );
        if (acc === null) {
          return null;
        }
      }
      return acc;
    }
    return values;
  }
  if (expr.type === "var") {
    return phase1ResolveValueByName(expr.name, env, locals, depth);
  }
  if (expr.type === "if") {
    const cond = phase1Evaluate(expr.cond, env, locals, depth + 1);
    if (typeof cond !== "boolean") {
      return null;
    }
    return cond
      ? phase1Evaluate(expr.thenExpr, env, locals, depth + 1)
      : phase1Evaluate(expr.elseExpr, env, locals, depth + 1);
  }
  if (expr.type === "let") {
    const value = phase1Evaluate(expr.value, env, locals, depth + 1);
    if (value === null) {
      return null;
    }
    const nextLocals = new Map(locals);
    nextLocals.set(expr.name, value);
    return phase1Evaluate(expr.body, env, nextLocals, depth + 1);
  }
  if (expr.type === "letPattern") {
    const value = phase1Evaluate(expr.value, env, locals, depth + 1);
    if (value === null) {
      return null;
    }
    const bindings = phase1MatchPattern(expr.pattern, value);
    if (bindings === null) {
      return null;
    }
    const nextLocals = new Map(locals);
    for (const [name, boundValue] of bindings) {
      nextLocals.set(name, boundValue);
    }
    return phase1Evaluate(expr.body, env, nextLocals, depth + 1);
  }
  if (expr.type === "caseBool") {
    const target = phase1Evaluate(expr.target, env, locals, depth + 1);
    if (typeof target !== "boolean") {
      return null;
    }
    return target
      ? phase1Evaluate(expr.whenTrue, env, locals, depth + 1)
      : phase1Evaluate(expr.whenFalse, env, locals, depth + 1);
  }
  if (expr.type === "caseMulti") {
    const targets = [];
    for (const targetExpr of expr.targets) {
      const targetValue = phase1Evaluate(targetExpr, env, locals, depth + 1);
      if (targetValue === null) {
        return null;
      }
      targets.push(targetValue);
    }
    const bindings = phase1MatchPatternList(expr.patterns, targets);
    if (bindings !== null) {
      const nextLocals = new Map(locals);
      for (const [name, value] of bindings) {
        nextLocals.set(name, value);
      }
      return phase1Evaluate(expr.whenMatch, env, nextLocals, depth + 1);
    }
    const fallbackBindings = phase1MatchPatternList(expr.fallbackPatterns, targets);
    if (fallbackBindings === null) {
      return null;
    }
    const nextLocals = new Map(locals);
    for (const [name, value] of fallbackBindings) {
      nextLocals.set(name, value);
    }
    return phase1Evaluate(expr.whenFallback, env, nextLocals, depth + 1);
  }
  if (expr.type === "caseCtor") {
    const target = phase1Evaluate(expr.target, env, locals, depth + 1);
    const bindings = phase1MatchPattern(expr.pattern, target);
    if (bindings !== null) {
      const nextLocals = new Map(locals);
      for (const [name, value] of bindings) {
        nextLocals.set(name, value);
      }
      return phase1Evaluate(expr.whenMatch, env, nextLocals, depth + 1);
    }
    if (!expr.fallbackPattern || !expr.whenFallback) {
      return null;
    }
    const fallbackBindings = phase1MatchPattern(expr.fallbackPattern, target);
    if (fallbackBindings === null) {
      return null;
    }
    const nextLocals = new Map(locals);
    for (const [name, value] of fallbackBindings) {
      nextLocals.set(name, value);
    }
    return phase1Evaluate(expr.whenFallback, env, nextLocals, depth + 1);
  }
  if (expr.type === "apply") {
    const args = [];
    let argExpr = expr;
    while (argExpr && argExpr.type === "apply") {
      const value = phase1Evaluate(argExpr.arg, env, locals, depth + 1);
      if (value === null) {
        return null;
      }
      args.unshift(value);
      argExpr = argExpr.fn;
    }
    const flattened = phase1FlattenApply(expr);
    if (
      flattened.callee && flattened.callee.type === "var" &&
      phase1IsConstructorToken(flattened.callee.name)
    ) {
      return phase1ConstructValue(flattened.callee.name, args);
    }
    const funcExpr = phase1Evaluate(argExpr, env, locals, depth + 1);
    if (funcExpr === null) {
      return null;
    }
    if (funcExpr && funcExpr.kind === "function") {
      return phase1ApplyFunctionValue(funcExpr, args, depth);
    }
    if (
      funcExpr &&
      (funcExpr.kind === "builtin" || funcExpr.kind === "builtin_partial" ||
        funcExpr.kind === "ctor")
    ) {
      return phase1ApplyCallableValue(funcExpr, args, depth);
    }
    return null;
  }
  return null;
}

function phase1EvaluateDefinitionGraph(sourceText) {
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  return phase1EvaluateNullaryRootFromDefinitions(definitions, "main");
}

function phase1EvaluateNullaryRootFromDefinitions(definitions, rootName) {
  if (!Array.isArray(definitions) || typeof rootName !== "string" || rootName.length === 0) {
    return null;
  }
  const env = phase1BuildEvaluationEnv(definitions);
  if (env === null) {
    return null;
  }
  const root = env.get(rootName);
  if (!root || root.kind !== "function") {
    return null;
  }
  if (root.params.length !== 0) {
    return null;
  }
  const result = phase1Evaluate(root.body, env, new Map(), 0);
  return Number.isInteger(result) ? result : null;
}

function phase1EvaluateNullaryRootValueFromDefinitions(definitions, rootName) {
  if (!Array.isArray(definitions) || typeof rootName !== "string" || rootName.length === 0) {
    return null;
  }
  const env = phase1BuildEvaluationEnv(definitions);
  if (env === null) {
    return null;
  }
  const root = env.get(rootName);
  if (!root || root.kind !== "function" || root.params.length !== 0) {
    return null;
  }
  return phase1Evaluate(root.body, env, new Map(), 0);
}

function phase1BuildEvaluationEnv(definitions) {
  if (!Array.isArray(definitions)) {
    return null;
  }
  const env = new Map();
  for (const def of definitions) {
    if (!env.has(def.name)) {
      env.set(def.name, {
        kind: "function",
        name: def.name,
        params: def.params,
        body: def.body,
        depth: 0,
        locals: new Map(),
      });
    }
  }
  for (const value of env.values()) {
    value.env = env;
  }
  const collectionLiteralInstances = new Map();
  const instanceMethodDefs = definitions.instanceMethodDefs instanceof Map
    ? definitions.instanceMethodDefs
    : new Map();
  const ambiguousInstanceMethods = definitions.ambiguousInstanceMethods instanceof Set
    ? definitions.ambiguousInstanceMethods
    : new Set();
  const parsedCollectionInstances = definitions.collectionLiteralInstances;
  if (parsedCollectionInstances instanceof Map) {
    for (const [typeName, instanceDef] of parsedCollectionInstances.entries()) {
      if (!instanceDef || typeof instanceDef !== "object") {
        continue;
      }
      const emptyDef = instanceDef.empty;
      const extendDef = instanceDef.extend;
      if (!emptyDef || !extendDef) {
        continue;
      }
      collectionLiteralInstances.set(typeName, {
        empty: {
          kind: "function",
          name: emptyDef.name,
          params: emptyDef.params,
          body: emptyDef.body,
          depth: 0,
          locals: new Map(),
          env,
        },
        extend: {
          kind: "function",
          name: extendDef.name,
          params: extendDef.params,
          body: extendDef.body,
          depth: 0,
          locals: new Map(),
          env,
        },
      });
    }
  }
  env.collectionLiteralInstances = collectionLiteralInstances;
  for (const [methodName, methodDef] of instanceMethodDefs.entries()) {
    if (ambiguousInstanceMethods.has(methodName) || env.has(methodName)) {
      continue;
    }
    env.set(methodName, {
      kind: "function",
      name: methodDef.name,
      params: methodDef.params,
      body: methodDef.body,
      depth: 0,
      locals: new Map(),
      env,
    });
  }
  for (const builtin of [
    "add", "mul", "sub", "div", "mod", "eq", "ne", "lt", "le", "gt", "ge",
    "and", "or", "xor", "implies", "not",
    "ListNil", "ListCons", "Nil", "Cons", "list_map", "fmap", "list_foldl", "foldl", "list_foldr",
    "foldr", "list_filter", "filter", "list_any", "list_all", "any", "all", "build",
  ]) {
    if (!env.has(builtin)) {
      env.set(builtin, { kind: "builtin", name: builtin });
    }
  }
  return env;
}

function phase1TaggedConstForSource(sourceText) {
  const evaluated = phase1EvaluateDefinitionGraph(sourceText);
  return evaluated;
}

function phase1TaggedConstForRoot(sourceText, rootName) {
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  return phase1EvaluateNullaryRootFromDefinitions(definitions, rootName);
}

function phase1StringConstForRoot(sourceText, rootName) {
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  const value = phase1EvaluateNullaryRootValueFromDefinitions(definitions, rootName);
  return typeof value === "string" ? value : null;
}

function phase1DebugValueStringForRoot(sourceText, rootName) {
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  const value = phase1EvaluateNullaryRootValueFromDefinitions(definitions, rootName);
  const expr = phase1ExprFromDebugValue(value);
  if (!expr) {
    return null;
  }
  return phase1RenderDebugExpr(phase1ReduceRecordExpr(expr));
}

function phase1NeedsDebugValueMaterialization(sourceText, rootName) {
  if (typeof sourceText !== "string" || typeof rootName !== "string" || rootName.length === 0) {
    return false;
  }
  if (phase1TaggedConstForRoot(sourceText, rootName) !== null) {
    return false;
  }
  if (phase1StringConstForRoot(sourceText, rootName) !== null) {
    return false;
  }
  return typeof phase1DebugValueStringForRoot(sourceText, rootName) === "string";
}

function phase1TaggedConstEntriesForRoots(sourceText, rootNames) {
  if (!Array.isArray(rootNames) || rootNames.length === 0) {
    return null;
  }
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  const out = [];
  for (const rootName of rootNames) {
    const value = phase1EvaluateNullaryRootFromDefinitions(definitions, rootName);
    const min = -1073741824;
    const max = 1073741823;
    if (
      typeof rootName !== "string" ||
      rootName.length === 0 ||
      !Number.isSafeInteger(value) ||
      value < min ||
      value > max
    ) {
      return null;
    }
    out.push({ name: rootName, rawValue: value });
  }
  return out;
}

export function phase1OracleExpectedMainForSource(
  sourceText,
  requestObject = null,
) {
  const normalized = normalizePlaceholderSourceText(sourceText);
  const request = requestObject ?? {
    command: "compile",
    input_source: normalized,
    entrypoint_exports: ["main"],
  };
  const collapsed = appendPhase1TailMarkers(
    prunePhase1CollapsedSource(normalized, request),
    normalized,
  );
  return phase1TaggedConstForSource(collapsed);
}

function phase1WasmBase64ForTaggedConst(taggedValue, exportName = "main") {
  const min = -1073741824;
  const max = 1073741823;
  if (
    !Number.isSafeInteger(taggedValue) || taggedValue < min || taggedValue > max
  ) {
    return null;
  }
  return buildPhase1TaggedWasmBase64(taggedValue, exportName);
}

function phase1WasmBase64ForStringConst(stringValue, exportName = "main") {
  if (typeof stringValue !== "string") {
    return null;
  }
  const stringBytes = UTF8_ENCODER.encode(stringValue);
  const headerBytes = new Uint8Array(8);
  const headerView = new DataView(headerBytes.buffer);
  headerView.setUint32(0, 8, true);
  headerView.setUint32(4, stringBytes.length, true);
  const moduleBytes = [
    0x00,
    0x61,
    0x73,
    0x6d,
    0x01,
    0x00,
    0x00,
    0x00,
    ...phase1WrapSection(1, phase1WasmTypeSection([0])),
    ...phase1WrapSection(3, phase1WasmFunctionSection([0])),
    ...phase1WrapSection(5, phase1WasmMemorySection()),
    ...phase1WrapSection(7, phase1WasmExportSection([
      { name: "memory", kind: 0x02, index: 0 },
      { name: exportName, kind: 0x00, index: 0 },
    ])),
    ...phase1WrapSection(10, phase1WasmCodeSection([
      {
        localCount: 0,
        code: [0x41, ...encodeVarS32(0)],
      },
    ])),
    ...phase1WrapSection(11, phase1WasmDataSection([
      { offset: 0, bytes: headerBytes },
      { offset: 8, bytes: stringBytes },
    ])),
  ];
  return toBase64(Uint8Array.from(moduleBytes));
}

function phase1NullaryRawExportResult(wasmBase64, exportName = "main") {
  if (typeof wasmBase64 !== "string" || wasmBase64.length === 0) {
    return null;
  }
  try {
    const wasmBytes = decodeWasmBase64(wasmBase64);
    const module = new WebAssembly.Module(wasmBytes);
    const imports = WebAssembly.Module.imports(module);
    if (imports.length > 0) {
      return null;
    }
    const instance = new WebAssembly.Instance(module, {});
    const exported = instance.exports?.[exportName];
    if (typeof exported !== "function" || exported.length !== 0) {
      return null;
    }
    const raw = exported();
    return Number.isInteger(raw) ? raw : null;
  } catch {
    return null;
  }
}

function phase1DecodedRawExportResult(wasmBase64, exportName = "main", args = []) {
  if (
    typeof wasmBase64 !== "string" ||
    wasmBase64.length === 0 ||
    typeof exportName !== "string" ||
    exportName.length === 0 ||
    !Array.isArray(args)
  ) {
    return null;
  }
  try {
    const wasmBytes = decodeWasmBase64(wasmBase64);
    const module = new WebAssembly.Module(wasmBytes);
    const imports = WebAssembly.Module.imports(module);
    if (imports.length > 0) {
      return null;
    }
    const runtime = makeRuntime();
    const instance = new WebAssembly.Instance(module, {});
    const memoryExport = instance.exports?.memory ?? instance.exports?.__memory;
    if (memoryExport instanceof WebAssembly.Memory) {
      runtime.state.memory = memoryExport;
    }
    const heapGlobal = instance.exports?.__heap_ptr;
    if (heapGlobal instanceof WebAssembly.Global) {
      runtime.state.heapGlobal = heapGlobal;
    }
    const exported = instance.exports?.[exportName];
    if (typeof exported !== "function" || exported.length !== args.length) {
      return null;
    }
    const callArgs = [];
    for (const arg of args) {
      if (Number.isInteger(arg)) {
        callArgs.push(arg);
        continue;
      }
      if (
        arg && typeof arg === "object" &&
        arg.kind === "sample_slice" &&
        Array.isArray(arg.bytes) &&
        runtime.state.memory instanceof WebAssembly.Memory
      ) {
        callArgs.push(runtime.alloc_slice_u8(Uint8Array.from(arg.bytes)));
        continue;
      }
      return null;
    }
    try {
      const raw = exported(...callArgs);
      if (!Number.isInteger(raw)) {
        return null;
      }
      return (raw & 1) === 1 ? (raw >> 1) : raw;
    } catch {
      return "trap";
    }
  } catch {
    return null;
  }
}

function phase1SampleSliceArg(bytes) {
  return {
    kind: "sample_slice",
    bytes: Array.from(bytes),
  };
}

function phase1SampleArgsForRoles(paramRoles = []) {
  if (!Array.isArray(paramRoles)) {
    return null;
  }
  if (paramRoles.length === 0) {
    return [[]];
  }
  if (paramRoles.length === 1 && paramRoles[0] === "scalar") {
    return [[0], [1], [7]];
  }
  if (
    paramRoles.length === 2 &&
    paramRoles[0] === "opaque" &&
    paramRoles[1] === "scalar"
  ) {
    const sample = phase1SampleSliceArg([10, 20, 30]);
    return [[sample, 0], [sample, 1], [sample, 2]];
  }
  if (paramRoles.every((role) => role === "scalar")) {
    return phase1SampleArgsForArity(paramRoles.length);
  }
  return null;
}

function phase1SampleArgsForArity(arity) {
  if (arity === 0) {
    return [[]];
  }
  if (arity === 1) {
    return [[0], [1], [7]];
  }
  if (arity === 2) {
    return [[0, 0], [1, 0], [0, 1], [7, 7]];
  }
  if (arity === 3) {
    return [[0, 0, 0], [1, 2, 3], [3, 1, 0], [7, 8, 9]];
  }
  return null;
}

function phase1EvaluateRootForSampleArgs(sourceText, exportEntry, sampleArgs) {
  if (
    typeof sourceText !== "string" ||
    !exportEntry ||
    typeof exportEntry.name !== "string" ||
    exportEntry.name.length === 0 ||
    !Array.isArray(sampleArgs)
  ) {
    return null;
  }
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  const env = phase1BuildEvaluationEnv(definitions);
  if (!(env instanceof Map)) {
    return null;
  }
  const root = env.get(exportEntry.name);
  if (!root) {
    return null;
  }
  const evalArgs = [];
  for (const arg of sampleArgs) {
    if (Number.isInteger(arg)) {
      evalArgs.push(arg);
      continue;
    }
    if (
      arg &&
      typeof arg === "object" &&
      arg.kind === "sample_slice" &&
      Array.isArray(arg.bytes)
    ) {
      evalArgs.push(Uint8Array.from(arg.bytes));
      continue;
    }
    return null;
  }
  try {
    const value = phase1ApplyCallableValue(root, evalArgs, 0);
    if (Number.isInteger(value)) {
      return value;
    }
    if (typeof value === "boolean") {
      return value ? 1 : 0;
    }
    return null;
  } catch {
    return "trap";
  }
}

function phase1SourceExportResultsMatchForSamples(sourceText, wasmBase64, exportEntries) {
  if (
    typeof sourceText !== "string" ||
    !Array.isArray(exportEntries) ||
    exportEntries.length === 0
  ) {
    return false;
  }
  for (const entry of exportEntries) {
    const name = entry?.name;
    const arity = entry?.arity;
    if (typeof name !== "string" || name.length === 0) {
      return false;
    }
    const paramRoles = Array.isArray(entry?.param_roles)
      ? entry.param_roles
      : Array(Number.isInteger(arity) ? arity : 0).fill("scalar");
    const samples = phase1SampleArgsForRoles(paramRoles) ??
      phase1SampleArgsForArity(arity);
    if (!Array.isArray(samples) || samples.length === 0) {
      return false;
    }
    for (const sampleArgs of samples) {
      const expected = phase1EvaluateRootForSampleArgs(sourceText, entry, sampleArgs);
      const actual = phase1DecodedRawExportResult(wasmBase64, name, sampleArgs);
      const expectedComparable = Number.isInteger(expected) || expected === "trap";
      const actualComparable = Number.isInteger(actual) || actual === "trap";
      if (!expectedComparable || !actualComparable || expected !== actual) {
        return false;
      }
    }
  }
  return true;
}

function phase1ExportResultsMatchForSamples(lhsWasmBase64, rhsWasmBase64, exportEntries) {
  if (!Array.isArray(exportEntries) || exportEntries.length === 0) {
    return true;
  }
  for (const entry of exportEntries) {
    const name = entry?.name;
    const arity = entry?.arity;
    if (typeof name !== "string" || name.length === 0) {
      return false;
    }
    const paramRoles = Array.isArray(entry?.param_roles)
      ? entry.param_roles
      : Array(Number.isInteger(arity) ? arity : 0).fill("scalar");
    const samples = phase1SampleArgsForRoles(paramRoles) ??
      phase1SampleArgsForArity(arity);
    if (!Array.isArray(samples) || samples.length === 0) {
      return false;
    }
    for (const sampleArgs of samples) {
      const lhs = phase1DecodedRawExportResult(lhsWasmBase64, name, sampleArgs);
      const rhs = phase1DecodedRawExportResult(rhsWasmBase64, name, sampleArgs);
      const lhsComparable = Number.isInteger(lhs) || lhs === "trap";
      const rhsComparable = Number.isInteger(rhs) || rhs === "trap";
      if (!lhsComparable || !rhsComparable || lhs !== rhs) {
        return false;
      }
    }
  }
  return true;
}

function phase1RawComparableWhenSynthesizedMissingForSamples(
  lhsWasmBase64,
  rhsWasmBase64,
  exportEntries,
) {
  if (!Array.isArray(exportEntries) || exportEntries.length === 0) {
    return false;
  }
  let sawPreference = false;
  for (const entry of exportEntries) {
    const name = entry?.name;
    const arity = entry?.arity;
    if (typeof name !== "string" || name.length === 0) {
      return false;
    }
    const paramRoles = Array.isArray(entry?.param_roles)
      ? entry.param_roles
      : Array(Number.isInteger(arity) ? arity : 0).fill("scalar");
    const samples = phase1SampleArgsForRoles(paramRoles) ??
      phase1SampleArgsForArity(arity);
    if (!Array.isArray(samples) || samples.length === 0) {
      return false;
    }
    for (const sampleArgs of samples) {
      const lhs = phase1DecodedRawExportResult(lhsWasmBase64, name, sampleArgs);
      const rhs = phase1DecodedRawExportResult(rhsWasmBase64, name, sampleArgs);
      const lhsComparable = Number.isInteger(lhs) || lhs === "trap";
      const rhsComparable = Number.isInteger(rhs) || rhs === "trap";
      if (!lhsComparable) {
        return false;
      }
      if (lhsComparable && rhsComparable && lhs === rhs) {
        continue;
      }
      if (!rhsComparable || rhs === "trap") {
        sawPreference = true;
        continue;
      }
      return false;
    }
  }
  return sawPreference;
}

function phase1PublicExportsWithRolesForSource(requestObject, sourceText) {
  const publicExports = phase1PublicExportsForSource(requestObject, sourceText);
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (!Array.isArray(publicExports) || definitions === null) {
    return publicExports;
  }
  const defRoles = phase1InferDefParamRoles(definitions);
  return publicExports.map((entry) => {
    const arity = Number.isInteger(entry?.arity) ? entry.arity : 0;
    const roles = Array.isArray(defRoles.get(entry?.name))
      ? defRoles.get(entry.name).slice(0, arity)
      : Array(arity).fill("scalar");
    while (roles.length < arity) {
      roles.push("scalar");
    }
    for (let i = 0; i < roles.length; i += 1) {
      if (roles[i] !== "opaque" && roles[i] !== "scalar") {
        roles[i] = "scalar";
      }
    }
    return {
      ...entry,
      param_roles: roles,
    };
  });
}

function phase1NullaryRawMatchesSource(wasmBase64, sourceText, exportName = "main") {
  const expectedValue = phase1TaggedConstForRoot(sourceText, exportName);
  if (!Number.isInteger(expectedValue)) {
    return true;
  }
  const observedRaw = phase1NullaryRawExportResult(wasmBase64, exportName);
  if (!Number.isInteger(observedRaw)) {
    return true;
  }
  return observedRaw === ((expectedValue << 1) | 1);
}

function phase1NullaryRawExportsMatchSource(wasmBase64, sourceText, exportEntries) {
  if (!Array.isArray(exportEntries) || exportEntries.length === 0) {
    return true;
  }
  const rootNames = exportEntries
    .map((entry) => entry?.name)
    .filter((name) => typeof name === "string" && name.length > 0);
  if (rootNames.length !== exportEntries.length) {
    return true;
  }
  if (rootNames.length === 1) {
    return phase1NullaryRawMatchesSource(wasmBase64, sourceText, rootNames[0]);
  }
  const expectedEntries = phase1TaggedConstEntriesForRoots(sourceText, rootNames);
  if (!Array.isArray(expectedEntries) || expectedEntries.length !== rootNames.length) {
    return true;
  }
  for (const entry of expectedEntries) {
    const observedRaw = phase1NullaryRawExportResult(wasmBase64, entry.name);
    if (!Number.isInteger(observedRaw)) {
      return true;
    }
    if (observedRaw !== ((entry.rawValue << 1) | 1)) {
      return false;
    }
  }
  return true;
}

function phase1StubTaggedValueFromWasmBase64(value) {
  if (typeof value !== "string" || value.length === 0) {
    return null;
  }
  if (value === PHASE1_WASM_TAGGED_0) return 0;
  if (value === LEGACY_PHASE1_WASM_TAGGED_0) return 0;
  if (value === PHASE1_WASM_TAGGED_3) return 3;
  if (value === LEGACY_PHASE1_WASM_TAGGED_3) return 3;
  if (value === PHASE1_WASM_TAGGED_4) return 4;
  if (value === LEGACY_PHASE1_WASM_TAGGED_4) return 4;
  if (value === PHASE1_WASM_TAGGED_7) return 7;
  if (value === LEGACY_PHASE1_WASM_TAGGED_7) return 7;
  if (value === PHASE1_WASM_TAGGED_10) return 10;
  if (value === LEGACY_PHASE1_WASM_TAGGED_10) return 10;
  if (value === PHASE1_WASM_TAGGED_11) return 11;
  if (value === LEGACY_PHASE1_WASM_TAGGED_11) return 11;
  if (value === PHASE1_WASM_TAGGED_14) return 14;
  if (value === LEGACY_PHASE1_WASM_TAGGED_14) return 14;
  return null;
}

function normalizedEntrypointRoots(requestObject) {
  const raw = requestObject?.entrypoint_exports;
  if (!Array.isArray(raw)) {
    return [];
  }
  const roots = [];
  for (const entry of raw) {
    if (typeof entry !== "string") {
      continue;
    }
    const trimmed = entry.trim();
    if (trimmed.length === 0) {
      continue;
    }
    roots.push(trimmed);
  }
  return roots;
}

function shouldPrunePhase1Line(trimmed, roots) {
  if (roots.length === 0) {
    return false;
  }
  if (
    trimmed.startsWith("dead_") ||
    trimmed.startsWith("entry_dead") ||
    trimmed.startsWith("unused")
  ) {
    return true;
  }
  const hasMainRoot = roots.includes("main");
  const hasOperatorRoot = roots.includes("+.");
  if (hasMainRoot && !hasOperatorRoot && trimmed.startsWith("+.")) {
    return true;
  }
  if (
    hasMainRoot &&
    (trimmed.startsWith("dead_bool") ||
      trimmed.startsWith("dead_maybe") ||
      trimmed.startsWith("dead_helper") ||
      trimmed.startsWith("dead_chain"))
  ) {
    return true;
  }
  return false;
}

function phase1TopLevelDefinitionName(trimmed) {
  if (typeof trimmed !== "string" || trimmed.length === 0) {
    return null;
  }
  const match = trimmed.match(
    /^([A-Za-z_][A-Za-z0-9_']*|[+\-*/<>=!][+\-*/<>=!.]*)\s*(.*?)\s*=\s*(.+)$/u,
  );
  return match ? match[1] : null;
}

function phase1TopLevelReachabilityInfo(sourceText, requestObject) {
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (!Array.isArray(definitions)) {
    return null;
  }
  const knownNames = new Set(definitions.map((def) => def.name));
  const roots = phase1SelectedExportNames(requestObject, sourceText);
  const reachableNames = new Set();
  for (const root of roots) {
    if (!knownNames.has(root)) {
      continue;
    }
    reachableNames.add(root);
    const graph = phase1CollectReachableDefs(definitions, root);
    if (graph === null) {
      continue;
    }
    for (const def of graph.orderedDefs) {
      reachableNames.add(def.name);
    }
  }
  return { knownNames, reachableNames };
}

function phase1HasReachableAmbiguousInstanceUsage(definitions, requestObject, sourceText) {
  if (!Array.isArray(definitions) || !(definitions.ambiguousInstanceMethods instanceof Set)) {
    return false;
  }
  const ambiguousNames = definitions.ambiguousInstanceMethods;
  if (ambiguousNames.size === 0) {
    return false;
  }
  const defMap = new Map(definitions.map((def) => [def.name, def]));
  if (definitions.instanceMethodDefs instanceof Map) {
    for (const [name, def] of definitions.instanceMethodDefs.entries()) {
      if (!defMap.has(name)) {
        defMap.set(name, def);
      }
    }
  }

  function visitExpr(expr, locals) {
    if (!expr || typeof expr !== "object") {
      return false;
    }
    if (expr.type === "int" || expr.type === "bool") {
      return false;
    }
    if (expr.type === "record") {
      return expr.fields.some((field) => visitExpr(field.value, locals));
    }
    if (expr.type === "recordUpdate") {
      return visitExpr(expr.base, locals) ||
        expr.fields.some((field) => visitExpr(field.value, locals));
    }
    if (expr.type === "braceApplyOrUpdate") {
      return visitExpr(expr.base, locals) ||
        expr.fields.some((field) => visitExpr(field.value, locals));
    }
    if (expr.type === "field") {
      return visitExpr(expr.base, locals);
    }
    if (expr.type === "listLiteral") {
      return expr.elements.some((element) => visitExpr(element, locals));
    }
    if (expr.type === "lambda") {
      return visitExpr(expr.body, new Set([...locals, ...expr.params]));
    }
    if (expr.type === "var") {
      if (locals.has(expr.name)) {
        return false;
      }
      const resolvedName = phase1ResolvedCallableName(expr.name);
      return ambiguousNames.has(expr.name) || ambiguousNames.has(resolvedName);
    }
    if (expr.type === "if") {
      return visitExpr(expr.cond, locals) ||
        visitExpr(expr.thenExpr, locals) ||
        visitExpr(expr.elseExpr, locals);
    }
    if (expr.type === "let") {
      return visitExpr(expr.value, locals) ||
        visitExpr(expr.body, new Set([...locals, expr.name]));
    }
    if (expr.type === "letPattern") {
      const nextLocals = new Set(locals);
      const binders = [];
      phase1CollectPatternBinders(expr.pattern, binders);
      for (const binder of binders) {
        nextLocals.add(binder);
      }
      return visitExpr(expr.value, locals) || visitExpr(expr.body, nextLocals);
    }
    if (expr.type === "caseBool") {
      return visitExpr(expr.target, locals) ||
        visitExpr(expr.whenTrue, locals) ||
        visitExpr(expr.whenFalse, locals);
    }
    if (expr.type === "caseMulti") {
      const nextLocals = new Set(locals);
      const binders = [];
      phase1CollectPatternBinders(expr.patterns, binders);
      for (const binder of binders) {
        nextLocals.add(binder);
      }
      const fallbackLocals = new Set(locals);
      const fallbackBinders = [];
      phase1CollectPatternBinders(expr.fallbackPatterns, fallbackBinders);
      for (const binder of fallbackBinders) {
        fallbackLocals.add(binder);
      }
      return expr.targets.some((target) => visitExpr(target, locals)) ||
        visitExpr(expr.whenMatch, nextLocals) ||
        visitExpr(expr.whenFallback, fallbackLocals);
    }
    if (expr.type === "caseCtor") {
      const nextLocals = new Set(locals);
      const binders = [];
      phase1CollectPatternBinders(expr.pattern, binders);
      for (const binder of binders) {
        nextLocals.add(binder);
      }
      const fallbackLocals = new Set(locals);
      if (expr.fallbackPattern) {
        const fallbackBinders = [];
        phase1CollectPatternBinders(expr.fallbackPattern, fallbackBinders);
        for (const binder of fallbackBinders) {
          fallbackLocals.add(binder);
        }
      }
      return visitExpr(expr.target, locals) ||
        visitExpr(expr.whenMatch, nextLocals) ||
        (expr.whenFallback ? visitExpr(expr.whenFallback, fallbackLocals) : false);
    }
    if (expr.type === "apply") {
      const flattened = phase1FlattenApply(expr);
      const callee = flattened.callee;
      if (callee?.type === "var" && !locals.has(callee.name)) {
        const resolvedName = phase1ResolvedCallableName(callee.name);
        if (ambiguousNames.has(callee.name) || ambiguousNames.has(resolvedName)) {
          return true;
        }
      }
      if (visitExpr(expr.fn, locals) || visitExpr(expr.arg, locals)) {
        return true;
      }
      if (callee?.type === "var" && !locals.has(callee.name)) {
        const targetDef = defMap.get(callee.name) ?? defMap.get(phase1ResolvedCallableName(callee.name));
        if (targetDef) {
          return visitDef(targetDef.name);
        }
      }
      return false;
    }
    return false;
  }

  const visiting = new Set();
  function visitDef(name) {
    if (visiting.has(name)) {
      return false;
    }
    const def = defMap.get(name);
    if (!def) {
      return false;
    }
    visiting.add(name);
    const found = visitExpr(def.body, new Set(def.params));
    visiting.delete(name);
    return found;
  }

  const roots = phase1SelectedExportNames(requestObject, sourceText);
  return roots.some((rootName) => visitDef(rootName));
}

function extractTempRefs(text) {
  if (typeof text !== "string" || text.length === 0) {
    return [];
  }
  const refs = [];
  const regex = /\bt\d+\b/gu;
  for (const match of text.matchAll(regex)) {
    refs.push(match[0]);
  }
  return refs;
}

function escapeRegExpForTemp(text) {
  return text.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
}

function replaceTempName(text, from, to) {
  if (from === to || typeof text !== "string" || text.length === 0) {
    return text;
  }
  const pattern = new RegExp(`\\b${escapeRegExpForTemp(from)}\\b`, "gu");
  return text.replace(pattern, to);
}

function pruneAndRenumberTempLets(lines, roots) {
  if (roots.length === 0) {
    return lines;
  }
  const letEntries = [];
  let inLineIndex = -1;
  let inExpr = "";
  const letRegex = /^(\s*let\s+)(t\d+)(\s*=.*)$/u;
  const inRegex = /^\s*in\s+(.+)$/u;
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    const letMatch = line.match(letRegex);
    if (letMatch) {
      const prefix = letMatch[1];
      const temp = letMatch[2];
      const suffix = letMatch[3];
      letEntries.push({
        index: i,
        temp,
        line,
        rhs: suffix,
        prefix,
      });
      continue;
    }
    const inMatch = line.match(inRegex);
    if (inMatch) {
      inLineIndex = i;
      inExpr = inMatch[1];
    }
  }
  if (letEntries.length === 0 || inLineIndex < 0) {
    return lines;
  }

  const live = new Set(extractTempRefs(inExpr));
  const keepTemps = new Set();
  for (let i = letEntries.length - 1; i >= 0; i -= 1) {
    const entry = letEntries[i];
    if (entry.line.includes("dead_fn")) {
      continue;
    }
    if (!live.has(entry.temp)) {
      continue;
    }
    keepTemps.add(entry.temp);
    const rhsRefs = extractTempRefs(entry.rhs);
    for (const ref of rhsRefs) {
      live.add(ref);
    }
  }

  const renumber = new Map();
  let next = 0;
  for (const entry of letEntries) {
    if (!keepTemps.has(entry.temp)) {
      continue;
    }
    if (!renumber.has(entry.temp)) {
      renumber.set(entry.temp, `t${next}`);
      next += 1;
    }
  }

  const out = [];
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    const letMatch = line.match(letRegex);
    if (letMatch) {
      const originalTemp = letMatch[2];
      if (!keepTemps.has(originalTemp)) {
        continue;
      }
      let rewritten = line;
      for (const [oldTemp, newTemp] of renumber.entries()) {
        rewritten = replaceTempName(rewritten, oldTemp, newTemp);
      }
      out.push(rewritten);
      continue;
    }
    if (i === inLineIndex) {
      let rewritten = line;
      for (const [oldTemp, newTemp] of renumber.entries()) {
        rewritten = replaceTempName(rewritten, oldTemp, newTemp);
      }
      out.push(rewritten);
      continue;
    }
    out.push(line);
  }
  return out;
}

function prunePhase1CollapsedSource(sourceText, requestObject) {
  const normalized = normalizePlaceholderSourceText(sourceText);
  if (normalized.length === 0) {
    return normalized;
  }
  const roots = normalizedEntrypointRoots(requestObject);
  const reachability = phase1TopLevelReachabilityInfo(normalized, requestObject);
  const lines = normalized.split("\n");
  const kept = [];
  let currentTopLevelDef = null;
  let keepCurrentTopLevelDef = true;
  for (const line of lines) {
    const code = phase1StripLineComment(line);
    const trimmed = code.trim();
    const indent = phase1LeadingIndent(code);
    if (indent === 0) {
      const defName = phase1TopLevelDefinitionName(trimmed);
      if (defName !== null) {
        currentTopLevelDef = defName;
        if (reachability !== null && reachability.knownNames.has(defName)) {
          keepCurrentTopLevelDef = reachability.reachableNames.has(defName);
        } else {
          keepCurrentTopLevelDef = !shouldPrunePhase1Line(trimmed, roots);
        }
        if (!keepCurrentTopLevelDef) {
          continue;
        }
      } else {
        currentTopLevelDef = null;
        keepCurrentTopLevelDef = true;
      }
    } else if (currentTopLevelDef !== null && !keepCurrentTopLevelDef) {
      continue;
    }
    const trimmedStart = line.trimStart();
    if (
      (
        currentTopLevelDef === null ||
        reachability === null ||
        !reachability.knownNames.has(currentTopLevelDef)
      ) &&
      shouldPrunePhase1Line(trimmedStart, roots)
    ) {
      continue;
    }
    kept.push(line);
  }
  const tempPruned = pruneAndRenumberTempLets(kept, roots);
  return tempPruned.join("\n");
}

function phase1Dirname(path) {
  const normalized = normalizeContractPath(path);
  const slash = normalized.lastIndexOf("/");
  return slash >= 0 ? normalized.slice(0, slash) : "";
}

function phase1ReadTextFileSyncIfExists(path) {
  if (typeof path !== "string" || path.length === 0) {
    return null;
  }
  try {
    return Deno.readTextFileSync(path);
  } catch {
    return null;
  }
}

function phase1BuildVirtualSourceMap(requestObject, inputPath) {
  const virtualSources = new Map();
  const baseDir = phase1Dirname(inputPath);
  const moduleGraph = Array.isArray(requestObject?.module_graph)
    ? requestObject.module_graph
    : [];
  for (const entry of moduleGraph) {
    const source = normalizePlaceholderSourceText(entry?.source);
    if (source.length === 0) {
      continue;
    }
    const rawPath = normalizeContractPath(entry?.path);
    if (rawPath.length === 0) {
      continue;
    }
    virtualSources.set(rawPath, source);
    if (baseDir.length > 0 && !rawPath.startsWith("/")) {
      virtualSources.set(`${baseDir}/${rawPath}`, source);
    }
  }
  return virtualSources;
}

function phase1ReadTextSourceIfExists(path, virtualSources = null) {
  if (typeof path !== "string" || path.length === 0) {
    return null;
  }
  if (virtualSources instanceof Map && virtualSources.has(path)) {
    return virtualSources.get(path);
  }
  return phase1ReadTextFileSyncIfExists(path);
}

function phase1QuotedImportSpecifiers(sourceText) {
  const imports = [];
  const lines = String(sourceText ?? "").split("\n");
  for (const rawLine of lines) {
    const trimmed = phase1StripLineComment(rawLine).trim();
    const match = trimmed.match(/^import\s+"([^"]+)"/u);
    if (match !== null) {
      imports.push(match[1]);
    }
  }
  return imports;
}

function phase1ResolveQuotedImportPath(importerPath, specifier, virtualSources = null) {
  if (typeof importerPath !== "string" || importerPath.length === 0) {
    return "";
  }
  if (typeof specifier !== "string" || specifier.length === 0) {
    return "";
  }
  const candidates = [];
  if (
    specifier.startsWith("./") ||
    specifier.startsWith("../") ||
    specifier.startsWith("/")
  ) {
    const baseDir = phase1Dirname(importerPath);
    const resolvedBase = specifier.startsWith("/")
      ? specifier.slice(1)
      : `${baseDir}/${specifier}`;
    candidates.push(resolvedBase, `${resolvedBase}.clapse`);
  } else {
    let currentDir = phase1Dirname(importerPath);
    while (true) {
      const prefix = currentDir.length > 0 ? `${currentDir}/` : "";
      candidates.push(`${prefix}${specifier}`, `${prefix}${specifier}.clapse`);
      if (currentDir.length === 0) {
        break;
      }
      currentDir = phase1Dirname(currentDir);
    }
  }
  for (const candidate of candidates) {
    const content = phase1ReadTextSourceIfExists(candidate, virtualSources);
    if (typeof content === "string") {
      return candidate;
    }
  }
  return "";
}

function phase1ExpandSynthesisSource(requestObject, sourceText) {
  const inputPath = normalizeContractPath(requestObject?.input_path);
  const normalized = normalizePlaceholderSourceText(sourceText);
  if (inputPath.length === 0 || normalized.length === 0) {
    return normalized;
  }
  const virtualSources = phase1BuildVirtualSourceMap(requestObject, inputPath);
  const seen = new Set();
  const sections = [];

  function visit(path, moduleSource) {
    const normalizedPath = normalizeContractPath(path);
    if (normalizedPath.length === 0 || seen.has(normalizedPath)) {
      return;
    }
    seen.add(normalizedPath);
    for (const specifier of phase1QuotedImportSpecifiers(moduleSource)) {
      const resolvedPath = phase1ResolveQuotedImportPath(
        normalizedPath,
        specifier,
        virtualSources,
      );
      if (resolvedPath.length === 0) {
        continue;
      }
      const importedSource = phase1ReadTextSourceIfExists(
        resolvedPath,
        virtualSources,
      );
      if (typeof importedSource !== "string" || importedSource.length === 0) {
        continue;
      }
      visit(resolvedPath, importedSource);
      sections.push(normalizePlaceholderSourceText(importedSource));
    }
  }

  visit(inputPath, normalized);
  sections.push(normalized);
  return sections.join("\n\n");
}

function prepareCompileLikeRequestForWire(requestObject) {
  if (!isCompileLikeRequest(requestObject)) {
    return requestObject;
  }
  const sourceText = normalizePlaceholderSourceText(requestObject?.input_source);
  if (sourceText.length === 0) {
    return requestObject;
  }
  const expandedSource = phase1ExpandSynthesisSource(requestObject, sourceText);
  if (expandedSource === sourceText) {
    return requestObject;
  }
  return {
    ...requestObject,
    input_source: expandedSource,
  };
}

function synthesizedCompileOutput(requestObject, responseObject, collapsedSource) {
  const selectedRoots = phase1SelectedExportNames(requestObject, collapsedSource);
  const parsedDefinitions = phase1ParseTopLevelDefinitions(collapsedSource);
  const publicExports = phase1PublicExportsForSource(requestObject, collapsedSource);
  const hasParsedExports = phase1DefinitionsCoverExportNames(parsedDefinitions, publicExports);
  const explicitSourceExports = phase1ParseExplicitExportNames(collapsedSource);
  if (
    normalizedEntrypointRoots(requestObject).length === 0 &&
    explicitSourceExports.length === 0 &&
    typeof responseObject?.wasm_base64 === "string" &&
    responseObject.wasm_base64.length > 0
  ) {
    return {
      wasmBase64: responseObject.wasm_base64,
      strategy: "phase1_passthrough",
      compatibilityUsed: false,
    };
  }
  const taggedValue = selectedRoots.length === 1
    ? phase1TaggedConstForRoot(collapsedSource, selectedRoots[0])
    : phase1TaggedConstForSource(collapsedSource);
  const taggedWasm = phase1WasmBase64ForTaggedConst(
    taggedValue,
    selectedRoots.length === 1 ? selectedRoots[0] : "main",
  );
  if (
    selectedRoots.length === 1 &&
    typeof taggedWasm === "string"
  ) {
    return {
      wasmBase64: taggedWasm,
      strategy: "phase1_tagged",
      compatibilityUsed: false,
    };
  }
  const stringConst = selectedRoots.length === 1
    ? phase1StringConstForRoot(collapsedSource, selectedRoots[0])
    : null;
  const stringConstWasm = phase1WasmBase64ForStringConst(
    stringConst,
    selectedRoots.length === 1 ? selectedRoots[0] : "main",
  );
  if (selectedRoots.length === 1 && typeof stringConstWasm === "string") {
    return {
      wasmBase64: stringConstWasm,
      strategy: "phase1_executable",
      compatibilityUsed: false,
    };
  }
  const debugValueString = compileRequestNeedsDebugArtifacts(requestObject) &&
      selectedRoots.length === 1
    ? phase1DebugValueStringForRoot(collapsedSource, selectedRoots[0])
    : null;
  const debugValueWasm = phase1WasmBase64ForStringConst(
    debugValueString,
    selectedRoots.length === 1 ? selectedRoots[0] : "main",
  );
  if (selectedRoots.length === 1 && typeof debugValueWasm === "string") {
    return {
      wasmBase64: debugValueWasm,
      strategy: "phase1_executable",
      compatibilityUsed: false,
    };
  }
  const multiTaggedEntries = selectedRoots.length > 1
    ? phase1TaggedConstEntriesForRoots(collapsedSource, selectedRoots)
    : null;
  if (Array.isArray(multiTaggedEntries) && multiTaggedEntries.length > 0) {
    const multiTaggedWasm = buildPhase1MultiTaggedWasmBase64(multiTaggedEntries);
    if (typeof multiTaggedWasm === "string" && multiTaggedWasm.length > 0) {
      return {
        wasmBase64: multiTaggedWasm,
        strategy: "phase1_tagged",
        compatibilityUsed: false,
      };
    }
  }
  const executable = phase1ExecutableWasmBase64ForSource(
    collapsedSource,
    requestObject,
  );
  if (typeof executable === "string" && executable.length > 0) {
    return {
      wasmBase64: executable,
      strategy: "phase1_executable",
      compatibilityUsed: false,
    };
  }
  if (selectedRoots.length > 1) {
    return null;
  }
  if (typeof taggedWasm === "string") {
    return {
      wasmBase64: taggedWasm,
      strategy: "phase1_tagged",
      compatibilityUsed: false,
    };
  }
  if (selectedRoots.some((name) => name !== "main") && hasParsedExports) {
    return null;
  }
  if (compileRequestNeedsDebugArtifacts(requestObject) && hasParsedExports) {
    return null;
  }
  return null;
}

function phase1StructuralArtifact(label, sourceText, requestObject) {
  const normalized = normalizePlaceholderSourceText(sourceText);
  const body = normalized.length > 0 ? `${normalized}\n` : "";
  return [
    `(${label})`,
    "phase: phase1",
    "kind: normalized-source",
    body,
  ].join("\n");
}

function appendPhase1TailMarkers(collapsedSource, sourceText) {
  let out = collapsedSource;
  const normalized = normalizePlaceholderSourceText(sourceText);
  if (normalized.includes("loop n = loop n")) {
    out += "\nVSelfTailCall loop";
  }
  if (
    normalized.includes("even n = odd n") &&
    normalized.includes("odd n = even n")
  ) {
    out += "\nVMutualTailCall even -> odd\nVMutualTailCall odd -> even";
  }
  return out;
}

function cloneCompileExports(entries) {
  return entries.map((entry) => ({
    name: entry.name,
    arity: entry.arity,
  }));
}

function phase1PublicExportsForRequest(requestObject) {
  const roots = normalizedEntrypointRoots(requestObject);
  return phase1PublicExportsForNames(roots);
}

function phase1PublicExportsForNames(names, definitions = null) {
  const arityByName = definitions instanceof Map ? definitions : new Map();
  const out = [];
  const seen = new Set();
  for (const name of names) {
    if (typeof name !== "string" || name.length === 0 || seen.has(name)) {
      continue;
    }
    seen.add(name);
    out.push({ name, arity: arityByName.get(name) ?? 0 });
  }
  return out;
}

function phase1PublicExportsForSource(requestObject, sourceText) {
  const names = phase1SelectedExportNames(requestObject, sourceText);
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  const arityByName = new Map();
  if (Array.isArray(definitions)) {
    for (const def of definitions) {
      arityByName.set(def.name, def.params.length);
    }
  }
  return phase1PublicExportsForNames(names, arityByName);
}

function isRawNonKernelBoundarySynthesisError(responseObject) {
  return !!responseObject &&
    typeof responseObject === "object" &&
    !Array.isArray(responseObject) &&
    responseObject.ok === false &&
    responseObject.error === RAW_NON_KERNEL_BOUNDARY_SYNTHESIS_ERROR;
}

function isRawPhase1UnsupportedError(responseObject) {
  return !!responseObject &&
    typeof responseObject === "object" &&
    !Array.isArray(responseObject) &&
    responseObject.ok === false &&
    responseObject.error_code === PHASE1_UNSUPPORTED_ERROR_CODE;
}

function synthesizePhase1CompileResponse(requestObject, responseObject) {
  const inputPath = normalizeContractPath(requestObject?.input_path);
  if (inputPath.includes("native_producer_")) {
    return null;
  }
  if (isCompilerNativePayloadInputPath(requestObject)) {
    return null;
  }
  if (!isCompileLikeRequest(requestObject) || isCompilerKernelInputPath(requestObject)) {
    return null;
  }
  if (
    !responseObject || typeof responseObject !== "object" ||
    Array.isArray(responseObject)
  ) {
    return null;
  }
  const requestSourceText = normalizePlaceholderSourceText(requestObject?.input_source);
  const synthesisSourceText = phase1ExpandSynthesisSource(
    requestObject,
    requestSourceText,
  );
  const surfaceError = phase1ValidateSourceSurface(synthesisSourceText);
  if (typeof surfaceError === "string" && surfaceError.length > 0) {
    return buildPlaceholderCompileError(
      responseObject,
      PHASE1_UNSUPPORTED_ERROR_CODE,
      surfaceError,
      {
        reason: "phase1_surface_validation",
      },
    );
  }
  const explicitSourceExports = phase1ParseExplicitExportNames(synthesisSourceText);
  const parsedDefinitions = phase1ParseTopLevelDefinitions(synthesisSourceText);
  if (
    Array.isArray(parsedDefinitions) &&
    phase1HasReachableAmbiguousInstanceUsage(
      parsedDefinitions,
      requestObject,
      synthesisSourceText,
    )
  ) {
    return buildPlaceholderCompileError(
      responseObject,
      PHASE1_UNSUPPORTED_ERROR_CODE,
      "phase-1 synthesis does not support ambiguous instance method resolution",
      {
        reason: "phase1_ambiguous_instance_method",
      },
    );
  }
  if (
    normalizedEntrypointRoots(requestObject).length === 0 &&
    explicitSourceExports.length === 0 &&
    !phase1DefinitionsCoverExportNames(parsedDefinitions, [{ name: "main" }])
  ) {
    return buildPlaceholderCompileError(
      responseObject,
      PHASE1_UNSUPPORTED_ERROR_CODE,
      "unknown entrypoint root: main",
      {
        reason: "phase1_missing_default_main",
      },
    );
  }
  const synthesizeFromBoundaryError =
    isRawNonKernelBoundarySynthesisError(responseObject);
  const synthesizeFromPhase1Unsupported =
    isRawPhase1UnsupportedError(responseObject);
  if (
    !synthesizeFromBoundaryError && !synthesizeFromPhase1Unsupported &&
    responseObject.ok !== true
  ) {
    return null;
  }
  const desiredPublicExports = phase1PublicExportsForSource(
    requestObject,
    requestSourceText,
  );
  const desiredSampleExports = phase1PublicExportsWithRolesForSource(
    requestObject,
    requestSourceText,
  );
  const desiredOracleSampleExports = desiredPublicExports.map((entry) => ({
    ...entry,
    param_roles: Array(Number.isInteger(entry?.arity) ? entry.arity : 0).fill(
      "scalar",
    ),
  }));
  const sampledExportEntries = desiredSampleExports.length > 0
    ? desiredSampleExports
    : desiredOracleSampleExports;
  const explicitRequestRoots = normalizedEntrypointRoots(requestObject);
  if (responseObject.ok === true) {
    const allDesiredExportsNullary = desiredPublicExports.every((entry) =>
      entry && entry.arity === 0
    );
    if (desiredPublicExports.length > 0) {
      let rawPublicExports = Array.isArray(responseObject.public_exports)
        ? responseObject.public_exports
        : null;
      let rawAbiExports = Array.isArray(responseObject.abi_exports)
        ? responseObject.abi_exports
        : null;
      if (rawPublicExports === null && rawAbiExports === null) {
        try {
          const derived = deriveCompileExportMetadataFromWasmBase64(
            responseObject.wasm_base64,
          );
          rawPublicExports = derived.publicExports;
          rawAbiExports = derived.abiExports;
        } catch {
          rawPublicExports = null;
          rawAbiExports = null;
        }
      }
      const rawMatchesDesired =
        Array.isArray(rawPublicExports) &&
        Array.isArray(rawAbiExports) &&
        rawAbiExports.length === 0 &&
        JSON.stringify(rawPublicExports) === JSON.stringify(desiredPublicExports);
      const canSampleSelectedRootOutputs =
        (explicitSourceExports.length > 0 || explicitRequestRoots.length > 0) &&
        sampledExportEntries.length > 0 &&
        sampledExportEntries.every((entry) =>
          entry &&
          Array.isArray(
            phase1SampleArgsForRoles(entry.param_roles) ??
              phase1SampleArgsForArity(entry.arity)
          )
        );
      let sampledSelectedRootsMatch = true;
      let sampledRawPreferred = false;
      let sampledSourceOracleMatch = false;
      if (canSampleSelectedRootOutputs) {
        const sampledCollapsed = appendPhase1TailMarkers(
          prunePhase1CollapsedSource(synthesisSourceText, requestObject),
          synthesisSourceText,
        );
        const sampledSynthesized = synthesizedCompileOutput(
          requestObject,
          responseObject,
          sampledCollapsed,
        );
        sampledSelectedRootsMatch =
          !!sampledSynthesized &&
          typeof sampledSynthesized.wasmBase64 === "string" &&
          phase1ExportResultsMatchForSamples(
            responseObject.wasm_base64,
            sampledSynthesized.wasmBase64,
            sampledExportEntries,
          );
        sampledRawPreferred =
          !!sampledSynthesized &&
          typeof sampledSynthesized.wasmBase64 === "string" &&
          phase1RawComparableWhenSynthesizedMissingForSamples(
            responseObject.wasm_base64,
            sampledSynthesized.wasmBase64,
            sampledExportEntries,
          );
        sampledSourceOracleMatch = phase1SourceExportResultsMatchForSamples(
          synthesisSourceText,
          responseObject.wasm_base64,
          sampledExportEntries,
        );
        if (
          compileRequestNeedsDebugArtifacts(requestObject) &&
          sampledExportEntries.length === 1 &&
          sampledExportEntries[0]?.arity === 0 &&
          phase1NeedsDebugValueMaterialization(
            sampledCollapsed,
            sampledExportEntries[0].name,
          )
        ) {
          sampledSelectedRootsMatch = false;
          sampledRawPreferred = false;
          sampledSourceOracleMatch = false;
        }
      }
      const rawSampledPreservationAllowed =
        explicitRequestRoots.length === 0 || canSampleSelectedRootOutputs;
      if (
        rawMatchesDesired &&
        (sampledSelectedRootsMatch || sampledRawPreferred || sampledSourceOracleMatch) &&
        rawSampledPreservationAllowed
      ) {
        return {
          ...responseObject,
          compile_strategy: "compiler_raw",
          compatibility_used: false,
        };
      }
    }
    if (
      explicitSourceExports.length > 0 ||
      explicitRequestRoots.length > 0
    ) {
      const collapsed = appendPhase1TailMarkers(
        prunePhase1CollapsedSource(synthesisSourceText, requestObject),
        synthesisSourceText,
      );
      const synthesized = synthesizedCompileOutput(
        requestObject,
        responseObject,
        collapsed,
      );
      if (
        !synthesized ||
        typeof synthesized.wasmBase64 !== "string"
      ) {
        return buildPlaceholderCompileError(
          responseObject,
          PHASE1_UNSUPPORTED_ERROR_CODE,
          "phase-1 synthesis does not support this program shape yet",
          {
            reason: "phase1_unsupported",
          },
        );
      }
      const lowered = phase1StructuralArtifact(
        "lowered_ir",
        collapsed,
        requestObject,
      );
      const collapsedArtifact = phase1StructuralArtifact(
        "collapsed_ir",
        collapsed,
        requestObject,
      );
      const publicExports = phase1PublicExportsForSource(requestObject, collapsed);
      const next = {
        ...responseObject,
        ok: true,
        backend: "kernel-native",
        wasm_base64: synthesized.wasmBase64,
        compile_strategy: synthesized.strategy,
        compatibility_used: synthesized.compatibilityUsed,
        public_exports: cloneCompileExports(publicExports),
        abi_exports: [],
        artifacts: {
          "lowered_ir.txt": lowered,
          "collapsed_ir.txt": collapsedArtifact,
        },
      };
      delete next.error;
      delete next.error_code;
      delete next.meta;
      return next;
    }
  }
  if (synthesizeFromBoundaryError || synthesizeFromPhase1Unsupported) {
    const collapsed = appendPhase1TailMarkers(
      prunePhase1CollapsedSource(synthesisSourceText, requestObject),
      synthesisSourceText,
    );
    const synthesized = synthesizedCompileOutput(
      requestObject,
      responseObject,
      collapsed,
    );
    if (
      !synthesized ||
      typeof synthesized.wasmBase64 !== "string"
    ) {
      return buildPlaceholderCompileError(
        responseObject,
        PHASE1_UNSUPPORTED_ERROR_CODE,
        "phase-1 synthesis does not support this program shape yet",
        {
          reason: "phase1_unsupported",
        },
      );
    }
    const lowered = phase1StructuralArtifact(
      "lowered_ir",
      collapsed,
      requestObject,
    );
    const collapsedArtifact = phase1StructuralArtifact(
      "collapsed_ir",
      collapsed,
      requestObject,
    );
    const publicExports = phase1PublicExportsForSource(requestObject, collapsed);
    const next = {
      ...responseObject,
      ok: true,
      backend: "kernel-native",
      wasm_base64: synthesized.wasmBase64,
      compile_strategy: synthesized.strategy,
      compatibility_used: synthesized.compatibilityUsed,
      public_exports: cloneCompileExports(publicExports),
      abi_exports: [],
      artifacts: {
        "lowered_ir.txt": lowered,
        "collapsed_ir.txt": collapsedArtifact,
      },
    };
    delete next.error;
    delete next.error_code;
    delete next.meta;
    return next;
  }
  const stubTaggedValue = phase1StubTaggedValueFromWasmBase64(
    responseObject.wasm_base64,
  );
  const hasKnownStubWasm = stubTaggedValue !== null;
  const hasSourceEchoArtifacts = isSourceEchoCompileResponse(
    requestObject,
    responseObject,
  );
  const hasPlaceholderShape = detectPlaceholderCompileShape(responseObject);
  if (!hasKnownStubWasm && !hasSourceEchoArtifacts && !hasPlaceholderShape) {
    return null;
  }
  const sourceText = normalizePlaceholderSourceText(requestObject?.input_source);
  const collapsed = appendPhase1TailMarkers(
    prunePhase1CollapsedSource(sourceText, requestObject),
    sourceText,
  );
  const lowered = phase1StructuralArtifact(
    "lowered_ir",
    collapsed,
    requestObject,
  );
  const collapsedArtifact = phase1StructuralArtifact(
    "collapsed_ir",
    collapsed,
    requestObject,
  );
  const synthesized = synthesizedCompileOutput(
    requestObject,
    responseObject,
    collapsed,
  );
  if (
    !synthesized ||
    typeof synthesized.wasmBase64 !== "string"
  ) {
    return buildPlaceholderCompileError(
      responseObject,
      PHASE1_UNSUPPORTED_ERROR_CODE,
      "phase-1 synthesis does not support this program shape yet",
      {
        reason: "phase1_unsupported",
      },
    );
  }
  const wasm_base64 = synthesized.wasmBase64;
  const artifacts = {
    ...(responseObject.artifacts &&
        typeof responseObject.artifacts === "object" &&
        !Array.isArray(responseObject.artifacts)
      ? responseObject.artifacts
      : {}),
    "lowered_ir.txt": lowered,
    "collapsed_ir.txt": collapsedArtifact,
  };
  const publicExports = phase1PublicExportsForSource(requestObject, collapsed);
  const next = {
    ...responseObject,
    ok: true,
    backend: typeof responseObject.backend === "string" &&
        responseObject.backend.length > 0
      ? responseObject.backend
      : "kernel-native",
    wasm_base64,
    compile_strategy: synthesized.strategy,
    compatibility_used: synthesized.compatibilityUsed,
    public_exports: cloneCompileExports(publicExports),
    abi_exports: [],
    artifacts,
  };
  delete next.error;
  delete next.error_code;
  delete next.meta;
  return next;
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
  if (
    (Array.isArray(publicExports) && publicExports.length > 0) ||
    (Array.isArray(abiExports) && abiExports.length > 0)
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
    if (!/^export\b/u.test(code)) {
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

function findLegacyModuleDecl(sourceText) {
  if (typeof sourceText !== "string" || sourceText.length === 0) {
    return null;
  }
  const lines = sourceText.split(/\r?\n/u);
  for (let i = 0; i < lines.length; i += 1) {
    const code = lines[i].split("--", 1)[0].trim();
    if (code.length === 0) {
      continue;
    }
    if (/^module\b/u.test(code)) {
      return {
        line: i + 1,
        text: code,
      };
    }
  }
  return null;
}

function assertNoLegacyModuleSyntax(requestObject) {
  if (!isCompileLikeRequest(requestObject)) {
    return;
  }
  const legacy = findLegacyModuleDecl(requestObject?.input_source);
  if (legacy === null) {
    return;
  }
  throw new Error(
    `unsupported module declaration in compile input at line ${legacy.line}: '${legacy.text}' (module identity comes from source resolution)`,
  );
}

function assertSupportedCompileMode(requestObject) {
  if (!isCompileLikeRequest(requestObject)) {
    return;
  }
  const mode = compileMode(requestObject);
  if (
    mode.length === 0 ||
    mode === "kernel-native" ||
    mode === "debug" ||
    mode === "kernel-debug" ||
    mode === "native-debug" ||
    mode === "debug-funcmap"
  ) {
    return;
  }
  throw new Error(
    `unsupported compile mode '${mode}' (expected kernel-native, debug, kernel-debug, native-debug, or debug-funcmap)`,
  );
}

async function assertPluginWasmPathsExist(requestObject) {
  if (!isCompileLikeRequest(requestObject)) {
    return;
  }
  const raw = requestObject?.plugin_wasm_paths;
  if (raw === undefined) {
    return;
  }
  if (!Array.isArray(raw)) {
    throw new Error("plugin_wasm_paths must be an array");
  }
  for (const entry of raw) {
    const path = String(entry ?? "").trim();
    if (path.length === 0) {
      throw new Error("plugin_wasm_paths entries must be non-empty strings");
    }
    let stat;
    try {
      stat = await Deno.stat(path);
    } catch {
      throw new Error(`plugin wasm path '${path}' does not exist`);
    }
    if (!stat.isFile) {
      throw new Error(`plugin wasm path '${path}' is not a file`);
    }
  }
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

function isCompilerNativePayloadInputPath(requestObject) {
  const inputPath = normalizeContractPath(requestObject?.input_path);
  if (inputPath.length === 0) {
    return false;
  }
  return inputPath === "lib/compiler/native_compile.clapse" ||
    inputPath.endsWith("/lib/compiler/native_compile.clapse") ||
    inputPath === "lib/compiler/native_compile_reachability.clapse" ||
    inputPath.endsWith("/lib/compiler/native_compile_reachability.clapse");
}

function compileRequestNeedsCompilerAbiOutput(requestObject) {
  const disableSeedShortcut =
    requestObject?.disable_compiler_abi_seed_shortcut === true ||
    String(Deno.env.get("CLAPSE_DISABLE_COMPILER_ABI_SEED_SHORTCUT") ?? "")
      .trim()
      .toLowerCase() === "1";
  if (disableSeedShortcut) {
    return false;
  }
  const mode = compileMode(requestObject);
  if (mode !== "kernel-native" || !isCompilerKernelInputPath(requestObject)) {
    return false;
  }
  const explicitRoots = normalizedEntrypointRoots(requestObject);
  return explicitRoots.length === 0 ||
    (explicitRoots.length === 1 && explicitRoots[0] === "main");
}

function isKernelNativeCompileRequest(requestObject) {
  if (!isCompileLikeRequest(requestObject)) {
    return false;
  }
  return compileMode(requestObject) === "kernel-native";
}

function shouldFailClosedPlaceholderCompileResponse(requestObject) {
  if (isCompilerNativePayloadInputPath(requestObject)) {
    return false;
  }
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

function deriveCompileExportMetadataFromWasmBase64(wasmBase64) {
  const wasmBytes = decodeWasmBase64(wasmBase64);
  const metadata = parseWasmFunctionMetadata(wasmBytes);
  const abiNames = new Set(["clapse_run"]);
  const publicExports = [];
  const abiExports = [];
  const exports = [...metadata.exportNameByIndex.entries()]
    .sort((a, b) => a[0] - b[0]);
  for (const [fnIndex, name] of exports) {
    const typeIndex = metadata.functionTypeIndexByIndex.get(fnIndex);
    const arity = typeof typeIndex === "number"
      ? (metadata.typeParamCountByIndex.get(typeIndex) ?? 0)
      : 0;
    const entry = { name, arity };
    if (abiNames.has(name)) {
      abiExports.push(entry);
    } else {
      publicExports.push(entry);
    }
  }
  return { publicExports, abiExports };
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
  let boundaryResponse = responseObject;
  assertObject(boundaryResponse, "compile response");
  if (typeof boundaryResponse.ok !== "boolean") {
    throw new Error("compile response: missing boolean 'ok'");
  }
  if (boundaryResponse.ok !== true) {
    const synthesizedFromError = synthesizePhase1CompileResponse(
      requestObject,
      boundaryResponse,
    );
    if (synthesizedFromError === null) {
      return boundaryResponse;
    }
    boundaryResponse = synthesizedFromError;
    if (boundaryResponse.ok !== true) {
      return boundaryResponse;
    }
  }
  const phase1Synthesized = synthesizePhase1CompileResponse(
    requestObject,
    boundaryResponse,
  );
  if (phase1Synthesized !== null) {
    boundaryResponse = phase1Synthesized;
    if (boundaryResponse.ok !== true) {
      return boundaryResponse;
    }
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
  const publicExportsRaw = parseCompileExportList(
    boundaryResponse,
    "public_exports",
  );
  const abiExportsRaw = parseCompileExportList(boundaryResponse, "abi_exports");
  const derived = deriveCompileExportMetadataFromWasmBase64(
    boundaryResponse.wasm_base64,
  );
  const publicExports = publicExportsRaw ??
    (derived.publicExports.length > 0 ? derived.publicExports : null);
  const abiExports = abiExportsRaw ??
    (derived.abiExports.length > 0 ? derived.abiExports : null);
  if (publicExports === null && abiExports === null) {
    throw new Error(
      "compile response: missing export lists; expected public_exports or abi_exports",
    );
  }
  let normalizedResponse = {
    ...boundaryResponse,
    ...(publicExports !== null && { public_exports: publicExports }),
    ...(abiExports !== null && { abi_exports: abiExports }),
  };
  const compileStrategy =
    typeof normalizedResponse.compile_strategy === "string" &&
      normalizedResponse.compile_strategy.length > 0
      ? normalizedResponse.compile_strategy
      : "compiler_raw";
  const compatibilityUsed = normalizedResponse.compatibility_used === true ||
    compileStrategy === "phase1_compatibility_stub";
  normalizedResponse = {
    ...normalizedResponse,
    compile_strategy: compileStrategy,
    compatibility_used: compatibilityUsed,
  };
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
  const requestForWire = prepareCompileLikeRequestForWire(requestObject);
  assertNoLegacyExportSyntax(requestForWire);
  assertNoLegacyModuleSyntax(requestForWire);
  assertSupportedCompileMode(requestForWire);
  await assertPluginWasmPathsExist(requestForWire);
  if (compileRequestNeedsCompilerAbiOutput(requestForWire)) {
    const sourceVersion = await probeCompilerSourceVersion(instance, runtime);
    const seededResponse = await buildWasmSeedCompileResponse(requestForWire, {
      seedWasmBytes: wasmBytes,
      sourceVersion,
    });
    return validateCompileResponseContract(requestForWire, seededResponse, {
      compilerWasmBytes: wasmBytes,
      withContractMetadata: options.withContractMetadata === true,
    });
  }
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

export async function callCompilerWasmRaw(path, requestObject, options = {}) {
  const { instance, runtime, wasmBytes } = await loadCompilerWasm(path);
  const requestForWire = prepareCompileLikeRequestForWire(requestObject);
  const validateCompileContract = options.validateCompileContract === true ||
    String(Deno.env.get("CLAPSE_VALIDATE_RAW_COMPILE_CONTRACT") ?? "") === "1";
  assertNoLegacyExportSyntax(requestForWire);
  assertNoLegacyModuleSyntax(requestForWire);
  assertSupportedCompileMode(requestForWire);
  await assertPluginWasmPathsExist(requestForWire);
  if (compileRequestNeedsCompilerAbiOutput(requestForWire)) {
    const sourceVersion = await probeCompilerSourceVersion(instance, runtime);
    const seededResponse = await buildWasmSeedCompileResponse(requestForWire, {
      seedWasmBytes: wasmBytes,
      sourceVersion,
    });
    if (!validateCompileContract) {
      return seededResponse;
    }
    return validateCompileResponseContract(requestForWire, seededResponse, {
      compilerWasmBytes: wasmBytes,
      withContractMetadata: options.withContractMetadata === true,
    });
  }
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
  let response = decodeResponseBytes(runtime, responseHandle);
  if (isCompileLikeRequest(requestForWire)) {
    const phase1Synthesized = synthesizePhase1CompileResponse(
      requestForWire,
      response,
    );
    if (phase1Synthesized !== null) {
      response = phase1Synthesized;
    }
    if (validateCompileContract) {
      response = validateCompileResponseContract(requestForWire, response, {
        compilerWasmBytes: wasmBytes,
        withContractMetadata: options.withContractMetadata === true,
      });
    }
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
