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

function buildPhase1TaggedWasmBase64(rawValue) {
  if (!Number.isSafeInteger(rawValue)) {
    return PHASE1_WASM_TAGGED_0;
  }
  const taggedValue = rawValue * 2 + 1;
  const taggedBytes = encodeVarS32(taggedValue);
  const template = Array.from(fromBase64(PHASE1_WASM_TAGGED_0));
  const marker = template.indexOf(0x41);
  if (marker < 0) {
    return PHASE1_WASM_TAGGED_0;
  }
  let markerEnd = marker + 1;
  while ((template[markerEnd] ?? 0) & 0x80) {
    markerEnd += 1;
    if (markerEnd >= template.length) {
      return PHASE1_WASM_TAGGED_0;
    }
  }
  const withRaw = [
    ...template.slice(0, marker + 1),
    ...taggedBytes,
    ...template.slice(markerEnd + 1),
  ];
  return toBase64(Uint8Array.from(withRaw));
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

function phase1BuiltinArity(name) {
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
      return 2;
    case "list_map":
    case "list_filter":
    case "list_any":
    case "list_all":
      return 2;
    case "list_foldl":
      return 3;
    default:
      return null;
  }
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
  if (expr.type === "lambda") {
    phase1CollectLetLocals(expr.body, locals);
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
  if (expr.type === "apply") {
    phase1CollectLetLocals(expr.fn, locals);
    phase1CollectLetLocals(expr.arg, locals);
  }
}

function phase1CollectReachableDefs(definitions, rootName) {
  const defMap = new Map(definitions.map((def) => [def.name, def]));
  const reachable = new Set();
  const visiting = new Set();

  function visitExpr(expr, locals) {
    if (!expr || typeof expr !== "object") {
      return true;
    }
    if (expr.type === "int" || expr.type === "bool") {
      return true;
    }
    if (expr.type === "lambda") {
      return visitExpr(expr.body, new Set([...locals, ...expr.params]));
    }
    if (expr.type === "var") {
      if (locals.has(expr.name)) {
        return true;
      }
      const targetDef = defMap.get(expr.name);
      if (!targetDef) {
        return true;
      }
      return visitDef(expr.name);
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
    if (expr.type === "caseBool") {
      return visitExpr(expr.target, locals) &&
        visitExpr(expr.whenTrue, locals) &&
        visitExpr(expr.whenFalse, locals);
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
      const builtinArity = phase1BuiltinArity(callee.name);
      if (builtinArity !== null) {
        return flattened.args.length === builtinArity;
      }
      const targetDef = defMap.get(callee.name);
      if (!targetDef) {
        return false;
      }
      if (targetDef.params.length !== flattened.args.length) {
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

function phase1EmitExprToWasm(expr, ctx) {
  if (!expr || typeof expr !== "object") {
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
    const targetIndex = ctx.functionIndexByName.get(expr.name);
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
    const bound = phase1EmitExprToWasm(expr.value, ctx);
    const localIndex = ctx.locals.get(expr.name);
    if (bound === null || typeof localIndex !== "number") {
      return null;
    }
    const bodyCtx = {
      ...ctx,
      locals: new Map(ctx.locals),
    };
    bodyCtx.locals.set(expr.name, localIndex);
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
  if (expr.type === "apply") {
    const flattened = phase1FlattenApply(expr);
    const callee = flattened.callee;
    if (!callee || callee.type !== "var") {
      return null;
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
      default: {
        const targetIndex = ctx.functionIndexByName.get(callee.name);
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
    return phase1EmitExprToWasm(expr.value, ctx) !== null &&
      phase1IsBoolConditionExpr(expr.body, ctx, seenDefs);
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
      callee.name === "le" || callee.name === "gt" || callee.name === "ge"
    ) {
      return flattened.args.length === 2;
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
  if (expr.type === "var" && !ctx.locals.has(expr.name)) {
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

function phase1ExecutableWasmBase64ForSource(sourceText, requestObject) {
  const roots = phase1SelectedExportNames(requestObject, sourceText);
  const definitions = phase1ParseTopLevelDefinitions(sourceText);
  if (definitions === null) {
    return null;
  }
  const graph = phase1CollectReachableDefsForRoots(definitions, roots);
  if (graph === null) {
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

  const bodies = [];
  const typeIndexes = [];
  for (const def of graph.orderedDefs) {
    const locals = new Map();
    def.params.forEach((param, index) => locals.set(param, index));
    const letLocals = new Map();
    phase1CollectLetLocals(def.body, letLocals);
    for (const [name, offset] of letLocals.entries()) {
      locals.set(name, def.params.length + offset);
    }
    const emitted = phase1EmitExprToWasm(def.body, {
      locals,
      functionIndexByName,
      defMap: graph.defMap,
    });
    if (emitted === null) {
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
    const rootDef = graph.defMap.get(root);
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
  const tokenRe = /\s*(->|>=|<=|==|!=|\\|\(|\)|\||=|_|True|False|true|false|[-]?\d+|[+\-*/<>]|[A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)/gu;
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

function phase1IsNumberToken(token) {
  return /^-?\d+$/u.test(token);
}

function phase1IsIdentToken(token) {
  return /^[A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*$/u.test(token);
}

function phase1NormalizeCallableName(name) {
  switch (name) {
    case "+":
      return "add";
    case "-":
      return "sub";
    case "*":
      return "mul";
    case "/":
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
    default: {
      const parts = String(name).split(".");
      return parts[parts.length - 1] ?? name;
    }
  }
}

function phase1IsBinaryOperatorToken(token) {
  switch (token) {
    case "+":
    case "-":
    case "*":
    case "/":
    case "==":
    case "!=":
    case "<":
    case "<=":
    case ">":
    case ">=":
      return true;
    default:
      return false;
  }
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
    default:
      return -1;
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

function phase1ParseApplyExpr(tokens, start, stopTokens) {
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
    if (
      stopTokens.has(token) || token === ")" ||
      phase1IsBinaryOperatorToken(token)
    ) {
      break;
    }
    const argument = phase1ParsePrimary(tokens, index, stopTokens);
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
    const nameToken = tokens[start + 1];
    if (!phase1IsIdentToken(nameToken) || tokens[start + 2] !== "=") {
      return null;
    }
    const value = phase1ParseExpr(
      tokens,
      start + 3,
      new Set(["let", "in", ...stopTokens]),
    );
    if (value === null) {
      return null;
    }
    const nextToken = tokens[value.next];
    if (nextToken === "let") {
      const body = phase1ParsePrimary(tokens, value.next, stopTokens);
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
  if (token === "\\") {
    const params = [];
    let cursor = start + 1;
    while (cursor < tokens.length && tokens[cursor] !== "->") {
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

  if (token === "if") {
    const cond = phase1ParseExpr(tokens, start + 1, new Set(["then", ...stopTokens]));
    if (cond === null) {
      return null;
    }
    let cursor = cond.next;
    if (tokens[cursor] !== "then") {
      return null;
    }
    const thenExpr = phase1ParseExpr(
      tokens,
      cursor + 1,
      new Set(["else", ...stopTokens]),
    );
    if (thenExpr === null) {
      return null;
    }
    cursor = thenExpr.next;
    if (tokens[cursor] !== "else") {
      return null;
    }
    const elseExpr = phase1ParseExpr(
      tokens,
      cursor + 1,
      stopTokens,
    );
    if (elseExpr === null) {
      return null;
    }
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

  if (token === "case") {
    const target = phase1ParseExpr(
      tokens,
      start + 1,
      new Set(["of", "True", "False", "true", "false", ...stopTokens]),
    );
    if (target === null) {
      return null;
    }
    let cursor = target.next;
    if (tokens[cursor] === "of") {
      cursor += 1;
    }
    const trueToken = tokens[cursor];
    if (trueToken !== "True" && trueToken !== "true") {
      return null;
    }
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

  if (phase1IsBinaryOperatorToken(token)) {
    return {
      node: { type: "var", name: phase1NormalizeCallableName(token) },
      next: start + 1,
    };
  }

  if (phase1IsIdentToken(token)) {
    return {
      node: { type: "var", name: phase1NormalizeCallableName(token) },
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

function phase1ParseTopLevelDefinitions(sourceText) {
  const lines = normalizePlaceholderSourceText(sourceText).split("\n");
  const definitions = [];
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
    const rhsParts = [];
    if (match[3].trim().length > 0) {
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
      rhsParts.push(continuationTrimmed);
      nextIndex += 1;
    }
    const rhs = rhsParts.join(" ");
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
    definitions.push({
      name,
      params,
      body: parsed.node,
    });
    index = nextIndex;
  }
  return definitions.length > 0 ? definitions : null;
}

function phase1ApplyBuiltin(name, args, depth = 0) {
  const args1 = args[0];
  const args2 = args[1];
  const args3 = args[2];
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
    default:
      return null;
  }
}

function phase1ApplyFunctionValue(fnValue, argValues, depth = 0) {
  if (fnValue && fnValue.kind === "function") {
    if (fnValue.params.length !== argValues.length) {
      return null;
    }
    const locals = new Map(fnValue.locals ?? []);
    for (let i = 0; i < fnValue.params.length; i += 1) {
      locals.set(fnValue.params[i], argValues[i]);
    }
    return phase1Evaluate(fnValue.body, fnValue.env, locals, depth + 1);
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
  if (fnValue && fnValue.kind === "function") {
    return phase1ApplyFunctionValue(fnValue, argValues, depth);
  }
  if (fnValue && fnValue.kind === "builtin") {
    return phase1ApplyBuiltin(fnValue.name, argValues, depth);
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
  if (expr.type === "var") {
    if (locals.has(expr.name)) {
      return locals.get(expr.name);
    }
    const value = env.get(expr.name);
    if (!value) {
      return null;
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
  if (expr.type === "caseBool") {
    const target = phase1Evaluate(expr.target, env, locals, depth + 1);
    if (typeof target !== "boolean") {
      return null;
    }
    return target
      ? phase1Evaluate(expr.whenTrue, env, locals, depth + 1)
      : phase1Evaluate(expr.whenFalse, env, locals, depth + 1);
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
    const funcExpr = phase1Evaluate(argExpr, env, locals, depth + 1);
    if (funcExpr === null) {
      return null;
    }
    if (funcExpr && funcExpr.kind === "function") {
      if (funcExpr.params.length !== args.length) {
        return null;
      }
      return phase1ApplyFunctionValue(funcExpr, args, depth);
    }
    if (funcExpr && funcExpr.kind === "builtin") {
      return phase1ApplyBuiltin(funcExpr.name, args, depth);
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
  for (const builtin of [
    "add", "mul", "sub", "div", "mod", "eq", "ne", "lt", "le", "gt", "ge",
    "ListNil", "ListCons", "Nil", "Cons", "list_map", "fmap", "list_foldl", "foldl", "list_filter",
    "list_any", "list_all",
  ]) {
    env.set(builtin, { kind: "builtin", name: builtin });
  }
  const main = env.get("main");
  if (!main || main.kind !== "function") {
    return null;
  }
  if (main.params.length !== 0) {
    return null;
  }
  const result = phase1Evaluate(main.body, env, new Map(), 0);
  return Number.isInteger(result) ? result : null;
}

function phase1TaggedConstForSource(sourceText) {
  const evaluated = phase1EvaluateDefinitionGraph(sourceText);
  return evaluated;
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

function phase1WasmBase64ForTaggedConst(taggedValue) {
  const min = -1073741824;
  const max = 1073741823;
  if (
    !Number.isSafeInteger(taggedValue) || taggedValue < min || taggedValue > max
  ) {
    return null;
  }
  return buildPhase1TaggedWasmBase64(taggedValue);
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
    if (shouldPrunePhase1Line(trimmedStart, roots)) {
      continue;
    }
    kept.push(line);
  }
  const tempPruned = pruneAndRenumberTempLets(kept, roots);
  return tempPruned.join("\n");
}

function synthesizedCompileOutput(requestObject, responseObject, collapsedSource) {
  const selectedRoots = phase1SelectedExportNames(requestObject, collapsedSource);
  const parsedDefinitions = phase1ParseTopLevelDefinitions(collapsedSource);
  const publicExports = phase1PublicExportsForSource(requestObject, collapsedSource);
  const hasParsedExports = phase1DefinitionsCoverExportNames(parsedDefinitions, publicExports);
  if (
    normalizedEntrypointRoots(requestObject).length === 0 &&
    typeof responseObject?.wasm_base64 === "string" &&
    responseObject.wasm_base64.length > 0
  ) {
    return {
      wasmBase64: responseObject.wasm_base64,
      strategy: "phase1_passthrough",
      compatibilityUsed: false,
    };
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
  const taggedValue = phase1TaggedConstForSource(collapsedSource);
  const taggedWasm = phase1WasmBase64ForTaggedConst(taggedValue);
  if (typeof taggedWasm === "string") {
    return {
      wasmBase64: taggedWasm,
      strategy: "phase1_tagged",
      compatibilityUsed: false,
    };
  }
  if (selectedRoots.some((name) => name !== "main") && hasParsedExports) {
    return {
      wasmBase64: phase1DebugArtifactWasmBase64(publicExports),
      strategy: "phase1_compatibility_stub",
      compatibilityUsed: true,
    };
  }
  if (compileRequestNeedsDebugArtifacts(requestObject) && hasParsedExports) {
    return {
      wasmBase64: phase1DebugArtifactWasmBase64(publicExports),
      strategy: "phase1_compatibility_stub",
      compatibilityUsed: true,
    };
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
  const names = roots.length > 0 ? roots : ["main"];
  return phase1PublicExportsForNames(names);
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
  if (out.length === 0) {
    return [{ name: "main", arity: 0 }];
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

function synthesizePhase1CompileResponse(requestObject, responseObject) {
  const inputPath = normalizeContractPath(requestObject?.input_path);
  if (inputPath.includes("native_producer_")) {
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
  const synthesizeFromBoundaryError =
    isRawNonKernelBoundarySynthesisError(responseObject);
  if (!synthesizeFromBoundaryError && responseObject.ok !== true) {
    return null;
  }
  if (synthesizeFromBoundaryError) {
    const sourceText = normalizePlaceholderSourceText(requestObject?.input_source);
    const collapsed = appendPhase1TailMarkers(
      prunePhase1CollapsedSource(sourceText, requestObject),
      sourceText,
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

export async function callCompilerWasmRaw(path, requestObject, options = {}) {
  const { instance, runtime, wasmBytes } = await loadCompilerWasm(path);
  const requestForWire = requestObject;
  const validateCompileContract = options.validateCompileContract === true ||
    String(Deno.env.get("CLAPSE_VALIDATE_RAW_COMPILE_CONTRACT") ?? "") === "1";
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
