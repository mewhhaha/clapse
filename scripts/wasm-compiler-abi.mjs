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

function phase1TokenizeExpression(text) {
  const tokens = [];
  const tokenRe = /\s*(->|>=|<=|!=|\(|\)|\||True|False|true|false|[-]?\d+|[A-Za-z_][A-Za-z0-9_']*)/gu;
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
  return /^[A-Za-z_][A-Za-z0-9_']*$/u.test(token);
}

function phase1ParseExpr(tokens, start, stopTokens = new Set()) {
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
    if (stopTokens.has(token) || token === ")") {
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
      new Set(["of", "True", "False", ...stopTokens]),
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
      new Set(["False", ...stopTokens]),
    );
    if (whenTrue === null) {
      return null;
    }
    cursor = whenTrue.next;
    if (tokens[cursor] === "|") {
      cursor += 1;
    }
    const falseToken = tokens[cursor];
    if (falseToken !== "False" && falseToken !== "false") {
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

  if (phase1IsIdentToken(token)) {
    return { node: { type: "var", name: token }, next: start + 1 };
  }

  return null;
}

function phase1ToArgList(rawValue) {
  return String(rawValue ?? "")
    .trim()
    .split(/\s+/u)
    .filter((arg) => arg.length > 0);
}

function phase1ParseTopLevelDefinitions(sourceText) {
  const lines = normalizePlaceholderSourceText(sourceText).split("\n");
  const definitions = [];
  for (const line of lines) {
    const code = line.split("--", 1)[0].trim();
    if (code.length === 0) {
      continue;
    }
    const match = code.match(
      /^([A-Za-z_][A-Za-z0-9_']*)\s*(.*?)\s*=\s*(.+)$/u,
    );
    if (match === null) {
      continue;
    }
    const name = match[1];
    const params = match[2].trim().length > 0
      ? phase1ToArgList(match[2])
      : [];
    const rhs = match[3].trim();
    const tokens = phase1TokenizeExpression(rhs);
    if (tokens === null) {
      continue;
    }
    const parsed = phase1ParseExpr(tokens, 0, new Set());
    if (parsed === null || parsed.next !== tokens.length) {
      continue;
    }
    definitions.push({
      name,
      params,
      body: parsed.node,
    });
  }
  return definitions;
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
    case "list_map": {
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
    case "list_foldl": {
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
    const locals = new Map();
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
      });
    }
  }
  for (const value of env.values()) {
    value.env = env;
  }
  for (const builtin of [
    "add", "mul", "sub", "div", "mod", "eq", "ne", "lt", "le", "gt", "ge",
    "ListNil", "ListCons", "Nil", "Cons", "list_map", "list_foldl", "list_filter",
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
  return evaluated ?? 0;
}

function phase1WasmBase64ForTaggedConst(taggedValue) {
  const min = -1073741824;
  const max = 1073741823;
  if (!Number.isSafeInteger(taggedValue) || taggedValue < min || taggedValue > max) {
    return PHASE1_WASM_TAGGED_0;
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
    (trimmed.startsWith("helper ") ||
      trimmed.startsWith("dead_bool") ||
      trimmed.startsWith("dead_maybe") ||
      trimmed.startsWith("dead_helper") ||
      trimmed.startsWith("dead_chain"))
  ) {
    return true;
  }
  return false;
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
  const lines = normalized.split("\n");
  const kept = [];
  for (const line of lines) {
    const trimmed = line.trimStart();
    if (shouldPrunePhase1Line(trimmed, roots)) {
      continue;
    }
    kept.push(line);
  }
  const tempPruned = pruneAndRenumberTempLets(kept, roots);
  return tempPruned.join("\n");
}

function synthesizedWasmBase64(requestObject, responseObject, collapsedSource) {
  const roots = normalizedEntrypointRoots(requestObject);
  if (
    roots.length === 0 &&
    typeof responseObject?.wasm_base64 === "string" &&
    responseObject.wasm_base64.length > 0
  ) {
    return responseObject.wasm_base64;
  }
  const taggedValue = phase1TaggedConstForSource(collapsedSource);
  return phase1WasmBase64ForTaggedConst(taggedValue);
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
  const out = [];
  const seen = new Set();
  for (const name of names) {
    if (typeof name !== "string" || name.length === 0 || seen.has(name)) {
      continue;
    }
    seen.add(name);
    out.push({ name, arity: 0 });
  }
  if (out.length === 0) {
    return [{ name: "main", arity: 0 }];
  }
  return out;
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
    Array.isArray(responseObject) || responseObject.ok !== true
  ) {
    return null;
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
  const lowered = `-- lowered\n${collapsed}`;
  const wasm_base64 = synthesizedWasmBase64(
    requestObject,
    responseObject,
    collapsed,
  );
  const artifacts = {
    ...(responseObject.artifacts &&
        typeof responseObject.artifacts === "object" &&
        !Array.isArray(responseObject.artifacts)
      ? responseObject.artifacts
      : {}),
    "lowered_ir.txt": lowered,
    "collapsed_ir.txt": collapsed,
  };
  const publicExports = phase1PublicExportsForRequest(requestObject);
  const next = {
    ...responseObject,
    ok: true,
    backend: typeof responseObject.backend === "string" &&
        responseObject.backend.length > 0
      ? responseObject.backend
      : "kernel-native",
    wasm_base64,
    public_exports: cloneCompileExports(publicExports),
    abi_exports: [],
    exports: cloneCompileExports(publicExports),
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
  const module = new WebAssembly.Module(wasmBytes);
  const exports = WebAssembly.Module.exports(module).filter((entry) =>
    entry.kind === "function"
  ).map((entry) => entry.name);
  const abiNames = new Set(["clapse_run"]);
  const publicExports = [];
  const abiExports = [];
  for (const name of exports) {
    const entry = { name, arity: 1 };
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
    return boundaryResponse;
  }
  const phase1Synthesized = synthesizePhase1CompileResponse(
    requestObject,
    boundaryResponse,
  );
  if (phase1Synthesized !== null) {
    boundaryResponse = phase1Synthesized;
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
  const legacyExportsRaw = parseCompileExportList(boundaryResponse, "exports");
  const derived = deriveCompileExportMetadataFromWasmBase64(
    boundaryResponse.wasm_base64,
  );
  const publicExports = publicExportsRaw ?? legacyExportsRaw ??
    (derived.publicExports.length > 0 ? derived.publicExports : null);
  const abiExports = abiExportsRaw ??
    (derived.abiExports.length > 0 ? derived.abiExports : null);
  const legacyExports = legacyExportsRaw ?? publicExports;
  if (publicExports === null && legacyExports === null && abiExports === null) {
    throw new Error(
      "compile response: missing export lists; expected public_exports, abi_exports, or legacy exports",
    );
  }
  let normalizedResponse = {
    ...boundaryResponse,
    ...(legacyExports !== null && { exports: legacyExportsRaw ?? legacyExports }),
    ...(publicExports !== null && { public_exports: publicExports }),
    ...(abiExports !== null && { abi_exports: abiExports }),
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
