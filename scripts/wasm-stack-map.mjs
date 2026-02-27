#!/usr/bin/env -S deno run -A

const UTF8 = new TextDecoder();

function usage() {
  return [
    "wasm-stack-map",
    "",
    "Usage:",
    "  deno run -A scripts/wasm-stack-map.mjs [--wasm <path>] [--offsets n1,n2,...]",
    "  <stack.txt deno run -A scripts/wasm-stack-map.mjs [--wasm <path>]",
    "",
    "Notes:",
    "  - Offsets are the final numeric column from wasm stack frames",
    "    like wasm://wasm/<id>:1:<offset>.",
    "  - Default wasm path: artifacts/latest/clapse_compiler.wasm",
  ].join("\n");
}

function parseArgs(rawArgs) {
  const out = {
    wasmPath: "artifacts/latest/clapse_compiler.wasm",
    offsets: [],
    help: false,
  };
  for (let i = 0; i < rawArgs.length; i += 1) {
    const arg = rawArgs[i];
    if (arg === "--help" || arg === "-h") {
      out.help = true;
      continue;
    }
    if (arg === "--wasm") {
      if (i + 1 >= rawArgs.length) {
        throw new Error("--wasm requires a path");
      }
      out.wasmPath = rawArgs[i + 1];
      i += 1;
      continue;
    }
    if (arg === "--offsets") {
      if (i + 1 >= rawArgs.length) {
        throw new Error("--offsets requires a comma-separated list");
      }
      out.offsets = parseOffsetsArg(rawArgs[i + 1]);
      i += 1;
      continue;
    }
    throw new Error(`unknown argument: ${arg}`);
  }
  return out;
}

function parseOffsetsArg(text) {
  return String(text)
    .split(",")
    .map((x) => Number.parseInt(x.trim(), 10))
    .filter((x) => Number.isInteger(x) && x >= 0);
}

function stripFrameTokenPunctuation(rawToken) {
  return String(rawToken)
    .replace(/^[\s"'`]+/u, "")
    .replace(/[)\]},"';]+$/u, "");
}

function parseDenoWasmStackOffset(rawToken) {
  const token = stripFrameTokenPunctuation(rawToken);
  const wasmAt = token.indexOf("wasm://");
  if (wasmAt < 0) {
    return null;
  }
  const marker = token.slice(wasmAt);
  if (!marker.startsWith("wasm://")) {
    return null;
  }
  const lastColon = marker.lastIndexOf(":");
  if (lastColon <= 0 || lastColon === marker.length - 1) {
    return null;
  }
  const secondLastColon = marker.lastIndexOf(":", lastColon - 1);
  if (secondLastColon <= 0 || secondLastColon + 1 >= lastColon) {
    return null;
  }
  const lineText = marker.slice(secondLastColon + 1, lastColon);
  const offsetText = marker.slice(lastColon + 1);
  if (!/^\d+$/u.test(lineText) || !/^\d+$/u.test(offsetText)) {
    return null;
  }
  return Number.parseInt(offsetText, 10);
}

function parseOffsetsFromStackText(text) {
  const out = [];
  const seen = new Set();
  const re = /wasm:\/\/[^\s)]+/g;
  for (const match of text.matchAll(re)) {
    const n = parseDenoWasmStackOffset(match[0]);
    if (n === null) {
      continue;
    }
    if (seen.has(n)) {
      continue;
    }
    seen.add(n);
    out.push(n);
  }
  return out;
}

async function readAllStdinText() {
  const chunks = [];
  const buf = new Uint8Array(16 * 1024);
  while (true) {
    const n = await Deno.stdin.read(buf);
    if (n === null) break;
    chunks.push(buf.slice(0, n));
  }
  const total = chunks.reduce((acc, c) => acc + c.length, 0);
  if (total === 0) return "";
  const out = new Uint8Array(total);
  let off = 0;
  for (const c of chunks) {
    out.set(c, off);
    off += c.length;
  }
  return UTF8.decode(out);
}

function readVar(bytes, at) {
  let p = at;
  let value = 0;
  let shift = 0;
  while (true) {
    if (p >= bytes.length) throw new Error("unexpected EOF in varuint");
    const b = bytes[p];
    p += 1;
    value |= (b & 0x7f) << shift;
    if ((b & 0x80) === 0) {
      return { value: value >>> 0, next: p };
    }
    shift += 7;
    if (shift > 35) throw new Error("varuint overflow");
  }
}

function readString(bytes, at) {
  const lenInfo = readVar(bytes, at);
  const start = lenInfo.next;
  const end = start + lenInfo.value;
  if (end > bytes.length) throw new Error("string out of bounds");
  return { value: UTF8.decode(bytes.slice(start, end)), next: end };
}

function skipLimits(bytes, at) {
  const flag = bytes[at];
  let p = at + 1;
  const minInfo = readVar(bytes, p);
  p = minInfo.next;
  if (flag === 1) {
    const maxInfo = readVar(bytes, p);
    p = maxInfo.next;
  }
  return p;
}

function parseImportFunctionCount(bytes, start, end) {
  let p = start;
  const countInfo = readVar(bytes, p);
  p = countInfo.next;
  let importFunctionCount = 0;
  for (let i = 0; i < countInfo.value; i += 1) {
    const moduleInfo = readString(bytes, p);
    p = moduleInfo.next;
    const nameInfo = readString(bytes, p);
    p = nameInfo.next;
    const kind = bytes[p];
    p += 1;
    if (kind === 0) {
      const typeInfo = readVar(bytes, p);
      p = typeInfo.next;
      importFunctionCount += 1;
    } else if (kind === 1) {
      p += 1; // elem type
      p = skipLimits(bytes, p);
    } else if (kind === 2) {
      p = skipLimits(bytes, p);
    } else if (kind === 3) {
      p += 1; // valtype
      p += 1; // mutability
    } else {
      throw new Error(`unknown import kind: ${kind}`);
    }
    if (p > end) throw new Error("import section out of bounds");
  }
  return importFunctionCount;
}

function parseCodeBodies(bytes, start, end, importFunctionCount) {
  let p = start;
  const countInfo = readVar(bytes, p);
  p = countInfo.next;
  const out = [];
  for (let i = 0; i < countInfo.value; i += 1) {
    const bodySizeInfo = readVar(bytes, p);
    p = bodySizeInfo.next;
    const bodyStart = p;
    const bodyEnd = bodyStart + bodySizeInfo.value;
    if (bodyEnd > end) throw new Error("code body out of bounds");
    out.push({
      functionIndex: importFunctionCount + i,
      localIndex: i,
      start: bodyStart,
      end: bodyEnd,
      size: bodySizeInfo.value,
    });
    p = bodyEnd;
  }
  return out;
}

function parseNameSection(bytes, start, end) {
  const secName = readString(bytes, start);
  let p = secName.next;
  const functionNames = new Map();
  if (secName.value !== "name") {
    return functionNames;
  }
  while (p < end) {
    const subId = bytes[p];
    p += 1;
    const subSizeInfo = readVar(bytes, p);
    p = subSizeInfo.next;
    const subStart = p;
    const subEnd = subStart + subSizeInfo.value;
    if (subEnd > end) throw new Error("name subsection out of bounds");
    if (subId === 1) {
      const countInfo = readVar(bytes, p);
      p = countInfo.next;
      for (let i = 0; i < countInfo.value; i += 1) {
        const idxInfo = readVar(bytes, p);
        p = idxInfo.next;
        const nmInfo = readString(bytes, p);
        p = nmInfo.next;
        functionNames.set(idxInfo.value, nmInfo.value);
      }
    }
    p = subEnd;
  }
  return functionNames;
}

function parseFuncMapSection(bytes, start, end) {
  const secName = readString(bytes, start);
  let p = secName.next;
  if (secName.value !== "clapse.funcmap") {
    return { matched: false, names: new Map() };
  }

  const names = new Map();
  const countInfo = readVar(bytes, p);
  p = countInfo.next;

  for (let i = 0; i < countInfo.value; i += 1) {
    const idxInfo = readVar(bytes, p);
    p = idxInfo.next;
    const nmInfo = readString(bytes, p);
    p = nmInfo.next;
    names.set(idxInfo.value, nmInfo.value);
  }

  // Future payload extensions may append metadata; preserve compatibility by
  // parsing only the expected index/name pairs.
  if (p > end) {
    throw new Error("clapse.funcmap section out of bounds");
  }
  return { matched: true, names };
}

function parseWasmIndex(bytes) {
  if (bytes.length < 8) throw new Error("wasm too small");
  if (
    bytes[0] !== 0x00 || bytes[1] !== 0x61 || bytes[2] !== 0x73 ||
    bytes[3] !== 0x6d
  ) {
    throw new Error("bad wasm magic");
  }
  if (
    bytes[4] !== 0x01 || bytes[5] !== 0x00 || bytes[6] !== 0x00 ||
    bytes[7] !== 0x00
  ) {
    throw new Error("unsupported wasm version");
  }

  let p = 8;
  let importFunctionCount = 0;
  let codeBodies = [];
  let functionNames = new Map();
  let functionNameSource = "unresolved";
  let sawNameSection = false;
  let sawFuncMap = false;
  while (p < bytes.length) {
    const id = bytes[p];
    p += 1;
    const sizeInfo = readVar(bytes, p);
    p = sizeInfo.next;
    const start = p;
    const end = start + sizeInfo.value;
    if (end > bytes.length) throw new Error("section out of bounds");
    if (id === 2) {
      importFunctionCount = parseImportFunctionCount(bytes, start, end);
    } else if (id === 10) {
      codeBodies = parseCodeBodies(bytes, start, end, importFunctionCount);
    } else if (id === 0) {
      const names = parseNameSection(bytes, start, end);
      if (names.size > 0) {
        functionNames = names;
        functionNameSource = "name-section";
        sawNameSection = true;
      }
      try {
        const parsedFuncMap = parseFuncMapSection(bytes, start, end);
        if (parsedFuncMap.matched) {
          sawFuncMap = true;
          if (!sawNameSection) {
            functionNames = parsedFuncMap.names;
            functionNameSource = "clapse.funcmap";
          }
        }
      } catch (_e) {
        // Ignore malformed clapse.funcmap sections while keeping name-section
        // behavior as-is.
      }
    }
    p = end;
  }
  if (sawFuncMap && !sawNameSection && functionNames.size === 0) {
    functionNameSource = "clapse.funcmap";
  }
  return {
    importFunctionCount,
    codeBodies,
    functionNames,
    functionNameSource,
  };
}

function mapOffsetToFunction(codeBodies, offset) {
  for (const body of codeBodies) {
    if (offset >= body.start && offset < body.end) {
      return body;
    }
  }
  return null;
}

async function main() {
  const cfg = parseArgs(Deno.args);
  if (cfg.help) {
    console.log(usage());
    return;
  }
  let offsets = cfg.offsets;
  if (offsets.length === 0) {
    const stackText = await readAllStdinText();
    offsets = parseOffsetsFromStackText(stackText);
  }
  if (offsets.length === 0) {
    throw new Error("no offsets provided; use --offsets or pipe a wasm stack trace");
  }
  const wasm = await Deno.readFile(cfg.wasmPath);
  const index = parseWasmIndex(wasm);
  const uniqueOffsets = [...new Set(offsets)].sort((a, b) => a - b);

  console.log(
    JSON.stringify({
      wasm_path: cfg.wasmPath,
      import_function_count: index.importFunctionCount,
      code_bodies: index.codeBodies.length,
      function_names: index.functionNames.size,
      function_name_source: index.functionNameSource,
      offsets: uniqueOffsets,
    }),
  );
  for (const offset of uniqueOffsets) {
    const body = mapOffsetToFunction(index.codeBodies, offset);
    if (body === null) {
      console.log(
        JSON.stringify({
          offset,
          mapped: false,
        }),
      );
      continue;
    }
    const name = index.functionNames.get(body.functionIndex) ?? "";
    console.log(
      JSON.stringify({
        offset,
        mapped: true,
        function_index: body.functionIndex,
        function_local_index: body.localIndex,
        function_name: name,
        function_name_source: index.functionNameSource,
        body_start: body.start,
        body_end: body.end,
        body_size: body.size,
      }),
    );
  }
}

if (import.meta.main) {
  await main();
}
