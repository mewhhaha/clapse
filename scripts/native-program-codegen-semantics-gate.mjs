#!/usr/bin/env -S deno run -A

import {
  callCompilerWasmRaw,
  decodeWasmBase64,
} from "./wasm-compiler-abi.mjs";
import {
  decodeInt,
  instantiateWithRuntime,
} from "./wasm-runtime.mjs";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function fail(message) {
  console.error(`native-program-codegen-semantics-gate: FAIL (${message})`);
  Deno.exit(1);
}

function resolveCompilerWasmPath() {
  const fromEnv = String(Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ?? "").trim();
  if (fromEnv.length > 0) {
    return fromEnv;
  }
  return "artifacts/latest/clapse_compiler.wasm";
}

function toHex(bytes) {
  let out = "";
  for (const value of bytes) {
    out += value.toString(16).padStart(2, "0");
  }
  return out;
}

async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return toHex(new Uint8Array(digest));
}

function readVarU32(bytes, cursor) {
  let value = 0;
  let shift = 0;
  let next = cursor;
  while (true) {
    if (next >= bytes.length) {
      throw new Error("wasm parse overflow while reading varuint32");
    }
    const current = bytes[next];
    next += 1;
    value |= (current & 0x7f) << shift;
    if ((current & 0x80) === 0) {
      return { value, next };
    }
    shift += 7;
    if (shift >= 35) {
      throw new Error("wasm varuint32 out of range");
    }
  }
}

function sectionPayloadById(bytes, sectionId) {
  if (bytes.length < 8) {
    throw new Error("invalid wasm header");
  }
  let cursor = 8;
  while (cursor < bytes.length) {
    const id = bytes[cursor];
    cursor += 1;
    const sizeInfo = readVarU32(bytes, cursor);
    const size = sizeInfo.value;
    cursor = sizeInfo.next;
    const start = cursor;
    const end = cursor + size;
    if (end > bytes.length) {
      throw new Error("invalid wasm section payload");
    }
    if (id === sectionId) {
      return bytes.slice(start, end);
    }
    cursor = end;
  }
  return null;
}

function structuralCodeSignature(wasmBytes) {
  const codePayload = sectionPayloadById(wasmBytes, 10);
  if (codePayload === null) {
    return "code=<missing>";
  }
  const countInfo = readVarU32(codePayload, 0);
  let cursor = countInfo.next;
  const functionCount = countInfo.value;
  const bodies = [];
  for (let i = 0; i < functionCount; i += 1) {
    const bodyLenInfo = readVarU32(codePayload, cursor);
    const bodyLen = bodyLenInfo.value;
    const bodyStart = bodyLenInfo.next;
    const bodyEnd = bodyStart + bodyLen;
    if (bodyEnd > codePayload.length) {
      throw new Error("invalid code body length");
    }
    let bodyCursor = bodyStart;
    const localsInfo = readVarU32(codePayload, bodyCursor);
    bodyCursor = localsInfo.next;
    let locals = localsInfo.value;
    while (locals > 0) {
      bodyCursor = readVarU32(codePayload, bodyCursor).next + 1;
      if (bodyCursor > bodyEnd) {
        throw new Error("invalid local declaration in code section");
      }
      locals -= 1;
    }
    const opBytes = codePayload.slice(bodyCursor, bodyEnd);
    bodies.push(`${bodyLen}:${toHex(opBytes.slice(0, 8))}`);
    cursor = bodyEnd;
  }
  return `code=${functionCount}|${bodies.join(",")}`;
}

function buildCompileRequest(inputPath, source) {
  return {
    command: "compile",
    compile_mode: "kernel-native",
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
    entrypoint_exports: ["main"],
  };
}

function normalizeMainResult(rawResult) {
  if (typeof rawResult !== "number") {
    return {
      text: `non-number:${typeof rawResult}`,
      value: NaN,
    };
  }
  if ((rawResult & 1) === 1) {
    try {
      return {
        text: `tagged-int:${decodeInt(rawResult)}`,
        value: decodeInt(rawResult),
      };
    } catch (_err) {
      // fall through
    }
  }
  return {
    text: `raw-number:${rawResult >>> 0}`,
    value: rawResult,
  };
}

async function evaluateMain(program) {
  try {
    const { instance } = await instantiateWithRuntime(program.wasmBytes);
    const exported = instance.exports.main;
    if (typeof exported !== "function") {
      return {
        ok: false,
        reason: "missing main export",
      };
    }
    const raw = exported();
    return {
      ok: true,
      result: normalizeMainResult(raw),
    };
  } catch (error) {
    return {
      ok: false,
      reason: String(error?.message ?? "runtime invocation failed"),
    };
  }
}

async function compileProgram(wasmPath, label, source) {
  const response = await callCompilerWasmRaw(
    wasmPath,
    buildCompileRequest(`${label}.clapse`, source),
    {
      validateCompileContract: true,
      withContractMetadata: true,
    },
  );
  assert(response && typeof response === "object",
    `native-program-codegen-semantics-gate: ${label} response must be an object`);
  assert(response.ok === true,
    `native-program-codegen-semantics-gate: ${label} compile failed: ${String(response.error ?? "unknown")}`);
  assert(typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `native-program-codegen-semantics-gate: ${label} missing wasm_base64`);

  const wasmBytes = decodeWasmBase64(response.wasm_base64);
  assert(
    wasmBytes.length >= 8 && wasmBytes[0] === 0x00 && wasmBytes[1] === 0x61 &&
      wasmBytes[2] === 0x73 && wasmBytes[3] === 0x6d,
    `native-program-codegen-semantics-gate: ${label} returned invalid wasm bytes`,
  );

  const wasmsum = await sha256Hex(wasmBytes);
  return {
    label,
    source,
    wasmBytes,
    wasmsum,
    response,
  };
}

async function assertProgramMainResult(wasmPath, label, source, expected) {
  const program = await compileProgram(wasmPath, label, source);
  const runResult = await evaluateMain(program);
  assert(
    runResult.ok,
    `native-program-codegen-semantics-gate: ${label} runtime failed (${runResult.reason ?? "unknown"})`,
  );
  assert(
    runResult.result.text === expected,
    `native-program-codegen-semantics-gate: ${label} expected ${expected}, got ${runResult.result.text}`,
  );
}

async function run() {
  const wasmPath = resolveCompilerWasmPath();

  const programA = await compileProgram(wasmPath, "program-a", [
    'export { main }',
    'main = add 1 2',
    '',
  ].join("\n"));

  const programB = await compileProgram(wasmPath, "program-b", [
    'export { main }',
    'main = mul 2 2',
    '',
  ].join("\n"));

  assert(
    programA.wasmsum !== programB.wasmsum,
    `native-program-codegen-semantics-gate: expected different wasm outputs; both hashes were ${programA.wasmsum}`,
  );

  const runA = await evaluateMain(programA);
  const runB = await evaluateMain(programB);
  assert(
    runA.ok && runB.ok,
    `native-program-codegen-semantics-gate: runtime helper unavailable (${runA.reason ?? "unknown"}, ${runB.reason ?? "unknown"})`,
  );
  assert(
    runA.result.text !== runB.result.text,
    `native-program-codegen-semantics-gate: expected different runtime results, got ${runA.result.text} and ${runB.result.text}`,
  );
  await assertProgramMainResult(
    wasmPath,
    "identity-fn",
    [
      "export { main }",
      "id x = x",
      "main = id 7",
      "",
    ].join("\n"),
    "tagged-int:7",
  );
  await assertProgramMainResult(
    wasmPath,
    "nested-add-mul",
    [
      "export { main }",
      "main = mul (add 2 3) (add 1 1)",
      "",
    ].join("\n"),
    "tagged-int:10",
  );
  await assertProgramMainResult(
    wasmPath,
    "def-call-chain",
    [
      "export { main }",
      "id x = x",
      "double x = add x x",
      "inc x = add x 1",
      "main = inc (double (id 3))",
      "",
    ].join("\n"),
    "tagged-int:7",
  );
  await assertProgramMainResult(
    wasmPath,
    "general-def-chain",
    [
      "export { main }",
      "id x = x",
      "double x = add x x",
      "inc x = add x 1",
      "main = add (inc (double 2)) (double (id 3))",
      "",
    ].join("\n"),
    "tagged-int:11",
  );
  await assertProgramMainResult(
    wasmPath,
    "list-map-foldl",
    [
      'import "prelude"',
      "export { main }",
      "square x = mul x x",
      "numbers = ListCons 1 (ListCons 2 (ListCons 3 ListNil))",
      "main = list_foldl add 0 (list_map square numbers)",
      "",
    ].join("\n"),
    "tagged-int:14",
  );
  await assertProgramMainResult(
    wasmPath,
    "if-and-case-bool",
    [
      "export { main }",
      "guard = if lt 1 2 then 5 else 0",
      "main = case True of True -> add guard 2 | False -> 0",
      "",
    ].join("\n"),
    "tagged-int:7",
  );
  console.log(
    `native-program-codegen-semantics-gate: PASS (hash_a=${programA.wasmsum}; hash_b=${programB.wasmsum}; main_a=${runA.result.text}; main_b=${runB.result.text})`,
  );
}

await run().catch((error) => fail(String(error?.message ?? "unknown")));
