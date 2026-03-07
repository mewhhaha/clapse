#!/usr/bin/env -S deno run -A

import {
  callCompilerWasmRaw,
  decodeWasmBase64,
  phase1OracleExpectedMainForSource,
} from "./wasm-compiler-abi.mjs";
import { assertStructuralArtifacts } from "./compile-artifact-contract.mjs";
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

function codeSectionFunctionCount(wasmBytes) {
  const codePayload = sectionPayloadById(wasmBytes, 10);
  if (codePayload === null) {
    return 0;
  }
  return readVarU32(codePayload, 0).value;
}

function buildCompileRequest(
  inputPath,
  source,
  compileMode = "kernel-native",
  entrypointExports = ["main"],
) {
  return {
    command: "compile",
    compile_mode: compileMode,
    input_path: inputPath,
    input_source: source,
    plugin_wasm_paths: [],
    entrypoint_exports: entrypointExports,
  };
}

function requestForOracle(inputPath, source, entrypointExports = ["main"]) {
  return buildCompileRequest(
    inputPath,
    source,
    "kernel-native",
    entrypointExports,
  );
}

function buildKernelPathCompileRequest(source, entrypointExports = ["main"]) {
  return buildCompileRequest(
    "lib/compiler/kernel.clapse",
    source,
    "kernel-native",
    entrypointExports,
  );
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

async function evaluateExport(program, exportName, args = []) {
  try {
    const { instance } = await instantiateWithRuntime(program.wasmBytes);
    const exported = instance.exports[exportName];
    if (typeof exported !== "function") {
      return {
        ok: false,
        reason: `missing ${exportName} export`,
      };
    }
    const raw = exported(...args);
    return {
      ok: true,
      result: normalizeMainResult(raw),
    };
  } catch (error) {
    return {
      ok: false,
      reason: String(error?.message ?? `runtime invocation failed for ${exportName}`),
    };
  }
}

async function compileProgram(
  wasmPath,
  label,
  source,
  entrypointExports = ["main"],
  inputPath = `${label}.clapse`,
) {
  const response = await callCompilerWasmRaw(
    wasmPath,
    buildCompileRequest(inputPath, source, "kernel-native", entrypointExports),
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
  assert(
    typeof response.compile_strategy === "string" &&
      response.compile_strategy.length > 0,
    `native-program-codegen-semantics-gate: ${label} missing compile_strategy`,
  );
  assert(
    response.compatibility_used !== true,
    `native-program-codegen-semantics-gate: ${label} unexpectedly used compatibility path (${String(response.compile_strategy)})`,
  );
  assertStructuralArtifacts(
    response?.artifacts?.["lowered_ir.txt"],
    response?.artifacts?.["collapsed_ir.txt"],
    {
      context: `native-program-codegen-semantics-gate: ${label}`,
      requiredDefs: entrypointExports,
    },
  );

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
  const oracleValue = phase1OracleExpectedMainForSource(
    source,
    requestForOracle(`${label}.clapse`, source),
  );
  const expectedValue = Number.parseInt(expected.replace("tagged-int:", ""), 10);
  assert(
    oracleValue === expectedValue,
    `native-program-codegen-semantics-gate: ${label} oracle expected ${expectedValue}, got ${String(oracleValue)}`,
  );
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

async function assertProgramCompileExports(
  wasmPath,
  label,
  source,
  entrypointExports,
  expectedPublicExports,
  inputPath = `${label}.clapse`,
) {
  const program = await compileProgram(
    wasmPath,
    label,
    source,
    entrypointExports,
    inputPath,
  );
  assert(
    JSON.stringify(program.response.public_exports) ===
      JSON.stringify(expectedPublicExports),
    `native-program-codegen-semantics-gate: ${label} expected public exports ${JSON.stringify(expectedPublicExports)}, got ${JSON.stringify(program.response.public_exports)}`,
  );
}

async function assertProgramExportResult(
  wasmPath,
  label,
  source,
  entrypointExports,
  exportName,
  expected,
  inputPath = `${label}.clapse`,
) {
  const program = await compileProgram(
    wasmPath,
    label,
    source,
    entrypointExports,
    inputPath,
  );
  const runResult = await evaluateExport(program, exportName);
  assert(
    runResult.ok,
    `native-program-codegen-semantics-gate: ${label} runtime failed (${runResult.reason ?? "unknown"})`,
  );
  assert(
    runResult.result.text === expected,
    `native-program-codegen-semantics-gate: ${label} expected ${expected}, got ${runResult.result.text}`,
  );
}

async function assertKernelPathRawCompileExports(
  wasmPath,
  label,
  source,
  entrypointExports,
  expectedPublicExports,
) {
  const response = await callCompilerWasmRaw(
    wasmPath,
    buildKernelPathCompileRequest(source, entrypointExports),
    {
      withContractMetadata: true,
    },
  );
  assert(
    response && typeof response === "object" && response.ok === true,
    `native-program-codegen-semantics-gate: ${label} raw kernel-path compile failed: ${String(response?.error ?? "unknown")}`,
  );
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    `native-program-codegen-semantics-gate: ${label} missing raw wasm_base64`,
  );
  assert(
    JSON.stringify(response.public_exports) === JSON.stringify(expectedPublicExports),
    `native-program-codegen-semantics-gate: ${label} expected raw public exports ${JSON.stringify(expectedPublicExports)}, got ${JSON.stringify(response.public_exports)}`,
  );
}

async function assertProgramCompileFails(
  wasmPath,
  label,
  source,
  errorCode,
  compileMode = "kernel-native",
) {
  const response = await callCompilerWasmRaw(
    wasmPath,
    buildCompileRequest(`${label}.clapse`, source, compileMode),
    {
      validateCompileContract: true,
      withContractMetadata: true,
    },
  );
  assert(
    response && typeof response === "object" && response.ok === false,
    `native-program-codegen-semantics-gate: ${label} expected compile failure`,
  );
  assert(
    response.error_code === errorCode,
    `native-program-codegen-semantics-gate: ${label} expected error_code=${errorCode}, got ${String(response.error_code ?? "missing")}`,
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
    "const-one",
    [
      "export { main }",
      "main = 1",
      "",
    ].join("\n"),
    "tagged-int:1",
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
    "lambda-prelude-fold-map",
    [
      'import "prelude"',
      "export { main }",
      "numbers = Cons 1 (Cons 2 (Cons 3 Nil))",
      "main = foldl (+) 0 (fmap (\\x -> x + 1) numbers)",
      "",
    ].join("\n"),
    "tagged-int:9",
  );
  await assertProgramMainResult(
    wasmPath,
    "qualified-callable-name",
    [
      'import "prelude" as prelude',
      "export { main }",
      "main = prelude.add 1 2",
      "",
    ].join("\n"),
    "tagged-int:3",
  );
  await assertProgramCompileExports(
    wasmPath,
    "explicit-helper-root-export-metadata",
    [
      "helper x = x",
      "",
    ].join("\n"),
    ["helper"],
    [{ name: "helper", arity: 1 }],
  );
  await assertProgramExportResult(
    wasmPath,
    "explicit-answer-root-runtime",
    [
      "answer = add 20 22",
      "",
    ].join("\n"),
    ["answer"],
    "answer",
    "tagged-int:42",
  );
  await assertKernelPathRawCompileExports(
    wasmPath,
    "kernel-path-explicit-helper-root-export-metadata",
    [
      "helper x = x",
      "",
    ].join("\n"),
    ["helper"],
    [{ name: "helper", arity: 1 }],
    "lib/compiler/kernel.clapse",
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
  await assertProgramMainResult(
    wasmPath,
    "fibonacci-recursive-case-bool",
    [
      'import "prelude" { eq, add, sub }',
      "export { main }",
      "fibonacci n = case eq n 0 of",
      "  true -> 0",
      "  _ -> case eq n 1 of",
      "    true -> 1",
      "    _ -> add (fibonacci (sub n 1)) (fibonacci (sub n 2))",
      "main = fibonacci 10",
      "",
    ].join("\n"),
    "tagged-int:55",
  );
  const fibProgram = await compileProgram(
    wasmPath,
    "fibonacci-recursive-case-bool-codegen",
    [
      'import "prelude" { eq, add, sub }',
      "export { main }",
      "fibonacci n = case eq n 0 of",
      "  true -> 0",
      "  _ -> case eq n 1 of",
      "    true -> 1",
      "    _ -> add (fibonacci (sub n 1)) (fibonacci (sub n 2))",
      "main = fibonacci 10",
      "",
    ].join("\n"),
  );
  assert(
    fibProgram.wasmBytes.length > 51,
    `native-program-codegen-semantics-gate: fibonacci should emit executable wasm, got ${fibProgram.wasmBytes.length} bytes (${structuralCodeSignature(fibProgram.wasmBytes)})`,
  );
  assert(
    codeSectionFunctionCount(fibProgram.wasmBytes) >= 2,
    `native-program-codegen-semantics-gate: fibonacci should emit multiple wasm functions (${structuralCodeSignature(fibProgram.wasmBytes)})`,
  );
  await assertProgramCompileFails(
    wasmPath,
    "unsupported-case-target",
    [
      "export { main }",
      "main = case 1 of",
      "  true -> 0",
      "  _ -> 1",
      "",
    ].join("\n"),
    "compile_phase1_unsupported",
  );
  console.log(
    `native-program-codegen-semantics-gate: PASS (hash_a=${programA.wasmsum}; hash_b=${programB.wasmsum}; main_a=${runA.result.text}; main_b=${runB.result.text})`,
  );
}

await run().catch((error) => fail(String(error?.message ?? "unknown")));
