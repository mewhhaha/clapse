import { callCompilerWasmRaw } from "./wasm-compiler-abi.mjs";

function fail(message) {
  throw new Error(message);
}

async function fileExists(path) {
  try {
    const stat = await Deno.stat(path);
    return stat.isFile;
  } catch {
    return false;
  }
}

const wasmPath = String(
  Deno.env.get("CLAPSE_COMPILER_WASM_PATH") ??
    "artifacts/latest/clapse_compiler.wasm",
).trim();
const inputPath = String(
  Deno.args[0] ?? "examples/bootstrap_phase3_entry.clapse",
).trim();

if (wasmPath.length === 0) {
  fail("native-parse-command-gate: missing CLAPSE_COMPILER_WASM_PATH");
}
if (!(await fileExists(wasmPath))) {
  fail(`native-parse-command-gate: wasm not found: ${wasmPath}`);
}
if (!(await fileExists(inputPath))) {
  fail(`native-parse-command-gate: input source not found: ${inputPath}`);
}

const inputSource = await Deno.readTextFile(inputPath);
const response = await callCompilerWasmRaw(wasmPath, {
  command: "parse",
  input_path: inputPath,
  input_source: inputSource,
});
if (!response || typeof response !== "object") {
  fail("native-parse-command-gate: parse response must be an object");
}
if (response.ok !== true) {
  const errorText = typeof response.error === "string"
    ? response.error
    : "unknown parse error";
  fail(`native-parse-command-gate: parse response failed: ${errorText}`);
}
const artifacts = response.artifacts;
if (!artifacts || typeof artifacts !== "object" || Array.isArray(artifacts)) {
  fail("native-parse-command-gate: parse response missing artifacts object");
}
const parsedCst = artifacts["parsed_cst.txt"];
if (typeof parsedCst !== "string" || parsedCst.trim().length === 0) {
  fail("native-parse-command-gate: parse response missing non-empty parsed_cst.txt");
}
console.log(
  `native-parse-command-gate: PASS (${wasmPath}; parsed_cst_bytes=${parsedCst.length})`,
);
