import { callCompilerWasm, decodeWasmBase64 } from "./wasm-compiler-abi.mjs";
import { decodeInt, encodeInt, instantiateWithRuntime, makeRuntime } from "./wasm-runtime.mjs";

const DEFAULT_COMPILER_WASM_PATH = "artifacts/latest/clapse_compiler.wasm";

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function assertThrows(label, fn, contains) {
  let threw = false;
  try {
    fn();
  } catch (err) {
    threw = true;
    const msg = err instanceof Error ? err.message : String(err);
    if (contains && !msg.includes(contains)) {
      throw new Error(`${label}: expected error containing "${contains}", got "${msg}"`);
    }
  }
  if (!threw) {
    throw new Error(`${label}: expected throw`);
  }
}

function testTaggedIntBounds() {
  const min = -1073741824;
  const max = 1073741823;
  assert(decodeInt(encodeInt(min)) === min, "tagged int min must round-trip");
  assert(decodeInt(encodeInt(max)) === max, "tagged int max must round-trip");
  assertThrows("tagged int below min", () => encodeInt(min - 1), "out of range");
  assertThrows("tagged int above max", () => encodeInt(max + 1), "out of range");
}

function testNoJsRuntimeFallbacks() {
  const runtime = makeRuntime();
  assert(!("rt" in runtime), "runtime must not expose JS rt_* fallback table");
}

function testSliceCopyIsolation() {
  const runtime = makeRuntime();
  runtime.state.memory = new WebAssembly.Memory({ initial: 1 });
  const handle = runtime.alloc_slice_u8(Uint8Array.from([1, 2, 3]));
  const live = runtime.read_slice_u8(handle);
  const copy = runtime.read_slice_u8_copy(handle);
  live[0] = 9;
  assert(copy[0] === 1, "slice copy must not alias live wasm memory");
}

async function compileAndInstantiateClapse(source) {
  const compilerWasmPath =
    Deno.env.get("CLAPSE_COMPILER_WASM_PATH") || DEFAULT_COMPILER_WASM_PATH;
  const response = await callCompilerWasm(compilerWasmPath, {
    command: "compile",
    input_path: "memory-model-check.clapse",
    input_source: source,
  });
  assert(response && response.ok === true, "compiler must compile memory-model fixture source");
  assert(
    typeof response.wasm_base64 === "string" && response.wasm_base64.length > 0,
    "compiler response must include wasm bytes",
  );
  const wasmBytes = decodeWasmBase64(response.wasm_base64);
  const { instance } = await instantiateWithRuntime(wasmBytes);
  assert(
    typeof instance.exports.main === "function",
    "compiled fixture must export main",
  );
  return instance;
}

function toBigInt(v) {
  return typeof v === "bigint" ? v : BigInt(v);
}

async function testSliceReuseLinear() {
  const instance = await compileAndInstantiateClapse(`
main =
  let s0 = slice_new_u8 64
      s1 = slice_set_u8 s0 0 1
      s2 = slice_set_u8 s1 1 2
      ptr1 = slice_data_ptr s1
      ptr2 = slice_data_ptr s2
      oldIsLiveLinear = eq (slice_get_u8 s1 1) 2
      oldIsLiveCOW = eq (slice_get_u8 s1 1) 0
      samePtr = eq ptr1 ptr2
      finalWriteAt1 = eq (slice_get_u8 s2 1) 2
      linearOk = and samePtr oldIsLiveLinear
      cowOk = and (eq samePtr 0) oldIsLiveCOW
      in if (and finalWriteAt1 (or linearOk cowOk)) 1 0
`);
  const delta = toBigInt(instance.exports.main());
  assert(delta === 1n, "linear slice write must either preserve descriptor or stay copy-on-write");
}

async function testSliceReuseCopyOnWriteAlias() {
  const instance = await compileAndInstantiateClapse(`
main =
  let alias = slice_new_u8 64
      s1 = slice_set_u8 alias 0 1
      s2 = slice_set_u8 alias 1 2
      aliasPtr = slice_data_ptr alias
      nextPtr = slice_data_ptr s2
      aliasAt1 = slice_get_u8 alias 1
      aliasAt1IsNew = eq aliasAt1 2
      aliasAt1IsOld = eq aliasAt1 0
      finalAt1 = eq (slice_get_u8 s2 1) 2
      sameAliasPtr = eq aliasPtr nextPtr
      linearOk = and sameAliasPtr aliasAt1IsNew
      cowOk = and (eq sameAliasPtr 0) aliasAt1IsOld
      in if (and finalAt1 (or linearOk cowOk)) 1 0
`);
  const delta = toBigInt(instance.exports.main());
  assert(delta === 1n, "aliased write chain should preserve semantics under either policy");
}

(async () => {
  testTaggedIntBounds();
  testNoJsRuntimeFallbacks();
  testSliceCopyIsolation();
  await testSliceReuseLinear();
  await testSliceReuseCopyOnWriteAlias();
  console.log("runtime contract smoke: PASS");
})();
