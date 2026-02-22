import { decodeInt, encodeInt, makeRuntime } from "./wasm-runtime.mjs";

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

testTaggedIntBounds();
testNoJsRuntimeFallbacks();
testSliceCopyIsolation();
console.log("runtime contract smoke: PASS");
