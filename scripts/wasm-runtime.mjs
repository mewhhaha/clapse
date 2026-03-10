const MAX_TAGGED_INT = 1073741823;
const MIN_TAGGED_INT = -1073741824;
const TEXT_DECODER = new TextDecoder();
const WASM_PAGE_SIZE = 65536;
const HOST_ALLOC_GUARD_BYTES = 16 * WASM_PAGE_SIZE;
const SLICE_DESC_SIZE = 8;
const CLOSURE_MAGIC = 0x434c_4f53; // "CLOS"
const CLOSURE_HEADER_SIZE = 16;
const STRUCT_MAGIC = 0x5354_5255; // "STRU"
const STRUCT_HEADER_SIZE = 12;

function asUnsigned(value) {
  return value >>> 0;
}

function readSliceDescriptorForRender(state, handle) {
  const descPtr = asUnsigned(handle);
  if (!(state.memory instanceof WebAssembly.Memory)) {
    throw new Error("wasm memory export is not available");
  }
  const memory = state.memory;
  if (descPtr + SLICE_DESC_SIZE > memory.buffer.byteLength) {
    throw new Error(`slice descriptor out of bounds: ${descPtr}`);
  }
  const view = new DataView(memory.buffer);
  const dataPtr = view.getUint32(descPtr, true);
  const len = view.getInt32(descPtr + 4, true);
  if (len < 0) {
    throw new Error(`invalid slice descriptor values: ptr=${dataPtr} len=${len}`);
  }
  if (dataPtr + len > memory.buffer.byteLength) {
    throw new Error(`slice payload out of bounds: ptr=${dataPtr} len=${len}`);
  }
  return { descPtr, dataPtr, len };
}

function readSliceBytesForRender(state, handle) {
  const desc = readSliceDescriptorForRender(state, handle);
  const memory = state.memory;
  return new Uint8Array(memory.buffer, desc.dataPtr, desc.len);
}

function isLikelyText(bytes) {
  for (let i = 0; i < bytes.length; i += 1) {
    const b = bytes[i];
    if (b === 0) {
      return false;
    }
    if (b !== 9 && b !== 10 && b !== 13 && (b < 32 || b > 126)) {
      return false;
    }
  }
  return true;
}

export function encodeInt(n) {
  if (!Number.isInteger(n)) {
    throw new Error(`non-integer value: ${n}`);
  }
  if (n < MIN_TAGGED_INT || n > MAX_TAGGED_INT) {
    throw new Error(`tagged i32 payload out of range: ${n}`);
  }
  return ((n | 0) << 1) | 1;
}

export function isTaggedInt(v) {
  return (v & 1) === 1;
}

export function decodeInt(v) {
  if (!isTaggedInt(v)) {
    throw new Error(`expected tagged int, got value handle: ${v}`);
  }
  return v >> 1;
}

export function makeRuntime() {
  const state = {
    memory: null,
    nextAlloc: null,
    heapGlobal: null,
  };

  function asUint8Array(input) {
    if (input instanceof Uint8Array) {
      return input;
    }
    if (ArrayBuffer.isView(input)) {
      return new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
    }
    if (input instanceof ArrayBuffer) {
      return new Uint8Array(input);
    }
    if (Array.isArray(input)) {
      return Uint8Array.from(input);
    }
    throw new Error("alloc_slice_u8 expects Uint8Array, ArrayBuffer, or byte array");
  }

  function ensureMemory() {
    if (!(state.memory instanceof WebAssembly.Memory)) {
      throw new Error("wasm memory export is not available");
    }
    return state.memory;
  }

  function alignUp(value, align) {
    const rounded = value + (align - 1);
    return rounded - (rounded % align);
  }

  function initLinearAllocator() {
    const memory = ensureMemory();
    if (state.nextAlloc === null) {
      state.nextAlloc = memory.buffer.byteLength + HOST_ALLOC_GUARD_BYTES;
    }
    if (state.heapGlobal instanceof WebAssembly.Global) {
      const floor = state.heapGlobal.value >>> 0;
      if (floor > state.nextAlloc) {
        state.nextAlloc = floor + HOST_ALLOC_GUARD_BYTES;
      }
    }
  }

  function ensureLinearCapacity(requiredEnd) {
    const memory = ensureMemory();
    const currentBytes = memory.buffer.byteLength;
    if (requiredEnd <= currentBytes) {
      return;
    }
    const missing = requiredEnd - currentBytes;
    const pages = Math.ceil(missing / WASM_PAGE_SIZE);
    memory.grow(pages);
  }

  function allocLinear(size, align) {
    initLinearAllocator();
    const start = alignUp(state.nextAlloc, align);
    const end = start + size;
    ensureLinearCapacity(end);
    state.nextAlloc = end;
    return start;
  }

  function readSliceDescriptor(handle) {
    const descPtr = asUnsigned(handle);
    if (descPtr > Number.MAX_SAFE_INTEGER) {
      throw new Error(`slice descriptor pointer out of range: ${descPtr}`);
    }
    const memory = ensureMemory();
    if (descPtr + SLICE_DESC_SIZE > memory.buffer.byteLength) {
      throw new Error(`slice descriptor out of bounds: ${descPtr}`);
    }
    const view = new DataView(memory.buffer);
    const dataPtr = view.getUint32(descPtr, true);
    const len = view.getInt32(descPtr + 4, true);
    if (len < 0) {
      throw new Error(`invalid slice descriptor values: ptr=${dataPtr} len=${len}`);
    }
    if (dataPtr + len > memory.buffer.byteLength) {
      throw new Error(`slice payload out of bounds: ptr=${dataPtr} len=${len}`);
    }
    return { descPtr, dataPtr, len };
  }

  function allocSliceU8(input) {
    const source = asUint8Array(input);
    const descPtr = allocLinear(SLICE_DESC_SIZE, 4);
    const dataPtr = allocLinear(source.length, 1);
    const memory = ensureMemory();
    const view = new DataView(memory.buffer);
    view.setUint32(descPtr, dataPtr >>> 0, true);
    view.setInt32(descPtr + 4, source.length, true);
    const payload = new Uint8Array(memory.buffer, dataPtr, source.length);
    payload.set(source);
    return descPtr;
  }

  function copySliceU8(handle, target) {
    if (!(target instanceof Uint8Array)) {
      throw new Error("copy_slice_u8 target must be Uint8Array");
    }
    const slice = readSliceDescriptor(handle);
    if (target.length !== slice.len) {
      throw new Error(`copy_slice_u8 length mismatch: target=${target.length} source=${slice.len}`);
    }
    const memory = ensureMemory();
    const payload = new Uint8Array(memory.buffer, slice.dataPtr, slice.len);
    target.set(payload);
    return target;
  }

  function readSliceU8(handle) {
    const slice = readSliceDescriptor(handle);
    const memory = ensureMemory();
    return new Uint8Array(memory.buffer, slice.dataPtr, slice.len);
  }

  function readSliceU8Copy(handle) {
    const source = readSliceU8(handle);
    const out = new Uint8Array(source.length);
    out.set(source);
    return out;
  }

  return {
    state,
    alloc_slice_u8: allocSliceU8,
    copy_slice_u8: copySliceU8,
    read_slice_u8: readSliceU8,
    read_slice_u8_copy: readSliceU8Copy,
  };
}

export function renderResult(value, state) {
  const vSigned = value | 0;
  const v = vSigned >>> 0;
  if (isTaggedInt(vSigned)) {
    return String(decodeInt(vSigned));
  }
  if (state.memory instanceof WebAssembly.Memory) {
    try {
      const closure = (() => {
        const memory = state.memory;
        if (v + CLOSURE_HEADER_SIZE > memory.buffer.byteLength) {
          throw new Error("closure header out of range");
        }
        const view = new DataView(memory.buffer);
        const magic = view.getInt32(v, true);
        if (magic !== CLOSURE_MAGIC) {
          throw new Error("not a closure");
        }
        const totalArity = view.getInt32(v + 8, true);
        const captureCount = view.getInt32(v + 12, true);
        if (captureCount < 0) {
          throw new Error(`invalid closure capture count: ${captureCount}`);
        }
        return { totalArity, captureCount };
      })();
      return `<closure fn=${new DataView(state.memory.buffer).getInt32(v + 4, true)} bound=${closure.captureCount}/${closure.totalArity}>`;
    } catch (_err) {
      // fall through to struct/slice rendering
    }
  }
  if (state.memory instanceof WebAssembly.Memory) {
    try {
      const memory = state.memory;
      if (v + STRUCT_HEADER_SIZE <= memory.buffer.byteLength) {
        const view = new DataView(memory.buffer);
        if (view.getInt32(v, true) === STRUCT_MAGIC) {
          const tag = view.getInt32(v + 4, true);
          const fieldCount = view.getInt32(v + 8, true);
          return `<struct tag=${tag} fields=${fieldCount}>`;
        }
      }
    } catch (_err) {
      // fall through to slice rendering
    }
  }
  if (state.memory instanceof WebAssembly.Memory) {
    try {
      const memory = state.memory;
      if (v + SLICE_DESC_SIZE <= memory.buffer.byteLength) {
        const desc = readSliceDescriptorForRender(state, v);
        const bytes = readSliceBytesForRender(state, desc.descPtr);
        if (isLikelyText(bytes)) {
          return TEXT_DECODER.decode(bytes);
        }
        return `<slice_u8 len=${desc.len}>`;
      }
    } catch (_err) {
      // fall through to unknown render
    }
  }
  return `<unknown:${v}>`;
}

export async function instantiateWithRuntime(wasmBytes) {
  const runtime = makeRuntime();
  const { instance } = await WebAssembly.instantiate(wasmBytes, {});
  const memoryExport = instance.exports.__memory ?? instance.exports.memory;
  if (!(memoryExport instanceof WebAssembly.Memory)) {
    throw new Error("wasm module must export __memory or memory");
  }
  runtime.state.memory = memoryExport;
  const heapGlobal = instance.exports.__heap_ptr;
  if (heapGlobal instanceof WebAssembly.Global) {
    runtime.state.heapGlobal = heapGlobal;
  }
  return { instance, runtime };
}
