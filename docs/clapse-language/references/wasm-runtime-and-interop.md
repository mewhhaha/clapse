# WASM Runtime and JS Interop

## Runtime Value Encoding (Current)

- Tagged integers: odd `i32` values (`(n << 1) | 1`), payload range `[-1073741824, 1073741823]`
- Non-tagged runtime refs:
  - linear-memory records for `closure`
  - linear-memory records/descriptors for `struct` and `string`
  - linear-memory descriptor pointers for `slice byte`
- no JS value objects for language/runtime values

## Runtime Imports (Current)

Target:

- WebAssembly 3.0 (`return_call`, `if/else`, ref types as emitted by this compiler)
- compiled modules currently require no `rt_*` function imports for closure/struct core runtime operations.
- JS host runtime no longer ships `rt_*` fallback imports.

The struct field/tag operations are now emitted in-module:

- Struct field/tag operations are emitted inline in generated Wasm code (including tag checks and projections).
- `get_field`/`has_tag`/`get_tag` helper names are not emitted as standalone runtime helpers.
- JS host runtime intentionally does not provide struct helper import fallbacks.

- `if` is lowered directly to WebAssembly `if/else` and no longer uses a runtime helper.
- Single-use saturating case/if branch thunks are lowered to direct branch calls, skipping closure allocation/apply in those branches.

## Data/Closure Layout (Current)

- Struct creation uses a module-local unique tag-id table (sorted by tag name) + ordered fields.
- Struct records use fixed field slots `field[0..7]` with a runtime `fieldCount` header (`field[0..fieldCount-1]` are semantically live).
- Closure creation includes function-table index + arity + captured argument count + captured arguments in linear memory.
- Closure records use a fixed header with magic marker for runtime validation:
  - `CLOS` marker (`0x434c4f53`) at record start
  - `fnIndex`
  - `totalArity`
  - `captureCount`
  - fixed capture slots `capture[0..7]` (only `capture[0..captureCount-1]` are semantically live)
- String literals are emitted as descriptors in linear data segments.
- String literal payload bytes are UTF-8 encoded.
- Slice values are linear-memory descriptors:
  - descriptor `[ptr,len]` at handle address
  - contiguous payload bytes at `ptr`
  - `slice_len`/`slice_get_u8`/`slice_set_u8` lower to wasm memory load/store ops.
  - allocation and bulk memory operations are lowered inline in generated Wasm (no standalone `rt_slice_*`, `rt_region_*`, `rt_memcpy`, or `rt_memset` helper bodies).

## Interop Boundaries (Current)

- JS can call exported wasm functions with tagged `i32` values in the payload range `[-1073741824, 1073741823]`.
- JS runtime can render decoded results through helper runtime functions.
- Runtime instantiation requires either `__memory` or `memory` exports.
- JS can allocate packed byte slices through `runtime.alloc_slice_u8(...)` (writes descriptor+payload into wasm linear memory).
- Clapse source can read slice data through:
  - `slice_len : slice byte -> i64`
  - `slice_get_u8 : slice byte -> i64 -> i64` (current backend numeric model)
  - `slice_set_u8 : slice byte -> i64 -> i64 -> slice byte`
- Clapse source also exposes low-level linear-memory builtins:
  - `slice_new_u8 : i64 -> slice byte`
  - `slice_data_ptr : slice byte -> i64`
  - `slice_len_raw : slice byte -> i64`
  - `region_mark : i64 -> i64` (call style: `region_mark 0`)
  - `region_alloc : i64 -> i64 -> i64`
  - `region_reset : i64 -> i64`
  - `memcpy_u8 : i64 -> i64 -> i64 -> i64`
  - `memset_u8 : i64 -> i64 -> i64 -> i64`
  - `struct_tag : a -> i64`
- Low-level memory builtins remain expression-level intrinsics; collapse dead-temp pruning keeps effectful ones live.
- Collapse IR now runs a simple slice ownership pass for `slice_set_u8`: linear temp chains reuse in place, shared targets lower through explicit copy (`slice_len_raw` + `slice_new_u8` + `slice_data_ptr` + `memcpy_u8`) before write.
- Effectful memory builtins are retained even if their result atoms are not referenced (`slice_set_u8`, `memcpy_u8`, `memset_u8`, `region_reset`).
- Keep explicit data dependencies when ordering of multiple effectful memory operations matters.
- No `rt_*` helper functions are emitted; allocation, closure apply, struct operations, and slice/region memory operations lower inline in generated Wasm.
- JS can copy mutated slice descriptors back into host arrays through `runtime.copy_slice_u8(...)`.
- `runtime.read_slice_u8(...)` returns a live view into wasm linear memory.
- `runtime.read_slice_u8_copy(...)` returns an owned copy when you need stability across memory growth.

## Why `[]` Is Not Packed Buffer Today

- `[]` lowers to abstract collection builtins (`collection_empty`/`collection_extend`).
- It currently behaves like a high-level pure collection encoding, not contiguous mutable memory.

## Buffer/Slice Direction (Current)

Clapse keeps `[]` as an abstract collection syntax and uses explicit slice intrinsics for JS interop.

- `[]` still lowers to `collection_empty` / `collection_extend`
- packed interop bytes use linear-memory slice descriptors
- intrinsics currently available: `slice_len`, `slice_get_u8`, `slice_set_u8`, `slice_new_u8`, `slice_data_ptr`, `slice_len_raw`
- low-level region/memory helpers are available: `region_mark`, `region_alloc`, `region_reset`, `memcpy_u8`, `memset_u8`

## Game-Engine Implication

Current Game of Life demo computes state transitions in wasm via `LifeState` + `step_state` and keeps rendering/timing in JS.

To move most simulation logic into Clapse, add:

1. bulk slice/memory traversal intrinsics in backend/runtime (beyond per-byte getters)
2. stable packed-memory ABI contract
3. exported bulk-step wasm function (`step(in_ptr, out_ptr, w, h)`)

Current practical split for the included Game of Life demo:

- Clapse owns state transition functions (`init_state`, `step_state`, `state_current`, `state_generation`).
- JS supplies packed board slices and handles rendering/timing/input.
