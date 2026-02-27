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

- WebAssembly 3.0 (`return_call`, wasm `if/else` opcodes, ref types as emitted by this compiler)
- compiled modules currently require no `rt_*` function imports for closure/struct core runtime operations.
- JS host runtime no longer ships `rt_*` fallback imports.

The struct field/tag operations are now emitted in-module:

- Struct field/tag operations are emitted inline in generated Wasm code (including tag checks and projections).
- `get_field`/`has_tag`/`get_tag` helper names are not emitted as standalone runtime helpers.
- JS host runtime intentionally does not provide struct helper import fallbacks.

- Internal conditionals generated from guards are lowered directly to WebAssembly `if/else` and no longer use a runtime helper.
- Single-use saturating case/guard branch thunks are lowered to direct branch calls, skipping closure allocation/apply in those branches.

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
  - `slice_len`/`slice_get_u8`/`slice_set_u8`/`slice_eq_u8` lower to wasm memory
    load/store ops.
  - allocation and bulk memory operations are lowered inline in generated Wasm (no standalone `rt_slice_*`, `rt_region_*`, `rt_memcpy`, or `rt_memset` helper bodies).

## Memory Model (Current)

### Allocation model

- Slice writes and copies are materialized in linear memory. The live object types are:
  - immutable string descriptors to embedded literal bytes
  - mutable byte-slice descriptors with a pointer+length pair and owned payload bytes
  - linear records for struct/closure data.
- `slice_new_u8 n` allocates `n` bytes in linear memory and returns a fresh descriptor.
- `runtime.alloc_slice_u8` is the JS-side path used for foreign-owned input buffers; it produces a slice descriptor that points into wasm memory so subsequent Clapse slice operations can use direct loads/stores.
- In the collapse rewrite path, COW also uses fresh `slice_new_u8` allocations to allocate replacement descriptors.
- `struct`/`closure` creation emits descriptor+payload inline and uses fixed-size headers for validation/runtime checks.
- Runtime ownership at this layer is represented by descriptor replacement on COW and descriptor reuse on linear chains; there is no host-side allocator ownership bookkeeping for rewrite decisions.

### Deallocation / reclaim model

- No general free list and no per-object `free` operation exist today.
- Memory regions can be reset only via the explicit region API (`region_mark`, `region_reset`) where a backend chooses to allocate from region-managed memory.
- `region_reset` is kept as an effectful primitive and is not elided by DCE even when its descriptor result is dead, because it participates in deterministic lifetime fencing.
- `memcpy_u8`/`memset_u8` are pure transformations from byte-level memory access perspective only in IR form; at runtime they still lower to memory side-effecting operations and must remain ordered.
- There is no host-side GC or sweeping hook for Clapse values; ownership and lifetime are enforced by compile-time rewrite decisions plus explicit region resets.
- Reclamation in practice means replacing uses of a descriptor with a fresh one in COW
  paths and allowing normal function-scope lifetime drop when chains end.
- Reclaim does not include per-object deallocation; only explicit region fences can shrink allocator scope.

### Reuse model

- Reuse is currently driven by Collapse IR memory passes, not by a runtime allocator:
  - `collapse_pipeline_slice_ownership_rewrite` controls whether a `slice_set_u8` call is emitted as an in-place store or a copy-on-write path.
  - For linear reuse, the backend emits direct overwrite semantics through `slice_set_u8`.
  - For copied reuse, rewrite expands to:
    1. `slice_new_u8` with same length as target
    2. byte-copy from source payload to fresh destination (`copy_slice_segment` loop in kernel path)
    3. write into the fresh destination.
  - Reuse requires that the write result is provably confined to a non-escaping local chain; if not provable, the conservative COW path is used.
- Reuse is deterministic and happens only in the kernel-level `slice_set_u8_rewrite` decision:
  - `OwnershipRewriteLinear` keeps the same descriptor if the chain is single-use local.
  - `OwnershipRewriteCopyOnWrite` allocates then copies then writes into a fresh descriptor.

### Alias / freeze / COW rule (current)

- Current rule implemented in the kernel:
  - Start from request-chain hint + in-bounds check.
  - Compute escape/lifetime, alias class, and freeze signal through chain-context helpers.
  - Use COW for any non-local, non-single-use, unknown-alias, freeze, or OOB signal.
- A slice is treated as **aliased** when its context classifier reports shared/escaped usage.
- A slice is treated as **linear-only** only when context is local, single-use, non-frozen, and non-OOB.
- There is no IR freeze bit today; freeze is represented by conservative classification in `slice_set_u8_context_with_freeze`.
- COW preserves original bytes for external observers and is mandatory for aliased/escaping/frozen/uncertain targets.
- Ownership selection is derived once per request by
  `collapse_pipeline_slice_write_policy` and then threaded through request response
  builders so `slice_set_u8` rewrites in copy/escape helpers use a consistent
  policy per dispatch.

### Scope and lifetime behavior

- Lifetimes are expressed functionally in the IR as temporary ownership chains over `slice_set_u8`.
- `EscapeLifetimeLocal`, `EscapeLifetimeEscaped`, and `EscapeLifetimeUnknown` are active in kernel policy classification:
  - `Local` permits linear reuse when alias/freeze checks stay clean.
  - `Escaped` forces copy-on-write.
  - `Unknown` stays conservative unless request-chain context and local checks both prove local single-use.
- The first pass slot (`collapse_pipeline_escape_lifetime`) is intentionally present and identity:
  - it is the designated point for escape/lifetime labeling
  - it currently does not change behavior to keep determinism while staging the contract.
- Function-local only chains are the only currently guaranteed reuse candidates:
  - writes that are consumed only by later temporaries in the same expression/function scope and not returned/externally passed can be kept linear.
  - once a slice value crosses expression/function boundaries, reuse is conservatively disallowed.

## Interop Boundaries (Current)

- JS can call exported wasm functions with tagged `i32` values in the payload range `[-1073741824, 1073741823]`.
- JS runtime can render decoded results through helper runtime functions.
- Runtime instantiation requires either `__memory` or `memory` exports.
- JS can allocate packed byte slices through `runtime.alloc_slice_u8(...)` (writes descriptor+payload into wasm linear memory).
- Clapse source can read slice data through:
  - `slice_len : slice byte -> i64`
  - `slice_get_u8 : slice byte -> i64 -> i64` (current backend numeric model)
  - `slice_set_u8 : slice byte -> i64 -> i64 -> slice byte`
  - `slice_eq_u8 : slice byte -> slice byte -> bool` (bytewise equality)
  - `str_to_slice : string -> slice byte` (bridge/view)
  - `slice_to_string : slice byte -> string` (bridge/view)
  - `str_eq : string -> string -> bool` (bytewise content equality)
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
- In the current kernel implementation, the COW copy path is materialized through `slice_set_u8_cow` + `copy_slice_segment`.
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
- intrinsics currently available: `slice_len`, `slice_get_u8`, `slice_set_u8`, `slice_eq_u8`, `str_to_slice`, `slice_to_string`, `slice_new_u8`, `slice_data_ptr`, `slice_len_raw`
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
