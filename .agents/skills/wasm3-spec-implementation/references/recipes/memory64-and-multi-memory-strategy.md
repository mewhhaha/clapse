# Memory64 and Multi-Memory Strategy

## Use When

Use this when planning for large-address-space workloads (`memory64`) or multiple isolated memories per module/runtime.

## Core Decisions

1. `memory64` policy:
- enable for large datasets only
- keep 32-bit fallback target where portability matters

2. multi-memory policy:
- decide memory roles (heap, I/O buffers, immutable data, sandboxed regions)
- document which functions read/write each memory

## Implementation Sketch

1. Abstract pointer/index operations in compiler IR.
2. Keep memory index explicit in load/store lowering.
3. Generate separate ABI signatures for 32-bit and 64-bit pointer modes.
4. Add feature detection and deterministic fallback at load time.
5. Add tests for boundary cases near 4GiB and above.

## Common Pitfalls

- accidental truncation when crossing 64-bit and JS number paths
- assuming single-memory semantics in optimization passes
- pointer-tag tricks that break under 64-bit assumptions
- mixing allocator assumptions across multiple memories

## Pointers

- Wasm-first feature policy: `references/recipes/wasm-first-design-principles.md`
- Interop numeric rules: `references/recipes/javascript-data-marshalling.md`
