# JavaScript Interop

## Use When

Use this when exposing language/runtime functions to JavaScript or calling JavaScript functions from Wasm.

## Interop Surface

- Wasm exports called from JS
- JS host functions imported into Wasm
- Shared `WebAssembly.Memory`
- Optional shared tables for indirect call patterns

## Implementation Sketch

1. Define a stable ABI document:
- function names
- parameter/return types
- ownership and lifetime rules
2. Keep exported functions narrow and typed.
3. Route non-trivial marshaling through runtime helper functions.
4. Distinguish recoverable language errors from traps at the boundary.
5. Recreate typed-array views after `memory.grow`.

## Common Pitfalls

- Keeping stale JS `TypedArray` views after memory growth.
- Mixing JS `number` with i64 semantics without explicit BigInt policy.
- Throwing host exceptions through Wasm frames without mapping rules.
- Leaking allocations by forgetting paired free/release calls.

## Pointers

- Host boundary model: `references/wasm3-notes.md`
- Data packing details: `references/recipes/packing-and-unpacking.md`
- Browser GPU upload paths: `references/recipes/browser-gpu-interop.md`
