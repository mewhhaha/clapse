# Borrowed vs Owned FFI Values

## Use When

Use this when values cross Wasm/host boundaries and lifetime ownership must stay explicit.

## Ownership Modes

- borrowed: valid only during call scope, no transfer
- owned: ownership transferred to caller/callee with explicit release policy
- pinned: stable address guaranteed for a scoped period

## Implementation Sketch

1. Annotate FFI signatures with ownership mode per parameter/result.
2. Generate wrapper stubs enforcing:
- borrow scope checks
- move/transfer rules
- release/finalizer hooks
3. Encode borrowed handles so post-return use is rejected.
4. Add leak and use-after-free tests across JS <-> Wasm calls.
5. Log ownership transitions in debug builds.

## Common Pitfalls

- Returning borrowed data as owned.
- Forgetting to unpin after async completion.
- Sharing pointers across allocators with incompatible free paths.
- Hidden copies that break expected move semantics.

## Pointers

- JS interop basics: `references/recipes/javascript-interop.md`
- Affine/linear ownership: `references/recipes/affine-types.md`
