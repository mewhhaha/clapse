# Affine Types

## Use When

Use this when values may be consumed at most once (ownership without mandatory use), especially for resources where dropping is valid.

## Core Rule

An affine value can be used zero or one times. Reuse is forbidden; explicit drop is allowed.

## Why Affine Over Linear

- Simpler ergonomics for optional paths and early exits.
- Better fit for APIs where not all acquired values are always consumed.
- Easier migration path from unrestricted languages.

## Implementation Sketch

1. Reuse linear ownership tracking but allow unused-at-end values.
2. Require explicit drop semantics for resource-backed values.
3. Insert compiler-generated drop calls where required by runtime model.
4. Keep move semantics identical to linear types.
5. Add diagnostics for likely accidental drops.

## Common Pitfalls

- Silent resource leaks if drop lowering is incomplete.
- Confusing affine "may drop" with unrestricted copy semantics.
- Missing drop behavior on exceptional/control-transfer paths.
- Treating host resources as plain values without cleanup hooks.

## Pointers

- Error/control transfer behavior: `references/recipes/exceptions-and-errors.md`
- Host/runtime boundaries: `references/wasm3-notes.md`
- Reuse strategies: `references/recipes/pure-memory-reuse.md`
