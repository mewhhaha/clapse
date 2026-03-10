# Drop Elaboration and Finalization

## Use When

Use this when affine/linear resources need deterministic cleanup without manual user cleanup calls everywhere.

## Core Idea

Insert compiler-generated drop/finalizer calls at control-flow exits while preserving pure language surface semantics.

## Implementation Sketch

1. Track owned resource values in typed IR.
2. Insert drop operations on:
- normal scope exit
- early return
- error/exception path
- pattern-match fallthrough exit
3. Keep drop order deterministic (usually reverse acquisition order).
4. Distinguish drop cleanup from GC finalizers:
- drop for deterministic release
- finalizer as fallback safety net
5. Add lints for values dropped only by fallback finalization.

## Common Pitfalls

- Missing drops on exceptional control paths.
- Double-drop when move analysis is unsound.
- Running drop code that can re-enter invalid runtime states.
- Finalizers relying on ordering guarantees they do not have.

## Pointers

- Affine ownership: `references/recipes/affine-types.md`
- Error/control paths: `references/recipes/exceptions-and-errors.md`
