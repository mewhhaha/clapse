# Dependent Types and Erasure

## Use When

Use this when carrying rich type-level proofs during checking but compiling to efficient runtime Wasm values.

## Core Rule

Keep computationally relevant terms at runtime; erase proof-only terms and type indices once correctness obligations are discharged.

## Implementation Sketch

1. Mark term relevance (`runtime`, `compile-time-only`).
2. Typecheck with full dependent terms and proofs.
3. Run erasure pass to remove irrelevant arguments/fields.
4. Reconstruct runtime layout for erased types.
5. Verify erased program preserves observable behavior.

## Design Tips

- encode proof fields separately from data fields in IR
- keep erasure deterministic and auditable with traces
- support debug mode that can retain selected evidence

## Common Pitfalls

- erasing values that are computationally needed
- retaining proofs and inflating runtime data layout
- mismatched constructor arities before/after erasure
- leaking erased terms into FFI ABI

## Pointers

- Type evidence model: `references/recipes/type-proofs-and-evidence.md`
- Sum type layouts: `references/recipes/sum-type-layouts-and-optimizations.md`
