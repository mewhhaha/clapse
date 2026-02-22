# Stream Fusion and Deforestation

## Use When

Use this when FP collection pipelines allocate too many intermediate lists/vectors.

## Goal

Preserve high-level combinator style (`map`, `filter`, `fold`) while compiling to tight loops with minimal temporary allocations.

## Implementation Sketch

1. Lower collection combinators into stream IR.
2. Fuse adjacent producers/consumers where safe.
3. Emit one loop kernel for fused pipelines.
4. Keep a non-fused fallback for debugging/semantics checks.
5. Add equivalence tests between fused and unfused outputs.

## Common Pitfalls

- Invalid fusion across effectful operations.
- Lost laziness/short-circuit semantics.
- Code-size blowups from over-fusion.
- Poor diagnostics once pipelines become opaque low-level loops.

## Pointers

- Loop lowering: `references/recipes/loops.md`
- SIMD follow-up optimization: `references/recipes/simd-and-data-parallelism.md`
