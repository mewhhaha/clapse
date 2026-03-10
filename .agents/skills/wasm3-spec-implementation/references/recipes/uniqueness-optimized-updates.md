# Uniqueness-Optimized Updates

## Use When

Use this when you want pure APIs but lower selected updates to in-place mutation when uniqueness is proven.

## Core Rule

Apply destructive update only when the value is uniquely owned at that program point; otherwise allocate copy-on-write fallback.

## Implementation Sketch

1. Propagate uniqueness facts through typed IR.
2. Lower update ops into dual path:
- unique path: in-place write
- shared path: clone + write
3. Keep both paths semantically equivalent.
4. Add debug assertions for uniqueness assumptions.
5. Benchmark allocation reductions and regression-test semantics.

## Common Pitfalls

- Unsound uniqueness inference across module/FFI boundaries.
- Optimizer merging paths and losing correctness guard.
- In-place update on structurally shared persistent nodes.
- Missing invalidation of moved aliases.

## Pointers

- Ownership systems: `references/recipes/linear-types.md`
- Pure reuse strategy: `references/recipes/pure-memory-reuse.md`
