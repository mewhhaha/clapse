# Pure Memory Reuse

## Use When

Use this when you want functional semantics (referential transparency) with low-allocation runtime behavior.

## Main Idea

Permit in-place mutation only when uniqueness/ownership proves there are no observable aliases. Expose a pure API while compiling to destructive updates internally.

## Practical Patterns

1. Uniqueness-gated update:
- if reference is unique, mutate in place
- otherwise copy and update

2. Builder/freeze pattern:
- build mutable structure in a scoped region
- freeze to immutable value at boundary

3. Region/arena epochs:
- allocate transient working data in scoped arenas
- bulk release at region end

4. Linear buffer threading:
- pass buffer ownership through pipeline stages
- return new owner after each stage

## Implementation Sketch

1. Add alias/ownership analysis to typed IR.
2. Classify updates as `in_place_safe` or `copy_required`.
3. Emit runtime fast path + safe fallback path.
4. Keep deterministic semantics tests comparing both paths.
5. Measure allocation reduction and verify no semantic drift.

## Common Pitfalls

- Assuming uniqueness without interprocedural evidence.
- Letting optimizer reintroduce shared aliases.
- Exposing mutable internals across FFI boundaries.
- Forgetting to invalidate moved ownership tokens.

## Pointers

- Linear discipline: `references/recipes/linear-types.md`
- Affine ownership: `references/recipes/affine-types.md`
- Persistent collections: `references/recipes/persistent-data-structures.md`
