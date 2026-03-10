# Lifetime and Escape Analysis

## Use When

Use this when deciding whether values can stay stack/region-local or must move to heap-managed storage.

## Core Goal

Infer value lifetime and alias escape behavior early so allocation strategy is predictable and GC pressure is reduced.

## Implementation Sketch

1. Add lifetime regions/scopes in typed IR.
2. Mark values that escape:
- returned from function
- captured by closure
- stored in heap/global/module state
- passed to unknown host APIs
3. Classify each allocation site:
- stack/region eligible
- heap required
4. Emit diagnostics in debug mode showing why a value escaped.
5. Verify analysis stability across optimization passes.

## Common Pitfalls

- Missing escape through closure capture paths.
- Losing analysis metadata after inlining/rewrite passes.
- Treating FFI calls as non-escaping by default.
- Unsound assumptions in recursive/mutually recursive functions.

## Pointers

- Region strategy: `references/recipes/region-inference-and-arenas.md`
- GC policy: `references/recipes/garbage-collection.md`
