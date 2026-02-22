# Region Inference and Arenas

## Use When

Use this when many allocations share a similar lifetime and can be reclaimed in bulk instead of per-object GC tracing.

## Core Idea

Infer region lifetimes from control flow and assign allocations to arenas that are released at region end.

## Implementation Sketch

1. Infer region boundaries from function/block scopes.
2. Assign allocation sites to regions when escape analysis permits.
3. Allocate region objects from bump-pointer arenas.
4. Release whole region at exit (or explicit checkpoint).
5. Keep escaping values promoted to heap.
6. Add debug mode with region validity checks.

## Common Pitfalls

- Returning references to released region memory.
- Mixing region and heap pointers without tags/validation.
- Holding region values in async tasks after scope exit.
- Fragmentation from too many tiny region classes.

## Pointers

- Escape rules: `references/recipes/lifetime-and-escape-analysis.md`
- Pure reuse patterns: `references/recipes/pure-memory-reuse.md`
