# Persistent Data Structures

## Use When

Use this when the language favors immutable collections (lists, maps, vectors) with structural sharing.

## Representation Strategy

- Node-based trees/tries for maps and sets
- Chunked vectors for indexed sequences
- Copy-on-write only for controlled interop paths

## Implementation Sketch

1. Define immutable API semantics first.
2. Choose persistent structure per access pattern.
3. Encode node layouts and sharing rules explicitly.
4. Track allocation pressure and GC impact in benchmarks.
5. Add equality/hash semantics consistent with immutability model.

## Common Pitfalls

- Accidental deep copies during updates.
- Hidden mutability in runtime helpers.
- Poor cache locality from overly pointer-heavy layouts.
- Inconsistent value equality across host/runtime boundaries.

## Pointers

- GC strategy: `references/recipes/garbage-collection.md`
- Strings/layout concerns: `references/recipes/strings-and-memory.md`
