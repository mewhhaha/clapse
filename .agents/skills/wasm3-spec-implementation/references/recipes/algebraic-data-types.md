# Algebraic Data Types

## Use When

Use this when the language has sum/product types (`Option`, `Result`, enums, records, tuples).

## Representation Options

1. Tagged layout in linear memory:
- compact and explicit
- requires manual decode/encode logic

2. GC-backed typed objects:
- cleaner high-level model
- requires GC-capable runtime support

## Implementation Sketch

1. Define canonical tag encoding per ADT.
2. Define payload layout and alignment per constructor.
3. Generate constructor and projection helpers in lowered IR.
4. Add ABI versioning for public ADTs across module boundaries.
5. Add serialization/debug formats for diagnostics and tests.

## Common Pitfalls

- Constructor tag instability between compiler versions.
- Inconsistent payload ordering across backends.
- Treating zero-sized constructors as if they carry payload.
- Forgetting ABI impact when reordering constructors.

## Pointers

- Memory/ABI guidance: `references/language-to-wasm-playbook.md`
- Runtime semantics: `references/spec-map.md`
- Linear memory creation/access details: `references/recipes/linear-memory-data-creation-and-access.md`
- Sum-type layouts (`Either`, recursive variants): `references/recipes/sum-type-layouts-and-optimizations.md`
