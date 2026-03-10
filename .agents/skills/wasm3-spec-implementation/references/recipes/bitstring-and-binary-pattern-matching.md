# Bitstring and Binary Pattern Matching

## Use When

Use this when language code parses/constructs binary protocols and needs ergonomic bit/byte pattern matching (Gleam/Erlang style).

## Core Representation

Model bitstrings with:

- base pointer
- bit offset
- bit length
- endianness metadata for multi-byte fields

## Implementation Sketch

1. Compile binary patterns into a checked decoder decision tree.
2. Emit bounds checks before each segment extract.
3. Support fixed-width and variable-width segments.
4. Emit construction paths with deterministic packing rules.
5. Keep hot-path decoders branch-efficient and SIMD-friendly when possible.

## Pattern Semantics

- explicit endianness per segment or protocol default
- explicit signed/unsigned interpretation
- explicit alignment behavior for unaligned segments

## Common Pitfalls

- reading past end from missing bit-length checks
- mixing bit and byte indexing in generated code
- inconsistent endianness defaults between encode/decode
- large pattern clauses compiling to non-deterministic branch order

## Pointers

- Binary packing contracts: `references/recipes/packing-and-unpacking.md`
- Data layout strategy: `references/recipes/linear-memory-data-creation-and-access.md`
