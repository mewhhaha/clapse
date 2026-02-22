# Strings and Memory Layout

## Use When

Use this when implementing language strings, arrays, and object layout inside linear memory or GC-backed heaps.

## Core Decision

Choose one string representation and keep it consistent:

- UTF-8 bytes + length
- UTF-16 code units + length
- rope/segmented strings for heavy concat workloads

## Implementation Sketch

1. Define canonical in-memory layout for strings/slices.
2. Define host interop conversion rules at API boundaries.
3. Implement bounds-safe slice/index operations.
4. Keep decode/encode helpers centralized in runtime library.
5. Add round-trip tests for non-ASCII and invalid encoding inputs.

## Common Pitfalls

- Implicit encoding conversion in hot paths.
- Assuming byte length equals character count.
- Mixing ownership rules for borrowed vs owned strings.
- Forgetting null-handling conventions across host boundaries.

## Pointers

- Memory semantics: `references/spec-map.md`
- ABI discipline: `references/language-to-wasm-playbook.md`
