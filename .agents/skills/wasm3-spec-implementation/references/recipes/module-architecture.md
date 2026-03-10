# Module Architecture

## Use When

Use this when designing how a Wasm-first language/runtime should split features across multiple modules.

## Boundary Rules

Choose module boundaries by change rate and runtime lifecycle:

- `core`: type/runtime primitives, stable ABI
- `stdlib`: collections, math, text, stable but larger
- `feature-*`: optional capabilities (regex, crypto, image, etc.)
- `app`: user program entry and composition

Keep high-churn or optional features out of core.

## Interface Design

For each module, define:

- exported symbols
- imported dependencies
- memory/table ownership
- initialization contract
- ABI version

Generate this from compiler metadata; do not hand-maintain it.

## Implementation Sketch

1. Build a dependency graph from package imports.
2. Collapse strongly-connected components or break cycles explicitly.
3. Emit one manifest per module with imports/exports and ABI version.
4. Validate graph acyclicity (or explicit cycle rules) before build output.
5. Add compatibility checks during load/link.

## Common Pitfalls

- Allowing transitive imports to leak as accidental public API.
- Sharing memory implicitly without ownership contract.
- Unstable symbol naming between compiler versions.
- Hidden startup ordering assumptions.

## Pointers

- Baseline linking: `references/recipes/modules-and-linking.md`
- Packing/versioning: `references/recipes/packing-and-unpacking.md`
- Interface contracts: `references/recipes/module-signatures-and-separate-compilation.md`
