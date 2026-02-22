# Wasm-First Design Principles

## Use When

Use this when defining the architecture of a language/runtime that treats WebAssembly as the primary execution target, not a secondary backend.

## Core Principles

1. Define a strict baseline target:
- choose an explicit MVP feature set
- gate every non-baseline feature behind named flags

2. Treat validation as a language contract:
- compiler output must be validator-clean by construction
- run validation checks at each major lowering stage

3. Freeze ABI and layout early:
- calling convention
- value representation
- ownership/lifetime rules
- error/trap boundary behavior

4. Keep host boundaries explicit:
- i64/BigInt policy in JS interop
- ownership of pointers/buffers
- `memory.grow` view invalidation handling

5. Require deterministic fallback paths:
- every optional feature has a baseline fallback
- preserve semantics across fallback/optimized variants

6. Version all contracts:
- module manifests
- packed payload schemas
- cross-module symbol/type contracts

## Wasm-Specific Features to Leverage

- `multi-value` returns to reduce tuple boxing
- tables/function references for dynamic dispatch
- bulk memory operations for runtime primitives
- SIMD for data kernels with scalar fallback
- atomics/shared memory for controlled parallel runtime paths
- custom sections for metadata (proofs, stack maps, manifests)
- structured control flow (`block`/`loop`/`br_table`) for predictable lowering
- lazy module loading with explicit manifests and integrity checks

When available for your deployment target:

- component-model/WIT-based boundaries for stable interop contracts

## Implementation Checklist

1. Publish feature policy matrix (`required`, `optional`, `experimental`).
2. Add compile-time and runtime feature detection strategy.
3. Add ABI compatibility checks in loader.
4. Add cross-engine test matrix for portability.
5. Keep optimization behavior deterministic and traceable.

## Common Pitfalls

- silently relying on one engine's non-portable behavior
- letting feature probes change semantics rather than implementation path
- introducing ABI drift across compiler/runtime versions
- skipping fallback tests for optional features

## Pointers

- Spec map: `references/spec-map.md`
- Interop rules: `references/recipes/javascript-interop.md`
- Module split/load: `references/recipes/lazy-module-loading.md`
- Optimization predictability: `references/recipes/predictable-optimization-rules.md`
