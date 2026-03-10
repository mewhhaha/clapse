# Lookup by Intent

Use this file as the first hop for recipe lookups. Open files in the listed order and stop when you have enough context.

## Runtime and Semantics

- User asks about opcode/runtime correctness, traps, validation:
  1. `references/spec-map.md`
  2. `references/conformance-workflow.md`
  3. `references/recipes/stack-and-call-frames.md`

- User asks about implementing a Wasm VM/runtime architecture:
  1. `references/recipes/wasm-first-design-principles.md`
  2. `references/wasm3-notes.md`
  3. `references/recipes/modules-and-linking.md`

## Language to Wasm

- User asks about compiling a language to Wasm:
  1. `references/language-to-wasm-playbook.md`
  2. `references/recipes/function-application.md`
  3. `references/recipes/loops.md`

- User asks about dependent typing/proofs (Lean/Idris style):
  1. `references/recipes/elaboration-and-unification.md`
  2. `references/recipes/dependent-types-and-erasure.md`
  3. `references/recipes/definitional-equality-and-normalization.md`

## Data Layout and Memory

- User asks about ADT/enum layout (`Either`, `Option`, recursive types):
  1. `references/recipes/sum-type-layouts-and-optimizations.md`
  2. `references/recipes/algebraic-data-types.md`
  3. `references/recipes/linear-memory-data-creation-and-access.md`

- User asks about ownership/linear-affine/pure in-place updates:
  1. `references/recipes/linear-types.md`
  2. `references/recipes/affine-types.md`
  3. `references/recipes/pure-memory-reuse.md`

- User asks about GC/lifetimes/safepoints:
  1. `references/recipes/garbage-collection.md`
  2. `references/recipes/lifetime-and-escape-analysis.md`
  3. `references/recipes/stack-maps-and-safepoints.md`

## Interop and Browser

- User asks about JS interop and marshaling:
  1. `references/recipes/javascript-interop.md`
  2. `references/recipes/javascript-data-marshalling.md`
  3. `references/recipes/packing-and-unpacking.md`

- User asks about browser GPU interop or Canvas upload:
  1. `references/recipes/browser-gpu-interop.md`
  2. `references/recipes/canvas-memory-layout.md`
  3. `references/recipes/simd-and-data-parallelism.md`

- User asks about randomness and deterministic replay:
  1. `references/recipes/randomness-and-reproducibility.md`
  2. `references/recipes/deterministic-parallel-evaluation.md`
  3. `references/recipes/effects-ffi-and-purity.md`

## Parallelism and Scheduling

- User asks about threads/actors/deterministic parallel execution:
  1. `references/recipes/deterministic-parallel-evaluation.md`
  2. `references/recipes/parallelism-threads.md`
  3. `references/recipes/parallel-runtime-principles-by-field.md`

- User asks about async/coroutines/effects:
  1. `references/recipes/async-and-green-threads.md`
  2. `references/recipes/continuations-and-coroutines.md`
  3. `references/recipes/algebraic-effect-handlers.md`

## Modules and Contracts

- User asks about module boundaries and lazy loading:
  1. `references/recipes/module-architecture.md`
  2. `references/recipes/lazy-module-loading.md`
  3. `references/recipes/cross-module-state-and-contracts.md`

- User asks about signatures, protocol evolution, component model:
  1. `references/recipes/module-signatures-and-separate-compilation.md`
  2. `references/recipes/typed-protocols-and-message-schema-evolution.md`
  3. `references/recipes/component-model-and-wit-interop.md`

## Optimization

- User asks about rewrite rules and compiler pass structure:
  1. `references/recipes/rewrite-rules-and-equational-laws.md`
  2. `references/recipes/optimization-pipeline-design.md`
  3. `references/recipes/predictable-optimization-rules.md`

- User asks about proof-guided optimization/runtime specialization:
  1. `references/recipes/type-proofs-and-evidence.md`
  2. `references/recipes/proof-carrying-optimizations.md`
  3. `references/recipes/proof-guided-runtime-wasm-rewrites.md`

## Keyword Aliases

- `ffi`, `interop`, `host boundary`: open JS interop block.
- `enum`, `adt`, `sum type`, `either`, `result`: open ADT layout block.
- `gc`, `lifetime`, `safepoint`, `escape`: open GC/lifetimes block.
- `scheduler`, `work stealing`, `deterministic`: open parallelism block.
- `webgpu`, `webgl`, `canvas`, `shader`: open browser GPU block.
- `proof`, `evidence`, `dependent`: open proof/dependent block.
