# Language Feature Recipes

Use these focused files for fast lookup while implementing Wasm-first runtimes and languages.

## Start Here

- Intent router (first hop): `references/recipes/lookup-by-intent.md`
- Core spec map (semantic authority): `references/spec-map.md`
- Conformance loop: `references/conformance-workflow.md`
- Language lowering baseline: `references/language-to-wasm-playbook.md`

## Top 10 Common Intents

1. Build runtime/validator/executor:
`references/spec-map.md`
2. Compile language features to Wasm:
`references/language-to-wasm-playbook.md`
3. ADT layout and `Either`-style encoding:
`references/recipes/sum-type-layouts-and-optimizations.md`
4. JS interop and data marshalling:
`references/recipes/javascript-interop.md`
5. Browser GPU/Canvas interop:
`references/recipes/browser-gpu-interop.md`
6. GC/lifetimes/safepoints:
`references/recipes/garbage-collection.md`
7. Parallelism with deterministic behavior:
`references/recipes/deterministic-parallel-evaluation.md`
8. Module boundaries and lazy loading:
`references/recipes/module-architecture.md`
9. Rewrite-rule optimization strategy:
`references/recipes/rewrite-rules-and-equational-laws.md`
10. Proof/evidence-driven optimization:
`references/recipes/type-proofs-and-evidence.md`

## Runtime and Execution

- Wasm-first design principles: `references/recipes/wasm-first-design-principles.md`
- Stack and call frames: `references/recipes/stack-and-call-frames.md`
- Exceptions and errors: `references/recipes/exceptions-and-errors.md`
- Tail calls: `references/recipes/tail-calls.md`
- Continuations and coroutines: `references/recipes/continuations-and-coroutines.md`
- Async and green threads: `references/recipes/async-and-green-threads.md`
- Algebraic effect handlers: `references/recipes/algebraic-effect-handlers.md`

## Data Layout, Ownership, and Memory

- Linear memory data creation/access: `references/recipes/linear-memory-data-creation-and-access.md`
- Sum type layouts and optimizations: `references/recipes/sum-type-layouts-and-optimizations.md`
- Algebraic data types: `references/recipes/algebraic-data-types.md`
- Strings and memory layout: `references/recipes/strings-and-memory.md`
- Packing and unpacking: `references/recipes/packing-and-unpacking.md`
- Linear types: `references/recipes/linear-types.md`
- Affine types: `references/recipes/affine-types.md`
- Borrowed vs owned FFI values: `references/recipes/borrowed-vs-owned-ffi-values.md`
- Pure memory reuse: `references/recipes/pure-memory-reuse.md`
- Uniqueness-optimized updates: `references/recipes/uniqueness-optimized-updates.md`

## GC and Lifetime Engineering

- Garbage collection: `references/recipes/garbage-collection.md`
- Lifetime and escape analysis: `references/recipes/lifetime-and-escape-analysis.md`
- Region inference and arenas: `references/recipes/region-inference-and-arenas.md`
- Stack maps and safepoints: `references/recipes/stack-maps-and-safepoints.md`
- GC safepoint scheduling: `references/recipes/gc-safepoint-scheduling.md`
- Generational GC write barriers: `references/recipes/generational-gc-write-barriers.md`
- Moving vs nonmoving heap policy: `references/recipes/moving-vs-nonmoving-heap-policy.md`
- Drop elaboration and finalization: `references/recipes/drop-elaboration-and-finalization.md`
- Heap profiling and lifetime telemetry: `references/recipes/heap-profiling-and-lifetime-telemetry.md`

## Type Systems and Proofs

- Elaboration and unification: `references/recipes/elaboration-and-unification.md`
- Definitional equality and normalization: `references/recipes/definitional-equality-and-normalization.md`
- Dependent types and erasure: `references/recipes/dependent-types-and-erasure.md`
- Totality and termination checking: `references/recipes/totality-and-termination-checking.md`
- Holes and goal-directed development: `references/recipes/holes-and-goal-directed-development.md`
- Typeclasses and dictionaries: `references/recipes/typeclasses-and-dictionaries.md`
- Type proofs and evidence: `references/recipes/type-proofs-and-evidence.md`
- Proof-carrying optimizations: `references/recipes/proof-carrying-optimizations.md`
- Proof-guided runtime Wasm rewrites: `references/recipes/proof-guided-runtime-wasm-rewrites.md`

## Control Flow and Core Language Features

- Function application: `references/recipes/function-application.md`
- Lambdas: `references/recipes/lambdas.md`
- Closures and captures: `references/recipes/closures-and-captures.md`
- If statements: `references/recipes/if-statements.md`
- Switch statements: `references/recipes/switch-statements.md`
- Loops: `references/recipes/loops.md`
- Pattern matching: `references/recipes/pattern-matching.md`
- Generics and polymorphism: `references/recipes/generics-and-polymorphism.md`
- Persistent data structures: `references/recipes/persistent-data-structures.md`
- Lazy evaluation: `references/recipes/lazy-evaluation.md`
- Stream fusion and deforestation: `references/recipes/stream-fusion-and-deforestation.md`

## Parallelism, Determinism, and Randomness

- Parallelism and threads: `references/recipes/parallelism-threads.md`
- Parallel runtime principles by field: `references/recipes/parallel-runtime-principles-by-field.md`
- Deterministic parallel evaluation: `references/recipes/deterministic-parallel-evaluation.md`
- Randomness and reproducibility: `references/recipes/randomness-and-reproducibility.md`
- SIMD and data parallelism: `references/recipes/simd-and-data-parallelism.md`
- Actor supervision and fault domains: `references/recipes/actor-supervision-and-fault-domains.md`
- Typed protocols and message schema evolution: `references/recipes/typed-protocols-and-message-schema-evolution.md`

## Interop and Browser Integration

- JavaScript interop: `references/recipes/javascript-interop.md`
- JavaScript data marshalling: `references/recipes/javascript-data-marshalling.md`
- Browser GPU interop (WebGPU/WebGL): `references/recipes/browser-gpu-interop.md`
- Canvas memory layout: `references/recipes/canvas-memory-layout.md`
- Bitstring and binary pattern matching: `references/recipes/bitstring-and-binary-pattern-matching.md`
- Effects, FFI, and purity boundaries: `references/recipes/effects-ffi-and-purity.md`
- Component model and WIT interop: `references/recipes/component-model-and-wit-interop.md`
- Memory64 and multi-memory strategy: `references/recipes/memory64-and-multi-memory-strategy.md`

## Modules, Linking, and Build Boundaries

- Modules and linking: `references/recipes/modules-and-linking.md`
- Module architecture: `references/recipes/module-architecture.md`
- Module signatures and separate compilation: `references/recipes/module-signatures-and-separate-compilation.md`
- Lazy module loading: `references/recipes/lazy-module-loading.md`
- Cross-module state and contracts: `references/recipes/cross-module-state-and-contracts.md`

## Optimization and Pass Design

- Rewrite rules and equational laws: `references/recipes/rewrite-rules-and-equational-laws.md`
- Optimization pipeline design: `references/recipes/optimization-pipeline-design.md`
- Predictable optimization rules: `references/recipes/predictable-optimization-rules.md`
