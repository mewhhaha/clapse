# Parallelism and Threads

## Use When

Use this when you need shared-state parallelism or background work that must run concurrently with other tasks.

## Wasm Building Blocks

- Shared linear memory
- Atomic load/store/RMW instructions
- Wait/notify synchronization
- Host-managed workers/threads

## Implementation Sketch

1. Define concurrency model in language design:
- actor/message passing
- shared-memory threads
- hybrid model
2. Default to message passing for safety; add shared memory intentionally.
3. Restrict shared mutable data types first, then relax with proven rules.
4. Provide standard synchronization APIs in the language runtime.
5. Add data-race guidance and diagnostics in compiler/runtime errors.
6. Build deterministic stress tests with high contention scenarios.

## Common Pitfalls

- Treating non-atomic reads/writes as thread-safe.
- Forgetting memory-order semantics in lock-free code.
- Blocking operations on single-threaded hosts.
- Assuming every embedding target exposes the same threading capabilities.

## Pointers

- Feature tracking: `https://webassembly.org/features/`
- Semantic baseline: `references/spec-map.md`
- Field-generalized guidance: `references/recipes/parallel-runtime-principles-by-field.md`
