# Deterministic Parallel Evaluation

## Use When

Use this when you want parallel speedups but reproducible outputs, logs, and test behavior across runs.

## Strategy Comparison

1. Work-stealing scheduler:
- great throughput for irregular workloads
- default behavior is often non-deterministic

2. Actor/message model:
- clearer isolation and ordering policies
- easier determinism via mailbox/step rules

For determinism-first systems, prefer actor model or deterministic work queues; add work-stealing only as an opt-in throughput mode with deterministic constraints.

## Deterministic Design Rules

1. Define deterministic task identity and ordering keys.
2. Restrict shared mutable state; favor message passing.
3. Make reductions explicitly associative/commutative or serialize them.
4. Use deterministic random seeds and replayable schedulers in tests.
5. Record schedule traces for failure reproduction.

## Implementation Sketch

1. Start with single-thread semantics as oracle.
2. Introduce parallel executor with deterministic queue policy.
3. Add parallel map/reduce primitives with deterministic merge rules.
4. Gate non-deterministic optimizations behind opt-in flags.
5. Add fuzz tests that compare parallel results against serial oracle.

## Common Pitfalls

- Hidden non-determinism from floating-point reduction order.
- Data races from accidental shared mutable state.
- Scheduler behavior depending on wall-clock timing.
- Debug logs that reorder without stable task ids.

## Pointers

- Threading basics: `references/recipes/parallelism-threads.md`
- SIMD follow-up for data kernels: `references/recipes/simd-and-data-parallelism.md`
- Random stream policy: `references/recipes/randomness-and-reproducibility.md`
- Broader runtime/scheduler field principles: `references/recipes/parallel-runtime-principles-by-field.md`
