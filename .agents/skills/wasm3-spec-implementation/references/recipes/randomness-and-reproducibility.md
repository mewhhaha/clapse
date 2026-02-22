# Randomness and Reproducibility

## Use When

Use this when language/runtime code needs random values while still supporting deterministic testing, replay, and debugging.

## Core Policy

Separate randomness into two modes:

- deterministic mode (seeded PRNG, reproducible)
- entropy mode (host/system randomness, non-reproducible)

Do not mix the two implicitly.

## Browser-Oriented Entropy Sources

- deterministic mode: runtime PRNG seeded from explicit user/test seed
- entropy mode: Web Crypto API (`crypto.getRandomValues`) at host boundary

Keep API calls explicit so callers choose mode intentionally.

## Implementation Sketch

1. Define random API surface:
- `rng.new(seed)`
- `rng.next_*`
- `rng.split` or per-task stream derivation
- `rng.entropy_*` (host-backed)
2. Thread/async model:
- one RNG stream per task/actor/fiber
- deterministic stream derivation by stable task id
3. Record replay metadata:
- root seed
- stream derivation policy version
- random call counts/checkpoints where needed
4. Add strict test mode that forbids entropy APIs.
5. Add fuzz mode with logged seeds for regression repro.

## Common Pitfalls

- using host entropy in code paths expected to be reproducible
- sharing one mutable RNG across parallel tasks
- nondeterministic stream assignment based on scheduler timing
- changing PRNG algorithm without versioning replay artifacts

## Pointers

- Deterministic execution policy: `references/recipes/deterministic-parallel-evaluation.md`
- Effects boundaries: `references/recipes/effects-ffi-and-purity.md`
- JS boundary contracts: `references/recipes/javascript-interop.md`
