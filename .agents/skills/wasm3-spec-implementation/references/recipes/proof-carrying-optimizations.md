# Proof-Carrying Optimizations

## Use When

Use this when optimization decisions should be justified by proof/evidence artifacts instead of ad-hoc heuristics.

## Core Idea

Require each non-trivial optimization to consume verifiable evidence and emit transformed code plus retained/invalidate evidence metadata.

## Implementation Sketch

1. Define evidence schema per optimization class.
2. Check evidence preconditions before rewrite/specialization.
3. Apply transform and re-validate type/effect/ownership invariants.
4. Preserve compatible evidence; invalidate stale evidence explicitly.
5. Log proof ids and rule ids in optimization trace.

## Runtime Extension

For runtime rewrites, combine static proof evidence with runtime guards and safe deoptimization fallback.

## Common Pitfalls

- accepting unverifiable or stale evidence from previous pass epochs
- optimizing with partial evidence and no fallback
- not versioning evidence format across compiler/runtime upgrades
- conflating "profile-hot" with "proof-safe"

## Pointers

- Evidence model: `references/recipes/type-proofs-and-evidence.md`
- Runtime patching: `references/recipes/proof-guided-runtime-wasm-rewrites.md`
