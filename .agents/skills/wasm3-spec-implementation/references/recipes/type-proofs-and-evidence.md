# Type Proofs and Evidence

## Use When

Use this when compiler/typechecker results should be carried forward as machine-checkable evidence for later optimization and runtime specialization.

## Core Idea

Attach proof/evidence artifacts to IR and Wasm metadata so optimizers can rely on explicit facts instead of re-deriving assumptions.

## Useful Evidence Kinds

- expression type witnesses
- effect/purity witnesses
- ownership/uniqueness witnesses
- layout/ABI witnesses (field offsets, tags, alignment)
- control-flow stack-shape witnesses

## Implementation Sketch

1. Emit typed IR with stable node ids.
2. Generate evidence records keyed by node id + pass epoch.
3. Preserve or invalidate evidence explicitly after every transform.
4. Serialize selected evidence into module custom sections or sidecar metadata.
5. Add verifier pass that rechecks evidence before optimization phases.

## Soundness Rules

- evidence must include provenance (which pass produced it)
- evidence must include validity domain (which assumptions are required)
- transforms that break assumptions must drop evidence, not reuse it
- optimization passes must require exact evidence kind, not generic "typed" flags

## Common Pitfalls

- stale evidence reused after inlining/canonicalization rewrites
- evidence keyed by unstable ids (breaks reproducibility)
- mixing debug-only and release evidence semantics
- using unverified evidence from untrusted modules

## Pointers

- Rewrite rules: `references/recipes/rewrite-rules-and-equational-laws.md`
- Predictability policy: `references/recipes/predictable-optimization-rules.md`
- Proof-carrying pipeline: `references/recipes/proof-carrying-optimizations.md`
