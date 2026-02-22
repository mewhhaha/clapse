# Proof-Guided Runtime Wasm Rewrites

## Use When

Use this when applying runtime rewrites/specialization (superinstructions, inline caches, hot-path lowering) while preserving correctness with explicit proof guards.

## Runtime Rewrite Model

Treat each rewrite as:

- `match`: runtime-observed pattern
- `required_evidence`: static/type/runtime facts that must hold
- `guard`: fast runtime condition checks
- `replacement`: optimized code path
- `deopt_path`: guaranteed-safe fallback

## Evidence Sources

- compiler-emitted type/effect evidence
- runtime profile evidence (hot counts, stable target ids)
- module hash/version evidence for ABI stability
- stack-map/frame-shape evidence at safepoints

## Implementation Sketch

1. Run rewrites only at safe patch points (function entry, safepoint, dispatcher).
2. Verify required evidence before patching.
3. Install rewritten path with guard checks.
4. Keep original baseline path available for deoptimization.
5. Invalidate patch on evidence/version change.
6. Log rewrite decisions for deterministic replay.

## Guard Examples

- call target monomorphic for N samples
- argument/value tags match expected type set
- function/module ABI version unchanged
- memory/table growth constraints unchanged

## Deoptimization Rules

- capture enough state to resume baseline execution safely
- make deopt idempotent and side-effect-safe
- include reason codes (`guard_fail`, `version_change`, `evidence_stale`)

## Common Pitfalls

- patching code without rollback path
- depending on profile evidence without type/effect evidence
- performing rewrites mid-frame without safepoint guarantees
- missing invalidation when module replacement or lazy loading occurs

## Pointers

- Type evidence model: `references/recipes/type-proofs-and-evidence.md`
- Safepoints: `references/recipes/stack-maps-and-safepoints.md`
- Lazy module versioning: `references/recipes/lazy-module-loading.md`
