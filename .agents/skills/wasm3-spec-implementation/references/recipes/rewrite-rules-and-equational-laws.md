# Rewrite Rules and Equational Laws

## Use When

Use this when adding compiler optimizations based on functional/algebraic laws, pattern rewrites, or fusion/canonicalization rules.

## Rule Design Goals

- preserve language semantics exactly
- reduce cost (ops, allocations, branches, calls)
- improve canonical form for later passes
- stay deterministic across builds

## Functional Laws Commonly Used

- beta reduction and eta reduction/expansion (with strict side conditions)
- map fusion: `map f (map g xs) -> map (f . g) xs`
- fold/build or stream-fusion style deforestation
- functor laws and monoid laws where effect/order semantics are preserved
- algebraic simplifications on pure expressions (`x + 0 -> x`, `x * 1 -> x`)

Apply only when purity, evaluation order, and numeric semantics permit equivalence.

## Rule Template

For every rewrite rule, record:

- `name`
- `pattern` (IR shape before)
- `replacement` (IR shape after)
- `preconditions` (types, purity, overflow, alias constraints)
- `cost_expectation` (why it should be cheaper or cleaner)
- `proof_note` (law or invariant justifying equivalence)
- `tests` (positive + negative cases)

## Implementation Sketch

1. Normalize IR first so patterns are stable.
2. Encode side conditions explicitly; never rely on implicit assumptions.
3. Apply rules in deterministic pass order.
4. Re-run type/ownership validation after rewrites.
5. Keep a bounded fixed-point loop with max-iteration cap.

## Termination and Confluence Guardrails

- orient rules toward a canonical form (one-way where possible)
- assign a rank/measure that must strictly decrease
- prevent ping-pong pairs (`A -> B` and `B -> A`) in same phase
- split canonicalization and cost-driven rewrites into separate phases

## Common Pitfalls

- Applying laws that are true in pure lambda calculus but invalid under strict effects.
- Assuming associativity/commutativity where floating-point or overflow breaks it.
- Fusing effectful operations that must remain ordered.
- Pattern matching after lowering destroyed needed IR structure.
- Rules that are locally good but globally regress code size or latency.

## Pointers

- Fusion context: `references/recipes/stream-fusion-and-deforestation.md`
- Purity/effects constraints: `references/recipes/effects-ffi-and-purity.md`
- Evidence format: `references/recipes/type-proofs-and-evidence.md`
