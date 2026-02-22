# Definitional Equality and Normalization

## Use When

Use this when type checking depends on conversion checking (`A` equals `B` after normalization).

## Core Approach

Implement conversion checking with normalization-by-evaluation (NbE) or a deterministic weak-head normalization strategy.

## Implementation Sketch

1. Define evaluation semantics for core terms.
2. Normalize both sides to comparable normal forms.
3. Compare alpha-equivalent normalized structures.
4. Cache normalization results by stable term hash.
5. Keep fuel/guardrails to avoid runaway reductions.

## Performance Tips

- normalize lazily (only where conversion check requires it)
- memoize normal forms for repeated constraints
- separate cheap syntactic checks from expensive normalization

## Common Pitfalls

- non-termination from unrestricted reduction rules
- inconsistent normalization order causing flaky errors
- mixing erased and non-erased terms in equality checks
- missing universe-level checks for dependent systems

## Pointers

- Elaboration constraints: `references/recipes/elaboration-and-unification.md`
- Totality/termination: `references/recipes/totality-and-termination-checking.md`
