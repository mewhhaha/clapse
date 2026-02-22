# Totality and Termination Checking

## Use When

Use this when your language needs sound recursion guarantees (Lean/Idris style) for proofs or total functions.

## Core Checks

- structural recursion decreases on each recursive call
- well-founded recursion has explicit decreasing measure
- coinductive definitions satisfy productivity constraints

## Implementation Sketch

1. Build call graph for definitions.
2. For each recursive call, verify strict decrease evidence.
3. Require explicit measures for non-structural cases.
4. Track guardedness/productivity for coinductive constructs.
5. Reject or mark partial functions explicitly by language policy.

## Common Pitfalls

- accepting hidden non-decreasing recursion after inlining
- conflating productivity with ordinary termination
- exposing partial functions as total through wrappers
- weak diagnostics that do not show failing call path

## Pointers

- Conversion checking: `references/recipes/definitional-equality-and-normalization.md`
- Recursion runtime model: `references/recipes/stack-and-call-frames.md`
