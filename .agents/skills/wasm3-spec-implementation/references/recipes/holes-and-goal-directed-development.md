# Holes and Goal-Directed Development

## Use When

Use this when supporting typed holes (`?goal`) and interactive refinement workflows like Lean/Idris.

## Core Behavior

Each hole carries:

- expected type (goal)
- local context bindings
- unresolved constraints/metavariables
- source span and stable hole id

## Implementation Sketch

1. Elaborate program while inserting metavariables for holes.
2. Report goal context in deterministic pretty-print format.
3. Allow hole filling/refinement and re-elaboration incrementally.
4. Recheck solved holes and remaining constraints.
5. Emit actionable diagnostics for unsolved or ambiguous holes.

## Common Pitfalls

- hole ids changing across edits without stable mapping
- context display missing implicit arguments
- solving a hole with term that later invalidates other constraints
- poor incremental performance from full recompilation

## Pointers

- Constraint solving: `references/recipes/elaboration-and-unification.md`
- Proof evidence flow: `references/recipes/type-proofs-and-evidence.md`
