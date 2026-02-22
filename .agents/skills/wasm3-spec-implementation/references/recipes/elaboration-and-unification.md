# Elaboration and Unification

## Use When

Use this when compiling high-level typed syntax (implicit args, holes, overloaded names) into a small, explicit core language.

## Core Responsibilities

- resolve implicit arguments
- create and solve metavariables
- insert coercions/casts if language permits
- disambiguate overloaded identifiers
- produce fully explicit core terms

## Implementation Sketch

1. Parse into surface AST with source spans.
2. Elaborate with bidirectional typing (`infer`/`check` modes).
3. Generate constraints while elaborating.
4. Solve constraints with unification plus occurs-check.
5. Re-run normalization where definitional equality is needed.
6. Emit explicit core term and evidence trace for diagnostics.

## Common Pitfalls

- unconstrained metavariables escaping into later phases
- non-terminating unification from missing occurs-check
- solving constraints in non-deterministic order
- poor error reporting that loses original source context

## Pointers

- Definitional equality: `references/recipes/definitional-equality-and-normalization.md`
- Typed holes workflow: `references/recipes/holes-and-goal-directed-development.md`
