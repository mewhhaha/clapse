# Optimization Pipeline Design

## Use When

Use this when deciding pass order, phase boundaries, and how optimization should run predictably for a Wasm-first compiler.

## Recommended Phase Layout

1. normalization phase:
- simplify syntax variants into canonical IR forms
- attach purity/ownership/escape metadata

2. law-based rewrite phase:
- equational rewrites and canonicalization
- no heavy code-size expansion

3. specialization phase:
- inlining/specialization with explicit budgets
- constant propagation and partial evaluation

4. data/memory phase:
- allocation placement
- uniqueness-based in-place updates

5. lowering phase:
- structured control lowering
- target-aware peephole rewrites

## Predictability Rules

- keep pass order static and documented
- run fixed-point loops with explicit iteration caps
- separate safe/default rules from aggressive/size-tradeoff rules
- keep deterministic tie-breakers in all heuristics

## Implementation Sketch

1. Define pass interfaces and invariants per phase.
2. Add pre/post validation checks for each pass.
3. Emit optimization trace logs (rules fired, pass timing, IR hashes).
4. Add a `-Otrace` style mode for debugging why a rule did or did not fire.
5. Keep `-O0` semantics as reference oracle.

## Common Pitfalls

- Interleaving lowering too early and destroying high-level rewrite opportunities.
- Running non-idempotent passes repeatedly without bounds.
- Heuristics depending on unstable map iteration order.
- Optimizing before purity/effect metadata is reliable.

## Pointers

- Rule authoring: `references/recipes/rewrite-rules-and-equational-laws.md`
- Correctness checks: `references/recipes/predictable-optimization-rules.md`
