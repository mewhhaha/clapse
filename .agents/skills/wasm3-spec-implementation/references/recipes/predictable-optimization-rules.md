# Predictable Optimization Rules

## Use When

Use this when you need optimization outcomes to be explainable, reproducible, and safe across compiler versions.

## Determinism Checklist

- stable rule registration order
- stable traversal order of IR nodes
- deterministic cost model and tie-breakers
- explicit random-seed ban in optimization decisions

## Safety Checklist Per Rule

Before enabling a rule by default:

1. prove semantic equivalence under stated preconditions
2. encode those preconditions in machine-checkable form
3. add negative tests where preconditions fail
4. benchmark representative workloads
5. add regression guard against compile-time blowups

## Rule Tiers

- `safe-default`: semantics-preserving and broadly beneficial
- `aggressive`: off by default, may trade compile time/code size for speed
- `experimental`: behind feature flag, requires explicit opt-in

## Verification Strategy

1. differential testing (`-O0` vs optimized output behavior).
2. property-based testing on generated programs.
3. conformance tests for Wasm validity and runtime behavior.
4. optimization trace replay tests for determinism.

## Common Pitfalls

- Rule enabled globally without workload evidence.
- Hidden interactions between rules across phases.
- Silent semantic drift from outdated precondition logic.
- Missing rollback path when a rule regresses production code.

## Pointers

- Pipeline ordering: `references/recipes/optimization-pipeline-design.md`
- Conformance loop: `references/conformance-workflow.md`
- Runtime patching model: `references/recipes/proof-guided-runtime-wasm-rewrites.md`
