---
name: good-haskell-code
description: Use this skill to write or review production Haskell for GHC 9.14.1 with correctness-first APIs, explicit invariants, and measurable performance. It provides concise GOOD/BAD patterns across architecture, effects/errors, concurrency, FRP, parsing/versioning, and profiling.
---

# Good Haskell Code (GHC 9.14.1)

## Goal

Produce Haskell that is hard to misuse, easy to review, and measurable in production.
Prefer explicit APIs, strong type-level modeling, and modern language ergonomics when they improve clarity.

## Workflow

1. Pick one routing slug from the table below.
2. Open the primary reference first.
3. Open at most one secondary reference unless the user explicitly asks for a broad survey.
4. Answer with paired `GOOD` and `BAD` snippets and one-sentence rationale tied to behavior or maintenance risk.
5. For performance claims, include a concrete measurement plan (`criterion`, allocations, regression gates).

Routing shortcut:
- If the user asks for a code review, start with `code-review-checklist`.

## Baseline Defaults

Use modern features intentionally, not indiscriminately.
Enable only what the module actually needs.
Prefer `default-language: GHC2024` in Cabal components, then add module-level extension pragmas as intentional deltas.

Typical extension set for modern application modules:

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
```

Compiler flags for production code:

```txt
-Wall -Wcompat -Wincomplete-uni-patterns -Wincomplete-record-updates
```

## Record Strategy (Important)

- Prefer dot syntax for readability in business logic (`user.profile.email`).
- Use overloaded updates where supported and readable.
- Prefer short type-scoped field names with `DuplicateRecordFields` (`id`, `adv`, `pos`) over verbose prefix-style names.
- Treat selector export as an API decision:
  - Keep selectors internal by default (`NoFieldSelectors` plus explicit accessors).
  - Expose selectors only when you are willing to treat field names as stable API.

## Routing Table

- `readability` -> `references/readability.md`: naming, module/API clarity.
- `module-architecture` -> `references/module-architecture.md`: public/internal boundaries and dependency direction.
- `api-evolution` -> `references/api-evolution-and-deprecation.md`: additive change strategy, shims, deprecations.
- `effect-boundaries` -> `references/effect-boundaries.md`: pure core plus thin effect edges.
- `error-architecture` -> `references/error-architecture.md`: layered typed errors and boundary logging.
- `typeclass-hygiene` -> `references/typeclass-hygiene.md`: laws, orphan policy, minimal constraints.
- `invariants` -> `references/invariants.md`: illegal states unrepresentable.
- `type-level-gadts` -> `references/type-level-and-gadts.md`: type-level protocol/state guarantees.
- `arrows-frp` -> `references/arrows-and-frp.md`: signal composition and deterministic stepping.
- `laziness-concurrency` -> `references/laziness-and-concurrency.md`: strictness/laziness, knot tying, lazy DP.
- `stm-parallelism` -> `references/stm-and-parallelism.md`: STM queues, async lifecycles, process boundaries.
- `resource-safety` -> `references/resource-safety-and-exceptions.md`: bracket/mask/cancellation discipline.
- `streaming-backpressure` -> `references/streaming-and-backpressure.md`: bounded streaming and queue policy.
- `render-logic-template` -> `references/render-logic-template.md`: end-to-end loop wiring skeleton.
- `performance` -> `references/performance.md`: strictness, allocation, traversal shape.
- `performance-pragmas` -> `references/performance-pragmas.md`: `INLINE`/`SPECIALIZE`/`RULES` policy.
- `pure-data-structures` -> `references/pure-data-structures.md`: container choice by operation profile.
- `space-leak-debugging` -> `references/space-leak-debugging.md`: retention diagnosis workflow.
- `low-latency` -> `references/low-latency.md`: frame budgets and GC pressure control.
- `benchmarking-profiling` -> `references/benchmarking-and-profiling.md`: reliable measurement and profiling loops.
- `observability` -> `references/observability.md`: logs/metrics/tracing with low overhead.
- `determinism-reproducibility` -> `references/determinism-and-reproducibility.md`: stable seeds, fixtures, ordering.
- `parsing-serialization` -> `references/parsing-and-serialization.md`: robust save/wire decode/encode.
- `protocol-versioning` -> `references/protocol-versioning-lifecycle.md`: compatibility lifecycle and migration matrices.
- `unboxed-ffi` -> `references/unboxed-and-ffi.md`: low-level data layout and FFI boundaries.
- `unsafe-boundary-policy` -> `references/unsafe-boundary-policy.md`: containment rules for unsafe operations.
- `tooling-deps` -> `references/tooling-and-deps.md`: Cabal layout, ghci/ghcid workflow, dependency policy.
- `deriving-template-haskell` -> `references/deriving-and-template-haskell.md`: deriving strategy and TH caching.
- `testing-tooling` -> `references/testing-and-tooling.md`: property/regression/golden gates.
- `code-review-checklist` -> `references/code-review-checklist.md`: severity-first review rubric.
- `project-inspiration` -> `references/project-inspiration.md`: high-quality project patterns.

## Output Contract

When writing guidance or reviews with this skill:

1. Start with the selected category.
2. Provide concrete `GOOD` / `BAD` snippets.
3. Explain tradeoff in one sentence (`allocation`, `API stability`, `totality`, `readability`).
4. End with a numbered upgrade plan when user code is being changed.

Avoid fluff; prioritize behaviorally meaningful examples.
