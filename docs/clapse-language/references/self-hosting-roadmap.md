# Self-hosting Roadmap

Goal: compile the Clapse compiler with Clapse-generated WASM so Haskell is only the bootstrap host during transition.

## Current checkpoint

- Done: bootstrap fixtures phase `1..8` exist and run.
- Done: parity smoke gates exist (`just bootstrap-check`, `just parity-check`).
- Remaining: compiler pipeline parity, deterministic output contract, and production cutover.

## Ordered milestones

### Milestone 1: Freeze compiler IR/ABI contract

Deliverables:
- Stable schemas for:
  - parsed AST
  - typed/core AST
  - collapsed IR
- Stable runtime ABI contract for:
  - tagged scalars
  - closure record layout
  - struct/data constructor layout
  - slice/region layouts and lifetime semantics
- Deterministic serialization format for parity diffs.

Acceptance gates:
- Golden tests for AST/IR serialization.
- ABI roundtrip tests for closure/data/slice values.
- Explicit compatibility note in `README.md` for any ABI-visible change.

### Milestone 2: Frontend parser/formatter in Clapse

Deliverables:
- Clapse tokenizer/parser module covering current language surface used by examples and bootstrap corpus.
- Source span tracking for diagnostics.
- Formatter-compatible normalized parse output.

Acceptance gates:
- Differential parser parity: Haskell parser vs Clapse parser on example corpus.
- Formatter roundtrip stability (`format` twice yields identical output).
- LSP parse diagnostics remain equivalent on corpus.

### Milestone 3: Type + class/law rewrite engine in Clapse

Deliverables:
- Inference/checking for current data/functions/signatures/constraints.
- Class/law/instance pipeline with rewrite derivation parity.
- Witness-constraint handling equivalent to current behavior.

Acceptance gates:
- Differential typing parity on corpus.
- Differential rewrite parity on corpus.
- No regressions in class/law/instance rewrite tests.

### Milestone 4: Lowering + collapse passes in Clapse

Deliverables:
- Closure/currying/case/let lowering in Clapse.
- Pass pipeline parity for normalize/inline/specialize/prune/tail/region/slice.
- Verifier invariants implemented in Clapse.

Acceptance gates:
- Collapsed IR equivalence against Haskell pipeline (allowing only alpha-renaming and stable reorder rules).
- Existing collapse/optimizer tests pass on both pipelines.
- Performance bars for abstraction-collapse fixtures remain within target envelope.

### Milestone 5: WASM emission in Clapse

Deliverables:
- Clapse-side emitter for current WASM target features in use.
- Export metadata generation parity (`.d.ts` contract parity).
- Runtime import contract parity.

Acceptance gates:
- wasm smoke parity on examples.
- life demo step API parity.
- benchmark fixtures compare within acceptable envelope versus Haskell emitter output.

### Milestone 6: Stage A/B/C bootstrap

Deliverables:
- Stage A: Haskell compiler compiles Clapse compiler sources to wasm artifact.
- Stage B: compiler-wasm compiles corpus.
- Stage C: compare Stage B outputs to Haskell outputs (IR + behavior + perf envelope).

Acceptance gates:
- Single command gate for full bootstrap parity.
- Corpus compile success rate target: 100% for supported surface.
- Differential execution parity for smokes and key demos.

### Milestone 7: Production CLI host wrapper

Deliverables:
- Thin host wrapper (Deno or Rust) handling only:
  - CLI args
  - filesystem/network boundary calls
  - wasm invocation and memory wiring
- Default compiler path uses compiler-wasm.

Acceptance gates:
- `clapse compile`, `clapse format`, `clapse lsp` operational through wrapper.
- Cold-start and steady-state latency measured and documented.

### Milestone 8: Haskell path deprecation

Deliverables:
- Transition window with fallback flag.
- Removal plan for duplicated logic once parity has held over releases.

Acceptance gates:
- Release checklist passes on self-hosted path.
- Fallback path usage near-zero and no blocking regressions.
- Deprecation notice and removal date documented.

## Dependency map

- Milestone 1 blocks all later milestones.
- Milestone 2 and milestone 3 can progress in parallel after milestone 1.
- Milestone 4 depends on milestones 2 and 3.
- Milestone 5 depends on milestone 4.
- Milestone 6 depends on milestone 5.
- Milestone 7 depends on milestone 6.
- Milestone 8 depends on milestone 7 and stability data.

## Practical next 10 tasks

1. Add AST/IR serialization goldens and differential harness skeleton.
2. Define IR equivalence rules (alpha-renaming, commutative normalization, order constraints).
3. Extract parser corpus list from `examples/` into one manifest file.
4. Build parser differential test command and integrate in `just parity-check`.
5. Add type/rewrite differential fixture format.
6. Add collapse IR differential fixture format.
7. Add wasm behavior differential runner for selected corpus entries.
8. Add benchmark comparison gate for hand-vs-abstraction fixtures.
9. Add bootstrap A/B/C orchestrator script with machine-readable report output.
10. Add CI-style local gate command for full self-hosting readiness.
