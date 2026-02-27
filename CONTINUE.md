# Resume Notes (Pause Handoff)

## Current objective

Continue hardening wildcard-demand ordering checks so drift fails fast in local install + CI.

## Completed in this session

- Added wildcard-demand regression fixture:
  - `examples/wildcard_demand_behavior_regressions.clapse`
  - Scenarios:
    - `main_wildcard_not_force`
    - `main_priority_after_reorder`
    - `main_ordered_demand`
- Added those scenarios to:
  - `examples/selfhost_behavior_corpus.json`
- Added docs skill sync note for this regression surface:
  - `docs/SKILL.md`
- Added fixture-map entry so current bridge fallback can execute this new example:
  - `scripts/wasm-behavior-fixture-map.json`
- Verified current outputs through wasm runner path:
  - `main_wildcard_not_force = 11`
  - `main_priority_after_reorder = 31`
  - `main_ordered_demand = 41`
- `just install` passes.

## Partially completed (resume here)

- Added new gate script file:
  - `scripts/wildcard-demand-check.mjs`
- This script is not yet wired into `Justfile` / CI / tooling docs.

## Next steps to finish

1. Wire a Just target:
   - Add `wildcard-demand-check` to `Justfile` calling:
     - `deno run -A scripts/wildcard-demand-check.mjs`
2. Run the gate during `just install` (or add explicit release/verify gating recipe).
3. Add CI step in `.github/workflows/release-verify.yml`:
   - Run wildcard-demand check with `CLAPSE_COMPILER_WASM_PATH=artifacts/latest/clapse_compiler.wasm`.
4. Sync docs:
   - Add `just wildcard-demand-check` to
     `docs/clapse-language/references/tooling-and-workflows.md` (Just targets / validation sections).
5. Re-run `just install`.

## Key context / constraints

- In this repo state, non-kernel compile still uses fixture-map fallback in practice.
- Native matcher/lowering path is not directly editable in `lib/compiler/*.clapse` yet.
- The strict regression gate therefore currently validates behavior through the active wasm runner + fallback path.
