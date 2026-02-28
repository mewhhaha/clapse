# Resume Notes (Pause Handoff)

## Current objective

Keep JS at the I/O boundary only, keep strict native compile contracts
fail-closed, and continue converging bootstrap toward fully native self-hosting.

## Completed in this session

- `lib/compiler/kernel.clapse`
  - `CommandSelfhost` now aliases `compile_response` directly.
  - `selfhost-artifacts` follows the same kernel-native compile contract path.
- `lib/compiler/json_response.clapse`
  - compile-ready requests still route through `clapse_host_run`; kernel source
    does not yet own full compile/lowering/emission semantics.
- `scripts/build-strict-native-seed.mjs`
  - strict seed generation is now native-only fail-closed.
  - wrapper fallback mode is removed from execution path (no wrapper recovery on
    probe failure).
  - bootstrap resolution now prefers strict seed inputs:
    `CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH` -> `CLAPSE_COMPILER_WASM_PATH` ->
    `CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH`/`artifacts/strict-native/seed.wasm`
    -> `artifacts/latest/clapse_compiler.wasm`.
  - selfhost probe depth is now configurable (`--probe-hops` /
    `CLAPSE_STRICT_NATIVE_SEED_PROBE_HOPS`, default `1`), enabling explicit
    transitive closure diagnostics.
- `Justfile`
  - `bootstrap-strict-native-seed` now retains an existing seed artifact when it
    already passes `scripts/native-selfhost-probe.mjs` (avoids seed regression).
  - `bootstrap-compiler` now retains a validated native bootstrap seed artifact
    when kernel self-compile output fails transitive self-host probe checks.
  - added `native-selfhost-probe` target with configurable `hops`.
- `scripts/run-clapse-compiler-wasm.mjs`
  - `selfhost-artifacts` now expects compile-contract artifacts and writes:
    - `lowered_ir.txt`
    - `collapsed_ir.txt`
    - `compile_response.json`
    - `backend.txt`
- Updated related consumers:
  - `scripts/wasm-compiler-abi.mjs`
    - kernel-native compile responses for `lib/compiler/kernel.clapse` now fail
      contract validation unless emitted wasm exports compiler ABI
      (`memory`/`__memory` + `clapse_run`).
  - `scripts/native-selfhost-probe.mjs`
    - failures now include stage hints derived from debug artifacts (for example
      `seed-stage1:kernel`) so transitive regressions are explicit at the hop
      that collapses.
  - `scripts/selfhost-diff.mjs`
  - `scripts/selfhost-parser-parity.mjs`
  - `scripts/bootstrap-phase9-kernel-smoke.mjs`
  - `scripts/build-wasm-selfhost-artifact-fixture-map.mjs`
- Synced docs:
  - `docs/SKILL.md`
  - `docs/clapse-language/references/tooling-and-workflows.md`

## Remaining gap

- Compile/selfhost semantics in kernel are still synthetic/static in key paths;
  this session removes wrapper-stage bootstrap fallback and aligns contracts,
  but does not yet implement full parser/lowering/emission semantics in
  `lib/compiler/*.clapse`.
- Two-hop closure evidence: `scripts/native-selfhost-probe.mjs --hops 2` on
  `artifacts/latest/clapse_compiler.wasm` fails at hop 2 because the next-hop
  compile output drops `clapse_run` (`memory,main` only), while strict-native
  workflow now preserves already-valid native seeds instead of degrading them
  through failed transitive closure attempts.

## Next steps

1. Replace static compile payload shaping with real native
   frontend/lowering/emitter output.
2. Upgrade diagnostics to structured compiler-phase errors.
3. Tighten release gates to explicitly fail on any synthetic payload markers.
