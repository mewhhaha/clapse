# Resume Notes (Pause Handoff)

## Current objective

Keep JS at the I/O boundary only, keep strict native compile contracts
fail-closed, and continue converging bootstrap toward fully native self-hosting.

## Completed in this session

- `lib/compiler/kernel.clapse`
  - `CommandSelfhost` now routes to `selfhost_ok_response` directly.
  - `selfhost-artifacts` no longer aliases compile dispatch in source.
- `lib/compiler/json_response.clapse`
  - compile-ready requests now route through kernel-local stub compile response
    shaping (no `clapse_host_run` delegation in source path).
  - response now carries `backend`, `wasm_base64`, `exports`, `dts`, and
    kernel-owned marker artifacts (`kernel:compile:lowered`,
    `kernel:compile:collapsed`).
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
  - pre-tag/bootstrap gates now default transitive probe depth to `2`
    (`CLAPSE_NATIVE_SELFHOST_PROBE_HOPS`,
    `CLAPSE_BOOTSTRAP_NATIVE_SELFHOST_PROBE_HOPS`,
    `CLAPSE_STRICT_NATIVE_SEED_PROBE_HOPS`).
- `scripts/run-clapse-compiler-wasm.mjs`
  - `selfhost-artifacts` now expects compile-contract artifacts and writes:
    - `lowered_ir.txt`
    - `collapsed_ir.txt`
    - `compile_response.json`
    - `backend.txt`
- Updated related consumers:
  - `scripts/wasm-compiler-abi.mjs`
    - kernel-native compile responses for `lib/compiler/kernel.clapse` now
      enforce compiler ABI (`memory`/`__memory` + `clapse_run`).
    - when kernel compile output exports `main` but not `clapse_run`, boundary
      validation now aliases `main` as `clapse_run` in wasm export metadata and
      rewrites response `wasm_base64`/`exports`/`dts` accordingly.
    - `selfhost-artifacts` responses now have a dedicated response validator
      (artifact contract) instead of compile-contract validation.
    - kernel ABI tiny-output fallback can now be fail-closed with
      `CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0`.
  - `scripts/native-selfhost-probe.mjs`
    - failures now include stage hints derived from debug artifacts (for example
      `seed-stage1:kernel`) so transitive regressions are explicit at the hop
      that collapses.
    - strict mode now supports
      `--fail-on-boundary-fallback` / `CLAPSE_NATIVE_SELFHOST_FAIL_ON_BOUNDARY_FALLBACK=1`.
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
- Multi-hop closure is now stabilized at the JS boundary:
  `scripts/native-selfhost-probe.mjs --hops 4` passes on
  `artifacts/latest/clapse_compiler.wasm` by ABI-normalizing `main` ->
  `clapse_run` and retaining current compiler wasm bytes when kernel compile
  output is tiny/unstable.
- The remaining gap is semantic, not transport:
  compiler outputs still report synthetic stage markers (for example
  `seed-stage1:kernel`), so transitive stability currently comes from boundary
  retention rather than real source-owned native lowering/emission.

## Next steps

1. Replace static compile payload shaping with real native
   frontend/lowering/emitter output.
2. Upgrade diagnostics to structured compiler-phase errors.
3. Tighten release gates to explicitly fail on any synthetic payload markers.
