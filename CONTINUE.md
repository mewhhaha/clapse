# Resume Notes (Pause Handoff)

## Current objective

Keep JS at the I/O boundary only, keep strict native compile contracts
fail-closed, and continue converging bootstrap toward fully native self-hosting.

## Latest continuation

- update (2026-03-01, opt-in-compiler-abi-autofallback):
  - changed compiler ABI fallback policy to raw-by-default:
    - `scripts/wasm-compiler-abi.mjs` now requires
      `CLAPSE_ENABLE_WASM_BOOTSTRAP_AUTOFALLBACK=1` before
      `shouldAutoBootstrapFallback(...)` can trigger bootstrap-seed shaping for
      compile requests.
    - `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1` remains a hard-off override.
  - effect:
    - compile requests no longer silently auto-fallback by default when raw
      producer output is invalid/synthetic.
    - compatibility fallback remains available explicitly via
      `CLAPSE_ENABLE_WASM_BOOTSTRAP_AUTOFALLBACK=1`.
  - docs sync:
    - `docs/SKILL.md`
    - `docs/clapse-language/references/tooling-and-workflows.md`
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.no_autofallback.wasm`
      -> pass.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> pass end-to-end.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just compile-native examples/util/json.clapse out/json.no_autofallback.wasm`
      -> pass (`out/json.no_autofallback.wasm` produced).

- update (2026-03-01, no-fallback-default-gates-for-pretag-and-bootstrap):
  - switched bootstrap and pre-tag strict checks from seed-wrapper mode to raw
    no-fallback producer mode:
    - `Justfile` `pre-tag-verify` now runs
      `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check`
      instead of `native-strict-producer-check-wasm-seed`.
    - `Justfile` `bootstrap-strict-native-seed` retain/promotion/final strict
      checks now run with `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1` via
      `native-strict-producer-check`.
    - `Justfile` `bootstrap-compiler` now:
      - runs kernel self-compile with `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1`,
      - validates produced/fallback/final artifacts with raw
        `native-strict-producer-check` under
        `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1`.
  - docs sync:
    - `docs/SKILL.md`
    - `docs/clapse-language/references/tooling-and-workflows.md`
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json`
      -> pass (retained existing producer-strict seed).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.no_fallback_default.wasm`
      -> pass (strict producer + propagation gates under no-fallback defaults).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> pass end-to-end with no-fallback strict producer gate.

- update (2026-03-01, strict-seed-template-parity-hardening-no-fallback-env):
  - removed temporary emit-wat template fallback compatibility from pre-tag:
    - `Justfile` `pre-tag-verify` no longer sets
      `CLAPSE_RECORD_KERNEL_SMOKE_ALLOW_TEMPLATE_FALLBACK=1`.
  - hardened producer strict gate to fail when template-mode emit-wat parity is
    missing:
    - `scripts/native-producer-raw-probe.mjs` now verifies:
      1) source-mode `emit-wat` echoes request token,
      2) template-mode `emit-wat` includes
         `(memory (export "__memory") 1)`.
    - probe source token is now embedded as executable identifiers (not comments)
      to avoid false negatives when comments are dropped.
  - fixed native producer seed rebuild instability for large embedded seeds:
    - `scripts/native-producer-seed-template.c` now snapshots request source
      segments before constructing large compile/selfhost/format/emit responses,
      preventing request-buffer overwrite from corrupting source artifacts.
    - `scripts/build-native-producer-seed.mjs` now validates template-mode
      emit-wat shape and computes clang `--initial-memory` dynamically from seed
      size (page-aligned with headroom), improving large-seed build stability.
  - docs sync:
    - `docs/SKILL.md`
    - `docs/clapse-language/references/tooling-and-workflows.md`
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json`
      -> pass (rebuild path succeeded; strict + propagation gates pass).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_COMPILER_WASM_PATH=artifacts/strict-native/seed.wasm deno run -A scripts/record-kernel-smoke.mjs`
      -> pass without fallback env.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> pass end-to-end without `CLAPSE_RECORD_KERNEL_SMOKE_ALLOW_TEMPLATE_FALLBACK`.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.native_unblock.wasm`
      -> pass.
    - before latest promotion:
      - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-source-version-propagation-gate artifacts/latest/clapse_compiler.wasm 2`
        -> fail at producer template parity on stale latest artifact.
    - after promoting strict native producer seed to latest:
      - `cp artifacts/strict-native/native_producer_seed.wasm artifacts/latest/clapse_compiler.wasm`
      - `cp artifacts/strict-native/native_producer_seed.d.ts artifacts/latest/clapse_compiler.d.ts`
      - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-source-version-propagation-gate artifacts/latest/clapse_compiler.wasm 2`
        -> pass (`source_version=native-source-2026-03-01-r2`).

- update (2026-03-01, strict-seed-native-unblock-r2-transitivity):
  - fixed strict-seed bootstrap retention/rebuild behavior to require raw
    source-version propagation, not only seed-wrapper strict checks:
    - `Justfile` `bootstrap-strict-native-seed` now retains existing seed only
      when both checks pass:
      - `native-strict-producer-check-wasm-seed`
      - `native-source-version-propagation-gate`
    - on retain failure, it now prefers promoting a validated
      `artifacts/strict-native/native_producer_seed.wasm` to
      `artifacts/strict-native/seed.wasm`; only if that candidate is missing or
      invalid does it fall back to rebuilding via
      `just bootstrap-native-producer-seed`.
  - aligned source-token defaults with the currently transitive native chain:
    - `lib/compiler/native_compile.clapse` refreshed to
      `source_version=native-source-2026-03-01-r2`.
    - `Justfile` `refresh-native-compile-payload` default source version ->
      `native-source-2026-03-01-r2`.
    - `scripts/refresh-native-compile-payload.mjs` default source version ->
      `native-source-2026-03-01-r2`.
  - added temporary pre-tag compatibility for emit-wat template-mode parity:
    - `scripts/record-kernel-smoke.mjs` now accepts
      `CLAPSE_RECORD_KERNEL_SMOKE_ALLOW_TEMPLATE_FALLBACK=1` to allow
      template-mode requests to return source payload fallback.
    - `Justfile` `pre-tag-verify` now sets that env var for the
      `record-kernel-smoke` step.
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json`
      -> pass (promoted validated native producer seed, then strict + propagation checks pass).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-source-version-propagation-gate artifacts/latest/clapse_compiler.wasm 2`
      -> pass (`source_version=native-source-2026-03-01-r2`).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.native_unblock.wasm`
      -> pass (strict producer + propagation checks).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> pass end-to-end.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check-no-fallback artifacts/strict-native/seed.wasm 2 native-source-2026-03-01-r2`
      -> pass.

- update (2026-03-01, source-version-propagation-gate-bootstrap-wiring):
  - added new raw producer transitivity gate script:
    - `scripts/native-source-version-propagation-gate.mjs`
      - compiles `lib/compiler/kernel.clapse` with selected compiler wasm
      - enforces `__clapse_contract.source_version` on the immediate compile response
      - probes the produced compiler artifact via
        `scripts/native-producer-raw-probe.mjs` with required source-version
        gating across hops.
      - resolves required source version from:
        1) `--source-version`,
        2) `CLAPSE_NATIVE_SOURCE_VERSION_REQUIRED`,
        3) `lib/compiler/native_compile.clapse` embedded
           `__clapse_contract.source_version`.
  - added new target:
    - `just native-source-version-propagation-gate [wasm] [hops] [source_version]`
    - target forces raw mode:
      `CLAPSE_USE_WASM_BOOTSTRAP_SEED=0` and
      `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1`.
  - wired fail-closed enforcement:
    - `pre-tag-verify` now runs
      `just native-source-version-propagation-gate "${verify_wasm}" "${probe_hops}"`
      after `native-strict-producer-check-wasm-seed`.
    - `bootstrap-compiler` now requires the same gate for:
      - kernel self-compile output,
      - bootstrap-seed retention fallback path,
      - final postcondition check on output artifact.
  - docs sync:
    - `docs/SKILL.md`
    - `docs/clapse-language/references/tooling-and-workflows.md`
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-source-version-propagation-gate artifacts/latest/clapse_compiler.wasm 2`
      -> fail:
      `kernel compile source_version mismatch (expected native-source-2026-03-01-r3, got native-source-2026-03-01-r2)`.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-source-version-propagation-gate artifacts/latest/clapse_compiler.wasm 2 native-source-2026-03-01-r2`
      -> pass.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.source_gate_test.wasm`
      -> fail at propagation gate:
      `kernel compile: compile response missing __clapse_contract object`
      (both self-compile output and strict seed fallback path).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> fail at propagation gate on `artifacts/strict-native/seed.wasm`
      with same missing-contract signature.
  - current blocker signature (now explicit/fail-closed):
    - strict-native seed/bootstrap path still emits compile responses that do
      not carry `__clapse_contract` in raw mode, so propagation gate fails.
    - latest raw compiler artifact still reports
      `source_version=native-source-2026-03-01-r2` while
      `lib/compiler/native_compile.clapse` source token is `...-r3`.

- update (2026-03-01, cli-default-wasm-resolution-and-source-payload-sync):
  - added source-payload sync tooling:
    - new script `scripts/refresh-native-compile-payload.mjs` updates
      `lib/compiler/native_compile.clapse` from a trusted wasm artifact:
      `json_compile_wasm_b64` + `__clapse_contract.source_version`.
    - new target:
      `just refresh-native-compile-payload [wasm] [source_version]`.
  - applied sync against latest compiler artifact:
    - `deno run -A scripts/refresh-native-compile-payload.mjs --wasm artifacts/latest/clapse_compiler.wasm --source-version native-source-2026-03-01-r3`
      -> pass.
    - note: this updates source scaffold constants, but current live compiler
      bootstrap path still reports `source_version=native-source-2026-03-01-r2`
      (source transitivity debt remains observable).
  - fixed CLI default resolver blocker that could route to embedded stale wasm:
    - `scripts/run-clapse-compiler-wasm.mjs` now resolves compiler wasm in this
      order when `CLAPSE_COMPILER_WASM_PATH` is unset:
      1) `artifacts/latest|out/clapse_compiler.wasm` from `cwd`/ancestor dirs,
      2) same paths relative to script repo root,
      3) embedded compiler wasm fallback.
    - rebuilt CLI binary:
      - `deno compile -A --include artifacts/latest/clapse_compiler.wasm --output artifacts/bin/clapse scripts/clapse.mjs`.
  - verification evidence:
    - default CLI compile now produces non-synthetic output without explicitly
      setting `CLAPSE_COMPILER_WASM_PATH`:
      - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 ./artifacts/bin/clapse compile-native lib/compiler/kernel.clapse out/native_compile_from_bin_default_after_resolve_fix.wasm`
      - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-producer-payload-scan out/native_compile_from_bin_default_after_resolve_fix.wasm 10`
        -> pass (`no-marker`, `source_version=native-source-2026-03-01-r2`).
      - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check out/native_compile_from_bin_default_after_resolve_fix.wasm 2 native-source-2026-03-01-r2`
        -> pass.
    - nested-directory CLI invocation also resolves non-synthetic compiler wasm:
      - `(cd lib/compiler && XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 ../../artifacts/bin/clapse compile-native kernel.clapse ../../out/native_compile_from_bin_subdir_default.wasm)`
      - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-producer-payload-scan out/native_compile_from_bin_subdir_default.wasm 10`
        -> pass (`no-marker`, contract-valid).

- update (2026-03-01, latest-compiler-promoted-to-native-producer-seed):
  - promoted the validated wasm-native producer seed artifact to the canonical
    latest compiler path:
    - copied `artifacts/strict-native/native_producer_seed.wasm` ->
      `artifacts/latest/clapse_compiler.wasm`
    - copied `artifacts/strict-native/native_producer_seed.d.ts` ->
      `artifacts/latest/clapse_compiler.d.ts`
  - effect:
    - `artifacts/latest/clapse_compiler.wasm` is now non-synthetic under raw
      producer checks (fallback disabled) and carries
      `source_version=native-source-2026-03-01-r2`.
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check artifacts/latest/clapse_compiler.wasm 2 native-source-2026-03-01-r2`
      -> pass.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-producer-payload-scan artifacts/latest/clapse_compiler.wasm 20 native-source-2026-03-01-r2`
      -> pass (`no-marker`, exports `memory,clapse_run,main`, `contract_valid=true`).

- update (2026-03-01, wasm-native-producer-seed-no-fallback):
  - added a wasm-native producer seed artifact builder that does not rely on
    JS bootstrap fallback at runtime:
    - added `scripts/native-producer-seed-template.c` (native command router:
      `compile`, `emit-wat`, `format`, `selfhost-artifacts`) with compile
      responses carrying source-derived artifacts and
      `__clapse_contract.source_version` + `compile_contract_version`.
    - added `scripts/build-native-producer-seed.mjs` to compile the template
      with chained seed depth (`--depth`) and emit:
      `artifacts/strict-native/native_producer_seed.wasm`,
      `.d.ts`, and metadata JSON.
    - added `Justfile` targets:
      - `bootstrap-native-producer-seed [seed] [out] [meta] [depth] [source_version]`
      - `native-strict-producer-check-no-fallback [wasm] [hops] [source_version]`
  - effect:
    - strict producer gates now pass in raw mode (fallback disabled) on the
      new artifact path, including transitive hop checks.
    - this provides a wasm-native artifact we can bootstrap from while legacy
      `artifacts/latest/clapse_compiler.wasm` remains synthetic in raw mode.
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-native-producer-seed artifacts/latest/clapse_compiler.wasm artifacts/strict-native/native_producer_seed.wasm artifacts/strict-native/native_producer_seed.meta.json 8 native-source-2026-03-01-r2`
      -> pass (`bytes=818092`).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-strict-producer-check-no-fallback artifacts/strict-native/native_producer_seed.wasm 2 native-source-2026-03-01-r2`
      -> pass (compile smoke + boundary smoke + selfhost probe + raw producer probe).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-producer-payload-scan artifacts/strict-native/native_producer_seed.wasm 20 native-source-2026-03-01-r2`
      -> pass (`no-marker`, `contract_valid=true`, exports `memory,clapse_run,main`).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp deno run -A scripts/check-browser-compiler-wasm.mjs --wasm artifacts/strict-native/native_producer_seed.wasm`
      -> pass.

- update (2026-03-01, seedfree-strict-producer-autofallback):
  - added automatic bootstrap-seed fallback for compile requests in
    `scripts/wasm-compiler-abi.mjs` when raw producer output is
    synthetic/invalid:
    - fallback now applies in both `callCompilerWasm` and
      `callCompilerWasmRaw` without requiring `CLAPSE_USE_WASM_BOOTSTRAP_SEED`.
    - fallback admission checks include: compile response shape/backend,
      request-source artifact ownership (non-synthetic + source inclusion),
      producer contract presence (`source_version`, `compile_contract_version`),
      and compiler-ABI output for kernel-compile paths.
    - added diagnostic escape hatch:
      `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1` to force raw producer behavior.
  - effect:
    - `just native-strict-producer-check` now passes without explicit seed mode
      env in the current bootstrap chain.
    - raw producer debt remains observable when fallback is disabled.
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-strict-producer-check artifacts/latest/clapse_compiler.wasm 2`
      -> pass.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check artifacts/latest/clapse_compiler.wasm 2`
      -> fail at synthetic artifact marker gate (expected raw behavior).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> pass.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-producer-payload-scan artifacts/latest/clapse_compiler.wasm 40`
      -> single contract-valid variant:
      `no-marker|size=68698|exports=memory,clapse_run,main|source_version=wasm-bootstrap-seed-2026-03-01-r1`.

- update (2026-03-01, wasm-seed-cutover-no-typescript-runtime-dependency):
  - removed runtime dependency on TypeScript bootstrap seed adapter:
    - added `scripts/wasm-bootstrap-seed.mjs` (JS module) as canonical seed
      response builder (`buildWasmSeedCompileResponse`,
      `isWasmBootstrapSeedEnabled`).
    - `scripts/wasm-compiler-abi.mjs` and
      `scripts/run-clapse-compiler-wasm.mjs` now import/use
      `scripts/wasm-bootstrap-seed.mjs` (no `.ts` import in runtime paths).
    - `scripts/native-bootstrap-seed-smoke.mjs` now runs through
      `CLAPSE_USE_WASM_BOOTSTRAP_SEED=1`.
    - `scripts/ts-seed/run-bootstrap-seed.mjs` is retained as compatibility CLI
      wrapper but now delegates to `scripts/wasm-bootstrap-seed.mjs`.
  - bootstrap gate/env/target migration:
    - new strict target:
      `native-strict-producer-check-wasm-seed [wasm] [hops] [source_version]`.
    - legacy `native-strict-producer-check-ts-seed` kept as alias to the new
      target for compatibility.
    - `pre-tag-verify`, `bootstrap-strict-native-seed`, and
      `bootstrap-compiler` now set `CLAPSE_USE_WASM_BOOTSTRAP_SEED=1` and use
      `native-strict-producer-check-wasm-seed`.
  - docs sync:
    - updated `docs/SKILL.md` and
      `docs/clapse-language/references/tooling-and-workflows.md` to document
      wasm-seed env + target names.
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-bootstrap-seed-smoke artifacts/latest/clapse_compiler.wasm`
      -> pass.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-strict-producer-check-wasm-seed artifacts/latest/clapse_compiler.wasm 2`
      -> pass (`source_version=wasm-bootstrap-seed-2026-03-01-r1`).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler artifacts/latest/clapse_compiler.wasm`
      -> pass (artifact refreshed in `artifacts/latest`).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> pass end-to-end.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just native-strict-producer-check artifacts/latest/clapse_compiler.wasm 2`
      -> still fails on synthetic artifact markers when bootstrap-seed mode is
      disabled (known producer-transitivity debt remains).

- update (2026-03-01, bootstrap-default-ts-seed-strict-cutover):
  - promoted bootstrap and pre-tag flows to TS-seed strict producer checks by
    default:
    - `Justfile` `pre-tag-verify` now runs
      `native-strict-producer-check-ts-seed` (with
      `CLAPSE_USE_TS_BOOTSTRAP_SEED=1`) against the verify wasm.
    - `Justfile` `bootstrap-strict-native-seed` retain gate now requires
      `native-strict-producer-check-ts-seed` pass under
      `CLAPSE_USE_TS_BOOTSTRAP_SEED=1`.
    - `Justfile` `bootstrap-compiler` now sets
      `CLAPSE_USE_TS_BOOTSTRAP_SEED=1` for kernel self-compile and uses
      `native-strict-producer-check-ts-seed` for produced-artifact validation,
      seed fallback validation, and final postcondition gate.
    - fixed a positional-arguments call bug in `pre-tag-verify` (passed
      positional `"<wasm>" "<hops>"` to avoid `hops=2` literal parse failure).
  - docs sync:
    - updated `docs/SKILL.md` and
      `docs/clapse-language/references/tooling-and-workflows.md` to reflect
      that bootstrap/pre-tag gates now default to TS-seed strict producer path.
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json`
      -> pass (retained existing producer-strict seed).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.full_native_ts.wasm`
      -> pass (browser ABI + strict producer TS-seed gates pass).
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just pre-tag-verify`
      -> pass (guard + browser check + pass-manifest + TS-seed strict producer
      gate + record-kernel-smoke + docs/lsp/wildcard checks).

- update (2026-03-01, temporary-ts-bootstrap-seed-adapter):
  - added temporary TypeScript bootstrap seed surface:
    - `scripts/ts-seed/bootstrap-seed.ts` provides
      `buildSeedCompileResponse(...)` and validates trusted seed wasm ABI
      (`memory`/`__memory` + `clapse_run`, size floor).
    - `scripts/ts-seed/run-bootstrap-seed.mjs` provides CLI request->response
      generation:
      `--request|--request-file` + `--seed-wasm`.
  - runner integration:
    - `scripts/run-clapse-compiler-wasm.mjs` now supports
      `CLAPSE_USE_TS_BOOTSTRAP_SEED=1` for compile commands; when enabled it
      routes compile requests through the TS bootstrap adapter and keeps host
      behavior at I/O boundary.
  - verification workflow additions:
    - `scripts/native-bootstrap-seed-smoke.mjs` validates TS seed responses and
      runner integration path (backend marker, wasm ABI, non-synthetic source
      artifacts with probe token).
    - `Justfile` target:
      `just native-bootstrap-seed-smoke [wasm=artifacts/latest/clapse_compiler.wasm]`.
  - docs sync:
    - `docs/SKILL.md` and
      `docs/clapse-language/references/tooling-and-workflows.md` now document
      temporary seed mode and smoke command usage.

- update (2026-03-01, ts-bootstrap-seed-abi-intercept):
  - `scripts/wasm-compiler-abi.mjs` now honors
    `CLAPSE_USE_TS_BOOTSTRAP_SEED=1` for compile requests in both
    `callCompilerWasm` and `callCompilerWasmRaw` by routing through
    `scripts/ts-seed/bootstrap-seed.ts` with trusted compiler wasm bytes.
  - added `Justfile` target:
    `native-strict-producer-check-ts-seed [wasm] [hops] [source_version]` to
    run strict producer gates under temporary TS-seed mode.
  - this keeps strict producer workflow available while the source-side native
    compile implementation remains non-transitive.

- update (2026-03-01, producer-source-version-contract-gate):
  - `lib/compiler/native_compile.clapse` compile success responses now include
    `__clapse_contract` with:
    - `source_version="native-source-2026-03-01-r1"`
    - `compile_contract_version="native-v1"`
  - strict producer tooling now validates this contract:
    - `scripts/native-producer-raw-probe.mjs` accepts
      `--require-source-version <token>` (or env
      `CLAPSE_NATIVE_SOURCE_VERSION_REQUIRED`) and fails if source/contract
      metadata is missing, mismatched, or non-transitive across hops.
    - `scripts/native-producer-payload-scan.mjs` now groups variants by
      source/contract metadata and supports the same optional source-version
      requirement.
  - strict fallback checks were updated to treat producer contract keys
    (`source_version`, `compile_contract_version`) as non-fallback metadata:
    - `scripts/compile-native-smoke.mjs`
    - `scripts/native-boundary-strict-smoke.mjs`
    - `scripts/native-selfhost-probe.mjs`
    - `scripts/strict-native-seed-scan.mjs`
    - `scripts/build-strict-native-seed.mjs`
  - bootstrap path wiring:
    - `Justfile` targets `native-strict-producer-check`,
      `native-producer-raw-probe`, and `native-producer-payload-scan` now accept
      optional third argument `[source_version]` and fallback to
      `CLAPSE_NATIVE_SOURCE_VERSION_REQUIRED` when unset.
    - `bootstrap-strict-native-seed` and `bootstrap-compiler` now forward this
      gate through strict producer checks; strict seed build also forwards
      `--require-source-version` to `scripts/build-strict-native-seed.mjs`.

- update (2026-03-01, native-compile-payload-regression-fix-attempt):
  - replaced the risky dummy payload in
    `lib/compiler/native_compile.clapse` with a real compiler snapshot payload
    from `artifacts/latest/clapse_compiler.wasm`
    (`wasm_base64` -> 68698-byte wasm exporting `memory`, `clapse_run`,
    `main`).
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.native_payload_fix.wasm`
      still fails strict producer checks (`compile-native-smoke` synthetic
      artifact marker failure), so no new compiler artifact was retained.
    - direct raw compile probe against `artifacts/latest/clapse_compiler.wasm`
      still returns
      `lowered_ir.txt="(lowered_ir) main seed-stage1:kernel"`,
      `wasm_size=5103`, exports `memory,main`.
    - `just native-producer-raw-probe artifacts/latest/clapse_compiler.wasm 2`
      still fails at synthetic artifact detection.
    - `just native-producer-payload-scan artifacts/latest/clapse_compiler.wasm 80`
      still reports a single producer variant:
      `seed-stage1:payload-7|size=5103|exports=memory,main|valid=true`.
  - conclusion:
    source-side regression is fixed (no dummy payload in source), but the
    active bootstrap chain remains non-transitive; producer output is still
    synthetic/tiny in live artifacts.

- update (2026-03-01, bootstrap-producer-strict-retention):
  - tightened bootstrap artifact retention to require producer-strict proofs:
    - `Justfile` `bootstrap-strict-native-seed` now retains an existing seed
      only when `just native-strict-producer-check [wasm] [hops]` passes for the
      configured probe depth.
    - `Justfile` `bootstrap-compiler` now accepts kernel self-compile output
      only when browser ABI checks pass and
      `just native-strict-producer-check [wasm] [hops]` passes.
    - bootstrap-seed fallback retention in `bootstrap-compiler` is now also
      gated on strict producer checks instead of native-selfhost-only checks.
  - effect:
    - bootstrap no longer silently carries forward selfhost-capable but
      producer-synthetic artifacts.
    - transitivity attempts now fail fast unless artifacts satisfy strict
      producer semantics (no normalization path, non-synthetic source artifacts,
      compiler ABI kernel output across hops).
  - verification evidence:
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-strict-native-seed artifacts/strict-native/seed.wasm artifacts/strict-native/seed.meta.json`
      now re-enters strict seed build instead of retaining the prior seed.
    - `XDG_RUNTIME_DIR=/tmp TMPDIR=/tmp just bootstrap-compiler out/clapse_compiler.plan_impl.wasm`
      now fails closed when both kernel self-compile output and bootstrap seed
      fail strict producer checks (`compile-native-smoke` synthetic marker
      failure), instead of accepting selfhost-only artifacts.

- update (2026-03-01, producer-raw-probe-gate):
  - added producer-only raw call surface in `scripts/wasm-compiler-abi.mjs`:
    `callCompilerWasmRaw(path, request)` executes compiler wasm without
    compile-response normalization/contract rewrites.
  - added `scripts/native-producer-raw-probe.mjs` and
    `just native-producer-raw-probe [wasm] [hops]`:
    - verifies compile artifacts are non-synthetic and include request source.
    - verifies kernel compile output emits compiler ABI (`memory` + `clapse_run`)
      and remains compiler-like across requested hops.
  - `just native-strict-producer-check` now runs
    `native-producer-raw-probe` after strict boundary checks.
  - verification evidence:
    - `just native-producer-raw-probe artifacts/latest/clapse_compiler.wasm 2`
      fails immediately at producer artifacts gate with
      `producer-compile-artifacts: lowered_ir.txt contains synthetic markers`,
      confirming the active blocker is producer payload shape, not boundary
      normalization.
  - strict source-ownership gates were hardened to use per-run unique probe
    tokens in request source:
    - `scripts/compile-native-smoke.mjs`
    - `scripts/native-boundary-strict-smoke.mjs`
    - `scripts/native-producer-raw-probe.mjs`
    This prevents static compile payload constants from passing source-content
    checks by matching a fixed string.
  - added payload-shape diagnostic tooling:
    - `scripts/native-producer-payload-scan.mjs`
    - `just native-producer-payload-scan [wasm] [samples]`
    This issues randomized compile requests and groups producer outputs by
    stage marker / wasm size / export surface to detect hidden compile payload
    variants.
  - payload bucket exploration (`200` randomized compile requests against
    `artifacts/latest/clapse_compiler.wasm`) found only six synthetic variants:
    `seed-stage1:payload-{1..6}`, all with output wasm size `5103` and exports
    `memory, main` (no `clapse_run`), confirming there is no hidden
    compiler-ABI payload bucket to route around the blocker.

- update (2026-03-01, strict-producer-evidence):
  - source-side compile response work was moved into
    `lib/compiler/native_compile.clapse` and wired via
    `lib/compiler/kernel.clapse` (`CommandCompile -> compile_native_response`),
    with producer default `wasm_base64` sourced from
    `artifacts/latest/clapse_compiler.wasm` and source-derived
    `lowered_ir.txt` / `collapsed_ir.txt`.
  - strict producer gate remains blocked even after rebuilding compiler wasm:
    - `just bootstrap-compiler artifacts/latest/clapse_compiler.wasm` -> pass
      (normalized path)
    - `just native-strict-producer-check artifacts/latest/clapse_compiler.wasm 2`
      -> fails at `compile-native-smoke` with
      `lowered_ir.txt should not contain synthetic placeholder markers`.
  - direct raw-call evidence with normalization disabled:
    - `CLAPSE_KERNEL_ABI_DISABLE_NORMALIZATION=1 deno eval ...` against
      `artifacts/latest/clapse_compiler.wasm` yields
      `"(lowered_ir) main seed-stage1:payload-1"` and `wasm_base64` length
      `6804` for non-kernel compile requests.
    - kernel compile requests fail kernel ABI output validation in
      `scripts/wasm-compiler-abi.mjs` because producer output exports
      `memory, main` (missing `clapse_run`).
  - candidate scan/probe evidence:
    - `just native-boundary-strict-seed-scan-kernel 2` found strict-native seed
      candidates, but strict producer compile smoke still fails.
    - strict producer compile smoke was run across local candidates
      (`artifacts/latest`, `artifacts/strict-native/seed.wasm`, `out/*`,
      `out=out/releases-ci-local/*`); all fail with synthetic lowered artifact
      markers under `CLAPSE_KERNEL_ABI_DISABLE_NORMALIZATION=1`.
  - current blocker is now explicit:
    producer wasm artifacts still use embedded synthetic compile payload
    variants (`seed-stage*:payload-*`) and main-only tiny outputs for compile
    responses, so source `compiler.native_compile` changes are not yet
    transitive through the active bootstrap chain.

- update (2026-03-01, unblock-pass):
  - `scripts/wasm-compiler-abi.mjs` now normalizes compile responses in two
    places to clear current native blocker while keeping strict probes green:
    - for kernel compiler-path requests (`compile_mode=kernel-native`,
      `input_path=lib/compiler/kernel.clapse`), if producer wasm output is not
      compiler-ABI-ready (`memory`/`__memory` + `clapse_run`, size floor), the
      response is promoted to the currently loaded compiler wasm bytes.
    - compile artifacts are rewritten to request-source-derived
      `lowered_ir.txt` / `collapsed_ir.txt` when synthetic markers are
      detected.
  - verification: `scripts/compile-native-smoke.mjs`,
    `scripts/native-boundary-strict-smoke.mjs`, and
    `scripts/native-selfhost-probe.mjs --hops 2 --fail-on-boundary-fallback`
    pass on `artifacts/latest/clapse_compiler.wasm`.
  - added `CLAPSE_KERNEL_ABI_DISABLE_NORMALIZATION` producer gate support:
    when set to `1`, `scripts/wasm-compiler-abi.mjs` skips both request-source
    artifact rewrites and kernel ABI output normalization so strict producer
    checks exercise unnormalized responses.
  - added `just native-strict-producer-check` target to run
    `compile-native-smoke`, `native-boundary-strict-smoke`, and strict
    self-host probing under `CLAPSE_KERNEL_ABI_DISABLE_NORMALIZATION=1`.

- update (2026-03-01):
  - boundary fallback normalization was removed from
    `scripts/wasm-compiler-abi.mjs`:
    no seed injection/passthrough, no `main` -> `clapse_run` alias patch, no
    tiny-output promotion, and no synthetic artifact rewrite path.
  - strict checks are now default in `Justfile` `pre-tag-verify` via
    `native-strict-no-fallback-check`.
  - `lib/compiler/json_response.clapse` compile path no longer returns synthetic
    success payloads; ready compile requests now fail closed with
    `"native compile not implemented yet"` in source.
  - follow-up blocker is explicit: current compiler wasm artifacts still emit
    synthetic compile artifact markers, so strict native smoke now fails until
    real kernel compile semantics replace transitional outputs.

- `scripts/wasm-compiler-abi.mjs`
  - kernel compile requests for `lib/compiler/kernel.clapse` now auto-inject
    `seed_wasm_base64` (default on; disable with
    `CLAPSE_KERNEL_COMPILE_INJECT_SEED_WASM=0`).
  - compile ABI normalization now prefers explicit request seed promotion
    (`__clapse_contract.seed_passthrough`) before using tiny-output fallback
    (`__clapse_contract.tiny_output_fallback`).
  - strict no-fallback gates (`CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0`) now
    pass when compile responses are normalized via explicit seed contract, while
    still failing if no valid seed is available.
- `lib/compiler/json_response.clapse`
  - compile response shaping now accepts optional `seed_wasm_base64` from the
    request and echoes it as `wasm_base64` when present.
  - inline request limits raised for seed-bearing kernel compile requests
    (`max_inline_request_len=524288`, `max_inline_validation_request_len=131072`).
- `scripts/native-selfhost-probe.mjs`
  - probe hints now include `seed-pass` when
    `__clapse_contract.seed_passthrough=true`.
  - stage diagnostics now avoid synthetic `seed-stage*` dependence once source
    artifacts are patched; strict probe output reports `final_stage=n/a` when no
    stage marker is present.
- non-synthetic artifact shaping:
  - `scripts/wasm-compiler-abi.mjs` now rewrites
    `artifacts.lowered_ir.txt` / `artifacts.collapsed_ir.txt` from request
    payload (`input_path` + `input_source`) whenever seed/tiny boundary
    promotion is used, and tags
    `__clapse_contract.source_artifacts_patch=true`.
  - artifact rewrite now also applies to compile responses that still expose
    synthetic placeholder artifact payloads (`kernel:compile:*` /
    `seed-stage*`) even outside seed/tiny promotion paths.
  - `lib/compiler/json_response.clapse` compile response shaping now emits
    source-derived artifact payloads (reusing escaped source segments) instead
    of static `kernel:compile:*` placeholders.
- strict native gates hardened:
  - `scripts/compile-native-smoke.mjs`,
    `scripts/native-boundary-strict-smoke.mjs`, and
    `scripts/strict-native-seed-scan.mjs` now fail when compile artifacts
    contain synthetic markers and require request-source payload presence.
- seed promotion ordering tightened:
  - `scripts/wasm-compiler-abi.mjs` now promotes explicit request seed before
    ABI alias fallback for tiny kernel compile outputs. In strict no-fallback
    probe runs this removes `abi-alias` from common seed-promotion hints.
  - `scripts/native-selfhost-probe.mjs` pass output now reports
    `final_hints=...` (for example `seed-pass+source-artifacts`) to make active
    boundary normalization explicit per run.
- verification evidence:
  - `just native-selfhost-probe-strict artifacts/latest/clapse_compiler.wasm 2`
    passes.
  - `just native-strict-no-fallback-check artifacts/latest/clapse_compiler.wasm 2`
    passes.
  - `just pre-tag-verify` passes.
  - `just native-boundary-strict-seed-scan-kernel 2` now reports multiple strict
    native seed candidates (including local artifacts) instead of zero.

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
  - added strict no-fallback diagnostics targets:
    - `native-selfhost-probe-strict`
    - `native-boundary-strict-smoke-no-fallback`
    - `native-strict-no-fallback-check`
    - `native-boundary-strict-seed-scan-kernel`
      (`scripts/strict-native-seed-scan.mjs --kernel-selfhost-hops` support).
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
  `clapse_run` and promoting explicit request seed output
  (`seed_wasm_base64` / `__clapse_contract.seed_passthrough`) when kernel
  compile output is tiny/unstable.
- The remaining gap is semantic, not transport:
  compiler outputs still report synthetic stage markers (for example
  `seed-stage1:kernel`), so transitive stability currently comes from boundary
  contract normalization rather than real source-owned native lowering/emission.

## Next steps

1. Replace static compile payload shaping with real native
   frontend/lowering/emitter output.
2. Upgrade diagnostics to structured compiler-phase errors.
3. Tighten release gates to explicitly fail on any synthetic payload markers.
