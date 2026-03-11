# Tooling and Workflows

## CLI Commands

Use the deno frontend CLI (host I/O boundary; kernel owns language behavior):

```bash
deno run -A scripts/clapse.mjs compile <input.clapse> [output.wasm] [--entrypoint-export <name>] [--entrypoint-exports <csv>]
deno run -A scripts/clapse.mjs compile-native <input.clapse> [output.wasm] [--entrypoint-export <name>] [--entrypoint-exports <csv>]
deno run -A scripts/clapse.mjs compile-native-debug <input.clapse> [output.wasm] [artifacts-dir] [--entrypoint-export <name>] [--entrypoint-exports <csv>]
deno run -A scripts/clapse.mjs compile-debug <input.clapse> [output.wasm] [artifacts-dir] [--entrypoint-export <name>] [--entrypoint-exports <csv>]
deno run -A scripts/clapse.mjs emit-wat <input.clapse> [output.wat]
deno run -A scripts/clapse.mjs format <file>
deno run -A scripts/clapse.mjs format --write <file>
deno run -A scripts/clapse.mjs format --stdin
deno run -A scripts/clapse.mjs lsp --stdio
deno run -A scripts/clapse.mjs bench [iterations]
```

- The frontend handles argument parsing, file I/O, and environment/process wiring,
  then delegates `compile`/`selfhost-artifacts`/`format`/`lsp` requests to the
  Clapse kernel.
- `emit-wat` is a kernel-owned pure emitter surface returning text output (`wat`)
  from Clapse source-side logic; JS remains the I/O boundary for writing stdout/files.
  The kernel defaults to `source-data` mode (echoes `input_source` into a WAT
  data segment) and accepts `emit_wat_mode: "source"` / `"source-data"` (explicit
  source mode) or `emit_wat_mode: "template"` for static template output.
- `compile`/`selfhost-artifacts`/`format`/`lsp` use compiler-wasm mode by
  default.
  - `compile_mode` now supports `native` / `kernel-native` for kernel-native
    compile response shaping. Default compile mode is kernel-native.
    Debug compile modes are `debug` / `native-debug` (with `kernel-debug` alias).
    CLI/runner command aliases are accepted for underscore forms:
    `compile_debug`, `compile_native`, and `compile_native_debug`.
    Entrypoint/module reachability pruning is now prepared at the runner before
    compile: commands build the import graph from the entry module via preferred
    quoted import specifiers:
    `import "mod/path" { symbol, type TypeName }` and
    `import "mod/path" as alias`.
    Specifiers resolve through `clapse.json` `include` paths (for bare
    specifiers) and filesystem-relative resolution for `./`, `../`, and `/`
    specifiers.
    Built-in prelude aliases (`"prelude"`, `"compiler/prelude"`)
    resolve to `lib/compiler/prelude.clapse` without
    requiring `include`.
    Collection literals (`[]`, `[a, b, c]`) lower through
    `collection_empty`/`collection_extend` as `CollectionLiteral` class methods,
    so target type can dispatch to non-`List` collection representations.
    Legacy prelude list constructors (`ListNil`/`ListCons`) are rewritten to
    `Nil`/`Cons` in runner demand-driven compile paths with deprecation
    warnings.
    The runner then executes fixed-point root propagation across modules.
    Roots are explicit `entrypoint_exports` when present, otherwise source
    `export { ... }` declarations, with `main` fallback.
    Runner requests now forward the resolved `entrypoint_exports` and a
    demand-driven `inputSourceOverride` to the compiler, so only required
    modules/functions/imports are compiled. DCE propagation now consumes:
    explicit imported symbol lists, alias-qualified calls (`alias.symbol`), and
    conservative unqualified usage against reachable target exports.
    Imported-module debug shaping also drops non-runtime declaration noise
    (`class`/`instance`/`law`/`infix`/`type`) from the runner-pruned source so
    collapsed IR tracks reachable execution payloads.
    Missing source roots for unresolved imports become hard errors when `include`
    is configured, and always for unresolved relative/absolute quoted imports.
    Explicit roots accept identifier names and symbolic operator names.
    Unknown explicit roots now fail compile with `unknown entrypoint root`.
    Unreachable top-level function definitions are removed in the native
    compile stage before compile artifacts are emitted.
    Compile responses expose:
    - `public_exports`: user-visible entrypoints for `main`-style execution
    - `abi_exports`: ABI/runtime exports (compiler-kernel artifacts include
      `clapse_run` and memory exports)
    Compile responses no longer expose a legacy top-level `exports` list; use
    `public_exports` for runnable program entrypoints and `abi_exports` for
    runtime/compiler ABI exports.
    Fresh native producer outputs now return direct raw non-kernel compile
    success with requested `public_exports`, empty `abi_exports`, truthful `.d.ts`,
    and debug artifacts instead of the old boundary-error / mini-stub path.
    Boundary synthesis now preserves those truthful raw responses as
    `compiler_raw`, and otherwise prefers executable wasm emission for the compiler-owned
    subset (`main`, arithmetic/comparison builtins, constant literals, `if ... then
    ... else ...`, `let ... in ...`, boolean `case`, and direct top-level calls/
    recursion) before falling back to constant synthesis for the older
    pure-evaluator subset.
    The evaluator subset now also understands symbolic operator references and
    infix arithmetic/comparison forms, qualified callable names by final
    segment (for example `prelude.add`), and lambda values flowing through the
    supported list-map/fold forms, plus captured-free single-argument closures through
    direct application, captured closures, and partial application of builtins/top-level
    defs (for example `inc = \x -> add x 1; main = inc 2` and
    `make_adder x = \y -> add x y; main = (make_adder 2) 3`) and
    simple list-constructor `case` forms with `_` fallback or a second list
    constructor branch (for example `case xs of Cons x _ -> x; _ -> 0` and
    `case xs of Cons x _ -> x; Nil -> 0`), simple custom uppercase constructor
    values/patterns (including constructor refs like `make = Just` and
    `fmap Just xs` and partial constructor application like `mk = Pair 1`),
    list literals such as `[1, 2, 3]`, including top-level typed custom
    `CollectionLiteral` targets, prelude list/bool combinators like
    `list_filter`/`filter`, `list_any`, `list_all`, `any`, `all`, `foldr`,
    `build`, `xor`, and `implies` when the surrounding program stays in the evaluator subset,
    plus function-level `where` local defs for direct local calls (including
    guarded local defs) when they lower to the same phase1 executable subset,
    plus unambiguous user-defined instance methods when exactly one parsed
    instance implementation owns the callable name, including class default
    methods when a single parsed instance fills the remaining method bodies and
    cross-class default chains such as `lift1 x = plus x 1` when the referenced
    method also resolves uniquely at compile time. Ambiguous user-defined
    instance method resolution, including class-default and cross-class-default
    dispatch that reaches ambiguous instance methods, now fails closed in the
    validated compile contract instead of silently succeeding through
    `phase1_passthrough`.
    and simple closed
    record literals plus dot projection/update (for example
    `options = { allow = true, include = Nothing }`, `options.allow`, and
    `options { allow = false }`, including grouped/local projections such as
    `(options { allow = false }).allow` and nested grouped projections such as
    `({ nested = { allow = true } }).nested.allow`, plus captured local-record
    field/update flows in non-nullary functions such as
    `let base = { value = x } in case eq (base { value = 7 }).value 7 of ...`,
    plus record-pattern `case` on closed records for exact-field and open-rest
    matches such as `case mk of { x = 1, y = 2 } -> 10; _ -> 0` and
    `case mk of { x = 1, _ } -> 20; _ -> 0`), including multi-root nullary record and
    bare record literals in argument position such as
    `allow_flag { allow = true }` where brace syntax after an expression is
    resolved semantically as record update when the base evaluates to a record
    and as function application otherwise, including multi-root nullary record and
    parameterized type-alias record exports that reduce through folded field
    projections, plus transparent `newtype` constructor flows
    including direct `case`, constructor refs as values, `let` pattern
    deconstruction, `fmap` mapping, and explicit non-`main` roots such as
    `unbox x = case x of Box y -> y`, plus builtin boolean operator chains such as
    `lt 1 2 && not false || false`, plus guarded `case of` boolean forms with
    chained guards and `otherwise` fallback (for example
    `case of | eq x 0 -> 0 | eq x 1 -> 1 | otherwise -> 2`), in
    kernel-native phase1 executable paths. The same lowering now covers
    guarded `let` bindings and guarded top-level function clauses with
    `| guard = expr` / `| otherwise = expr` arms, plus simple constructor
    pattern deconstruction in `let` bindings and simple multi-scrutinee
    pattern-arm `case`, plus simple literal-pattern `case` arms and char
    literals parsed as integer codepoints, plus custom symbolic infix
    operators in the existing phase1 operator-name subset (for example `+.`).
    If the requested `public_exports` still require non-`main` structural
    output outside that subset parser, boundary synthesis emits a compatibility
    wasm stub for the selected public exports so root-pruning and DCE flows
    still get a user-only output surface. Debug artifact requests can also use
    that structural fallback when the executable subset does not yet cover the
    requested program shape. Demand-driven debug module graphs now elide
    stitched local imports before the request crosses the wasm boundary, and
    unsupported debug shapes now fail closed instead of using a compatibility
    stub.
    If the source does parse in the subset but still cannot be lowered or
    evaluated, the boundary returns
    `error_code: "compile_phase1_unsupported"` instead of synthetic tagged
    constants.
    The remaining fail-closed surface is now narrower than the old prelude helper
    families: the int-producing prelude map lookup chain and
    `eval_state (state_bind ...)` stay on a real executable path, and direct
    constructor-valued debug roots like `map_from_list_by`, direct
    `map_lookup_by`, and direct `state_bind` now materialize as deterministic
    UTF-8 debug-value slices instead of failing closed or using fake stub wasm.
    Higher-order prelude helper chains now extend through `reader_ap`.
    Generic class-default helper dispatch now fails closed once the
    demand-driven stitcher hits ambiguous instance methods, so helpers like
    `map_replace_default` / `map_replace`, `ap_default`,
    `keep_left_default` / `keep_left`, and
    `keep_right_default` / `keep_right` report
    `compile_phase1_unsupported` instead of silently reducing to placeholder
    wasm. Bare class-method roots without enough instance context, such as
    `pure 201`, also fail closed instead of inventing a container type. Custom
    infix operator application still has a smaller executable-path gap too, so
    named helpers like `alt` compile while operator-alias uses like `<|>` fail
    closed.
    Demand-driven root stitching now also keeps value-position top-level helpers
    with short names, so alias-bound chains like
    `s = set_from_list_by ...; main = set_member_by ... s` no longer get pruned
    before the request reaches the wasm boundary.
    Wrapper paths
    (`callCompilerWasm`, `callCompilerWasmRaw` with contract validation, and the
    runner CLI) recognize that explicit boundary error and synthesize the stable
    reachability-shaped program response:
    `public_exports` follows selected roots, while `abi_exports` is empty for
    user-program outputs (kernel self-host/compiler outputs keep compiler ABI).
    Compile responses also report whether synthesis stayed on a real subset path
    or fell back to temporary compatibility:
    `compile_strategy` is one of `compiler_raw`, `phase1_passthrough`,
    `phase1_executable`, or `phase1_tagged`, and `compatibility_used` must stay
    `false` on the current fail-closed boundary.
    Compile requests also fail closed at the JS boundary when `compile_mode` is
    not one of `kernel-native`, `debug`, `kernel-debug`, `native-debug`, or
    `debug-funcmap`.
    Compile requests with `plugin_wasm_paths` also fail closed at the JS
    boundary when any referenced plugin wasm path is missing or is not a file.
    Mixed selected-root export sets can stay on a real path when callable roots
    are executable and nullary roots are evaluable constants, including
    quoted-module alias cycles such as mutually recursive `even`/`odd`
    definitions compiled with both `main` and the callable helper exported.
    For remaining multi-root or other non-kernel compile requests that the
    current executable/tagged subset still cannot represent as real wasm
    exports, the boundary now returns
    `error_code: "compile_phase1_unsupported"` instead of emitting a
    compatibility wasm stub.
    Compiler-owned bootstrap seed inputs are external artifacts, not embedded
    payloads under `lib/compiler/native_compile*.clapse`, so this exception is
    about the selected seed/runtime path rather than rewriting compiler source.
    When a compile response omits explicit export metadata, the ABI layer now
    derives function arities from wasm type/function sections instead of
    assuming every function export takes one argument.
    The bootstrap seed raw backend can emit structural tiny wasm for explicit
    non-`main` roots, with matching `public_exports`/`dts`. The self-hosted
    compiler-owned phase-1 path now also accepts explicit non-`main` roots:
    when a single selected root is a nullary definition the phase-1 evaluator
    can execute, it emits a real tagged-result wasm export for that root;
    otherwise it falls back to the structural tiny-wasm root stub path.
    Bundle size tracks reachable function count, while kernel self-host compile
    requests still require full compiler ABI output.
    Legacy env
    toggles `CLAPSE_ENTRYPOINT_DCE` and `CLAPSE_INTERNAL_ENTRYPOINT_DCE` remain
    for compatibility but do not control compile request shaping anymore.
    `just native-ir-liveness-size-gate` now enforces strict emitted wasm shrink
    for entrypoint-pruned compile requests (`pruned_bytes < baseline_bytes`).
    Native debug artifacts include kernel-owned `lowered_ir.txt` and
    `collapsed_ir.txt` payloads. These now use a stable artifact header:
    `(lowered_ir)` / `(collapsed_ir)` on the first line, followed by
    `phase:` / `kind:` metadata lines and then normalized source/IR content
    rather than raw request-source echo.
  - host-bridge compile execution is removed from JS boundary code; compile
    requests must execute on a native clapse compiler artifact.
  - compile response validation is strict/fail-closed at the JS boundary:
    compile success must provide `backend: "kernel-native"` and non-empty
    `wasm_base64`; debug modes must also provide required debug artifacts.
    Known tiny placeholder payloads and source-echo marker responses fail closed
    with structured errors (`ok: false`, `error_code`, `error`). Legacy marker
    shaped source-echo compile artifacts are normalized at the boundary before
    strict placeholder checks.
  - `CLAPSE_COMPILE_ENGINE=native|kernel-native` remains accepted for explicit
    native intent on plain `compile`.
  - compiler wasm is resolved from `CLAPSE_COMPILER_WASM_PATH`, then
    `artifacts/latest|out/clapse_compiler.wasm` searched from
    `cwd`/ancestor directories, then the same paths relative to script repo
    root, then embedded compiler wasm when bundled.
  - bootstrap seed mode is opt-in:
    `CLAPSE_USE_WASM_BOOTSTRAP_SEED=1` routes compile requests in
    `scripts/run-clapse-compiler-wasm.mjs` through
    `scripts/wasm-bootstrap-seed.mjs` while reusing a trusted compiler wasm
    payload for `wasm_base64`.
    The same flag also applies in `scripts/wasm-compiler-abi.mjs` for
    `callCompilerWasm` and `callCompilerWasmRaw`, but only for non-
    `kernel-native` compile requests.
    `kernel-native` compile requests fail closed when
    `CLAPSE_USE_WASM_BOOTSTRAP_SEED=1` is set.
    Compile-request auto-fallback has been removed from the ABI path. Use
    `CLAPSE_USE_WASM_BOOTSTRAP_SEED=1` explicitly only for non-`kernel-native`
    bootstrap-seed shaping.
    The helper CLI is
    `deno run -A scripts/ts-seed/run-bootstrap-seed.mjs --request '<json>' --seed-wasm <path>`.
  - `just clapse-bin`/`just install` embed `artifacts/latest/clapse_compiler.wasm`
    into `artifacts/bin/clapse` when present, so formatter/LSP can run without
    setting `CLAPSE_COMPILER_WASM_PATH`.
  - `just bootstrap-strict-native-seed` builds a strict native bootstrap seed at
    `artifacts/strict-native/seed.wasm` (plus `.d.ts` + metadata). The target
    retains an existing seed only when it passes both strict producer checks in
    raw no-fallback mode (`native-strict-producer-check` with
    `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1`) and raw
    source-version propagation checks (`native-source-version-propagation-gate`),
    preventing bootstrap regressions from seed churn.
    Retention is also invalidated when
    `scripts/native-producer-seed-template.c`,
    `lib/compiler/native_compile.clapse`, or
    `lib/compiler/native_compile_reachability.clapse` are newer than the
    retained seed, so compiler/raw-backend source changes force a rebuild.
    If retention fails, it first promotes a validated
    `artifacts/strict-native/native_producer_seed.wasm`; if that is unavailable
    or invalid, it rebuilds through
    `just bootstrap-native-producer-seed`.
    Bootstrap wasm resolution order is:
    `CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH`, `CLAPSE_COMPILER_WASM_PATH`,
    `CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH` or
    `artifacts/strict-native/seed.wasm`.
    It no longer falls back to `artifacts/latest/clapse_compiler.wasm`
    implicitly.
  - `just bootstrap-compiler` now also prefers
    `artifacts/strict-native/seed.wasm` before an existing
    `artifacts/latest/clapse_compiler.wasm` when no explicit bootstrap path is
    set, so ad hoc rebuilds do not default into the giant public-compiler
    feedback loop.
  - `just pre-tag-verify` now generates the strict-native seed first and defaults
    verification commands to `artifacts/strict-native/seed.wasm` when
    `CLAPSE_COMPILER_WASM_PATH` is not explicitly set, including
    `just native-strict-producer-check` with
    `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1` and
    `just native-source-version-propagation-gate` in raw producer mode
    (`CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1`) so source-version transitivity
    is checked independently, plus
    `just native-fold-laws-gate` to ensure `Foldable`/`Buildable` law surface
    appears in collapsed debug artifacts, plus
    `scripts/record-kernel-smoke.mjs` without template fallback overrides, plus
    `scripts/native-selfhost-probe.mjs` with default transitive depth `2`
    (`CLAPSE_NATIVE_SELFHOST_PROBE_HOPS` to override).
    `scripts/native-selfhost-probe.mjs` supports `--hops <n>` for transitive
    closure checks (default `--hops 1`).
  - `just bootstrap-compiler` recompiles `lib/compiler/kernel.clapse` into a compiler
    wasm artifact using a bootstrap compiler wasm (`CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH`,
    then `CLAPSE_COMPILER_WASM_PATH`, then the existing output/latest compiler
    artifact, then `CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH`
    or `artifacts/strict-native/seed.wasm`).
    Produced compiler artifacts are now required to pass browser ABI checks and
    strict producer checks in raw no-fallback mode
    (`CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check`) plus raw
    source-version propagation checks (`just native-source-version-propagation-gate`).
  - if direct kernel self-compile output fails strict producer checks,
    `just bootstrap-compiler` retains a validated native bootstrap seed artifact
    as output instead of failing the pipeline. Bootstrap compile probe depth
    defaults to `2` (`CLAPSE_BOOTSTRAP_NATIVE_SELFHOST_PROBE_HOPS` to override).
  - `just bootstrap-strict-native-seed` retain/build checks now default to
    probe depth `2` (`CLAPSE_STRICT_NATIVE_SEED_PROBE_HOPS` to override).
  - `just install` now runs `just bootstrap-strict-native-seed` and
    `just bootstrap-compiler`, then refreshes
    `artifacts/latest/clapse_compiler.wasm` + `.d.ts` from that kernel recompile.
    If `deno compile` cannot run (for example offline `denort` download
    failures), install reuses an existing `artifacts/bin/clapse` when present,
    otherwise generates a `deno run` shim at `artifacts/bin/clapse`.
    Install also falls back to a temporary writable `XDG_CONFIG_HOME` when
    Helix config paths are not writable.
  - `just install` runs wildcard-demand gate only when
    `CLAPSE_RUN_WILDCARD_DEMAND_CHECK=1`.
  - bridge artifacts are deprecated/unsupported in runtime validation paths.
  - `selfhost-artifacts` now uses a dedicated kernel response path with required
    debug artifact keys (`lowered_ir.txt`, `collapsed_ir.txt`), then the runner writes these files plus
    `compile_response.json` / `backend.txt`.
  - kernel-path compile responses are validated strictly at the JS boundary:
    compiler ABI must already include memory (`memory` or `__memory`) and
    `clapse_run`. Tiny kernel-compiler outputs are hard failures.
  - boundary normalization for kernel compiler-path requests is removed; compile
    responses for native-owned kernel outputs must already satisfy the strict
    ABI contract and include valid `lowered_ir.txt` / `collapsed_ir.txt` content.
- `compile-debug` contract:
  - request shape: `command: "compile"` with `compile_mode: "debug"`
    (native migration also accepts `compile_mode: "native-debug"`; wire-compatible
    alias `command: "compile-debug"` is accepted)
  - response must include normal compile payload (`ok`, `wasm_base64`) plus
    `artifacts.lowered_ir.txt` and `artifacts.collapsed_ir.txt`
  - missing compile debug artifacts are treated as hard runner errors.
  - known placeholder stub compile payloads are rejected by native/debug compile
    commands in the runner with `ok: false`, `error_code`, and `error`.
  - this contract is native-only; host-bridge compile execution is rejected.
  - strict native boundary gate:
  - `just native-boundary-strict-smoke` requires kernel-native compile contract
    fields (`backend` + debug artifacts) plus native `emit-wat` support
    directly from compiler responses.
    The compile artifact contract now rejects synthetic marker payloads
    (`kernel:compile:*`, `seed-stage*`) and requires request-source content in
    `lowered_ir.txt` / `collapsed_ir.txt`.
  - `just native-boundary-strict-smoke-no-fallback` is retained as a strict
    alias; runtime toggles are not required for the current normalization path.
  - `just native-selfhost-probe-strict [wasm=...] [hops=...]` runs selfhost
    probe with fail-closed boundary checks.
    Probe pass output now includes `final_hints=...` to surface active boundary
    contract tags when present.
  - `just native-strict-no-fallback-check [wasm=...] [hops=...]` chains
    compile smoke + boundary smoke + selfhost probe under strict no-fallback
    settings.
  - `just native-strict-producer-check [wasm=...] [hops=...]` runs
    compile smoke, strict boundary smoke, and strict selfhost probe with
    producer output required to satisfy the kernel contract natively.
    Compile-request auto-fallback has been removed from the ABI path. Use
    `CLAPSE_USE_WASM_BOOTSTRAP_SEED=1` explicitly for bootstrap-seed compile
    shaping, or keep it unset for raw producer-only behavior.
  - bootstrap and pre-tag flows now default to
    `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1 just native-strict-producer-check`
    so producer strictness is verified on raw output by default.
    The same flows now also run
    `just native-source-version-propagation-gate [wasm=...] [hops=...] [source_version=...]`
    in raw producer mode (`CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1`) to fail
    when transitive source-version propagation is missing.
  - `just native-boundary-strict-seed-scan` scans local wasm artifacts (and
    sibling `../clapse2/artifacts/releases` when present) and reports which
    compiler seeds, if any, satisfy strict compile + emit-wat contract checks.
    Set `CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK=1` (or pass
    `--require-no-boundary-fallback`) to force fail-closed scanning when any
    boundary fallback markers are detected.
    Set `--kernel-selfhost-hops <n>` (or
    `CLAPSE_STRICT_NATIVE_KERNEL_SELFHOST_HOPS=<n>`) to require kernel
    selfhost closure across `n` compile hops during scan.
  - `just native-boundary-strict-seed-scan-kernel [hops=...]` runs the seed
    scan with no-boundary-fallback and kernel selfhost-hop enforcement over
    local roots (`artifacts`, `out`, `out=out`).
  - `just bootstrap-strict-native-seed` is the canonical local generator for a
    strict-native bootstrap seed artifact when no suitable seed is available.
    It now requires both strict producer and raw source-version propagation
    gates for retention, then prefers promoting a validated
    `native_producer_seed.wasm` candidate before triggering a rebuild.
    Set `CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK=1` (or pass
    `--require-no-boundary-fallback`) to fail seed builds that expose boundary
    fallback markers.
  - `just bootstrap-native-producer-seed [seed=...] [out=...] [meta=...] [depth=...] [source_version=...]`
    builds a wasm-native producer seed artifact from
    `scripts/native-producer-seed-template.c` through
    `scripts/build-native-producer-seed.mjs`. The builder validates raw compile
    + emit-wat contracts with `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1` before
    writing output, including emit-wat template-mode shape checks.
    Default producer seed depth is `1`; override via explicit `depth=...` or
    `CLAPSE_NATIVE_PRODUCER_SEED_DEPTH`.
    The template snapshots request source segments before constructing large
    responses so source artifacts remain stable for large embedded seeds.
  - `just native-strict-producer-check-no-fallback [wasm=...] [hops=...] [source_version=...]`
    runs strict producer gates with
    `CLAPSE_DISABLE_WASM_BOOTSTRAP_FALLBACK=1` for fail-closed producer-only
    verification.
  - `just native-source-version-propagation-gate [wasm=...] [hops=...] [source_version=...]`
    compiles `lib/compiler/kernel.clapse` once with the selected compiler wasm,
    then probes the produced compiler artifact with
    `scripts/native-producer-raw-probe.mjs` under required source-version
    gating plus emit-wat source/template parity. If `source_version` is omitted and
    `CLAPSE_NATIVE_SOURCE_VERSION_REQUIRED` is unset, the gate derives the
    required token from `artifacts/strict-native/seed.meta.json`; when that
    metadata is unavailable it falls back to the observed `kernel compile`
    contract token. Set `CLAPSE_NATIVE_SEED_META_PATH` to override the metadata
    lookup path.
- `bench` is currently invoked via the same deno command surface through the wasm runner.

## Just Targets

Main CI is now [main.yml](/home/mewhhaha/src/clapse/.github/workflows/main.yml), which runs `just verify-compiler-fixpoint`. Release/tag workflows have been removed from `.github/workflows`.

`just verify-compiler-fixpoint` is the primary compiler integrity gate. It rebuilds the committed compiler artifact at `artifacts/latest/clapse_compiler.wasm` through the bounded strict-seed rebuild path (`scripts/build-strict-native-seed.mjs`) and requires byte-identical wasm plus identical `.d.ts` output. `bootstrap-compiler` now also rejects oversized compiler candidates by default via `CLAPSE_MAX_COMPILER_WASM_BYTES` (default `67108864` bytes), so CI and local bootstrap paths do not silently regenerate giant latest-compiler artifacts.

`full-compiler-verify` is now a real green acceptance gate for the current full-compiler surface. By default it verifies the existing `artifacts/latest/clapse_compiler.wasm`; it does not rebuild that public compiler artifact as part of the wrapper. It also runs `just selfhost-compile-strategy-report-success`, which compiles the deduplicated `(entry, export)` pairs from `examples/selfhost_behavior_corpus.json` through the public `compile_mode: "debug"` path and records current strategy counts in `out/selfhost-compile-strategy-report.json`. The current public floor on `artifacts/latest` is `39/39` successful cases with `0` `phase1_compatibility_stub`, `39` `compiler_raw`, `0` `phase1_executable`, and `0` `phase1_tagged`; `require_success=1`, `require_no_compatibility=1`, and `require_raw_only=1` are green. The final step to `100% compiler_raw` was closed at the JS boundary: once sampled export comparison or the source-oracle sampler proves the producer-returned module already matches the requested public ABI and runtime behavior, `synthesizePhase1CompileResponse` now preserves that module as `compiler_raw` instead of carrying forward a stale `phase1_executable` marker. Strict two-hop `native-selfhost-probe` still passes, and `just full-compiler-verify` remains the acceptance gate for the current `artifacts/latest/clapse_compiler.wasm`.

`just full-compiler-last-mile-raw-verify` is the focused raw-only verifier for the final compiler-raw ratchet. It runs reduced raw-only checks over the last-mile feature shapes that used to block `100%`, and it is now green alongside `full-compiler-verify`. It still requires `compile_strategy === "compiler_raw"` and the expected runtime result for each reduced case.

Current targets in `Justfile`:

- `just clapse-bin`
- `just compile <input> [output]`
- `just compile-native <input> [output]`
- `just compile-native-debug <input> [output] [artifacts]`
- `just compile-debug <input> [output] [artifacts]`
- `just compile_debug <input> [output] [artifacts]` (compat alias)
- `just format <file>`
- `just format-write <file>`
- `just lsp`
- `just formatter-golden-fixtures`
- `just lsp-wasm-fixtures`
- `just docs-validate`
- `just fib-memo-plugin-smoke`
- `just pre-tag-verify`
- `just browser-compiler-wasm-check [wasm=...]`
- `just pass-manifest-check`
- `just native-compile-smoke`
- `just compile-debug-smoke`
- `just native-fold-laws-gate`
- `just native-entrypoint-dce-strict-gate`
- `just native-entrypoint-exports-dce-gate`
- `just native-program-codegen-semantics-gate`
- `just native-ir-liveness-size-gate`
- `just native-bootstrap-seed-smoke [wasm=...]`
- `just native-selfhost-probe [wasm=...] [hops=...]`
- `just native-selfhost-probe-strict [wasm=...] [hops=...]`
- `just native-boundary-strict-smoke`
- `just native-boundary-strict-smoke-no-fallback`
- `just native-strict-producer-check [wasm=...] [hops=...]`
- `just native-strict-producer-check-wasm-seed [wasm=...] [hops=...] [source_version=...]`
- `just native-strict-producer-check-ts-seed [wasm=...] [hops=...] [source_version=...]` (compat alias)
- `just native-source-version-propagation-gate [wasm=...] [hops=...] [source_version=...]`
- `just native-strict-no-fallback-check [wasm=...] [hops=...]`
- `just native-boundary-strict-seed-scan`
- `just native-boundary-strict-seed-scan-kernel [hops=...]`
- `just selfhost-compile-strategy-report [manifest=...] [out=...] [mode=debug|kernel-native] [require_no_compatibility=0|1] [require_raw_only=0|1] [require_success=0|1]`
- `just selfhost-compile-strategy-report-success`
- `just full-compiler-last-mile-raw-verify`
- `just bootstrap-strict-native-seed [out=...] [meta=...]`
- `just bootstrap-native-producer-seed [seed=...] [out=...] [meta=...] [depth=...] [source_version=...]`
- `just native-strict-producer-check-no-fallback [wasm=...] [hops=...] [source_version=...]`
- `just bootstrap-compiler [out=...]`
- `just semantics-check` (currently runs `just wildcard-demand-check` and
  `just native-program-codegen-semantics-gate`)
- `just wildcard-demand-check` (kernel demand-order regression check)
  - validated from `scripts/wasm-behavior-fixture-map.json` with source-hash
    drift checks against `examples/wildcard_demand_behavior_regressions.clapse`
  - checks both expected results and deterministic repeat evaluation
- `just highlights`
- `just highlights-update`
- `just highlights-expect`
- `just highlights-real`
- `just highlights-helix`
- `just install`
- `just verify-compiler-fixpoint`
  - rebuilds the committed compiler through the bounded strict-seed path and
    requires byte-identical `clapse_compiler.wasm` plus identical
    `clapse_compiler.d.ts`
  - now hard-fails if the rebuilt compiler exceeds `CLAPSE_MAX_COMPILER_WASM_BYTES`
    or diverges from the committed compiler artifact

## LSP and Formatter

- Formatter is implemented in the Clapse kernel and returned by `format`
  requests with normalization already applied. CLI (`format` command) and LSP
  `textDocument/formatting` now forward kernel output directly:
  - inline whitespace collapse in expressions while preserving indentation,
    string literals, and line comments
  - normalize parenthesized application spacing, including trimming redundant
    spaces immediately inside parens (for example `f    ( g )` -> `f (g)`)
  - enforce a max line width of 100 with vertical wrapping
  - prefer breaking at ` => `, ` = `, ` -> `, ` >>= `, ` >> `, ` && `, ` || `
  - continuation lines are indented by two spaces
  - keep monadic chain normalization for `>>=`/`>>`
- LSP currently provides:
  - compile diagnostics from wasm compiler responses
  - suppress runnable-entrypoint diagnostics like `unknown entrypoint root: main`
    for editor buffers and plugin library files, so diagnostics stay focused on
    document issues rather than executable-root policy
  - hover for `--|` doc comments, falling back to declaration line text when docs are missing
  - local value hover fallback for truthful gaps the kernel does not yet expose:
    function parameter hover types from an existing top-level signature, plus
    simple local `let` alias/literal hover types propagated from the local env,
    plus simple `let` call-result hover types when the callee already has a
    source signature and the arguments are locally-known values, plus signed
    nullary top-level defs used as local values, plus shallow structured
    substitutions like `Maybe a -> Maybe a`, plus local aliases to signed
    top-level functions and `f x` results through those aliases, plus
    constructor-pattern case binders when the scrutinee already has a truthful
    source signature, plus signed closed-record projection chains like
    `opts.allow` and `default_nested.nested.allow`, plus dotted field completion
    for signed closed-record values like `opts.`
  - definitions
  - completion
  - signature help
  - semantic tokens (full)
  - workspace symbols
  - references
  - document symbols
  - prepare rename + rename edits
  - quick-fix code actions (rename/doc-comment suggestions plus missing-signature scaffolds)
- LSP reads `plugins` from `clapse.json` and adds compiled plugin artifacts to
  compiler compile requests via `plugin_wasm_paths`.
- Formatter is conservative and source-preserving:
  - validate syntax
  - normalize trailing/inner whitespace rules
  - render multiline `let` blocks in Haskell-style layout (`let` + aligned
    bindings + `in` line)
  - preserve class/instance `where` block declarations without semantic rewrite
  - avoid semantic rewrites in formatter pass
- Keep this boundary explicit: JS/TS hosts do not perform formatter normalization;
  they forward kernel formatter output unchanged.
- Parser hardline invariants are enforced at both the validated compile
  boundary and the runner-side module-graph parser: case scrutinee/arm arity
  mismatch is a hard parse error, `newtype` accepts exactly one constructor +
  one field, and class fundep tails reject trailing commas such as
  `class map_like f a | f -> a, where`.
- Tail recursion evidence is native-owned: compile artifacts now include
  collapsed tail markers from the producer itself
  (`VSelfTailCall <fn>`, `VMutualTailCall <fn> -> <target>`). The JS boundary
  does not rewrite wasm tail-call opcodes after compile.
- Formatter logic is decomposed into kernel-side `compiler.formatter`, with
  `bootstrap_phase9_compiler_kernel` acting as command router while further
  kernel module splits are staged.
- `selfhost-artifacts` now has a dedicated kernel dispatch path that returns
  required debug artifacts. Tooling should consume the artifact contract
  (`lowered_ir.txt`, `collapsed_ir.txt`) and treat `backend` as optional.

## Release Metadata and Checksums

- `scripts/release-metadata.mjs` accepts repeated `--cli-bin` arguments.
- Manifest output writes:
  - `artifacts.cli_binary`: legacy single-object entry for first `--cli-bin`
    (kept for compatibility).
  - `artifacts.cli_binaries`: array of CLI binaries in argument order, each with
    `path`, `bytes`, and `sha256`.
- `checksums.sha256` includes one line per CLI binary path, matching the order of
  all `--cli-bin` occurrences.

## Project Configuration (`clapse.json`)

- LSP reads `clapse.json` from the current file directory, then walks parent
  directories until it finds one.
- Supported config keys:
  - `include`
  - `plugins`
- Example:

```json
{
  "include": ["src", "examples"],
  "plugins": ["examples/plugins"]
}
```

- `include` is the only supported module-search key in `clapse.json`.
- If `include` is empty or missing, imports are unrestricted and unresolved
  imports do not trigger runner-level module-resolution errors (legacy fail-open).
- `plugins` is a list of plugin source directories. Each directory is recursively
  scanned for `*.clapse` files, and each plugin source is compiled to a sibling
  `.wasm` artifact before compiling the requested input.
- Plugin compilation artifacts are written with a `.wasm` extension beside their
  source file, and compiled program requests pass those artifact paths to the
  compiler as `plugin_wasm_paths`.

- `include` contains directory names. Bare quoted import specifiers are resolved
  by checking `<dir>/<specifier>` and `<dir>/<specifier>.clapse`.
  Relative/absolute specifiers (`./`, `../`, `/`) resolve from the importing
  file path.

## Tree-sitter and Helix

- Grammar source: `tree-sitter-clapse/`
- Local Helix setup: `just install` (includes setup script + health check)

## Change Workflow

When changing syntax/semantics/lowering/WASM behavior:

1. update code
2. update tests
3. update `README.md`
4. update examples/docs (including this skill references when relevant)
5. run validation commands

## Minimal Validation Set

```bash
just pre-tag-verify
just browser-compiler-wasm-check
just pass-manifest-check
just docs-validate
just lsp-wasm-fixtures
just formatter-golden-fixtures
just semantics-check
```
