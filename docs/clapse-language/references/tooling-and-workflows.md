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
    Direct raw `clapse_run` non-kernel compile requests now fail closed with
    `non-kernel raw compile requires boundary synthesis` instead of returning
    the 352-byte mini compiler stub.
    Boundary synthesis now prefers executable wasm emission for a first-order
    integer/boolean subset (`main`, direct top-level calls/recursion,
    `if`, boolean `case`, and arithmetic/comparison builtins) before falling
    back to constant synthesis for the older pure-evaluator subset.
    The evaluator subset now also understands symbolic operator references and
    infix arithmetic/comparison forms, qualified callable names by final
    segment (for example `prelude.add`), and lambda values flowing through the
    supported list-map/fold forms.
    If the requested `public_exports` still require non-`main` structural
    output outside that subset parser, boundary synthesis emits a compatibility
    wasm stub for the selected public exports so root-pruning and DCE flows
    still get a user-only output surface. Debug artifact requests can also use
    that structural fallback when the executable subset does not yet cover the
    requested program shape. Demand-driven debug module graphs now elide
    stitched local imports before the request crosses the wasm boundary, but
    unsupported debug shapes still use the compatibility stub today.
    If the source does parse in the subset but still cannot be lowered or
    evaluated, the boundary returns
    `error_code: "compile_phase1_unsupported"` instead of synthetic tagged
    constants.
    Wrapper paths
    (`callCompilerWasm`, `callCompilerWasmRaw` with contract validation, and the
    runner CLI) recognize that explicit boundary error and synthesize the stable
    reachability-shaped program response:
    `public_exports` follows selected roots, while `abi_exports` is empty for
    user-program outputs (kernel self-host/compiler outputs keep compiler ABI).
    Compile responses also report whether synthesis stayed on a real subset path
    or fell back to temporary compatibility:
    `compile_strategy` is one of `compiler_raw`, `phase1_passthrough`,
    `phase1_executable`, `phase1_tagged`, or `phase1_compatibility_stub`, and
    `compatibility_used` is `true` only for the compatibility-stub path.
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
    `artifacts/strict-native/seed.wasm`, then
    `artifacts/latest/clapse_compiler.wasm`.
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
    then `CLAPSE_COMPILER_WASM_PATH`, then `CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH`
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
  - `just refresh-native-compile-payload [wasm=...] [source_version=...]`
    rewrites `lib/compiler/native_compile*.clapse` embedded compile payload and
    `__clapse_contract.source_version` from a trusted wasm artifact via
    `scripts/refresh-native-compile-payload.mjs`.
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
    required token from `lib/compiler/native_compile*.clapse`; when those files are
    unavailable it falls back to the observed `kernel compile` contract token.
    Set `CLAPSE_NATIVE_COMPILE_SOURCE_PATH` to override source lookup path.
- `bench` is currently invoked via the same deno command surface through the wasm runner.

## Just Targets

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
- `just bootstrap-strict-native-seed [out=...] [meta=...]`
- `just bootstrap-native-producer-seed [seed=...] [out=...] [meta=...] [depth=...] [source_version=...]`
- `just refresh-native-compile-payload [wasm=...] [source_version=...]`
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
- `just release-candidate [out=...]`
  - release artifacts include `clapse_compiler.wasm` and
    `clapse_compiler.d.ts`
  - recompiles `lib/compiler/kernel.clapse` into release compiler artifacts using a
    bootstrap compiler wasm (`CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH`,
    `CLAPSE_COMPILER_WASM_PATH`, `CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH`
    or `artifacts/strict-native/seed.wasm`)
  - now hard-fails if generated `clapse_compiler.wasm` is not browser-runnable
    (`scripts/check-browser-compiler-wasm.mjs`):
    native ABI + kernel-native compile smoke response contract +
    emitted `public_exports` containing `main`
  - release metadata now tracks one or more CLI binaries passed to
    `scripts/release-metadata.mjs` via repeated `--cli-bin` flags; each binary
    is added as a separate manifest entry and checksum line.
  - for local/offline release checks, `release-candidate` supports
    `CLAPSE_RELEASE_SKIP_CROSS_TARGET_CLI=1` (skip cross-target CLI builds) and
    `CLAPSE_RELEASE_ALLOW_BIN_REUSE=1` (reuse `artifacts/bin/clapse` for host
    CLI when host compile fails).
    `just ci-local` defaults both env vars to `1`.
  - CLI compile outputs are replaced on rerun: `just clapse-bin`, `just install`,
    and `just release-candidate` remove pre-existing target binaries before
    invoking `deno compile`.
  - `.github/workflows/release-verify.yml` also supports manual release publish
    via `workflow_dispatch`: it runs bootstrap + `just pre-tag-verify` +
    release bundling first, then creates/pushes `release_tag` (workflow input
    or `default` to bump the latest `v*` tag patch segment; fallback
    `v<VERSION>` if no tags exist) and publishes the GitHub release from that
    tag.
  - release-verify keeps seed-lock/release-manifest release-id parity as a hard
    check, but treats seed-lock compiler checksum drift as warning so release
    proceeds with the current compiler wasm after strict verification passes.

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
  - hover for `--|` doc comments, falling back to declaration line text when docs are missing
  - definitions
  - completion
  - signature help
  - semantic tokens (full)
  - workspace symbols
  - references
  - document symbols
  - prepare rename + rename edits
  - quick-fix code actions (rename/doc-comment suggestions)
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
