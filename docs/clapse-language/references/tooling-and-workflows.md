# Tooling and Workflows

## CLI Commands

Use the deno frontend CLI (host I/O boundary; kernel owns language behavior):

```bash
deno run -A scripts/clapse.mjs compile <input.clapse> [output.wasm]
deno run -A scripts/clapse.mjs compile-native <input.clapse> [output.wasm]
deno run -A scripts/clapse.mjs compile-native-debug <input.clapse> [output.wasm] [artifacts-dir]
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
    Native debug artifacts include kernel-owned `lowered_ir.txt` and
    `collapsed_ir.txt` payloads.
  - host-bridge compile execution is removed from JS boundary code; compile
    requests must execute on a native clapse compiler artifact.
  - compile response validation is strict/fail-closed at the JS boundary:
    compile success must provide `backend: "kernel-native"` and non-empty
    `wasm_base64`; debug modes must also provide required debug artifacts.
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
    `callCompilerWasm` and `callCompilerWasmRaw` compile requests.
    Compile-request auto-fallback has been removed from the ABI path. Use
    `CLAPSE_USE_WASM_BOOTSTRAP_SEED=1` explicitly when bootstrap-seed shaping is
    desired for compile requests.
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
  - boundary normalization is active for kernel compiler-path requests:
    when producer output lacks compiler ABI exports or is undersized, response
    `wasm_base64` is promoted to the currently loaded compiler wasm payload.
    compile artifacts are also normalized from request source when synthetic
    markers are detected.
  - set `CLAPSE_KERNEL_ABI_DISABLE_NORMALIZATION=1` to disable both
    `normalizeCompileArtifactsFromRequest` and
    `normalizeKernelCompilerAbiWasm` in strict producer checks.
- `compile-debug` contract:
  - request shape: `command: "compile"` with `compile_mode: "debug"`
    (native migration also accepts `compile_mode: "native-debug"`; wire-compatible
    alias `command: "compile-debug"` is accepted)
  - response must include normal compile payload (`ok`, `wasm_base64`) plus
    `artifacts.lowered_ir.txt` and `artifacts.collapsed_ir.txt`
  - missing compile debug artifacts are treated as hard runner errors.
  - known placeholder stub compile payloads are rejected by native/debug compile
    commands in the runner.
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
    `CLAPSE_KERNEL_ABI_DISABLE_NORMALIZATION=1` so producer output must already
    satisfy the kernel contract without boundary normalization.
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
    The template snapshots request source segments before constructing large
    responses so source artifacts remain stable for large embedded seeds.
  - `just refresh-native-compile-payload [wasm=...] [source_version=...]`
    rewrites `lib/compiler/native_compile.clapse` embedded compile payload and
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
    required token from `lib/compiler/native_compile.clapse`.
- `bench` is currently invoked via the same deno command surface through the wasm runner.

## Just Targets

Current targets in `Justfile`:

- `just clapse-bin`
- `just compile <input> [output]`
- `just compile-native <input> [output]`
- `just compile-native-debug <input> [output] [artifacts]`
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
- `just semantics-check`
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
    emitted `main` export validity
  - release metadata now tracks one or more CLI binaries passed to
    `scripts/release-metadata.mjs` via repeated `--cli-bin` flags; each binary
    is added as a separate manifest entry and checksum line.
  - for local/offline release checks, `release-candidate` supports
    `CLAPSE_RELEASE_SKIP_CROSS_TARGET_CLI=1` (skip cross-target CLI builds) and
    `CLAPSE_RELEASE_ALLOW_BIN_REUSE=1` (reuse `artifacts/bin/clapse` for host
    CLI when host compile fails).
    `just ci-local` defaults both env vars to `1`.

## LSP and Formatter

- Formatter is implemented in the Clapse kernel and returned by `format`
  requests with normalization already applied. CLI (`format` command) and LSP
  `textDocument/formatting` now forward kernel output directly:
  - collapse repeated internal whitespace in expressions while preserving
    indentation, string literals, and line comments
  - normalize repeated spaces before parenthesized application arguments
    (for example `f    (g x)` -> `f (g x)`)
  - enforce a max line width of 100 with vertical wrapping
  - prefer breaking at ` => `, ` = `, ` -> `, ` >>= `, ` >> `, ` && `, ` || `
  - continuation lines are indented by two spaces
  - keep monadic chain normalization for `>>=`/`>>`
- LSP currently provides:
  - compile diagnostics from wasm compiler responses
  - hover for `--|` doc comments, falling back to declaration line text when docs are missing
  - definitions
  - references
  - document symbols
  - prepare rename + rename edits
  - rename
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
- Wasm compile artifacts now include a post-emit tail-call opcode rewrite:
  terminal `call`/`call_indirect` suffixes in function bodies are rewritten to
  `return_call`/`return_call_indirect` when structurally safe for that suffix.
  Set `CLAPSE_EMIT_WASM_TAIL_CALLS=0` to disable this rewrite.
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
- If `include` is empty or missing, imports are unrestricted.
- `plugins` is a list of plugin source directories. Each directory is recursively
  scanned for `*.clapse` files, and each plugin source is compiled to a sibling
  `.wasm` artifact before compiling the requested input.
- Plugin compilation artifacts are written with a `.wasm` extension beside their
  source file, and compiled program requests pass those artifact paths to the
  compiler as `plugin_wasm_paths`.

- `include` contains directory names. `import` targets are resolved by checking
  `<dir>/<dotted_module_name>.clapse` for each configured directory.

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
