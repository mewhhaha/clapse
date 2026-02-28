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
    `artifacts/latest/clapse_compiler.wasm`, then `out/clapse_compiler.wasm`.
  - `just clapse-bin`/`just install` embed `artifacts/latest/clapse_compiler.wasm`
    into `artifacts/bin/clapse` when present, so formatter/LSP can run without
    setting `CLAPSE_COMPILER_WASM_PATH`.
  - `just bootstrap-strict-native-seed` builds a strict native bootstrap seed at
    `artifacts/strict-native/seed.wasm` (plus `.d.ts` + metadata) using a
    native-first seed builder (`scripts/build-strict-native-seed.mjs`):
    compile kernel through bootstrap wasm and probe selfhost closure. The
    `just` target retains an existing seed when it already passes native
    self-host probing, preventing bootstrap regressions from seed churn.
    Bootstrap wasm resolution order is:
    `CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH`, `CLAPSE_COMPILER_WASM_PATH`,
    `CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH` or
    `artifacts/strict-native/seed.wasm`, then
    `artifacts/latest/clapse_compiler.wasm`.
  - `just pre-tag-verify` now generates the strict-native seed first and defaults
    verification commands to `artifacts/strict-native/seed.wasm` when
    `CLAPSE_COMPILER_WASM_PATH` is not explicitly set, including
    `scripts/native-selfhost-probe.mjs` with default transitive depth `2`
    (`CLAPSE_NATIVE_SELFHOST_PROBE_HOPS` to override).
    `scripts/native-selfhost-probe.mjs` supports `--hops <n>` for transitive
    closure checks (default `--hops 1`).
  - `just bootstrap-compiler` recompiles `lib/compiler/kernel.clapse` into a compiler
    wasm artifact using a bootstrap compiler wasm (`CLAPSE_BOOTSTRAP_COMPILER_WASM_PATH`,
    then `CLAPSE_COMPILER_WASM_PATH`, then `CLAPSE_BOOTSTRAP_STRICT_NATIVE_SEED_PATH`
    or `artifacts/strict-native/seed.wasm`).
    Produced compiler artifacts are now required to pass both
    `just native-compile-smoke` and `scripts/native-selfhost-probe.mjs`.
  - if direct kernel self-compile output fails the transitive self-host probe,
    `just bootstrap-compiler` retains a validated native bootstrap seed artifact
    as output instead of failing the pipeline. Bootstrap compile probe depth
    defaults to `2` (`CLAPSE_BOOTSTRAP_NATIVE_SELFHOST_PROBE_HOPS` to override).
  - `just bootstrap-strict-native-seed` retain/build checks now default to
    probe depth `2` (`CLAPSE_STRICT_NATIVE_SEED_PROBE_HOPS` to override).
  - `just install` now runs `just bootstrap-strict-native-seed` and
    `just bootstrap-compiler`, then refreshes
    `artifacts/latest/clapse_compiler.wasm` + `.d.ts` from that kernel recompile.
  - `just install` runs wildcard-demand gate only when
    `CLAPSE_RUN_WILDCARD_DEMAND_CHECK=1`.
  - bridge artifacts are deprecated/unsupported in runtime validation paths.
  - `selfhost-artifacts` now uses a dedicated kernel response path with required
    debug artifact keys (`lowered_ir.txt`, `collapsed_ir.txt`), then the runner writes these files plus
    `compile_response.json` / `backend.txt`.
  - kernel-path compile responses are ABI-normalized at the JS boundary when
    wasm exports `main` but not `clapse_run`: the boundary aliases `main` as
    `clapse_run` and rewrites response `wasm_base64`/`exports`/`dts`. Tiny
    kernel compiler outputs are stabilized by retaining current compiler wasm
    bytes at the same boundary so multi-hop selfhost probes stay deterministic
    (`CLAPSE_KERNEL_ABI_ALLOW_TINY_FALLBACK=0` to fail instead). If
    normalization cannot produce valid compiler ABI, the response hard-fails.
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
  - `just native-boundary-strict-smoke-no-fallback` runs the same gate with
    JS ABI tiny-output fallback disabled.
  - kernel compile requests for `lib/compiler/kernel.clapse` now inject
    `seed_wasm_base64` by default (`CLAPSE_KERNEL_COMPILE_INJECT_SEED_WASM=0`
    disables injection), and boundary metadata may report
    `__clapse_contract.seed_passthrough` when that explicit seed is promoted.
    In these promoted/fallback paths, compile artifacts are rewritten from
    request source payload and tagged via
    `__clapse_contract.source_artifacts_patch` to avoid placeholder stage
    markers in `lowered_ir.txt` / `collapsed_ir.txt`.
  - `just native-selfhost-probe-strict [wasm=...] [hops=...]` runs selfhost
    probe with tiny-output fallback disabled and fail-closed.
  - `just native-strict-no-fallback-check [wasm=...] [hops=...]` chains
    compile smoke + boundary smoke + selfhost probe under strict no-fallback
    settings.
  - `just native-boundary-strict-seed-scan` scans local wasm artifacts (and
    sibling `../clapse2/artifacts/releases` when present) and reports which
    compiler seeds, if any, satisfy strict compile + emit-wat contract checks.
    Set `CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK=1` (or pass
    `--require-no-boundary-fallback`) to fail candidates that only pass through
    JS ABI tiny-output fallback.
    Set `--kernel-selfhost-hops <n>` (or
    `CLAPSE_STRICT_NATIVE_KERNEL_SELFHOST_HOPS=<n>`) to require kernel
    selfhost closure across `n` compile hops during scan.
  - `just native-boundary-strict-seed-scan-kernel [hops=...]` runs the seed
    scan with no-boundary-fallback and kernel selfhost-hop enforcement over
    local roots (`artifacts`, `out`, `out=out`).
  - `just bootstrap-strict-native-seed` is the canonical local generator for a
    strict-native bootstrap seed artifact when no suitable seed is available.
    Set `CLAPSE_STRICT_NATIVE_REQUIRE_NO_BOUNDARY_FALLBACK=1` (or pass
    `--require-no-boundary-fallback`) to fail seed builds that rely on boundary
    tiny-output fallback.
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
- `just native-selfhost-probe [wasm=...] [hops=...]`
- `just native-selfhost-probe-strict [wasm=...] [hops=...]`
- `just native-boundary-strict-smoke`
- `just native-boundary-strict-smoke-no-fallback`
- `just native-strict-no-fallback-check [wasm=...] [hops=...]`
- `just native-boundary-strict-seed-scan`
- `just native-boundary-strict-seed-scan-kernel [hops=...]`
- `just bootstrap-strict-native-seed [out=...] [meta=...]`
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
