# Tooling and Workflows

## CLI Commands

Use the deno frontend CLI (host I/O boundary; kernel owns language behavior):

```bash
deno run -A scripts/clapse.mjs compile <input.clapse> [output.wasm]
deno run -A scripts/clapse.mjs format <file>
deno run -A scripts/clapse.mjs format --write <file>
deno run -A scripts/clapse.mjs format --stdin
deno run -A scripts/clapse.mjs lsp --stdio
deno run -A scripts/clapse.mjs bench [iterations]
```

- The frontend handles argument parsing, file I/O, and environment/process wiring,
  then delegates `compile`/`selfhost-artifacts`/`format`/`lsp` requests to the
  Clapse kernel.
- `compile`/`selfhost-artifacts`/`format`/`lsp` use compiler-wasm mode by
  default.
  - compiler wasm is resolved from `CLAPSE_COMPILER_WASM_PATH`, then
    `out/clapse_compiler.wasm`.
  - `just clapse-bin`/`just install` embed `artifacts/latest/clapse_compiler.wasm`
    into `artifacts/bin/clapse` when present, so formatter/LSP can run without
    setting `CLAPSE_COMPILER_WASM_PATH`.
  - `just install` currently refreshes `artifacts/latest/clapse_compiler.wasm`
    from the selected compiler artifact path (no in-install kernel self-recompile).
  - `just install` runs wildcard-demand gate only when
    `CLAPSE_RUN_WILDCARD_DEMAND_CHECK=1`.
  - bridge artifacts are deprecated/unsupported in runtime validation paths.
- `bench` is currently invoked via the same deno command surface through the wasm runner.

## Just Targets

Core:

- `just install`
- `just grammar`
- `just wasm-smoke`
- `just wasm-closure-smoke`
- `just wasm-string-smoke`
- `just bench`
- `just formatter-golden-fixtures`
- `just lsp-wasm-fixtures`
- `CLAPSE_EXPECT_CORE_LSP_BACKENDS=1 just lsp-wasm-fixtures` (require Clapse
  backend for core hover/definition fixture assertions)
- `just fib-memo-plugin-smoke`
- `just wildcard-demand-check` (kernel demand-order regression check)

WASM runtime perf:

- `just bench-wasm-main`
- `just bench-wasm-compare`
- `just bench-wasm-compare-slice-set`

Browser Game of Life demo:

- `just life-build`
- `just life-serve 8080`
- `just life-smoke`

Self-host parity:

- `just selfhost-artifacts`
- `just selfhost-parser-parity`
- `just selfhost-parser-parity-strict`
- `just selfhost-diff`
- `just selfhost-behavior-diff`
- `just formatter-idempotence-corpus`
- `just selfhost-bootstrap-abc`
- `just selfhost-check`
- `just selfhost-check-strict`
- `just selfhost-check-wasm` (requires `CLAPSE_COMPILER_WASM_PATH`)
- `just selfhost-bench`
- `just selfhost-bench-wasm`
- `just selfhost-bench-wasm-fresh`
- `scripts/selfhost-bench.mjs --reuse-compiles-across-repeats 0|1` (default `1`
  for steady-state compile reuse)
- `scripts/run-clapse-compiler-wasm.mjs` (strict right-engine entrypoint;
  executes compiler wasm through `clapse_run` ABI)
  - validates ABI exports (`memory`/`__memory`, `clapse_run`) and response JSON
    shape before writing outputs
  - `engine-mode` reports `wasm-native` when configured

## LSP and Formatter

- Formatter is implemented in the Clapse kernel and returned by `format`
  requests with normalization already applied. CLI (`format` command) and LSP
  `textDocument/formatting` now forward kernel output directly:
  - collapse repeated internal whitespace in expressions while preserving
    indentation, string literals, and line comments
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
- Formatter logic is decomposed into kernel-side `compiler.formatter`, with
  `bootstrap_phase9_compiler_kernel` acting as command router while further
  kernel module splits are staged.

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
just selfhost-check-wasm
just bench
just wasm-smoke
just wildcard-demand-check
just life-smoke
just selfhost-diff
just selfhost-behavior-diff
```
