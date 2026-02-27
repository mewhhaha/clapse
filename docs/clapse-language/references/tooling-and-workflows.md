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
    `artifacts/latest/clapse_compiler.wasm`, then `out/clapse_compiler.wasm`.
  - `just clapse-bin`/`just install` embed `artifacts/latest/clapse_compiler.wasm`
    into `artifacts/bin/clapse` when present, so formatter/LSP can run without
    setting `CLAPSE_COMPILER_WASM_PATH`.
  - `just install` currently refreshes `artifacts/latest/clapse_compiler.wasm`
    from the selected compiler artifact path (no in-install kernel self-recompile).
  - `just install` runs wildcard-demand gate only when
    `CLAPSE_RUN_WILDCARD_DEMAND_CHECK=1`.
  - bridge artifacts are deprecated/unsupported in runtime validation paths.
- `compile-debug` contract:
  - request shape: `command: "compile"` with `compile_mode: "debug"`
  - response must include normal compile payload (`ok`, `wasm_base64`) plus
    `artifacts.lowered_ir.txt` and `artifacts.collapsed_ir.txt`
  - if `compile` omits debug artifacts, the runner resolves them via
    `selfhost-artifacts` for the same input and writes the required debug files
    from that response.
  - this contract is preserved in native and host-bridge compile paths.
- `bench` is currently invoked via the same deno command surface through the wasm runner.

## Just Targets

Current targets in `Justfile`:

- `just clapse-bin`
- `just compile <input> [output]`
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
  - packages the validated seed compiler artifact from
    `CLAPSE_COMPILER_WASM_PATH` (default `artifacts/latest/clapse_compiler.wasm`)
    instead of kernel self-recompile in the release bundling step
  - now hard-fails if generated `clapse_compiler.wasm` is not browser-runnable
    (`scripts/check-browser-compiler-wasm.mjs`)
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
- Formatter logic is decomposed into kernel-side `compiler.formatter`, with
  `bootstrap_phase9_compiler_kernel` acting as command router while further
  kernel module splits are staged.

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
