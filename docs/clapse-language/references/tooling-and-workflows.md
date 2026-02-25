# Tooling and Workflows

## CLI Commands

Use the deno frontend CLI:

```bash
deno run -A scripts/clapse.mjs compile <input.clapse> [output.wasm]
deno run -A scripts/clapse.mjs format <file>
deno run -A scripts/clapse.mjs format --write <file>
deno run -A scripts/clapse.mjs format --stdin
deno run -A scripts/clapse.mjs lsp --stdio
deno run -A scripts/clapse.mjs bench [iterations]
```

- `compile`/`selfhost-artifacts`/`format`/`lsp` use compiler-wasm mode by
  default.
  - compiler wasm is resolved from `CLAPSE_COMPILER_WASM_PATH`, then
    `out/clapse_compiler.wasm`.
  - transitional bridge artifact (`out/clapse_compiler_bridge.wasm`) requires
    `CLAPSE_ALLOW_BRIDGE=1`.
- `bench` is currently invoked via the same deno command surface through the wasm runner.

## Just Targets

Core:

- `just install`
- `just grammar`
- `just wasm-smoke`
- `just wasm-closure-smoke`
- `just wasm-string-smoke`
- `just bench`

WASM runtime perf:

- `just bench-wasm-main`
- `just bench-wasm-compare`
- `just bench-wasm-compare-slice-set`

Browser Game of Life demo:

- `just life-build`
- `just life-smoke`
- `just life-serve 8080`

Self-host parity:

- `just selfhost-artifacts`
- `just selfhost-parser-parity`
- `just selfhost-parser-parity-strict`
- `just selfhost-diff`
- `just selfhost-behavior-diff`
- `just formatter-idempotence-corpus`
- `CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler_bridge.wasm just lsp-wasm-fixtures`
- `just selfhost-bootstrap-abc`
- `just selfhost-check`
- `just selfhost-check-strict`
- `just selfhost-check-wasm` (requires `CLAPSE_COMPILER_WASM_PATH`)
- `just selfhost-build-wasm-bridge`
- `just selfhost-check-wasm-bridge`
- `just selfhost-bench`
- `just selfhost-bench-wasm`
- `just selfhost-bench-wasm-fresh`
- `scripts/selfhost-bench.mjs --reuse-compiles-across-repeats 0|1` (default `1`
  for steady-state compile reuse)
- `scripts/run-clapse-compiler-wasm.mjs` (strict right-engine entrypoint;
  executes compiler wasm through `clapse_run` ABI)
  - validates ABI exports (`memory`/`__memory`, `clapse_run`) and response JSON
    shape before writing outputs
  - `engine-mode` reports `wasm-native` or `wasm-bridge`

## LSP and Formatter

- LSP currently provides parse diagnostics, type diagnostics, hover inferred
  types, and inlay hints.
- Formatter is conservative and source-preserving:
  - validate syntax
  - normalize trailing/inner whitespace rules
  - render multiline `let` blocks in Haskell-style layout (`let` + aligned
    bindings + `in` line)
  - avoid semantic rewrites in formatter pass

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
just life-smoke
just selfhost-diff
just selfhost-behavior-diff
```
