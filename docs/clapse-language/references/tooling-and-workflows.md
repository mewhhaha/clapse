# Tooling and Workflows

## CLI Commands

Use the single executable:

```bash
cabal run clapse -- format <file>
cabal run clapse -- format --write <file>
cabal run clapse -- format --stdin
cabal run clapse -- compile <input.clapse> [output.wasm]
cabal run clapse -- bench [iterations]
cabal run clapse -- lsp --stdio
```

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

Browser Game of Life demo:

- `just life-build`
- `just life-smoke`
- `just life-serve 8080`

## LSP and Formatter

- LSP currently provides parse diagnostics, type diagnostics, hover inferred types, and inlay hints.
- Formatter is conservative and source-preserving:
  - validate syntax
  - normalize trailing/inner whitespace rules
  - render multiline `let` blocks in Haskell-style layout (`let` + aligned bindings + `in` line)
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
CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal test
just bench
just wasm-smoke
just life-smoke
```
