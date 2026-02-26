---
name: docs
description: Author and maintain the Clapse documentation set with executable examples. Use this for docs structure, reference edits, tutorial snippets, and docs validation workflows.
---

# Clapse Docs Skill

## Purpose

Maintain docs as executable specs. Every Clapse code fence should compile with the current wasm compiler unless explicitly marked `skip`.

## Rules

1. Prefer `clapse` code fences in markdown for language examples.
2. Keep snippets minimal but complete enough to compile.
3. Use `--|` doc comments above public functions in examples where intent matters.
4. Mark non-compilable examples as ` ```clapse skip `.
5. Keep `clapse.json` examples in sync with LSP behavior when module-scope rules change.
6. Keep `clapse.json` plugin fields in sync with plugin directory conventions and LSP/CLI behavior.

## Validation

Run:

```bash
just clapse-bin
just docs-validate
deno run -A scripts/lsp-wasm-fixtures.mjs
just lsp-wasm-fixtures
just highlights
just highlights-expect
just highlights-real
just highlights-helix
just formatter-golden-fixtures
```

Or directly:

```bash
CLAPSE_COMPILER_WASM_PATH=artifacts/latest/clapse_compiler.wasm deno run -A scripts/validate-docs.mjs
```

## Ongoing sync rule

- Keep `docs/clapse-language/references/tooling-and-workflows.md` updated when `clapse.json` configuration shape changes.
- When compiler behavior around dispatch, class-method resolution, or rewrite simplification changes, update `docs/clapse-language/references/syntax-reference.md` and `docs/clapse-language/references/optimization-and-collapse-ir.md` together with any `SKILL.md` workflow updates in this folder.
- Keep class/instance docs aligned with the compiler target syntax: Haskell-style `where` blocks are canonical for new docs and examples.
- Keep prelude operator mapping docs aligned with code (`<$`, `<$>`, `<*>`, `<*`, `*>`, `>>=`, `>>`, `<|>`) and reflect any helper-default changes in the syntax reference.
- Keep Boolean class docs aligned with prelude method surface (`not`, `and`, `or`, `xor`, `implies`, `&&`, `||`) and method-default behavior.
- Record memory-model pass changes in `docs/clapse-language/references/optimization-and-collapse-ir.md` when kernel collapse pipeline stages change (escape/lifetime annotations, ownership/COW rewrite ordering), and mirror behavior notes in `docs/SKILL.md` with concise pass-level comments.
- Record allocation/reuse/COW scope contract updates in both
  `docs/clapse-language/references/optimization-and-collapse-ir.md` and
  `docs/clapse-language/references/wasm-runtime-and-interop.md` when kernel rewrite policy changes.
- Keep `docs/clapse-language/references/optimization-and-collapse-ir.md` and
  `docs/clapse-language/references/wasm-runtime-and-interop.md` aligned on
  memory-model semantics whenever allocation, reclaim, alias/freeze, or scope/lifetime behavior changes.

## Memory model checkpoint

- `clapse_run` now executes `collapse_pipeline_run` once per request and threads the derived `OwnershipRewriteMode` through request-scoped response builders and `slice_set_u8` rewrite helpers.
- `slice_set_u8` rewrite now uses explicit linear writes on the copied descriptor in the COW path (`slice_set_u8_cow`) so copy-on-write remains descriptor-local and does not accidentally recurse into COW policy.

### plugin precompile contract

- `clapse.json` may include `"plugins"` as an array of directories.
- `scripts/run-clapse-compiler-wasm.mjs` now walks upward from the input directory (or cwd), compiles discovered plugin `.clapse` files to `.wasm`, and sends their paths as `plugin_wasm_paths` in the compile request.
- Canonical plugin + memo fixture smoke path: `scripts/fib-memo-plugin-smoke.mjs` with
  `examples/plugins/memo_fib_plugin.clapse` and `examples/fib_memo.clapse`.
