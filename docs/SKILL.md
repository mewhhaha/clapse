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

## Validation

Run:

```bash
just docs-validate
just highlights
just highlights-expect
just highlights-real
just highlights-helix
```

Or directly:

```bash
CLAPSE_COMPILER_WASM_PATH=artifacts/latest/clapse_compiler.wasm deno run -A scripts/validate-docs.mjs
```

## Ongoing sync rule

- When compiler behavior around dispatch, class-method resolution, or rewrite simplification changes, update `docs/clapse-language/references/syntax-reference.md` and `docs/clapse-language/references/optimization-and-collapse-ir.md` together with any `SKILL.md` workflow updates in this folder.
- Keep class/instance docs aligned with the compiler target syntax: Haskell-style `where` blocks are canonical for new docs and examples.
- Keep prelude operator mapping docs aligned with code (`<$`, `<$>`, `<*>`, `<*`, `*>`, `>>=`, `>>`, `<|>`) and reflect any helper-default changes in the syntax reference.
