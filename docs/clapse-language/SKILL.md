---
name: clapse-language
description: AI-first reference for Clapse syntax, type/data model, purity and rewrite laws, lowering/collapse optimization pipeline, WASM runtime ABI, JS interop boundaries, and tooling workflows. Use when implementing, reviewing, debugging, or planning Clapse language/compiler/runtime changes, writing examples/tests/docs, or answering "how does Clapse do X?" with repo-accurate behavior.
---

# Clapse Language

## Overview

Use this skill to keep Clapse language decisions and implementations aligned with the current repository behavior. Load only the reference files needed for the active task, then verify assumptions in code before editing.

## Workflow

1. Identify the task area first: syntax/parser, typing/data, optimization/collapse IR, WASM ABI/interop, or tooling.
2. Read only the matching `references/*.md` files from the map below.
3. Verify behavior in source before claiming support or proposing changes.
4. Keep implemented behavior and proposals separate; label proposals explicitly.
5. Update `README.md` and tests/examples when syntax/semantics/backend behavior changes.

## Reference Map

- `references/language-overview.md`
Use to orient on goals, purity model, implemented features, and current gaps.

- `references/syntax-reference.md`
Use for parsing/syntax decisions: identifiers, functions, `let`, `case`, `data`, operators, class/law/instance declarations, and signatures.

- `references/types-and-data.md`
Use for primitive types, inference behavior, constructor/deconstruction model, and collection semantics.

- `references/optimization-and-collapse-ir.md`
Use for lowering and collapse pipeline behavior, optimization passes, verifier invariants, and benchmark expectations.

- `references/wasm-runtime-and-interop.md`
Use for runtime value encoding, imports, closures/struct/string layout, JS interop boundaries, and buffer/slice proposal status.

- `references/tooling-and-workflows.md`
Use for CLI/Just workflows, LSP/formatter behavior, tree-sitter/Helix integration, and validation command sequences.

## Fast Validation Commands

```bash
CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal test
just bench
just wasm-smoke
just life-smoke
```

## Editing Rules

- Prefer repo-accurate statements over aspirational wording.
- Mark status as `implemented`, `partially implemented`, or `not implemented`.
- Keep examples in current language conventions: `snake_case` values/functions, `PascalCase` data/constructors.
- Add migration notes when proposing ABI-visible changes (especially WASM/JS interop).
