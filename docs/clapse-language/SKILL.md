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

- `references/pass-manifest.json`
Use for machine-checked optimization pass status (`implemented`, `partially implemented`, `not implemented`) and keep it synchronized with optimization docs.

- `references/wasm-runtime-and-interop.md`
Use for runtime value encoding, imports, closures/struct/string layout, JS interop boundaries, and buffer/slice proposal status.

- `references/tooling-and-workflows.md`
Use for CLI/Just workflows, LSP/formatter behavior, tree-sitter/Helix integration, and validation command sequences.

- `references/self-hosting-roadmap.md`
Use for the execution checklist to move from Haskell-hosted compilation to Clapse self-hosting with explicit parity/perf gates.

## Fast Validation Commands

```bash
just selfhost-check-wasm
just bench
just wasm-smoke
just life-smoke
deno run -A scripts/native-ir-liveness-size-gate.mjs
deno run -A scripts/check-pass-manifest.mjs
```

## Optimization workstream documentation checks

- Keep `references/pass-manifest.json` and `references/optimization-and-collapse-ir.md`
  pass inventories in exact sync via `deno run -A scripts/check-pass-manifest.mjs`.
- For Workstream C, record residual limits explicitly: current native gate coverage
  is `native-ir-liveness-size-gate`; it validates dead-function liveness
  behavior but does not prove `dead_temp_pruning` or `temp_renumbering`.

## Editing Rules

- Prefer repo-accurate statements over aspirational wording.
- Mark status as `implemented`, `partially implemented`, or `not implemented`.
- Keep examples in current language conventions: `snake_case` values/functions, `PascalCase` data/constructors.
- Add migration notes when proposing ABI-visible changes (especially WASM/JS interop).
