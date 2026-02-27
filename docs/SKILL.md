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
- Keep Boolean class docs aligned with prelude method surface (`not`, `and`, `or`, `xor`, `implies`, `&&`, `||`), including eager boolean-method signatures (`&&`, `|| : b -> b -> b`) and updated default-instance semantics in `docs/clapse-language/references/syntax-reference.md`.
- Keep prelude abstraction docs aligned with code for:
  `Pair`/`Maybe`/`List`, `reader`/`state` combinators, and the list-backed
  `map`/`set` baseline APIs (`*_by` equality-driven operations).
- Keep declaration-kind naming rules synchronized across docs/examples:
  `data` declarations are Capitalized-only, and lowercase primitive-backed
  declarations use `primitive` (for example `primitive bool = true<1> | false<0>`).
- Keep wildcard-demand matching docs aligned with compiler behavior:
  `_` remains a wildcard binder and may reduce argument demand during clause
  matching via deterministic demand ordering; keep this rule synchronized
  between syntax reference, compiler lowering notes, and boolean/prelude examples.
- Keep wildcard-demand regression coverage current in
  `examples/wildcard_demand_behavior_regressions.clapse` and
  `examples/selfhost_behavior_corpus.json` when clause-demand semantics or
  declaration-order tie-break behavior changes.
- Record memory-model pass changes in `docs/clapse-language/references/optimization-and-collapse-ir.md` when kernel collapse pipeline stages change (escape/lifetime annotations, ownership/COW rewrite ordering), and mirror behavior notes in `docs/SKILL.md` with concise pass-level comments.
- Record allocation/reuse/COW scope contract updates in both
  `docs/clapse-language/references/optimization-and-collapse-ir.md` and
  `docs/clapse-language/references/wasm-runtime-and-interop.md` when kernel rewrite policy changes.
- Keep `docs/clapse-language/references/optimization-and-collapse-ir.md` and
  `docs/clapse-language/references/wasm-runtime-and-interop.md` aligned on
  memory-model semantics whenever allocation, reclaim, alias/freeze, or scope/lifetime behavior changes.
- Keep CLI packaging docs aligned with runtime behavior: `just clapse-bin` and
  `just install` bundle `artifacts/latest/clapse_compiler.wasm` into
  `artifacts/bin/clapse` when available, with `CLAPSE_COMPILER_WASM_PATH` as an
  override.
- Keep formatter behavior aligned with runtime behavior: canonical formatter
  normalization (string/comment-preserving whitespace collapse, 100-character
  max-width wrapping at ` => `, ` = `, ` -> `, ` >>= `, ` >> `, ` && `,
  ` || `, and monadic chain normalization for `>>=`/`>>`) now runs in the
  Clapse kernel and is returned by the `format` command response already
  normalized.
- Keep formatter resilience docs aligned with runtime behavior: CLI/LSP surface
  formatter errors/overflows directly (including stack-size issues), and return
  the kernel `formatted` result only from successful `format` responses.
- Keep debug-trace docs aligned with runtime behavior:
  `CLAPSE_DEBUG_STACK=1` prints full JS/Wasm stack traces from CLI errors, and
  `scripts/wasm-stack-map.mjs` maps wasm stack offsets to function indices for
  native compiler artifacts. Function-name mapping prefers the standard WASM
  `name` section and falls back to `clapse.funcmap` only when missing; output
  includes `function_name_source` provenance (`name-section`, `clapse.funcmap`,
  or `unresolved`).
- Add `CLAPSE_DEBUG_FUNC_MAP=1` (or set `compile_mode` to `funcmap`,
  `emit-funcmap`, `debug-funcmap`, or `debug`) on a `compile` request to
  append a custom `clapse.funcmap` section in compiler output. This section maps
  function indices to function names before falling back to `func_<index>`.
- Caveat: this section is emitted only on the host compile bridge path and is
  intentionally omitted for non-debug output; `name`-section names (if present)
  still take precedence in downstream stack-map lookup.
- Keep compile fallback docs aligned with runtime behavior:
  `CLAPSE_ALLOW_KERNEL_FIXED_POINT` controls kernel fixed-point fallback and
  `CLAPSE_REQUIRE_NATIVE_COMPILE=1` disables compile fallback strategies for
  strict verification/release paths.
- JS/TS is the host I/O boundary and does not own language semantics:
  it marshals CLI/path/env/file I/O and invokes kernel requests; compile,
  formatter, and LSP semantic behavior remain in the Clapse kernel.
- Canonical kernel module map (current):
  - `bootstrap_phase9_compiler_kernel`: command routing and bridge orchestration.
  - `compiler.json_response`: shared JSON request/response contract.
  - `compiler.formatter`: formatter behavior.
  - `compiler.lsp_kernel`: LSP request handling and response shaping.
- Host bridge mode (`clapse_run` command `"compile"`) now delegates to a native
  compiler artifact (`CLAPSE_COMPILER_WASM_PATH` or
  `artifacts/latest/clapse_compiler.wasm`) via a sync subprocess and returns a
  JSON response containing `ok`, `wasm_base64`, `exports`, and `dts`. Bridge
  recursion is guarded via `CLAPSE_HOST_COMPILE_DEPTH` so nested host compile
  re-entry fails explicitly instead of overflowing the stack.
  `compile` now routes through `host.clapse` via `clapse_host_run` in the kernel
  and no longer emits `compile_stub_success_response` placeholders.

## LSP migration status

- In this first step, `clapse`-core now owns semantic extraction for:
  - declaration symbol index (`lsp-symbol-index`)
  - hover payload lookup (`lsp-hover`)
  - definition lookup (`lsp-definition`)
- JS now sends `lsp-symbol-index` on open/change for `coreSymbolIndex`, and
  `textDocument/hover` / `textDocument/definition` attempt core lookups first with
  JSON responses marked `backend: "clapse"`, then fall back to JS local logic.
- JS-side LSP features still implemented in transport for this release: rename,
  references, document symbols, and code actions.
- Fixture checkpoint: `definition_rename_and_code_action` in
  `examples/lsp_wasm_fixtures.json` now asserts core backend usage for hover and
  definition when `CLAPSE_EXPECT_CORE_LSP_BACKENDS=1` is set.
- Next steps: add core entrypoints for `references`, `documentSymbol`, and code
  action payload generation, then remove remaining local fallbacks after parity tests
  pass.

## Memory model checkpoint

- `clapse_run` now executes `collapse_pipeline_run` once per request and threads the derived `OwnershipRewriteMode` through request-scoped response builders and `slice_set_u8` rewrite helpers.
- `slice_set_u8` rewrite now uses explicit linear writes on the copied descriptor in the COW path (`slice_set_u8_cow`) so copy-on-write remains descriptor-local and does not accidentally recurse into COW policy.

### plugin precompile contract

- LSP reads plugin directories from `clapse.json` and passes discovered plugin
  `.wasm` artifacts to the compiler in `plugin_wasm_paths`.
- `clapse.json` may include `"plugins"` as an array of directories.
- `scripts/run-clapse-compiler-wasm.mjs` now walks upward from the input directory (or cwd), compiles discovered plugin `.clapse` files to `.wasm`, and sends their paths as `plugin_wasm_paths` in the compile request.
- Canonical plugin + memo fixture smoke path: `scripts/fib-memo-plugin-smoke.mjs` with
  `examples/plugins/memo_fib_plugin.clapse` and `examples/fib_memo.clapse`.
