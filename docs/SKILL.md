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
just pre-tag-verify
just browser-compiler-wasm-check
just pass-manifest-check
just semantics-check
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

`release-verify` CI runs the same pre-tag gate via `just pre-tag-verify` before
release bundling and publication.

Or directly:

```bash
CLAPSE_COMPILER_WASM_PATH=artifacts/latest/clapse_compiler.wasm deno run -A scripts/validate-docs.mjs
```

## Ongoing sync rule

- Keep `docs/clapse-language/references/tooling-and-workflows.md` updated when `clapse.json` configuration shape changes.
- When compiler behavior around dispatch, class-method resolution, or rewrite simplification changes, update `docs/clapse-language/references/syntax-reference.md` and `docs/clapse-language/references/optimization-and-collapse-ir.md` together with any `SKILL.md` workflow updates in this folder.
- When nested complement-chain boolean rewrites change, update `docs/clapse-language/references/optimization-and-collapse-ir.md` and `RESEARCH.md` to match the active `ClassLawRule` fixed-point registry behavior: bool-only subject/type, pure-effect guard, and one-way size-reducing orientation.
- Keep class/instance docs aligned with the compiler target syntax: Haskell-style `where` blocks are canonical for new docs and examples.
- Keep prelude operator mapping docs aligned with code (`<$`, `<$>`, `<*>`, `<*`, `*>`, `>>=`, `>>`, `<|>`) and reflect any helper-default changes in the syntax reference.
- Keep Boolean class docs aligned with prelude method surface (`not`, `and`, `or`, `xor`, `implies`, `&&`, `||`), including eager boolean-method signatures (`&&`, `|| : b -> b -> b`) and updated default-instance semantics in `docs/clapse-language/references/syntax-reference.md`.
- Keep prelude abstraction docs aligned with code for:
  `Pair`/`Maybe`/`List`, `reader`/`state` combinators, and the list-backed
  `map`/`set` baseline APIs (`*_by` equality-driven operations).
- Keep declaration-kind naming rules synchronized across docs/examples:
  `data` declarations are Capitalized-only, and lowercase primitive-backed
  declarations use `primitive` (for example `primitive bool = true<1> | false<0>`).
  `data` declarations must include explicit constructors (`data X = X`), and
  bare `data X` is invalid.
- Keep wildcard-demand matching docs aligned with compiler behavior:
  `_` remains a wildcard binder and may reduce argument demand during clause
  matching via deterministic demand ordering; keep this rule synchronized
  between syntax reference, compiler lowering notes, and boolean/prelude examples.
- Keep wildcard-demand regression coverage current in
  `examples/wildcard_demand_behavior_regressions.clapse` and
  `examples/selfhost_behavior_corpus.json` when clause-demand semantics or
  declaration-order tie-break behavior changes.
- Record class-law signature caching where applied: class-law pass signatures are cached as before-cost values once per expression state during each rewrite pass and reused across guard checks; this is a performance optimization with no rewrite-policy or semantics changes.
- Record dispatch-state-key refinement for scheduler redispatch: class-law in-pass keying now uses
  `(root-kind, signature-family)` and improves signature-aware selection correctness/performance.
  Guard predicates, cost policy, `ClassDispatch*` eligibility, and rewrite semantics are unchanged.
- Keep boolean rewrites aligned with the class-law registry contract: constant-negation
  laws (`not true`, `not false`) are admitted through `ClassLawRule` only when the
  bool+pure guard discipline holds.
- Keep nested complement-chain annihilation boolean rewrites aligned with the same
  contract (`ClassLawRule` fixed-point, `ClassDispatchStatic`, bool-only and
  pure-effect guards, one-way size-reducing orientation).
- Keep `scripts/wildcard-demand-check.mjs` aligned with
  `scripts/wasm-behavior-fixture-map.json`: the check now validates the
  wildcard-demand fixture via precomputed wasm, fails on source-hash drift, and
  asserts deterministic repeated evaluation for each scenario export.
- Keep `docs/clapse-language/references/syntax-reference.md` semantic contract
  section aligned with kernel behavior whenever strictness, wildcard-demand, or
  explicit-laziness semantics change.
- Keep parser hardline invariants synchronized between kernel and docs:
  case scrutinee/arm arity mismatch is a hard parse error, `newtype` accepts
  exactly one constructor + one field (no `|` alternatives), and class fundep
  tails reject trailing commas.
- Record memory-model pass changes in `docs/clapse-language/references/optimization-and-collapse-ir.md` when kernel collapse pipeline stages change (escape/lifetime annotations, ownership/COW rewrite ordering), and mirror behavior notes in `docs/SKILL.md` with concise pass-level comments.
- Record allocation/reuse/COW scope contract updates in both
  `docs/clapse-language/references/optimization-and-collapse-ir.md` and
  `docs/clapse-language/references/wasm-runtime-and-interop.md` when kernel rewrite policy changes.
- Keep `docs/clapse-language/references/optimization-and-collapse-ir.md` and
  `docs/clapse-language/references/wasm-runtime-and-interop.md` aligned on
  memory-model semantics whenever allocation, reclaim, alias/freeze, or scope/lifetime behavior changes.
- Keep `docs/clapse-language/references/pass-manifest.json` synchronized with
  `docs/clapse-language/references/optimization-and-collapse-ir.md`; status
  labels are machine-checked by `scripts/check-pass-manifest.mjs`.
- Keep `docs/clapse-language/references/optimization-and-collapse-ir.md` and `RESEARCH.md` synchronized when
  registry-driven boolean rewrites change (fixed-point mode, strict purity/type guards, or
  size-reducing orientation).
- Keep `RESEARCH.md` current as the optimization-theory ledger: every new
  optimization family must include invariant statements, typed/effect guard
  conditions, and at least one primary research citation with direct quote.
- Keep CLI packaging docs aligned with runtime behavior: `just clapse-bin` and
  `just install` bundle `artifacts/latest/clapse_compiler.wasm` (and
  `artifacts/latest/clapse_compiler.d.ts` when present) into
  `artifacts/bin/clapse` when available, with `CLAPSE_COMPILER_WASM_PATH` as an
  override.
- Keep release bundle docs aligned with `just release-candidate` and `.github`
  release verification outputs when release artifact membership changes (for
  example when adding `prelude.clapse` or `clapse_compiler.d.ts`).
- Keep release bundling behavior aligned with implementation: release packaging
  now copies the validated seed compiler artifact
  (`CLAPSE_COMPILER_WASM_PATH`/`artifacts/latest`) rather than performing
  kernel self-recompile during bundle creation.
- Keep release metadata/checksum docs aligned with release artifact shape changes:
  `scripts/release-metadata.mjs` now accepts repeated `--cli-bin` and writes a
  `artifacts.cli_binaries` array in the manifest plus one checksum entry per
  binary, while retaining `artifacts.cli_binary` for single/legacy compatibility.
- Keep browser-runnable compiler contract aligned between `just release-candidate`,
  `.github/workflows/release-verify.yml`, and `scripts/check-browser-compiler-wasm.mjs`:
  released `clapse_compiler.wasm` must be native ABI (`clapse_run` + memory),
  non-tiny, and pass compile-smoke (`main` export) checks; release bundles must
  publish `clapse_compiler.d.ts` when produced.
- Keep formatter behavior aligned with runtime behavior: canonical formatter
  normalization (string/comment-preserving whitespace collapse, 100-character
  max-width wrapping at ` => `, ` = `, ` -> `, ` >>= `, ` >> `, ` && `,
  ` || `, and monadic chain normalization for `>>=`/`>>`) now runs in the
  Clapse kernel and is returned by the `format` command response already
  normalized.
- Keep formatter resilience docs aligned with runtime behavior:
  by default, CLI/LSP formatter stack overflows fall back to returning input
  unchanged with a stderr hint (including wasm stack offsets + map command).
  Set `CLAPSE_FORMAT_STACK_IDENTITY_FALLBACK=0` to surface stack overflows as
  hard formatter errors.
- Add formatter non-progress recursion guard telemetry in the kernel: recursive
  chain walkers now emit `ok:false` with a function-specific message when the
  formatter recursion budget is exhausted, helping isolate exact recursion loops.
- Keep debug-trace docs aligned with runtime behavior:
  `CLAPSE_DEBUG_STACK=1` prints full JS/Wasm stack traces from CLI errors, and
  `scripts/wasm-stack-map.mjs` maps wasm stack offsets to function indices for
  native compiler artifacts by parsing Deno-style `wasm://...:line:offset` frames.
  Formatter stack-overflow errors now emit offsets and a ready-to-run
  `wasm-stack-map` command directly in CLI output for quick triage.
  Function-name mapping prefers the standard WASM `name` section and falls back
  to `clapse.funcmap` only when missing; output includes
  `function_name_source` provenance (`name-section`, `clapse.funcmap`, or
  `unresolved`).
- Add `CLAPSE_DEBUG_FUNC_MAP=1` (or set `compile_mode` to `funcmap`,
  `emit-funcmap`, `debug-funcmap`, or `debug`) on a `compile` request to
  append a custom `clapse.funcmap` section in compiler output. This section maps
  function indices to function names before falling back to `func_<index>`.
- This section is emitted for debug-mode outputs; `name`-section names (if
  present) still take precedence in downstream stack-map lookup.
  `run-clapse-compiler-wasm.mjs` now applies debug-mode function map injection
  to emitted native compile artifacts so function-name lookup remains available
  for stack-mapping.
- Keep compile fallback docs aligned with runtime behavior:
  JS compile accepts host-bridge compile responses when they include a valid
  `ok:true` + non-empty `wasm_base64` payload, and writes the emitted wasm/dts
  artifacts from that response.
- Keep selfhost artifact docs aligned with runtime behavior:
  JS no longer patches placeholder selfhost payloads. If the kernel returns a
  placeholder/incomplete `selfhost-artifacts` response, the command now fails
  explicitly.
- Smoke expectations are native-only:
  `scripts/bootstrap-phase9-kernel-smoke.mjs` and
  `scripts/fib-memo-plugin-smoke.mjs` now require successful native compile
  output and no longer accept stub compile artifacts as pass conditions.
- JS/TS is the host I/O boundary and does not own language semantics:
  it marshals CLI/path/env/file I/O and invokes kernel requests; compile,
  formatter, and LSP semantic behavior remain in the Clapse kernel.
- Canonical kernel module map (current):
  - `bootstrap_phase9_compiler_kernel`: command routing and bridge orchestration.
  - `compiler.json_response`: shared JSON request/response contract.
  - `compiler.formatter`: formatter behavior.
  - `compiler.lsp_kernel`: LSP request handling and response shaping.
- Host bridge compile recursion is still guarded by
  `CLAPSE_HOST_COMPILE_DEPTH`; compile calls that recurse too deeply fail with
  an explicit recursion error. Path resolution remains
  `CLAPSE_COMPILER_WASM_PATH`, then `artifacts/latest/clapse_compiler.wasm`,
  then `out/clapse_compiler.wasm`.

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

- `clapse_run` now uses the staged request from `collapse_pipeline_run` as a passthrough value, derives `OwnershipRewriteMode` through a dedicated helper (`collapse_pipeline_slice_write_policy`), and threads that mode through request-scoped response builders.
- `slice_set_u8` rewrite now uses explicit linear writes on the copied descriptor in the COW path (`slice_set_u8_cow`) so copy-on-write remains descriptor-local and does not accidentally recurse into COW policy.
- `apply_class_law_rewrites` now applies boolean class-law rewrites through a bounded structural fixed-point driver (4 iterations or until stabilization) for static dispatch, with a strict class-level cost policy: boolean rewrites must strictly decrease `class_method_expr_cost`; compose/map remain bounded (`+0`, with `+1` only for map-fusion candidates).
- Boolean associative-idempotence chain reductions now execute in the same class-law fixed-point registry (`ClassLawRule`) and are constrained by bool-typed, pure-effect guards plus non-increasing-cost rewrite orientation.
- Chain reductions are strictly one-way and one-step: `x && (x && y) -> x && y`, `x && (y && x) -> x && y`, `(x && y) && x -> x && y`, `(y && x) && x -> y && x` plus the four analogous `||` variants.
- Compose associativity canonicalization is one-way in the registry: `compose f (compose g h) -> compose (compose f g) h` under compose-shape, pure, and compatibility guards; there is no inverse rule to avoid oscillation.
- Boolean rewrites are now registry-driven consensus fixed-point rewrites in `ClassLawRule` (not a dedicated bool-collapse helper path).
- The bool simplification set now includes idempotence (`x && x`, `x || x`) and remains gated by bool-type and pure-effect checks in class-law dispatch.
- Boolean absorption and complement families are also in the same consensus registry-driven fixed-point set (for example `x && (x || y) -> x`, `x || (x && y) -> x`, `x && not x -> false`, `x || not x -> true`) with the same deterministic rule ordering and guard discipline.
- One-way consensus rewrites were added as dedicated registry laws with strict boolean+pure guards and exact structural matches: `x && (not x || y) -> x && y`, `x && (y || not x) -> x && y`, `x || (not x && y) -> x || y`, `x || (y && not x) -> x || y`, plus the outer-operand-swapped equivalents.
- These complement/absorption rewrites are applied only through `ClassLawRule` registry dispatch; existing guard checks (`class_law_rule_guard`) are still enforced.
- Class-law fixed-point rewriting is structural-cost guarded (`class_method_expr_cost`) with a bounded growth budget: default zero-growth, with a `+1` budget only when map-fusion candidates are present.
- Class-law rewriting now uses an explicit rule registry (`ClassLawRule`) with deterministic ordering, and adds lightweight local expression metadata (`ClassMethodExprType`, `ClassMethodExprEffect`) plus per-rule guarded dispatch checks before rewriting (`class_law_rule_guard`): compose laws require a pure `CCompose` shape with non-boolean compatible inputs, and map laws require a pure `CMap` shape with non-boolean compatible inputs.

### plugin precompile contract

- LSP reads plugin directories from `clapse.json` and passes discovered plugin
  `.wasm` artifacts to the compiler in `plugin_wasm_paths`.
- `clapse.json` may include `"plugins"` as an array of directories.
- `scripts/run-clapse-compiler-wasm.mjs` now walks upward from the input directory (or cwd), compiles discovered plugin `.clapse` files to `.wasm`, and sends their paths as `plugin_wasm_paths` in the compile request.
- Canonical plugin + memo fixture smoke path: `scripts/fib-memo-plugin-smoke.mjs` with
  `examples/plugins/memo_fib_plugin.clapse` and `examples/fib_memo.clapse`.
- Factoring rewrites are now represented in the `ClassLawRule` registry with bool+pure guards and size-reducing orientation (`x && (x && y) -> x && y`, `x || (x || y) -> x || y`) under static-mode fixed-point scheduling.

### Root-shape class-law selection (deterministic)

Root-shape class-law dispatch now uses deterministic rule grouping by expression root (`CCompose`, `CMap`, boolean root forms) before fixed-point application. After each successful rewrite, `rewrite_class_law_rules_once_list` re-dispatches immediately to the new root rule set before continuing within the same pass; this does not alter any cost/guard policy, strict-decrease check, or static/dynamic dispatch gates.
- Root-kind dispatch is now table-driven: precomputed subset lookups for root-kinds (`compose/map/bool`) provide constant-time dispatch and avoid per-step allocation churn while preserving all rewrite guards, policies, semantics, and dispatcher gates.
- Dispatch now also applies root-kind + signature-family pruning before member checks: Not/And/Or roots route only on `ClassMethodExprTypeBool` + pure effect, compose roots only on compose-pure, map roots only on functor-pure, while Bool/Var remain empty and `Other` is now empty.
- `Other` is empty by construction because all current class-law families are root-specific (`CCompose`, `CMap`, boolean roots); this change is a scheduling/refinement swap only and does not alter rewrite semantics.
