# Optimization and Collapse IR

## Pipeline Shape

1. Parse command/tag from request slice.
2. Run staged request passthrough (`collapse_pipeline_run`) over request bytes.
3. Derive ownership policy per request
   (`collapse_pipeline_slice_ownership_mode`).
4. Dispatch to per-command response builders with both staged request and
   request-derived policy.
5. Return command response:
   - compile/format/selfhost/lsp still flow through kernel handlers
   - compile with ready input returns kernel-local compile response after kernel
     staging (no source-side host bridge delegation)

### Unified wasm+IR compile-debug command

`deno run -A scripts/run-clapse-compiler-wasm.mjs compile-debug <input.clapse> [output.wasm] [artifacts-dir]`

This runs a unified one-step compile path and emits the same artifacts:

- `<artifacts-dir>/lowered_ir.txt`
- `<artifacts-dir>/collapsed_ir.txt`

Current kernel-native compile behavior:

- `compile_native_response` is source-local in `compiler.native_compile` and
  emits kernel-native JSON payloads for `CompileRequestReady`.
- Boundary normalization remains available during migration; set
  `CLAPSE_KERNEL_ABI_DISABLE_NORMALIZATION=1` to enforce producer-only strict
  behavior in native compile gates.
- raw producer artifacts in the legacy `artifacts/latest/clapse_compiler.wasm`
  chain can still be synthetic; strict-native gates reject synthetic markers
  (`kernel:compile:*`, `seed-stage*`) and require request-source inclusion. Use
  `just bootstrap-native-producer-seed` to mint a wasm-native producer seed
  artifact that satisfies these contracts without JS fallback.
- `selfhost-artifacts` dispatches to `selfhost_ok_response` in kernel routing
  and returns required debug artifact keys directly.
- command dispatch runs after staging; the request is **passthrough** through
  the kernel pipeline and is not rewritten before handler selection.
- `collapse_pipeline` and ownership policy derivation are both request-scoped
  and deterministic.
- ownership mode is derived in a separate pass and threaded into each response
  builder and rewrite helper.
- CI gate `just native-ir-liveness-size-gate` compares baseline vs
  entrypoint-pruned compile requests and requires strict emitted wasm shrink
  (`pruned_bytes < baseline_bytes`) while asserting dead-marker removal in
  pruned IR artifacts.

## Record Lowering Status and Native Plan

Current shipped behavior:

- Closed-record syntax is now consumed directly by native kernel compile
  handling (runtime compile paths no longer pre-lower records in JS).
- Parity gating uses kernel-native compile smoke + LSP fixtures in
  `pre-tag-verify`.

Next-stage native work remains:

1. Make record lowering/typing pass boundaries explicit in kernel IR docs.
2. Add record-specific pass-manifest entries when those passes are surfaced as
   named optimization/lowering stages.
3. Continue fixture expansion for ambiguous/projection/update edge cases under
   kernel-owned diagnostics.

## Pass Status Inventory (normative)

This inventory is canonical and machine-checked against
`docs/clapse-language/references/pass-manifest.json`.

- [pass:collapse_escape_lifetime_annotation] status: implemented
- [pass:collapse_slice_ownership_rewrite] status: implemented
- [pass:slice_set_u8_linear_reuse] status: implemented
- [pass:slice_set_u8_copy_on_write] status: implemented
- [pass:class_law_bool_simplification] status: partially implemented
- [pass:class_law_collection_map_identity] status: partially implemented
- [pass:class_law_collection_map_fusion] status: partially implemented
- [pass:class_law_strictness_gated_fixedpoint] status: implemented
- [pass:currying_normalization] status: not implemented
- [pass:closure_apply_collapse] status: not implemented
- [pass:constant_argument_specialization] status: not implemented
- [pass:wrapper_inlining] status: not implemented
- [pass:dead_function_pruning] status: partially implemented
- [pass:dead_temp_pruning] status: not implemented
- [pass:temp_renumbering] status: not implemented
- [pass:self_tail_call_normalization] status: partially implemented

## Collapsed IR Concepts

Atoms:

- constants (`AConstI32`, `AConstString`)
- locals (`ALocal`)
- temporaries (`ATemp`)
- globals placeholder (`AGlobal`, currently unsupported in wasm backend)

Values:

- direct call (`VCallDirect`)
- curry/direct partial (`VCurryDirect`)
- closure create/call (`VClosure`, `VCallClosure`, `VApply`)
- self tail call marker (`VSelfTailCall`)

## Currently Active Rewrite Patterns

- post-emit wasm tail-call opcode rewrite:
  - tail-position `call`/`call_indirect` suffixes are rewritten to
    `return_call`/`return_call_indirect`
  - explicit `call`/`call_indirect` followed by `return` is rewritten to
    `return_call`/`return_call_indirect`
  - runtime toggle: `CLAPSE_EMIT_WASM_TAIL_CALLS=0` disables rewrite

## Memory Model Staging (Current)

Collapse now carries explicit memory-model stage ordering:

1. `MemoryPassEscapeLifetimeAnnotation`
2. `MemoryPassSliceOwnershipRewrite`

Current kernel wiring keeps both stages semantics-preserving over request
payloads. Stage 2 no longer performs direct structural rewriting in that slot;
it exists as the ownership-policy derivation stage for `slice_set_u8` chains.

Request-policy pipeline behavior is deterministic and pure:

- Stage 1 scans request payload text and materializes `SliceSetU8WriteChainHint`
  (escape marker + alias hint + explicit-chain-presence marker).
- Stage 2 converts staged hints into an `OwnershipRewriteMode` policy
  (`OwnershipRewriteLinear` or `OwnershipRewriteCopyOnWrite`) and passes request
  payload unchanged.
- `clapse_run` invokes `collapse_pipeline_slice_ownership_mode` explicitly so
  policy derivation is separate from `collapse_pipeline_run`.

### Allocation / Deallocation and Ownership Semantics

Allocation and reclaim in the optimizer are modeled as ownership metadata
transforms, with no change to runtime allocator behavior in this stage:

- `slice_set_u8` owns a target descriptor and byte payload only in IR reasoning.
- The rewrite phase cannot emit frees; reclaim only appears as explicit
  `region_reset` and control of where new temporaries replace old descriptors.
- `slice_new_u8` introduces fresh allocation IR; `slice_set_u8` may either:
  - reuse the same descriptor when analysis proves a single mutable path, or
  - allocate a replacement descriptor and copy bytes when alias risk exists.
- No pass currently rewrites non-slice object allocation patterns to compact
  pools; struct/closure allocation remains structural and stable under existing
  passes.

### Escape / Lifetime / Alias analysis contract (implemented)

The analysis is staged as:

- `EscapeLifetimeAnnotation`:
  - `EscapeLifetimeLocal` means ownership is treated as local to the current
    write-chain scope.
  - `EscapeLifetimeEscaped` means the value can be observed outside that scope.
  - `EscapeLifetimeUnknown` is a conservative fallback when tracking is
    incomplete.
- `OwnershipRewriteMode`:
  - `OwnershipRewriteLinear` is the in-place reuse result.
  - `OwnershipRewriteCopyOnWrite` is the defensive copy result.
- `collapse_pipeline_escape_lifetime` now runs a deterministic metadata analysis
  over request text to extract request-chain hints for later ownership policy
  selection.
- `collapse_pipeline_slice_ownership_rewrite` consumes that policy through
  `slice_set_u8_context_mode_with_policy` (via `slice_set_u8_rewrite`) to choose
  in-place reuse vs. COW paths. The first pass does no rewriting by itself; it
  only preserves deterministic hints for the ownership pass.
- The current request-chain hinting uses `slice_set_u8` presence plus explicit
  escape (`"output"`/`"return"`) and alias hints (`.asSlice`) to seed
  `SliceSetU8WriteChainHint` (`EscapeLifetime*`, alias class, has-chain flag).

Current rule model used in the kernel rewrite path:

- `slice_set_u8_base_context` seeds analysis from requested policy
  (`OwnershipRewriteMode`) and index validity.
- `slice_set_u8_context_with_alias` elevates to shared/escaped when target
  aliasing is suspected.
- `slice_set_u8_context_with_freeze` elevates to copy-on-write when freeze-like
  usage is inferred.
- In stage-2 policy selection, uncertainty remains conservative:
  unknown/escaped/sized-out scopes default to `OwnershipRewriteCopyOnWrite`,
  while `EscapeLifetimeLocal` + `SliceWriteAliasSingleUse` + non-escaped local
  chains can remain linear.
- `slice_set_u8_rewrite` resolves to `OwnershipRewriteLinear` only when:
  - escape is local,
  - alias class is single-use, and
  - no freeze/OOB signal is present.
- All other combinations resolve to `OwnershipRewriteCopyOnWrite`.

### Alias / freeze / COW rule (explicit)

- A rewrite is linear when all of the following are true:
  1. Target descriptor flows from a single write chain.
  2. Target does not escape (`EscapeLifetimeLocal`).
  3. No aliasing/freeze risk is inferred by chain context.
- A rewrite is copy-on-write when any condition above is false:
  1. Inferred escape is `EscapeLifetimeEscaped`/`Unknown`.
  2. Inferred alias class is shared/unknown.
  3. Freeze signal is present, or the operation is out-of-bounds with respect to
     the inferred chain context.
- In COW mode, optimizer behavior is: `slice_set_u8_cow` = allocate fresh
  slice + full-copy + local write.
- In both modes, no runtime allocator deallocation is inserted by these passes;
  ownership intent is expressed as descriptor replacement and in-place
  descriptor reuse.
- Freeze is currently a conservative classification in kernel analysis, not a
  literal IR flag: freeze-like writes keep the operation on COW paths.

### Root-shape class-law dispatch selection (deterministic)

Rule dispatch now first classifies the expression by root shape (for example
`CCompose`, `CMap`, or boolean composition/operator shapes), then checks cached
family signatures against candidate `ClassLawRule` buckets before probing
members within the bucket.

The scheduler now uses precomputed deterministic subset tables keyed by the
dispatch-state-key: `(root-kind, signature-family)` where
`signature-family = root-kind + signature`. This gives constant-time,
signature-aware rule-group selection and reduced dispatch allocations while
preserving candidate ordering within each bucket.

This gates impossible families early when the expression signature cannot match
a rule family; candidates in excluded buckets are never checked, while
`class_law_rule_guard` and policy gates remain unchanged.

After a successful rewrite, the scheduler performs an immediate redispatch using
the current dispatch-state-key before the next fixed-point step so subsequent
candidates are selected against the updated expression root/signature. This is a
scheduler correction/performance optimization only and preserves deterministic
ordering semantics.

The precomputed tables are deterministic, policy-agnostic, and only affect
lookup machinery; they do not alter dispatch eligibility, `class_law_rule_guard`
outcomes, static/dynamic class dispatch gating, cost policy, or rewrite
semantics.

Class-law guard evaluation now caches before-cost signatures once per expression
state during each rewrite pass and reuses them across candidate rule checks,
reducing repeated signature work during fixed-point scheduling. This
optimization changes only lookup/evaluation cost; rewrite semantics and
class-law policies are unchanged.

Scheduler execution also removes redundant internal work:

- once `class_law_rule_guard_with_signature` succeeds for a
  `(rule, expr, signature)` step, the candidate rewrite now applies directly
  without a second guard pass on the same tuple.
- post-rewrite redispatch no longer branches on dispatch-key equality when both
  branches select the same next rule bucket.

These are execution-cost optimizations only; dispatch eligibility,
strictness/cost policy, fixed-point bounds, and rewrite results are unchanged.

State dispatch still uses root-kind first, then signature-family gating, then
full guards; no `ClassDispatch*` eligibility or rewrite-policy semantics
changed. `Other` is now an empty fallback bucket, and this is safe because all
active laws are root-specific (`CCompose`, `CMap`, and boolean roots), so
`Other` candidates are unreachable today. Rewrite semantics are unchanged.

And/Or sub-dispatch adds a conservative-superset child-shape gate within boolean
roots:

- before checking per-rule candidates, `And`/`Or` expressions are grouped by
  child-shape family.
- only buckets whose child-shape family could match are attempted; all remaining
  candidates still pass unchanged `class_law_rule_guard` and dispatch-mode
  checks.
- `And`/`Or` child-shape checks now reuse one cached `(has_and, has_or)` pair
  per child root-kind instead of repeating root-kind case checks in each branch,
  and this changes only execution cost.
- this is lookup pruning only: strictness/cost policy, dispatch-mode,
  fixed-point bounds, and rewrite outcomes are unchanged.

This optimization does **not** change class-law policy behavior:

- `class_law_rule_guard` checks remain unchanged (shape/type/purity/effect
  gates).
- `ClassDispatchStatic`/`ClassDispatchDynamic` eligibility is unchanged.
- strict-decrease mode now keys off the dispatch-state/signature-family table:
  boolean rules still require strict cost decrease, while compose/map rules stay
  on budget growth policy (`0`, `+1` only for map-fusion candidates).
- fixed-point control remains bounded by the existing cost policy and
  strict-decrease governance (`class_method_expr_cost`).

### Scope and lifetime behavior (function-local vs escaping)

- Function-local write-chain scope for this kernel pass means:
  - request-derived hints provide explicit initial state; local context checks
    then refine ownership choice.
  - if no observed cross-boundary consumer is detected in the analyzed chain and
    alias/freeze checks are clear, then write semantics stay in
    `OwnershipRewriteLinear`.
- Escaping scope is inferred conservatively from textual indicators and/or
  explicit `SliceSetU8WriteChainHint` context:
  - any `EscapeLifetimeEscaped` marker,
  - any alias hint,
  - or uncertain/unknown alias/equivalence information routes the rewrite to
    COW.

## Verifier Focus

- Contiguous temp numbering
- In-scope use of locals/temps
- No dead temps
- Currying arity correctness for known functions
- Tail-call argument count correctness

## Benchmark Expectations

- Compare abstraction-heavy code against hand-written baseline
- Require parity first
- Check runtime and static-cost bars
- Keep optimization-bar checks explicit in benchmark output

## Proposal Discipline

- Add new rewrites/passes only with verifier-safe invariants and tests.
- Prefer small, composable passes over large opaque rewrites.

## Class-Law Rewrite Stage (Static vs Dynamic)

The class rewrite stage sits before lowering and chooses method implementations
from class metadata:

- `resolve_class_method` selects between `static_method` and `dynamic_method`
  with the mode witness.
- `apply_class_law_rewrites` applies bool-law rewrites only when mode is
  `ClassDispatchStatic`.
- `rewrite_class_law_expr_fixedpoint_stages` applies a bounded fixed-point pass
  over bool laws (4 iterations or until no change), which is the same kernel
  policy used by `derive_law_expr*` helpers.
- this fixed-point pass is registry-driven consensus rewriting: all boolean laws
  come from the shared `ClassLawRule` registry and are reapplied until
  saturation under deterministic rule order. constant-negation laws
  (`not true -> false`, `not false -> true`) are included in this same path and
  share the same fixed-point scheduling.
- `class_law_rule_guard` additionally requires expression-shape, purity, and
  static type compatibility preconditions before each law fires:
  - composition rules only match on `CCompose` expressions, require pure method
    expressions, and require non-boolean compose-compatible inputs.
  - functor/map rules only match on `CMap` expressions, require pure method
    expressions, and require non-boolean map-compatible inputs.
  - boolean simplification rules additionally require boolean-type metadata and
    pure-effect metadata for both subject and argument expressions.
- fixed-point iteration is structural-cost guarded (`class_method_expr_cost`)
  under a rule-level cost policy in the `ClassLawRule` registry:
  - boolean class-law rewrites must apply only when the rewrite strictly
    decreases cost,
  - compose/map rewrites retain bounded growth governance with default `+0` and
    a `+1` exception only for map-fusion candidates,
  - if a rewrite step exceeds its allowed budget, iteration halts and keeps the
    previous expression.
- `ClassDispatchDynamic` keeps the law method unchanged.

Boolean constant-negation rewrites are class-law registry rewrites subject to
bool+pure guard discipline.

### Compose-associativity canonicalization policy

- Compose associativity is treated as one-way canonicalization in the class-law
  registry:
  - `compose f (compose g h) -> compose (compose f g) h` for canonical
    association.
  - Registry match requires compose-shape, pure `ClassMethodExprEffect`, and
    compose-compatible `ClassMethodExprType`.
  - No reverse rewrite rule is registered, so `compose (compose f g) h` is not
    expanded back, preventing rewrite oscillation.

Bool fold set is now implemented through the class-law registry fixed-point
rewriter (`ClassLawRule`), not through a standalone bool-collapse helper:

- `not (not x) -> x` (double negation)
- `true && x -> x` (identity)
- `false && x -> false` (annihilation)
- `true || x -> true` (annihilation)
- `false || x -> x` (identity)
- `x && x -> x` (idempotence)
- `x || x -> x` (idempotence)
- absorption:
- `x && (x || y) -> x` (absorption left)
- `x && (y || x) -> x` (absorption left, commuted inner operand)
- `x || (x && y) -> x` (absorption right)
- `x || (y && x) -> x` (absorption right, commuted inner operand)
- complement:
- `x && not x -> false`
- `not x && x -> false`
- `x || not x -> true`
- `not x || x -> true`
- `not true -> false`
- `not false -> true`
- nested-complement chain annihilation (class-law fixed-point):
- `x && (not x || y) -> x && y`
- `x && (y || not x) -> x && y`
- `x || (not x && y) -> x || y`
- `x || (y && not x) -> x || y`
- outer-operand-swapped variants are included when shape-match is stable

## Registry-driven associative-idempotence chain reductions

- Factoring is implemented through the same `ClassLawRule` registry used by
  other class-law rewrites.
- `class_law_rule_guard` applies boolean-type and pure-effect side conditions
  before these rules fire.
- Rewrite orientation is fixed to size-reducing shape, e.g.
  `x && (x && y) -> x && y`, `x || (x || y) -> x || y`.
- `x && (x && y) -> x && y`
- `x || (x || y) -> x || y`
- `x && (y && x) -> x && y`
- `x || (y || x) -> x || y`

These rules run in the same static `ClassLawRule` fixed-point driver as other
boolean laws and are gated by:

- `ClassDispatchStatic`
- `ClassMethodExprType` = boolean on all participating expressions
- pure/effect discipline via `class_law_rule_guard`
- strictness- and shape-compatible subjects

Boolean rewrites now use strict-cost governance in the fixed-point policy
(`class_method_expr_cost` must strictly decrease), while compose/map rewrites
keep bounded growth policy (`0` growth, `+1` only for map-fusion candidates) to
preserve termination and deterministic convergence.

Boolean simplification, absorption, complement, and nested-complement-chain
annihilation now run as the same consensus rewrite cluster:

- all laws share the same deterministic registry order and fixed-point
  convergence gate,
- all law applications pass through existing `class_law_rule_guard`
  shape/purity/type/effect checks,
- `ClassDispatchStatic` remains the only enabled dispatch mode for these
  rewrites.
- nested-complement-chain rules are one-way size-reducing (or cost-neutral under
  existing map-fusion exceptions), and no inverse rewrites are registered to
  prevent oscillation.

Collection law set currently implemented in class-law scheduler:

- `compose id f` -> `f`
- `compose f id` -> `f`
- `map id xs` -> `xs`
- `map f (map g xs)` -> `map (compose f g) xs`

Current activation status:

- These folds are implemented through a bounded fixed-point rewrite driver over
  the shared `ClassLawRule` registry, with deterministic rule ordering over a
  `ClassLawRule` registry and monotonic-cost guard.
- They are not yet wired as a mandatory compile-stage rewrite in the active
  `compile_response` route (which currently uses host compile passthrough for
  ready requests).

### Strict law-driven optimization extension (implemented)

`[pass:class_law_strictness_gated_fixedpoint]` is implemented with
dispatch-state/signature-family strictness derivation:

- static dispatch only (`ClassDispatchStatic`);
- existing `class_law_rule_guard` checks (shape/purity/signature compatibility)
  still apply;
- strictness-mode now comes from the dispatch-state/signature-family bucket:
  - boolean rewrites require strict cost decrease;
  - compose/map rewrites remain budgeted (`0`, `+1` only for map-fusion
    candidates);
- fixed-point scheduling and deterministic rule order stay unchanged; this pass
  is additive and policy-gated, not a replacement.
