# Optimization and Collapse IR

## Pipeline Shape

1. Parse source
2. Derive/apply class-law rewrites from class/law/instance declarations
   - class dispatch is explicit (static default), with static rewrites applying
     boolean normalization for law terms (`not not`, `true/false` identities and
     annihilations for `and`/`or`) and dynamic dispatch preserving method shape
3. Lower to stack ops
4. Collapse to normalized IR
5. Verify IR invariants
6. Emit WASM

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

## Implemented Optimization Passes

- Currying normalization
- Immediate closure/curry apply collapse
- Constant-argument direct-call specialization for small wrapper-style callees
- Small non-recursive interprocedural inlining for wrapper-style callees
- Root-based dead-function pruning
- slice ownership rewrite for `slice_set_u8` (linear reuse vs shared-target copy path)
- Dead-temp pruning and compact temp renumbering
- Self tail-call normalization
- Compile-time bool-law simplification (`CAnd`/`COr`/`CNot`) when class dispatch is static

## Memory Model Staging (Current)

Collapse now carries explicit memory-model stage ordering:

1. `MemoryPassEscapeLifetimeAnnotation`
2. `MemoryPassSliceOwnershipRewrite`

Current kernel wiring keeps stage 1 semantics-preserving over request payloads and
keeps stage 2 aligned with explicit ownership policy selection for
`slice_set_u8` chains.

Pipeline behavior is deterministic and pure:

- Stage 1 scans request payload text and materializes `SliceSetU8WriteChainHint`
  (escape marker + alias hint + explicit-chain-presence marker).
- Stage 2 converts that hint into an `OwnershipRewriteMode` policy
  (`OwnershipRewriteLinear` or `OwnershipRewriteCopyOnWrite`) and returns
  unchanged payload in this bootstrap.

### Allocation / Deallocation and Ownership Semantics

Allocation and reclaim in the optimizer are modeled as ownership metadata transforms, with no change to runtime allocator behavior in this stage:

- `slice_set_u8` owns a target descriptor and byte payload only in IR reasoning.
- The rewrite phase cannot emit frees; reclaim only appears as explicit `region_reset`
  and control of where new temporaries replace old descriptors.
- `slice_new_u8` introduces fresh allocation IR; `slice_set_u8` may either:
  - reuse the same descriptor when analysis proves a single mutable path, or
  - allocate a replacement descriptor and copy bytes when alias risk exists.
- No pass currently rewrites non-slice object allocation patterns to compact pools; struct/closure allocation remains structural and stable under existing passes.

### Escape / Lifetime / Alias analysis contract (implemented)

The analysis is staged as:

- `EscapeLifetimeAnnotation`:
  - `EscapeLifetimeLocal` means ownership is treated as local to the current write-chain scope.
  - `EscapeLifetimeEscaped` means the value can be observed outside that scope.
  - `EscapeLifetimeUnknown` is a conservative fallback when tracking is incomplete.
- `OwnershipRewriteMode`:
  - `OwnershipRewriteLinear` is the in-place reuse result.
  - `OwnershipRewriteCopyOnWrite` is the defensive copy result.
- `collapse_pipeline_escape_lifetime` now runs a deterministic metadata analysis over request text
  to extract request-chain hints for later ownership policy selection.
- `collapse_pipeline_slice_ownership_rewrite` consumes that policy through
  `slice_set_u8_context_mode_with_policy` (via `slice_set_u8_rewrite`) to choose
  in-place reuse vs. COW paths. The first pass does no rewriting by itself; it only
  preserves deterministic hints for the ownership pass.
- The current request-chain hinting uses `slice_set_u8` presence plus explicit
  escape (`"output"`/`"return"`) and alias hints (`.asSlice`) to seed
  `SliceSetU8WriteChainHint` (`EscapeLifetime*`, alias class, has-chain flag).

Current rule model used in the kernel rewrite path:

- `slice_set_u8_base_context` seeds analysis from requested policy (`OwnershipRewriteMode`) and
  index validity.
- `slice_set_u8_context_with_alias` elevates to shared/escaped when target aliasing is suspected.
- `slice_set_u8_context_with_freeze` elevates to copy-on-write when freeze-like usage is inferred.
- In stage-2 policy selection, uncertainty remains conservative: unknown/escaped/sized-out
  scopes default to `OwnershipRewriteCopyOnWrite`, while `EscapeLifetimeLocal` +
  `SliceWriteAliasSingleUse` + non-escaped local chains can remain linear.
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
- In COW mode, optimizer behavior is: `slice_set_u8_cow` = allocate fresh slice + full-copy + local write.
- In both modes, no runtime allocator deallocation is inserted by these passes; ownership intent is expressed as descriptor replacement and in-place descriptor reuse.
- Freeze is currently a conservative classification in kernel analysis, not a literal IR flag:
  freeze-like writes keep the operation on COW paths.

### Scope and lifetime behavior (function-local vs escaping)

- Function-local write-chain scope for this kernel pass means:
  - request-derived hints provide explicit initial state; local context checks then refine ownership choice.
  - if no observed cross-boundary consumer is detected in the analyzed chain and
    alias/freeze checks are clear, then write semantics stay in `OwnershipRewriteLinear`.
- Escaping scope is inferred conservatively from textual indicators and/or explicit
  `SliceSetU8WriteChainHint` context:
  - any `EscapeLifetimeEscaped` marker,
  - any alias hint,
  - or uncertain/unknown alias/equivalence information
  routes the rewrite to COW.

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

The class rewrite stage sits before lowering and chooses method implementations from class metadata:

- `resolve_class_method` selects between `static_method` and `dynamic_method` with the mode witness.
- `apply_class_law_rewrites` applies the bool-law rewrite only when mode is `ClassDispatchStatic`.
- `ClassDispatchDynamic` keeps the law method unchanged.

Safe fold set currently implemented in `rewrite_bool_law_expr`:

- `not (not x)` -> `x`
- `true && x` -> `x`
- `false && x` -> `false`
- `true || x` -> `true`
- `false || x` -> `x`

All of these are conservative bool simplifications and are documented as compile-time only.
