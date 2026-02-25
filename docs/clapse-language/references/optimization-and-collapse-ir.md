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
