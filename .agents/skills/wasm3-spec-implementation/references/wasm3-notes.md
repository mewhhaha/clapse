# wasm3-Oriented Notes

Use these notes when the user explicitly wants a wasm3-style implementation approach.

## What "wasm3-style" Usually Means

- prioritize portability and embeddability
- prefer interpreter-first execution model
- keep host integration simple and explicit
- optimize for predictable resource usage

Use this as an implementation style choice, not as a replacement for specification semantics.

## Semantic Guardrail

Implement Core spec behavior first.
Apply wasm3-inspired optimizations only after conformance behavior is stable.

## Practical Runtime Constraints to Enforce

- keep stack frame and value representation explicit
- enforce deterministic trap points (bounds, type mismatches, invalid indices)
- avoid host-side hidden coercions
- isolate feature flags to avoid accidental semantic drift

## Embedding Checklist

When wiring host functions:

1. define import namespace/name resolution behavior
2. validate imported function signatures before invocation
3. convert host exceptions to explicit trap/error boundaries
4. keep memory/table access checks inside runtime boundaries

## Performance Tuning Order

1. pass decoder/validator/runtime semantic tests
2. measure instruction hotspots and host-call overhead
3. optimize dispatch/memory access paths
4. rerun conformance tests after each optimization set

If an optimization changes observable behavior, revert it or feature-gate it.

## Useful Project References

- wasm3 repository: https://github.com/wasm3/wasm3
- wasm3 project site: https://wasm3.org/
