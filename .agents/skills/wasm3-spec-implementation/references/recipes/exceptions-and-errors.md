# Exceptions and Errors

## Use When

Use this when the language has `throw/catch`, recoverable errors, or needs clear boundaries between host exceptions and Wasm traps.

## Error Model Decision

Choose one baseline model first:

- explicit result types (`Result`, tagged union)
- exception mechanism (feature-gated)
- mixed model with strict conversion rules

## Implementation Sketch

1. Define trap vs language-error semantics:
- trap: runtime violation (bounds/type/etc.)
- language error: expected recoverable failure
2. Encode recoverable errors in the language ABI.
3. Keep trap paths deterministic and non-recoverable unless explicitly mapped.
4. At host boundary, translate exceptions exactly once.
5. Add tests for nested calls crossing host and Wasm boundaries.

## Common Pitfalls

- Catching traps as normal language exceptions.
- Silent conversion of host exceptions into success values.
- Divergent behavior between interpreted and optimized paths.
- Missing stack/context in runtime error reporting.

## Pointers

- Runtime and host semantics: `references/spec-map.md`
- Conformance loop: `references/conformance-workflow.md`
