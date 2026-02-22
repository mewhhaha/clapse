# If Statements

## Use When

Use this when lowering `if/else` expressions or statements from the source language.

## Wasm Mapping

- condition expression -> value consumed by `if`
- then branch -> `if ...`
- else branch -> `else ...`
- merge point -> `end`

For expression-valued `if`, ensure both branches produce compatible result types.

## Implementation Sketch

1. Type-check condition and branch result compatibility.
2. Lower condition first, then emit structured `if`.
3. Emit then block, optional else block, then `end`.
4. Keep side effects explicit in branch order.
5. Add tests for nested and expression-valued conditionals.

## Common Pitfalls

- Branch type mismatch in expression position.
- Dropping values on one branch but not the other.
- Reordering side effects while lowering.
- Treating non-boolean numerics inconsistently across backends.

## Pointers

- Validation/execution semantics: `references/spec-map.md`
- Lowering baseline: `references/language-to-wasm-playbook.md`
