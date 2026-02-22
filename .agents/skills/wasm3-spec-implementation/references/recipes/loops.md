# Loops

## Use When

Use this when lowering `while`, `for`, and iterator-style loops.

## Wasm Mapping

Typical loop shape:

- outer `block` as break target
- inner `loop` as continue target
- `br_if` to exit/break
- `br` back to loop head for continue

## Implementation Sketch

1. Define semantics for `break` and `continue` before lowering.
2. Emit loop skeleton (`block` + `loop`) first.
3. Lower condition checks and back-edges with explicit branch depths.
4. Lower body with correct handling of nested labels.
5. Add tests for nested loops, early exits, and continue-heavy code.

## Common Pitfalls

- Swapping break and continue label depths.
- Emitting condition checks in the wrong position (`while` vs `do-while`).
- Leaking stack values across loop iterations.
- Incorrect semantics for loop-carried mutable variables.

## Pointers

- Control flow validation rules: `references/spec-map.md`
- General lowering guidance: `references/language-to-wasm-playbook.md`
