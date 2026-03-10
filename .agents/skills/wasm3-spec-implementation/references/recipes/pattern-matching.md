# Pattern Matching

## Use When

Use this when lowering `match`/`case` over ADTs, tuples, literals, or guards.

## Lowering Strategy

- Compile to a decision tree in typed IR.
- Emit branch sequence or `br_table` depending on tag density.
- Keep exhaustiveness and unreachable-arm diagnostics from the typechecker.

## Implementation Sketch

1. Normalize match patterns into decision-tree nodes.
2. Order tests to preserve language semantics (including guards).
3. Compile constructor/literal tests to explicit branches.
4. Bind pattern variables at decision-tree leaves.
5. Emit fallback for non-exhaustive matches based on language policy.

## Common Pitfalls

- Reordering checks in a way that changes guard semantics.
- Duplicate arm acceptance without warning.
- Incorrect bindings when nested patterns share names.
- Lost exhaustiveness information after optimization passes.

## Pointers

- Control flow lowering: `references/recipes/switch-statements.md`
- ADT encoding: `references/recipes/algebraic-data-types.md`
