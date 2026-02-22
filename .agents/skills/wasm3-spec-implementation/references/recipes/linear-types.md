# Linear Types

## Use When

Use this when values must be consumed exactly once (buffers, capability tokens, file/socket handles, unique mutable arrays).

## Core Rule

A linear value has exactly one owner and must be used exactly once in every control-flow path.

## Wasm Mapping

- Keep runtime representation ordinary (pointers/indices/structs).
- Enforce linearity in type checking and IR validation.
- Lower moves as ownership transfer (source binding becomes invalid).

## Implementation Sketch

1. Add ownership metadata to typed AST/IR.
2. Track linear variable state across branches and merges.
3. Require explicit `move` semantics in lowering.
4. Reject duplicate uses and missing uses at compile time.
5. Add "consume then invalidate" checks in debug runtime mode.

## Common Pitfalls

- Forgetting path-sensitive usage accounting in `if`/`match`.
- Accidental implicit copies during optimization passes.
- Letting closure captures duplicate linear values.
- Reordering code in a way that changes consumption order.

## Pointers

- Function and closure lowering: `references/recipes/function-application.md`
- Control-flow checks: `references/recipes/if-statements.md`
- Pure reuse patterns: `references/recipes/pure-memory-reuse.md`
