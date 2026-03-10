# Stack and Call Frames

## Use When

Use this when implementing evaluator/runtime internals: operand stack behavior, locals, call frames, recursion limits, and trap boundaries.

## Stack Model

Track at least these structures explicitly:

- operand/value stack (instruction inputs/outputs)
- control stack (block/loop/if label frames)
- call stack (function frames with locals and return targets)

Do not merge these implicitly unless invariants remain auditable.

## Implementation Sketch

1. Define frame layout:
- function id/signature
- locals storage
- base pointer or stack window bounds
- return program counter / continuation label
2. Define push/pop semantics per instruction category.
3. Enforce stack-effect checks in validator before execution.
4. Enforce runtime overflow/underflow checks with deterministic traps.
5. Add recursion-depth and max-stack safeguards as configurable limits.
6. Keep stack traces or frame snapshots for debugging failures.

## Common Pitfalls

- Operand stack leaks across block boundaries.
- Inconsistent behavior between validator assumptions and runtime frame code.
- Missing stack cleanup on trap/early-return paths.
- Silent overflow in host embeddings with small native stacks.

## Pointers

- Validation and execution chapters: `references/spec-map.md`
- wasm3 runtime constraints: `references/wasm3-notes.md`
