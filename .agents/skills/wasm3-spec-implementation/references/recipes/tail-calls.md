# Tail Calls

## Use When

Use this when functional recursion, interpreter loops, or continuation-style code should run without unbounded stack growth.

## Core Idea

Compile calls in tail position to tail-call instructions when available; otherwise compile through an explicit trampoline.

## Implementation Sketch

1. Mark tail position during type-checking/lowering.
2. Emit normal `call` for non-tail positions.
3. Emit tail-call form for tail positions (feature-gated).
4. Fallback path:
- return a thunk/state tuple
- drive execution in a loop trampoline
5. Add tests for deep recursion and mutual recursion.

## Common Pitfalls

- Incorrectly marking non-tail calls as tail calls.
- Losing cleanup semantics before a tail transition.
- Mismatched parameter/result signatures in tail position.
- Failing to keep fallback and native tail-call paths behaviorally identical.

## Pointers

- Feature tracking: `https://webassembly.org/features/`
- Validation/execution references: `references/spec-map.md`
