# Algebraic Effect Handlers

## Use When

Use this when modeling side effects as typed operations handled by composable effect handlers instead of direct host calls.

## Core Lowering Idea

Lower effectful computations into explicit handler-passing form (or continuation-passing form) where each effect operation is an explicit runtime dispatch.

## Implementation Sketch

1. Represent effect operations in typed IR (`perform op payload`).
2. Represent handlers as scoped runtime values with operation clauses.
3. Lower `perform` to:
- capture current continuation (delimited)
- invoke matching handler clause
4. Lower `handle` blocks to push/pop handler frames.
5. Add fallback for unhandled effects (typed error or trap by policy).
6. Keep purity boundaries explicit at host interop edges.

## Strategy Choices

1. Handler-passing (simpler implementation, explicit dictionaries)
2. CPS/delimited continuations (more flexible control effects)

Start with handler-passing unless advanced control effects are required.

## Common Pitfalls

- Incorrect handler scope during nested `handle` blocks.
- Resuming a continuation multiple times when semantics are one-shot.
- Conflating effect errors with runtime traps.
- Unclear performance costs from excessive continuation capture.

## Pointers

- Continuations and suspension: `references/recipes/continuations-and-coroutines.md`
- Purity and FFI boundaries: `references/recipes/effects-ffi-and-purity.md`
