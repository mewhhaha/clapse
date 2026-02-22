# Continuations and Coroutines

## Use When

Use this for suspension/resumption semantics (`yield`, generators, coroutine schedulers), or call/cc-style control operators.

## Core Lowering Strategy

Represent suspended computation as explicit continuation state:

- program counter / state tag
- captured locals/environment
- pending result/error slot

Prefer explicit state machines and trampolines over native stack capture.

## Implementation Sketch

1. Define continuation semantics (one-shot vs multi-shot).
2. Lower coroutine bodies to state-machine functions.
3. Encode `yield` as state save + return to scheduler.
4. Encode `resume` as dispatch on saved state tag.
5. Use a trampoline loop to avoid deep recursive host calls.
6. Add tests for nested suspension and re-entrancy errors.

## call/cc-Style Notes

- Full multi-shot continuations are expensive in Wasm-first runtimes.
- Start with delimited/one-shot continuations for predictable memory use.
- If multi-shot is needed, copy continuation state explicitly.

## Common Pitfalls

- Capturing transient stack references that become invalid.
- Allowing resumed continuations after final completion.
- Mixing continuation state with thread-local scheduler state.
- Unbounded continuation growth due to missing cleanup.

## Pointers

- Stack and frame model: `references/recipes/stack-and-call-frames.md`
- Tail/trampoline patterns: `references/recipes/tail-calls.md`
