# Async and Green Threads

## Use When

Use this for high-concurrency workflows where many lightweight tasks should run without one OS thread per task.

## Runtime Model Choices

1. Async futures/tasks:
- each task is a pollable state machine
- scheduler drives tasks to completion

2. Green threads:
- each fiber has runtime-managed stack/continuation state
- cooperative or preemptive scheduling policy

## Implementation Sketch

1. Define cancellation and wakeup semantics first.
2. Compile `async` functions to resumable state machines.
3. Provide scheduler primitives:
- spawn
- park/suspend
- wake
- cancel
4. Isolate I/O in host adapters that post wake events.
5. Keep deterministic scheduler mode for tests.
6. Add backpressure and queue limits to prevent task storms.

## Common Pitfalls

- Blocking host calls on scheduler thread.
- Non-cancellable tasks leaking resources.
- Waking completed tasks due to stale handles.
- Hidden shared mutability between tasks without synchronization.

## Pointers

- Continuation lowering: `references/recipes/continuations-and-coroutines.md`
- Host boundaries and errors: `references/recipes/effects-ffi-and-purity.md`
