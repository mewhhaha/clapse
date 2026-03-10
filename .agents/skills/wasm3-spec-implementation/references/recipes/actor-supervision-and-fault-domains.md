# Actor Supervision and Fault Domains

## Use When

Use this when modeling concurrency in a Gleam/Erlang style with isolated processes and supervised restart behavior.

## Core Model

- each actor has isolated state and mailbox
- actors communicate via typed messages
- supervisors own child actors and restart policy
- failure boundaries are explicit fault domains

## Implementation Sketch

1. Define actor lifecycle states (`init`, `running`, `stopping`, `failed`).
2. Define supervision strategies:
- one-for-one
- one-for-all
- rest-for-one
3. Persist minimal restart context in runtime metadata.
4. Keep crash reasons structured and typed.
5. Add backoff and restart intensity limits.

## Common Pitfalls

- shared mutable state bypassing actor isolation
- restart loops without backoff limits
- supervisors depending on non-restartable hidden state
- mailbox growth without backpressure policy

## Pointers

- Async runtime: `references/recipes/async-and-green-threads.md`
- Protocol contracts: `references/recipes/typed-protocols-and-message-schema-evolution.md`
