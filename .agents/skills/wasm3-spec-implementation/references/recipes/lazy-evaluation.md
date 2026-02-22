# Lazy Evaluation

## Use When

Use this when values should compute on demand (lazy lists, deferred computations, infinite streams).

## Core Representation

Represent thunks as:

- code pointer/lambda
- environment
- evaluated flag
- cached result slot

## Implementation Sketch

1. Define strict vs lazy evaluation points in language semantics.
2. Lower lazy expressions into thunk allocation.
3. Implement `force` operation with memoization.
4. Ensure `force` handles recursive/self-referential thunks safely.
5. Add strictness analysis later for performance improvements.

## Common Pitfalls

- Recomputing thunks due to missing memoization.
- Space leaks from long-lived thunk chains.
- Unexpected side effects when forcing at different times.
- Non-thread-safe forcing in concurrent runtimes.

## Pointers

- Closures and captures: `references/recipes/closures-and-captures.md`
- Stack/runtime safeguards: `references/recipes/stack-and-call-frames.md`
