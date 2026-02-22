# Effects, FFI, and Purity Boundaries

## Use When

Use this when combining pure functional core logic with impure host interactions (I/O, clocks, randomness, files, network).

## Design Principle

Keep effectful behavior explicit in types/IR and isolate host calls behind small runtime APIs.

## Implementation Sketch

1. Define effect model:
- explicit effect types
- capability passing
- runtime effect handlers
2. Keep pure computations free of hidden host calls.
3. Lower effectful ops to runtime/host boundaries only.
4. Convert host exceptions into explicit language error channels.
5. Add deterministic test mode for time/randomness.

## Common Pitfalls

- Hidden impurity introduced by optimizer/runtime helpers.
- Leaking host exceptions into pure paths.
- Non-deterministic tests due to unmocked effects.
- Mixing trap semantics with recoverable effect errors.

## Pointers

- Host boundary rules: `references/wasm3-notes.md`
- Error modeling: `references/recipes/exceptions-and-errors.md`
