# Closures and Captures

## Use When

Use this when nested functions capture variables from outer scopes.

## Core Representation

Model a closure as:

- callable target (function id/table index)
- environment object with captured values

## Implementation Sketch

1. Run closure conversion pass:
- lift nested functions
- replace free variables with environment loads
2. Define capture policy:
- capture by value for immutable bindings
- capture cell/pointer for mutable bindings
3. Generate environment layout deterministically (stable field order).
4. Pass environment explicitly as hidden parameter.
5. Ensure GC/root logic includes closure environments.

## Common Pitfalls

- Capturing stack addresses that become invalid.
- Wrong semantics for mutable captured variables.
- Non-deterministic environment field ordering across compilers/builds.
- Forgetting to include environments in serialization/debug output.

## Pointers

- Lowering workflow: `references/language-to-wasm-playbook.md`
- Memory/runtime semantics: `references/spec-map.md`
