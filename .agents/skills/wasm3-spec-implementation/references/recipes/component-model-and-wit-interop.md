# Component Model and WIT Interop

## Use When

Use this when targeting the WebAssembly Component Model and defining stable typed interfaces across languages/runtimes.

## Core Benefits

- explicit world/interface definitions
- generated bindings for host and guest
- stronger cross-language contract checking than ad-hoc ABI docs

## Implementation Sketch

1. Define WIT packages/interfaces for public APIs.
2. Keep interface evolution versioned and reviewed like ABI changes.
3. Generate bindings as part of build pipeline.
4. Map language-specific types to WIT types deterministically.
5. Validate component composition against expected worlds.

## Migration Strategy

- keep core runtime ABI stable while introducing WIT layer
- start with boundary modules (I/O, host services, plugin APIs)
- retain fallback path for non-component runtimes if needed

## Common Pitfalls

- drifting hand-written wrappers from generated WIT bindings
- unclear ownership semantics for lists/strings across boundaries
- version skew between component packages
- treating component model as drop-in replacement without contract design

## Pointers

- JS interop baseline: `references/recipes/javascript-interop.md`
- Module signatures: `references/recipes/module-signatures-and-separate-compilation.md`
