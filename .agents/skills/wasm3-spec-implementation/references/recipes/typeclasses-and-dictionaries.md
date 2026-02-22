# Typeclasses and Dictionary Passing

## Use When

Use this when supporting ad-hoc polymorphism (typeclasses, traits, protocols, interfaces) in a functional style.

## Core Compilation Pattern

Lower constrained functions by adding hidden dictionary parameters that carry method implementations and laws/metadata if needed.

## Implementation Sketch

1. Resolve instances during type checking.
2. Reify dictionary values in IR.
3. Pass dictionaries as hidden parameters at call sites.
4. Inline dictionary methods for hot paths when profitable.
5. Keep dictionary ABI stable across module boundaries.

## Common Pitfalls

- Ambiguous instance resolution not surfaced clearly.
- Dictionary layout drift between compilation units.
- Large runtime overhead from repeated dictionary construction.
- Confusion between dictionary passing and subtype-style dispatch.

## Pointers

- Generic strategy options: `references/recipes/generics-and-polymorphism.md`
- Function call lowering: `references/recipes/function-application.md`
