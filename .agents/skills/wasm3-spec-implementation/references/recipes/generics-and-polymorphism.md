# Generics and Polymorphism

## Use When

Use this when the language supports generic functions/types, interfaces/traits, or dynamic polymorphism.

## Three Common Compilation Strategies

1. Monomorphization:
- generate specialized code per concrete type
- fast runtime, larger binaries

2. Dictionary passing:
- pass method tables/typeclass dictionaries as hidden params
- smaller binaries, extra call overhead

3. Tagged dynamic values:
- uniform boxed representation with runtime type checks
- simpler backend, slower hot paths

## Implementation Sketch

1. Pick default strategy per feature tier (MVP vs optimized builds).
2. Keep IR type metadata precise until late lowering.
3. Specialize only proven hot paths first.
4. Stabilize name mangling and symbol identity for generic instances.
5. Add ABI tests for cross-module generic calls.

## Common Pitfalls

- Monomorphization explosion in public libraries.
- Inconsistent type-id generation across compilation units.
- Hidden dictionary ABI drift between compiler versions.
- Mixing static and dynamic dispatch without explicit policy.

## Pointers

- Function call lowering: `references/recipes/function-application.md`
- Module ABI concerns: `references/recipes/modules-and-linking.md`
