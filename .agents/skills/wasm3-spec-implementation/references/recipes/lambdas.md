# Lambdas

## Use When

Use this when the language supports anonymous functions, inline function literals, or higher-order callbacks.

## Core Lowering Pattern

Lower each lambda into:

- a lifted function body
- an optional environment object for captured values
- a function object pairing code target + environment

If nothing is captured, use a zero-environment fast path.

## Implementation Sketch

1. Parse lambda syntax into explicit AST nodes.
2. Determine free variables at lambda boundary.
3. Lift lambda body into a named internal function.
4. Materialize environment layout for captured values.
5. Emit function-object construction at lambda expression site.
6. Reuse closure call path from `function-application.md`.

## Common Pitfalls

- Capturing mutable bindings by value when reference semantics are required.
- Non-deterministic naming/symbol generation for lifted lambdas.
- Recreating identical non-capturing lambdas unnecessarily.
- Forgetting source-map/debug name mapping for anonymous functions.

## Pointers

- Captures and environments: `references/recipes/closures-and-captures.md`
- Call dispatch details: `references/recipes/function-application.md`
