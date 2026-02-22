# Function Application

## Use When

Use this when implementing function calls, higher-order functions, dynamic dispatch, currying, or partial application.

## Wasm Building Blocks

- Direct calls (`call`) for statically known functions
- Indirect calls (`call_indirect`) for table-based dispatch
- Function references/tables for dynamic dispatch models
- Multi-value returns for richer calling conventions

## Implementation Sketch

1. Split call paths:
- direct-call lowering for known targets
- indirect-call lowering for first-class functions
2. Define one runtime function object shape:
- code pointer/table index
- environment pointer (optional)
- type id/signature info
3. Implement currying/partial application as closure wrappers.
4. Validate call signatures before indirect call sites.
5. Add inline-cache style optimization only after conformance is stable.

## Common Pitfalls

- Signature drift between caller and callee in indirect calls.
- Captured environment lifetime bugs.
- Overusing indirect calls when direct call targets are known.
- Not documenting ABI rules for tuples/multiple returns.

## Pointers

- Call validation/execution chapters: `references/spec-map.md`
- Lowering details: `references/language-to-wasm-playbook.md`
