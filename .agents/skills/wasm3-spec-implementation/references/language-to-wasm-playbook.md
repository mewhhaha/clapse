# Language-to-Wasm Playbook

Use this guide when the user wants to build "our own WebAssembly language" and compile it to Wasm.

## 1. Freeze Language Semantics First

Define before codegen:

- primitive and compound types
- integer overflow and float NaN behavior
- mutable/immutable binding rules
- function call and return conventions
- error model (trap, recoverable error, or result type)

Do not design codegen before these semantics are explicit.

## 2. Lower Through a Typed IR

Use at least one intermediate representation with explicit types and control-flow edges.
Keep the IR closer to source semantics than to stack-machine details.

Benefits:

- detect type/control errors early
- simplify optimization passes
- generate clearer validation failures

## 3. Lower Structured Control Flow Correctly

Map high-level control flow to structured Wasm constructs:

- `if/else` -> `if ... else ... end`
- `while` -> `block` + `loop` + `br_if`
- `break`/`continue` -> depth-indexed `br`
- early return -> `return` or branch to epilogue block

Track label depth explicitly in the lowering state.

## 4. Make Stack Effects Explicit During Codegen

Generate with per-instruction metadata:

- expected input stack types
- produced output stack types
- trap conditions

Run a validator-style pass after lowering and before binary emission.

## 5. Stabilize ABI and Memory Layout

Document:

- argument/result passing convention
- linear memory ownership model
- stack pointer/global usage policy
- string/array/struct layout

Keep this ABI spec versioned. Runtime and compiler must evolve together.

## 6. Emit Deterministic Binaries

Require deterministic output for equivalent input program + feature flags.
Deterministic output makes conformance bisects and cache behavior reliable.

## 7. Add Feature Gates Early

Use explicit feature flags from day one:

- baseline core behavior (default on)
- proposal-specific behavior (default off unless requested)

Never change baseline semantics without a new gate or a migration plan.

## 8. Minimum Acceptance Criteria

Ship a language feature only when all pass:

1. parser/typing tests
2. lowering tests
3. Wasm validation tests
4. runtime behavior tests
5. at least one conformance-style regression test
