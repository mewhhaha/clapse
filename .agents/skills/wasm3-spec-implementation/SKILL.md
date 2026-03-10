---
name: wasm3-spec-implementation
description: Build or debug a WebAssembly runtime, validator, parser, compiler frontend, or Wasm-first language using the official WebAssembly specifications as source-of-truth, with practical wasm3-style constraints. Use when mapping high-level language constructs to Wasm, designing data layout and ABI contracts, implementing JS/browser/GPU interop, planning feature gates, triaging conformance failures, or applying proof-guided optimization strategies.
---

# Wasm3 Spec Implementation

## Overview

Use this skill to implement a Wasm engine or a language that targets Wasm without drifting from the specification. Prioritize semantic correctness first, then apply wasm3-style runtime constraints (small footprint, interpreter-oriented execution, portable embedding).

## Source Priority

1. Use Core spec chapters from `references/spec-map.md` for syntax, validation, and execution semantics.
2. Use JS API or Web API specs only when implementing browser embedding behavior.
3. Use wasm3 notes as operational guidance, not as semantic authority.
4. If two sources disagree, follow the Core spec and put deviations behind feature flags.

## Quick Routing

1. Open `references/recipes/lookup-by-intent.md` first.
2. Match user intent to one intent block and open only the first recommended file.
3. Open the second or third file in that block only if the first file is insufficient.
4. Keep browsing scoped to one category until the requested task is resolved.

## Workflow

1. Define a feature target.
Decide the exact baseline first (MVP core opcodes and module sections). Record unsupported features explicitly.

2. Build the decoder and parser.
Decode binary sections in canonical order, preserve immediates in IR, and reject malformed encodings early.

3. Implement validation.
Track value stack and control stack invariants, type-check instruction sequences, and enforce index-space rules.

4. Implement execution semantics.
Model the store, frames, and trap rules exactly. Perform bounds checks before mutation.

5. Implement host boundary behavior.
Define import resolution, export marshaling, and host-error/trap separation.

6. Run conformance and iterate.
Use the conformance loop in `references/conformance-workflow.md`, map each failure to a spec clause, then patch.

## Custom Language to Wasm

When users ask to build "our own WebAssembly language":

1. Specify source-language semantics (types, control flow, memory model, error model) before code generation.
2. Lower to a typed IR that is easier to validate than raw stack-machine code.
3. Convert IR to structured Wasm control flow (`block`/`loop`/`if`) and explicit locals.
4. Run validator checks after every lowering pass.
5. Keep ABI and data layout rules in one document to avoid backend drift.
6. Use `references/language-to-wasm-playbook.md` for lowering patterns.

## Decision Rules

- Open `references/spec-map.md` first when semantics are unclear.
- Add new proposals behind named feature flags; do not alter baseline behavior silently.
- Prefer deterministic parser/validator errors over permissive recovery.
- Separate semantic fixes from performance fixes in different commits or patches.

## Deliverables Per Task

- `spec_trace`: exact spec URLs/chapters consulted.
- `implementation_delta`: invariants added and files changed.
- `conformance_delta`: tests added/updated and pass/fail movement.
- `deviation_log`: unsupported features and intentional non-spec behavior.

## References

- Spec chapter map and implementation checkpoints: `references/spec-map.md`
- Language lowering patterns: `references/language-to-wasm-playbook.md`
- Conformance triage loop: `references/conformance-workflow.md`
- wasm3-style runtime constraints: `references/wasm3-notes.md`
- Intent-based lookup router: `references/recipes/lookup-by-intent.md`
- Language feature quick lookups: `references/recipes/index.md`
