# Cross-Module State and Contracts

## Use When

Use this when multiple Wasm modules must share data or call each other safely over time.

## State Sharing Options

1. Explicit message passing (recommended default):
- serialize request/response payloads
- clear ownership and version boundaries

2. Shared linear memory:
- fast path, but requires strict layout contracts
- needs explicit synchronization for concurrent access

3. Host-mediated handles:
- modules exchange opaque ids managed by host/runtime registry

## Contract Rules

Define and version:

- function signatures
- packed struct layouts and tags
- ownership/lifetime of buffers
- error/trap behavior

Reject incompatible version mismatches at load/link time.

## Implementation Sketch

1. Put all public contracts in generated ABI manifests.
2. Validate imported signature + version before instantiation.
3. Use adapter stubs only for versions explicitly declared compatible by policy.
4. Add contract tests that run module pairs across versions.
5. Log contract violations with module id and symbol name.

## Common Pitfalls

- Implicitly relying on same-compiler build for layout compatibility.
- Sharing pointers across modules with different allocators.
- Breaking wire format without version bump.
- Silent fallback from strict contract checks.

## Pointers

- Binary layout rules: `references/recipes/packing-and-unpacking.md`
- Threading concerns: `references/recipes/parallelism-threads.md`
