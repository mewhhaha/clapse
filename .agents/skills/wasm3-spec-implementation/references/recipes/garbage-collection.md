# Garbage Collection

## Use When

Use this when the language has heap objects (lists, strings, closures, structs) and manual `malloc/free` is not acceptable for user code.

## Two Practical Paths

1. Linear-memory GC runtime:
- store objects in linear memory
- maintain object headers (size, tag, mark bits)
- implement tracing GC or reference counting in runtime code

2. Wasm GC types (feature-gated):
- represent objects with Wasm reference types
- map language objects to engine-managed heap values
- keep a fallback for runtimes that do not support GC features

## Collector Model Scope

Choose one primary collector model first:

- tracing collector (mark-sweep/generational/incremental)
- reference counting (optionally with cycle detection)

Use safepoints/stack maps guidance for tracing exact collectors.
For pure RC runtimes, keep deterministic retain/release and optional cycle-collection checkpoints instead.

## Implementation Sketch

1. Define object model first (header layout, pointer/tagging strategy).
2. Decide root set ownership:
- VM stack
- globals
- closure environments
- host handles
3. Implement allocation fast path and OOM/trap behavior.
4. Implement collector:
- mark-sweep for simpler debugging
- generational later if needed
5. Add write barriers before introducing generations/incremental collection.
6. Add stress mode (`collect_every_alloc`) for deterministic bug finding.

## Common Pitfalls

- Missing roots from host-facing handles.
- Moving collector without precise pointer metadata.
- GC running during unsafe host callbacks.
- Not separating language exceptions from OOM/trap failures.

## Pointers

- Feature tracking: `https://webassembly.org/features/`
- Semantic baseline: `references/spec-map.md`
