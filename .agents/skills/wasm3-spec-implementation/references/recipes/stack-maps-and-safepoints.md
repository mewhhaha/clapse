# Stack Maps and Safepoints

## Use When

Use this for exact GC in runtimes that need reliable root discovery at known execution points.
This is primarily for tracing/moving collectors; pure RC runtimes may not require full stack maps.

## Core Idea

Emit safepoints and stack maps so the collector can enumerate live references precisely without conservative scanning.

## Implementation Sketch

1. Choose safepoint locations:
- function calls
- loop backedges
- allocation slow paths
2. Emit per-safepoint metadata:
- live reference locals
- live stack slots
- frame layout id
3. Generate compact stack-map tables in module metadata.
4. At GC trigger, stop at safepoint and enumerate roots from tables.
5. Add verifier that checks stack-map metadata against generated frames.

## Common Pitfalls

- Missing live roots due to stale liveness info.
- Register/stack location mismatch after late code transforms.
- Excessive safepoints causing high overhead.
- Incompatible metadata format across module versions.

## Pointers

- Frame model: `references/recipes/stack-and-call-frames.md`
- GC trigger policy: `references/recipes/gc-safepoint-scheduling.md`
