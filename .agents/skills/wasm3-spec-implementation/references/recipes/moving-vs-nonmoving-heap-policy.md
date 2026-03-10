# Moving vs Nonmoving Heap Policy

## Use When

Use this when choosing collector architecture and deciding which objects may be relocated.

## Tradeoff Summary

1. Moving heap:
- improves locality and fragmentation
- requires pointer updates and pinning rules

2. Nonmoving heap:
- simpler FFI/pointer stability
- can fragment and reduce locality

3. Hybrid:
- moving young gen + pinned/nonmoving old or FFI-critical objects

## Implementation Sketch

1. Classify objects by relocation safety.
2. Add pin/unpin API for FFI-exposed objects.
3. Define forwarding-pointer/update protocol for moved objects.
4. Keep relocation barriers and root updates verified in tests.
5. Document which APIs return stable vs relocatable addresses.

## Common Pitfalls

- Moving objects while host holds raw pointers.
- Excessive pinning defeating compaction benefits.
- Incomplete pointer rewriting during compaction.
- Divergent behavior between debug and optimized collectors.

## Pointers

- FFI ownership rules: `references/recipes/borrowed-vs-owned-ffi-values.md`
- Write barrier rules: `references/recipes/generational-gc-write-barriers.md`
