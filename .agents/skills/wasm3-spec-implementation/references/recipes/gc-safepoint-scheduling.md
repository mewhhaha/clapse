# GC Safepoint Scheduling

## Use When

Use this when defining when GC may run so behavior is predictable and latency is controllable.
Apply this directly to tracing collectors; for pure RC, reuse only the scheduling/telemetry parts relevant to cycle checks.

## Trigger Policies

- allocation budget (bytes since last collection)
- explicit safepoint checks in loops/calls
- host-driven pressure signal
- manual debug trigger mode

## Implementation Sketch

1. Define hard and soft GC trigger thresholds.
2. Insert cheap safepoint polls at selected program points.
3. Trigger GC only at safepoints unless stop-the-world policy says otherwise.
4. Record reason for each collection cycle in telemetry.
5. Add deterministic test mode with fixed budget/seed.

## Common Pitfalls

- Long-running loops with no safepoint checks.
- Unbounded pause spikes from rare but huge collections.
- Triggering GC from unsafe runtime states.
- Different trigger behavior across debug/release builds.

## Pointers

- Root precision metadata: `references/recipes/stack-maps-and-safepoints.md`
- Lifetime telemetry: `references/recipes/heap-profiling-and-lifetime-telemetry.md`
