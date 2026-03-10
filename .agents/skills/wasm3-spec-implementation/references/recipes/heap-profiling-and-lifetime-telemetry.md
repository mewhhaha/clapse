# Heap Profiling and Lifetime Telemetry

## Use When

Use this when investigating leaks, high allocation churn, or retention spikes in language/runtime programs.

## Key Metrics

- allocation rate by site/type
- live bytes by generation/region
- object survival curves
- pause time and frequency
- top retaining paths/roots

## Implementation Sketch

1. Assign stable allocation-site ids in compiler output.
2. Emit runtime counters and periodic snapshots.
3. Correlate allocations with lifetime class:
- short-lived
- medium-lived
- long-lived
4. Export trace format for offline analysis.
5. Add CI regression thresholds for allocation/pause budgets.

## Common Pitfalls

- Sampling too little to capture real leak source.
- High-overhead profiling mode changing runtime behavior too much.
- Unstable allocation-site ids across builds.
- No correlation between telemetry and source locations.

## Pointers

- Trigger policy: `references/recipes/gc-safepoint-scheduling.md`
- Escape analysis: `references/recipes/lifetime-and-escape-analysis.md`
