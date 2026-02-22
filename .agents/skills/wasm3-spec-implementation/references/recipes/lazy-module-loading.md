# Lazy Module Loading

## Use When

Use this when startup time and initial download size matter, and optional features should load only when first used.

## Loader Model

Split into:

- eagerly loaded bootstrap module (`core` + minimal runtime)
- lazily loaded feature modules
- optional prefetch list for likely next modules

In JS hosts, load lazy modules with `fetch` + `WebAssembly.instantiate` or `instantiateStreaming`.

## Split Strategy

Good lazy-load candidates:

- heavy but infrequently used libraries
- optional codecs/parsers
- feature-gated subsystems
- plugin modules

Bad lazy-load candidates:

- tiny modules with high call frequency
- modules needed on every request/frame

## Implementation Sketch

1. Mark modules with load policy (`eager`, `lazy`, `prefetch`).
2. Emit a loader manifest:
- module id
- URL/path
- ABI version
- dependency list
- integrity/hash
3. On first call to lazy symbol:
- resolve dependencies
- instantiate module
- cache instance
- patch dispatch table/registry
4. Expose metrics for load latency and cache hit rate.
5. Add failure fallback for missing/corrupt module.

## Common Pitfalls

- Lazy module requiring imports not available in current environment.
- Race conditions when multiple calls trigger the same first-load path.
- Version mismatch between already loaded and newly loaded modules.
- Recreating instances repeatedly without cache policy.

## Pointers

- JS boundary details: `references/recipes/javascript-interop.md`
- Module contracts: `references/recipes/module-architecture.md`
