# SIMD and Data Parallelism

## Use When

Use this for vector math, DSP, image processing, physics kernels, or other hot loops over numeric arrays.

## Wasm Mapping

- Scalar loop baseline
- SIMD vector path behind a feature gate
- Runtime/compile-time dispatch between scalar and SIMD implementations

## Implementation Sketch

1. Keep scalar reference implementation as semantic baseline.
2. Add vectorized kernels for stable hot paths.
3. Choose explicit memory layout (alignment, stride, lane packing).
4. Isolate SIMD intrinsics in dedicated modules.
5. Add parity tests comparing scalar vs SIMD outputs.
6. Keep deterministic fallback for runtimes without SIMD support.

## Common Pitfalls

- Relying on alignment assumptions not guaranteed by allocator.
- Divergence between scalar and SIMD code paths.
- Hidden precision differences in floating-point reductions.
- SIMD path leaking into non-feature-gated builds.

## Pointers

- Feature tracking: `https://webassembly.org/features/`
- Memory/runtime semantics: `references/spec-map.md`
