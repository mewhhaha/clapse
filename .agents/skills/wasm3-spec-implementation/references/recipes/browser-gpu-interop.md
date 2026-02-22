# Browser GPU Interop (WebGPU/WebGL)

## Use When

Use this when a Wasm-first language/runtime needs to send data to the browser GPU pipeline (compute or rendering) and read results back safely.

## Interop Targets

- WebGPU: preferred for modern compute/render workflows
- WebGL2: fallback for wider compatibility
- Canvas2D: presentation path for CPU-generated RGBA buffers

## Data Flow Patterns

1. Upload path:
- Wasm linear memory -> JS typed view -> GPU buffer/texture upload

2. Compute path:
- GPU storage buffer/texture -> compute pass -> optional readback buffer

3. Present path:
- GPU render target -> canvas context/swapchain
- or Wasm RGBA buffer -> `ImageData`/`putImageData`

## Implementation Sketch

1. Define one canonical binary layout for GPU payloads:
- struct alignment
- stride/padding
- endianness assumptions
2. Keep resource ownership explicit:
- who allocates/frees buffers/textures
- lifetime tied to frame/pass or retained cache
3. Minimize copies:
- batch uploads
- reuse staging buffers
- avoid readbacks on hot paths
4. Recreate JS typed-array views after `memory.grow`.
5. Add capability probing:
- WebGPU available?
- required limits/features?
- fallback to WebGL/CPU path when unavailable

## Browser-Specific Pitfalls

- mismatched WGSL/GLSL layout vs Wasm-packed structs
- frequent GPU readbacks causing pipeline stalls
- recreating GPU resources every frame instead of pooling
- stale views into Wasm memory after growth
- assuming all browsers expose identical GPU limits/features

## Validation and Debugging

1. Keep scalar/CPU reference implementation for parity checks.
2. Add frame-level checksums for deterministic render tests where feasible.
3. Log selected backend/features in diagnostics.
4. Test fallback behavior explicitly (WebGPU off, WebGL off, CPU-only).

## Pointers

- Canvas linear-memory layout: `references/recipes/canvas-memory-layout.md`
- JS interop rules: `references/recipes/javascript-interop.md`
- Packing/version discipline: `references/recipes/packing-and-unpacking.md`
- SIMD hot-path considerations: `references/recipes/simd-and-data-parallelism.md`
