# Canvas Memory Layout

## Use When

Use this when Wasm renders pixels and JavaScript uploads them to `CanvasRenderingContext2D` or WebGL/WebGPU pipelines.

## 2D Canvas Baseline Format

For `ImageData`, use:

- row-major order
- 4 bytes per pixel
- channel order RGBA
- top-left origin unless your pipeline states otherwise

Canonical offset:

`offset = ((y * width) + x) * 4`

## Implementation Sketch

1. Allocate contiguous pixel buffer in Wasm memory.
2. Write RGBA bytes in deterministic row-major order.
3. Expose pointer + width + height to JS.
4. In JS, create `Uint8ClampedArray(memory.buffer, ptr, width * height * 4)`.
5. Wrap in `ImageData` and blit with `putImageData` (or upload as texture).
6. Recreate typed-array view after any `memory.grow`.

## Packing Rules for Predictable Uploads

- Fix stride to `width * 4` unless explicit padding is documented.
- Document alpha convention (straight vs premultiplied).
- Document color space assumptions and conversion policy.
- Keep one canonical pixel format and convert at boundaries only.

## Common Pitfalls

- Accidentally writing BGRA while JS expects RGBA.
- Flipped Y axis between render backend and canvas presentation.
- Reusing stale views after memory growth.
- Writing out-of-bounds when width/height mismatch buffer length.

## Pointers

- Generic interop and ownership: `references/recipes/javascript-interop.md`
- Packing details: `references/recipes/packing-and-unpacking.md`
- Browser GPU integration: `references/recipes/browser-gpu-interop.md`
- SIMD optimization path: `references/recipes/simd-and-data-parallelism.md`
