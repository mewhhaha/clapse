# Predictable Packing and Unpacking

## Use When

Use this when defining binary formats for language values in Wasm memory that JS and other hosts must decode predictably.

## Binary Layout Rules

1. Fix endianness to little-endian for all multi-byte fields.
2. Version every top-level packed payload.
3. Include explicit lengths for variable-size data.
4. Use explicit tag fields for unions/enums.
5. Reserve bits/bytes for forward compatibility.

## Recommended Header Pattern

- magic/version
- total byte length
- payload kind tag
- flags/reserved

Then payload body with documented offsets.

## Implementation Sketch

1. Write a single schema doc for offsets and types.
2. Implement encoder/decoder once per language runtime.
3. Keep JS decode code on `DataView` with explicit little-endian reads.
4. Validate lengths and tags before reading payload fields.
5. Fuzz malformed buffers to verify robust failure handling.

## Common Pitfalls

- Depending on host struct packing or compiler layout defaults.
- Omitting schema versioning.
- Trusting unbounded lengths from untrusted input.
- Breaking compatibility without bumping layout version.

## Pointers

- JS marshalling workflows: `references/recipes/javascript-data-marshalling.md`
- Module ABI/versioning: `references/recipes/modules-and-linking.md`
