# JavaScript Data Marshalling

## Use When

Use this when moving structured data between JS and Wasm (strings, arrays, structs, tagged values).

## Preferred Data Shapes

1. Scalars:
- i32/f32/f64 as plain numeric parameters
- i64 as BigInt with explicit API contract

2. Buffers:
- pointer + length pairs into Wasm memory
- JS side uses `Uint8Array`/`DataView` over `memory.buffer`

3. Strings:
- UTF-8 bytes in memory
- decode/encode via `TextDecoder`/`TextEncoder`

4. Structs/ADTs:
- fixed binary layout with explicit tags and field offsets
- avoid ad-hoc JSON in hot paths

## Implementation Sketch

1. Standardize marshaling helpers in one runtime module.
2. Make ownership explicit (`borrow`, `copy`, or `move`).
3. Return result values via:
- direct scalar returns
- out-pointer buffers
- tagged error/result objects in memory
4. Add round-trip tests JS -> Wasm -> JS for each type family.

## Common Pitfalls

- Implicit JS string conversions on hot paths.
- Inconsistent signedness between JS and Wasm interpretation.
- Reading structs with incorrect offset/alignment assumptions.
- Not validating input lengths before decode/parse.

## Pointers

- String layout: `references/recipes/strings-and-memory.md`
- ABI layout discipline: `references/language-to-wasm-playbook.md`
