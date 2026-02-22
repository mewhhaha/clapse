# Sum Type Layouts and Optimizations

## Use When

Use this when encoding enums/ADTs like `Either a b`, `Option a`, `Result e a`, or larger recursive variants in linear memory.

## Baseline Tagged Union Layout

Default representation:

- fixed header with `tag`
- payload region large enough for max constructor payload

Example shape for `Either a b`:

- `tag = 0` => `Left a`
- `tag = 1` => `Right b`
- payload interpreted by tag-specific field layout metadata

This is simple, stable, and easy to validate.

## Faster/Specialized Layout Patterns

1. Niche optimization:
- encode one constructor in invalid value space of another
- example: nullable pointer => `None` without explicit tag byte

2. Tag-in-pointer (aligned pointers only):
- use low pointer bits as constructor tag
- keep fallback for unaligned or memory64 cases

3. Split variant pools:
- allocate each constructor in separate arena/pool
- tag known from pool identity for hot-path scans

4. Small-inline / large-boxed:
- small payload constructors inline
- large payload constructors store pointer to secondary block

5. Array-of-variants sidecar tags:
- tags in one byte array
- payloads in constructor-specific arrays (SoA-like)
- strong for SIMD/filter workloads

## Advanced Type Example

For recursive types like:

`Tree a = Leaf a | Node (Tree a) (Tree a)`

use one of:

- classic tagged node blocks (simple, general)
- separate `Leaf` and `Node` pools (better branch prediction/cache locality)
- index-based arena handles instead of raw pointers (easier compaction/relocation)

## Accessor Generation Rules

1. Generate constructor checks before field projection.
2. Generate total pattern-match dispatch from tag metadata.
3. Keep field offsets in one generated table per monomorphized type.
4. Reject stale layout versions at module boundaries.

## "Better Than Existing Languages" Opportunities

- make layout a first-class language feature (opt-in annotations per ADT)
- allow profile-guided constructor reordering and pool selection
- combine uniqueness proofs with in-place constructor transforms
- emit multiple layout variants and choose by target profile

Keep semantic behavior identical across layout variants.
Keep variant selection deterministic (target/profile id + ABI version), and publish the selected variant in module metadata.

## Common Pitfalls

- layout explosion from over-specializing every generic instantiation
- unsound niche assumptions when value domains evolve
- pointer-tag tricks conflicting with moving GC/FFI pinning
- forgetting debug/introspection support for custom layouts

## Pointers

- Base ADT guidance: `references/recipes/algebraic-data-types.md`
- Memory creation/access patterns: `references/recipes/linear-memory-data-creation-and-access.md`
- GC movement constraints: `references/recipes/moving-vs-nonmoving-heap-policy.md`
