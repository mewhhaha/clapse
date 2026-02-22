# Generational GC Write Barriers

## Use When

Use this when adding generational or incremental GC where old-to-young references must be tracked.

## Core Invariant

If an old object gains a reference to a young object, record that mutation before next young-generation collection.

## Implementation Sketch

1. Choose remembered-set strategy:
- card marking
- object remembered sets
2. Insert write barriers on reference field updates.
3. Keep barrier fast path minimal and branch-predictable.
4. Verify barrier insertion survives optimization passes.
5. Add stress tests that mutate old objects with young references.

## Common Pitfalls

- Missing barrier on one mutation opcode/path.
- Barrier applied to non-reference writes causing overhead.
- Remembered-set corruption under concurrent mutation.
- Collector assumptions drifting from emitted barrier protocol.

## Pointers

- Baseline collector: `references/recipes/garbage-collection.md`
- Heap movement policy: `references/recipes/moving-vs-nonmoving-heap-policy.md`
