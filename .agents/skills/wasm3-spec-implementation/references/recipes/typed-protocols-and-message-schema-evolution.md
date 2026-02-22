# Typed Protocols and Message Schema Evolution

## Use When

Use this when actors/services exchange messages across modules or versions and you need forward/backward compatibility rules.

## Core Policy

Treat every message schema as a versioned contract with explicit compatibility guarantees.

## Implementation Sketch

1. Define protocol ids and message type ids.
2. Version message schemas (`major.minor` or compatible hash policy).
3. Encode messages with explicit tag/version/length fields.
4. Generate encoder/decoder from typed schema definitions.
5. Validate protocol compatibility at startup/link time.
6. Add adapters for minor-version compatibility paths.

## Compatibility Rules

- major change: incompatible by default
- minor change: additive fields with defaults only
- field removals: retain decode fallback for deprecation window

## Common Pitfalls

- relying on implicit field order without schema version
- evolving enums without unknown-tag handling
- accepting incompatible messages silently
- forgetting to update property tests with new schema versions

## Pointers

- Packing rules: `references/recipes/packing-and-unpacking.md`
- Cross-module contracts: `references/recipes/cross-module-state-and-contracts.md`
