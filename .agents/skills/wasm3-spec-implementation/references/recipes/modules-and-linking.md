# Modules and Linking

## Use When

Use this when language packages map to multiple Wasm modules, or when you need plugin/dynamic loading behavior.

## Wasm Building Blocks

- Imports/exports for cross-module boundaries
- Tables/memory/globals as shared boundaries
- Host loader policy for resolving dependencies

## Implementation Sketch

1. Define module ABI contract:
- symbol naming
- type signatures
- memory/table ownership
2. Decide linking model:
- static composition at build time
- dynamic resolution at load time
3. Generate import/export manifests from compiler metadata.
4. Validate linked graph before instantiation.
5. Add versioning to avoid silent ABI breakage.

## Common Pitfalls

- Unstable symbol mangling across compiler versions.
- Shared memory assumptions that differ per module.
- Cyclic dependencies without explicit init ordering.
- Missing validation for plugin-provided function signatures.

## Pointers

- Module instantiation semantics: `references/spec-map.md`
- Host boundary strategy: `references/wasm3-notes.md`
- Architecture planning: `references/recipes/module-architecture.md`
- Lazy split/load strategy: `references/recipes/lazy-module-loading.md`
