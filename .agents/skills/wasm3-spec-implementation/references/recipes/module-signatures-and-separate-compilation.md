# Module Signatures and Separate Compilation

## Use When

Use this when compiling modules independently while preserving strong typing, proof/evidence contracts, and deterministic rebuild behavior.

## Signature Contents

For each module, publish a signature/interface artifact with:

- exported term/type declarations
- visibility metadata
- proof/evidence exports (if allowed by policy)
- ABI layout/version hashes
- dependency signature hashes

## Implementation Sketch

1. Typecheck module and produce canonical signature.
2. Hash signature and embed hash in build metadata.
3. Compile dependents against signatures, not full source.
4. Trigger rebuild only when relevant signature changes.
5. Revalidate ABI/evidence compatibility at link/load time.

## Common Pitfalls

- signature not capturing all layout-relevant changes
- non-deterministic pretty-print causing hash churn
- leaking private proof internals into public signature
- separate compilation bypassing totality/proof obligations

## Pointers

- Module contracts: `references/recipes/cross-module-state-and-contracts.md`
- Proof/evidence transport: `references/recipes/type-proofs-and-evidence.md`
