# Switch Statements

## Use When

Use this when lowering multi-branch dispatch (`switch`, `match` over integer tags, opcode dispatch).

## Wasm Mapping

- dense integer cases -> `br_table`-based dispatch
- sparse cases -> chain of `if/else` or jump-table with remapping
- default case -> default `br_table` target

`br_table` targets labels, so model each case as a labeled block.

## Implementation Sketch

1. Normalize case labels and detect density.
2. For dense ranges, emit nested blocks + `br_table`.
3. For sparse ranges, choose compare-chain or remapped table.
4. Lower each case body with explicit fallthrough policy.
5. Add tests for default path, missing case, and large case counts.

## Common Pitfalls

- Incorrect label depth mapping in `br_table`.
- Unintended fallthrough when source language forbids it.
- Case value truncation/sign issues.
- Forgetting default behavior for out-of-range values.

## Pointers

- Instruction and control semantics: `references/spec-map.md`
- Conformance loop for edge cases: `references/conformance-workflow.md`
