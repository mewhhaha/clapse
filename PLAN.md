# Clapse Self-Hosting Plan

## Goal

Make `clapse` fully self-hosted for:

1. compiler
2. formatter
3. LSP

Meaning:
- core logic is implemented in Clapse source
- compiled to wasm
- hosted by Deno for CLI/stdio/filesystem/process boundary IO
- Haskell implementation remains bootstrap-only fallback until final cutover

## Non-goals

- removing Deno host runtime (host IO shell is intentional)
- adding non-wasm backends
- broad language expansion unrelated to self-hosting blockers

## Current baseline (as of now)

- `compile`, `format`, `lsp` routes exist through wasm host wrappers.
- Phase fixtures exist through `bootstrap_phase9_compiler_kernel`.
- parser/lowering/collapse/wasm pipelines are still primarily Haskell.
- Clapse-source utility modules are now present for:
  - slice scanning
  - string/slice bridge usage
- semantic fixes landed:
  - relational operator support
  - char literals
  - `newtype`
  - wasm `str_eq` content equality

## Architecture target

### Runtime boundary

- Deno host handles:
  - CLI args
  - stdio for LSP transport
  - file reads/writes
  - process exit codes
- Clapse wasm handles:
  - parse + type + rewrite + lower + collapse + wasm emit
  - formatting
  - LSP document state transforms and diagnostics payload generation

### ABI contract

- single exported entrypoint:
  - `clapse_run(request_slice_handle: i32) -> response_slice_handle: i32`
- UTF-8 JSON request/response over slice descriptors
- stable commands:
  - `compile`
  - `format`
  - `lsp`
  - `selfhost-artifacts`
  - `engine-mode`

## Workstreams

## 1) Core language completeness for self-hosting

### 1.1 Must-have semantics

- keep purity and deterministic lowering
- avoid one-off compiler-only semantics
- features needed for compiler code readability/perf:
  - `newtype` (done)
  - char literals (done)
  - string/slice bridge (done)
  - slice scanning primitives (`stdlib` now, intrinsic only if needed)
  - robust pattern/case layout handling

### 1.2 Remaining language blockers (candidate list)

- parser robustness for deeply nested multiline `case`/`let` in compiler kernels
- stronger sequence/record ergonomics as needed by parser and LSP code
- predictable module import behavior for deeply nested compiler modules

Acceptance:
- all compiler-in-clapse source files parse/format roundtrip cleanly
- no fallback rewrite of logic into unnatural forms just to satisfy parser quirks

## 2) Clapse compiler implementation parity

### 2.1 Frontend parity

- Clapse parser in Clapse:
  - module/import/export
  - attributes
  - data/newtype
  - classes/laws/instances
  - operators/backticks/guards/case
- Clapse type inference in Clapse:
  - builtin signatures
  - constructor helpers
  - string/slice bridge
- rewrite normalization parity:
  - trait/class-law-instance derivation compatibility

Acceptance:
- AST/type/rewrite artifacts equivalent to Haskell (allow alpha renaming/order rules)

### 2.2 Lowering/collapse parity

- Lowering IR generation in Clapse
- Collapse pipeline in Clapse:
  - currying normalize
  - inline/specialize
  - prune
  - tail-call pass
  - region/slice ownership passes
- Verifier parity in Clapse

Acceptance:
- collapsed IR equivalence on selfhost corpus
- behavior parity on behavior corpus

### 2.3 Wasm emit parity

- Wasm section/type/import/export/code/data emission in Clapse
- inline runtime ops:
  - closures
  - structs
  - slices/region/memcpy/memset
  - string descriptor and content equality
- deterministic output mode for reproducible diffing

Acceptance:
- wasm module validates and runs for full corpus
- export/import/runtime contract unchanged

## 3) Formatter implementation in Clapse

### 3.1 Parser-aware formatter core

- stable normalization rules:
  - let layout (Haskell-like)
  - case multiline preference
  - guard alignment
  - whitespace collapse
  - operator spacing

### 3.2 Roundtrip guarantees

- parse(format(src)) must succeed
- format(format(src)) idempotent

Acceptance:
- formatter corpus green
- no destructive behavior (no declaration loss)

## 4) LSP implementation in Clapse

### 4.1 Protocol scope (phase 1)

- initialize/shutdown
- didOpen/didChange/didClose
- diagnostics
- hover
- go-to definition (if symbol tables ready)

### 4.2 Protocol scope (phase 2)

- completion
- formatting request integration
- semantic tokens (as available)
- inlay hints policy:
  - no noisy signature-inline hints
  - keep hover as primary type surface

Acceptance:
- editor workflows stable in Helix
- latency acceptable for medium files

## 5) Host bridge hardening (Deno)

### 5.1 Strict boundary

- no hidden host fallback for core compile/format/lsp once cutover flag is enabled
- explicit fallback mode only for bootstrap/debug

### 5.2 Typed request/response

- versioned JSON payload schemas
- robust error contracts (structured errors, codes, spans)

Acceptance:
- host wrappers are thin transport shims only

## 6) Treesitter + DX parity

### 6.1 Grammar/query coverage

- parser (done baseline)
- highlights (improve operators/keywords consistency)
- textobjects
- indent
- tags
- rainbow

### 6.2 Helix integration

- `just install` provisions grammar + query + lsp wiring
- highlight and formatting behavior tested on example corpus

Acceptance:
- “open file and code” experience works without manual setup

## 7) Performance and optimization bars

### 7.1 Correctness before speed

- parity gates must pass first

### 7.2 Performance targets

- compiler throughput:
  - compare Haskell vs Clapse-wasm on fixed corpus
- runtime throughput:
  - abstraction-vs-handwritten bars remain within accepted bounds
- no major regressions from bridge abstractions

### 7.3 Measurement commands

- keep `selfhost-bench`, `selfhost-diff`, behavior diff in gate chain
- include median-based reporting

Acceptance:
- documented performance deltas and no unexplained regressions

## 8) Testing strategy

### 8.1 Gate layers

1. parser/type/lowering unit tests
2. collapse verifier/property-like checks
3. wasm compile/run behavior diff
4. selfhost artifact diff
5. formatter idempotence corpus
6. LSP request-response fixtures

### 8.2 Corpus sets

- bootstrap phases
- language examples
- selfhost corpus manifest
- LSP/formatter regression fixtures

### 8.3 Definition of done per feature

- feature test added
- parity diff remains green
- docs updated in same change

## 9) Milestone plan (execution order)

## Milestone A: Language + parser stability for compiler source

- close parser/layout blockers from pain-point list
- ensure compiler Clapse modules parse/format robustly
- keep no special runtime semantics

Exit criteria:
- compiler source modules compile without workaround syntax contortions

## Milestone B: Compiler frontend parity in Clapse

- parser + type + rewrite in Clapse
- artifact diff against Haskell for corpus

Exit criteria:
- AST/type/rewrite parity accepted

## Milestone C: Lowering/collapse parity in Clapse

- lower + collapse + verifier in Clapse

Exit criteria:
- collapsed IR parity accepted

## Milestone D: Wasm emit parity in Clapse

- wasm emitter in Clapse

Exit criteria:
- behavior parity and wasm validation green

## Milestone E: Formatter/LSP parity in Clapse

- formatter logic in Clapse
- LSP analysis/rendering in Clapse

Exit criteria:
- editor-facing parity + latency acceptable

## Milestone F: Cutover

- default all commands to Clapse-wasm engine
- keep explicit fallback only for bootstrap emergency

Exit criteria:
- release checklist green

## 10) Cutover checklist

- `just install` green
- `cabal test` green
- selfhost diff green
- behavior diff green
- benchmark report generated
- formatter idempotence corpus green
- LSP fixture suite green
- Helix smoke tested
- docs updated:
  - README
  - roadmap
  - syntax/runtime references

## 11) Risk register

1. Parser fragility in large compiler modules
- Mitigation: dedicated parser/formatter corpus and fast regression fixtures

2. Hidden semantic drift between Haskell and Clapse engines
- Mitigation: artifact + behavior diff gates required on every milestone

3. Performance regressions in wasm compiler engine
- Mitigation: median benchmark gates and per-pass profiling snapshots

4. LSP responsiveness degradation
- Mitigation: incremental state model + request budget measurements

5. Host boundary complexity creep
- Mitigation: strict “thin bridge only” policy and schema contracts

## 12) Immediate next queue

1. DONE: Add first-class `slice_eq_u8` builtin (`slice byte -> slice byte -> i64`) as inline-wasm bytewise equality and simplify `string_slice` module to use it.
2. DONE: Port and lock parser-layout pain-point fixtures from compiler kernels (`test/Main.hs` parser regressions for nested case chains, multiline parenthesized application, and inline case declarations).
3. DONE: Start Clapse-source parser module parity harness against Haskell parser artifacts (`scripts/selfhost-parser-parity.mjs`, manifest `examples/selfhost_parser_corpus.txt`).
4. DONE: Add formatter idempotence corpus gate for compiler-source files (`scripts/formatter-idempotence-corpus.mjs`, manifest `examples/compiler_source_corpus.txt`).
5. DONE: Add initial Clapse-source LSP diagnostics/hover fixture runner through wasm ABI (`scripts/lsp-wasm-fixtures.mjs`, fixtures `examples/lsp_wasm_fixtures.json`).
6. DONE: Add parser-combinator pilot support for top-level `#[...]` attribute lines and lock regression coverage with `examples/parser_attribute_pain_points.clapse` in parser/formatter corpora.
7. DONE: Add parser-combinator pilot support for guarded function declaration shape parsing (`name ... | ... = ...`) and malformed-guard rejection fixture in phase11 sample checks.
8. DONE: Split phase11 declaration artifact function tags into plain vs guarded forms and propagate new counters through stats/stream hashing.
9. DONE: Add parser-combinator pilot support for multiline plain function declaration blocks (`name ... =` followed by indented RHS lines) and wire it through tagged/untagged phase11 declaration parsing.
10. DONE: Add parser-combinator pilot support for indented guarded continuation lines (`| ... = ...`) and lock malformed continuation rejection in phase11 sample checks.
11. DONE: Add parser-combinator pilot support for module-optional declaration artifact entrypoints (`parse_program_decl_*` paths) with positive fixture coverage for import-first sources.
12. DONE: Introduce explicit `Bool`/`MaybePos` adapters in phase11 parser entrypoints and replace direct `p == -1` sentinel branching in `parse_program_decl_*` + final sample guards.
13. DONE: Switch phase11 optional-position encoding to canonical polymorphic `data Maybe a = Just a | Nothing` and remove custom `MaybePos` ADT.
14. DONE: Add shared phase11 parser helpers (`parse_function_head_m`, `parse_function_head_eq_m`, `parse_decl_line_m`, `parse_line_from_payload_m`) and refactor major dispatchers to `Maybe` composition (`maybe_bind`/`maybe_or`).
