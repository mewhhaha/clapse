# Self-host Pain Points

This page tracks friction discovered while building the Clapse compiler kernel
in Clapse itself. The goal is to make missing language/tooling features explicit
so we can close them intentionally.

## Current pain points (observed)

1. Parser fragility for multiline expression forms (partially addressed).

- Patterns that repeatedly failed in kernel code:
  - nested `_ -> case ... of` chains (fixed in parser + tests)
  - multiline parenthesized function application (fixed in parser + tests)
  - long `= case ... of` inline declarations in some contexts
- Impact:
  - simple byte/JSON scanner logic became disproportionately hard to express.

2. Relational operator support landed; keep parity coverage.

- Builtins now include `<`, `<=`, `>`, `>=` (`lt`, `le`, `gt`, `ge`).
- Remaining work:
  - keep parser/type/collapse/wasm parity tests green as syntax evolves.

3. No ergonomic byte-slice search helpers.

- For protocol parsing we need stable primitives like:
  - find byte
  - find subsequence
  - bounded scan
- Impact:
  - JSON command detection currently relies on positional assumptions.

4. No lightweight JSON utilities in pure core.

- Current kernel returns static JSON payloads by writing byte constants.
- Impact:
  - difficult to implement robust request parsing and dynamic response
    generation.

5. Formatter/parser mismatch risk for complex layout.

- Some syntactic shapes that look reasonable still failed parse.
- Impact:
  - formatter + parser need stricter shared contracts for multiline expression
    layout.

## Haskell-first implementations that are sensible now

These are good bootstrap-first changes in Haskell so Clapse sources can express
compiler logic cleanly, then we can port/replace in Clapse later.

1. Parser robustness pass for multiline expression grammar.

- Accept nested `case` and multiline function application forms consistently.
  - status: implemented in parser coalescing + regression tests.
- Keep extending regression tests using phase-9-style kernel snippets.

2. Keep relational operator coverage as a regression gate.

- Ensure parser + type + lowering + wasm tests remain in parity corpus.

3. Add pure slice scanning builtins (or keep as stdlib if sufficient).

- Implemented now as pure module helpers in `examples/util/slice_scan.clapse`:
  - `slice_find_u8 bytes needle start -> index_or_minus1`
  - `slice_find_seq_u8 haystack needle start -> index_or_minus1`
- Remaining decision:
  - promote these to backend intrinsics only if profiling proves a hotspot.

4. Add minimal JSON helper surface (pure).

- Candidate helpers:
  - `json_get_command_tag request_slice -> i64` (or equivalent)
  - `json_escape_string_to_slice input -> slice`
- This can start as constrained helpers for compiler ABI traffic only.

5. Add formatter/parser conformance tests.

- A dedicated corpus for kernel-like code:
  - nested case
  - multiline parenthesized calls
  - long chained let bindings
  - status: implemented via test roundtrip coverage and
    `examples/parser_layout_pain_points.clapse` in parser/formatter corpus gates.

## Rule for new pain points

When we hit a new blocker while self-hosting:

1. add it here with a minimal reproducer,
2. classify as `language`, `tooling`, or `runtime`,
3. decide whether to do a Haskell-first bootstrap fix or a Clapse-side
   workaround,
4. add/adjust tests before moving on.

## Native compiler pre-ship pain points (2026-02-24)

1. `runtime`: phase9 JSON payload assembly is fragile for dynamic strings.

- Symptom:
  - native `selfhost-artifacts`/`format` responses can produce invalid JSON when
    request source contains quotes/escapes/newlines.
- Root cause:
  - manual byte-copy of raw source into quoted JSON fields without escaping.
- Current mitigation:
  - strict JSON escape path is gated in self-host checks before release.
- Ship blocker to close:
  - native JSON escaping helper (`json_escape_string_to_slice`) and parity tests
    for quote/backslash/newline-heavy inputs.

2. `runtime`: recursive request scanning/copy in phase9 can overflow stack.

- Symptom:
  - `Maximum call stack size exceeded` on native path for some corpus entries.
- Root cause:
  - deep recursive scanners/copy loops over request payloads.
- Current mitigation:
  - replace recursive request scanning/copy with bounded iterative paths under
    self-host parity gates.
- Ship blocker to close:
  - bounded iterative scanners/copy loops (or recursion depth guards) in native
    compiler kernel.

3. `runtime`: unchecked span math can hit memory OOB.

- Symptom:
  - `memory access out of bounds` in native path and in lexer-like copy loops.
- Root cause:
  - index/span assumptions (`start <= end <= len`) not consistently guarded.
- Current mitigation:
  - local clamping fixes in phase10 lexer fixture.
- Ship blocker to close:
  - standard library/runtime helpers for clamped slicing/copy; avoid ad-hoc span
    math in compiler-kernel code.

4. `tooling`: enforce explicit native engine-mode checks.

- Symptom:
  - strict checks using a generic `wasm` label can hide whether native-only
    validation actually ran.
- Pain:
  - easy to misread gate coverage when mode naming is not explicit.
- Ship blocker to close:
  - use explicit `wasm-native` in strict gates where native-only guarantees are
    required.
- Status update (2026-03-01):
  - boundary normalization is currently enabled for kernel compiler-path compile
    responses to promote ABI-stable wasm output and source-derived artifacts
    while producer-side kernel-native compile semantics are still converging.
  - strict probe verification remains part of `pre-tag-verify`.

5. `language/tooling`: surprising source ergonomics surfaced during parity work.

- Surprises observed:
  - `if2` is strict, so recursive branches that look short-circuiting can still
    blow stack unless rewritten as `case`.
  - `if/then/else` is intentionally unsupported in source; parser now directs
    users to `case`.
  - some nested `case`/`let` multiline forms are still formatting-sensitive.
  - operator syntax like `||` is not universally available unless declared or
    imported via prelude.
- Ship blocker to close:
  - document/normalize canonical formatting and prelude expectations for compiler
    source files, and add regression corpus entries for these shapes.
