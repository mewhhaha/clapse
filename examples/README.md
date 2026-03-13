# Examples

Simple sample programs for the current Clap syntax.

Default source style is operator-first (`+ - * / == &&`). Custom operator
declarations extend or override builtin operator tokens. Any function can also
be used as a backtick operator without a declaration (for example
``a `mod` b``).

## Files

- `identity.clap`: identity and apply basics
- `currying.clap`: partial application and saturated calls
- `closures.clap`: closure creation and use
- `signatures_and_collections.clap`: function signatures with optional named
  witness constraints and `[]` collection literals
- `collection_literal_targets.clap`: class-dispatched `[]` literals targeting
  `List`, `Vec`, `Seq`, and `Array` via `CollectionLiteral` instances
- `let_bindings.clap`: local `let ... in ...` bindings inside function bodies
- `case_of.clap`: `case ... of` expression matching over multiple values and
  constructor patterns
- `http_request_parser.clap`: fixed-width numeric HTTP request parsing
  pipeline with data + let bindings
- `recursion.clap`: tail-recursive and non-tail recursive patterns
- `operators.clap`: custom infix operators with precedence/backticks (on top
  of builtin operators)
- `traits.clap`: builtin trait-oriented expression patterns
- `class_arithmetic_rewrites.clap`: compile-time class/law/instance rewrites
  for `add`/`sub`/`mul`/`div`
- `class_algebra_rewrites.clap`: compile-time class/law/instance rewrites for
  monoid/functor/applicative/monad
- `traits_ord_slice.clap`: compile-time class/law/instance rewrites for
  `ord` and `slice` kinds (`lt/le/gt/ge` + slice method surface)
- `monads_maybe_either.clap`: `Maybe` and `Either` ADT constructors plus
  law-driven monad instances using HKT-style class/instance declarations
- `data.clap`: parametric `data` declaration with constructor + let-pattern
  deconstruction
- `strings.clap`: static string literal compile + runtime rendering
- `interop_slice.clap`: byte-slice interop (`slice_len`, `slice_get_u8`, `slice_eq_u8`, `slice_set_u8`, `str_to_slice`, `slice_to_string`)
- `util/slice_scan.clap`: pure byte-slice scanning helpers
  (`slice_find_u8`, `slice_find_seq_u8`)
- `util/string_slice.clap`: pure bytewise slice/string comparison helpers
  (`slice_eq_u8` builtin, `slice_eq_str`)
- `util/json.clap`: tiny pure byte-slice JSON command marker helper module
- `interop_slice.mjs`: Deno/Node-compatible runner showing `Uint8Array` ->
  Clap slice descriptor (`ptr,len`) interop
- `wasm_linear_memory_helpers.clap`: low-level linear-memory helper usage
  (`slice_new_u8`, `slice_data_ptr`, `slice_len_raw`, `region_*`, `memcpy_u8`,
  `memset_u8`) with explicit dataflow threading to avoid collapse-time dead-code
  elimination
- `game_of_life.clap`: wasm Life rule + pure `LifeState` plus
  `LifeEvent`/`apply_event` transition API over slice descriptors with per-step
  `region_mark`/`region_reset` cleanup
- `game_of_life.html`: browser canvas demo shell
- `game_of_life.mjs`: browser engine loop where JS only dispatches events
  (`event_tick`, `event_toggle`, `event_clear`, `event_load`) and renders
  wasm-owned state
- `mario_ecs.clap`: tiny ECS-like pure state machine (`kinds/xs/lanes/active`
  slices) with event-driven stepping
- `mario_ecs.html`: browser canvas shell for the Mario-like ECS demo
- `mario_ecs.mjs`: browser render/input glue that dispatches `event_tick` /
  `event_reset` into wasm
- `assets/sprite_regions.md`: descriptive sprite-sheet region notes used by the
  Mario-like demo atlas
- `wasm_main.clap`: simple program for wasm compile + node execution
- `wasm_closure.clap`: closure + currying wasm smoke input
- `bench_wasm_hand.clap`: direct numeric-expression wasm benchmark fixture
- `bench_wasm_abstraction.clap`: abstraction-heavy equivalent numeric wasm
  benchmark fixture
- `bench_wasm_closure_env_hand.clap`: direct closure/environment-warmup wasm
  benchmark fixture
- `bench_wasm_closure_env_abstraction.clap`: closure/environment abstraction
  wasm benchmark fixture
- `bench_wasm_struct_field_hand.clap`: direct struct-field-free wasm benchmark
  fixture
- `bench_wasm_struct_field_abstraction.clap`: struct-field abstraction wasm
  benchmark fixture
- `bench_wasm_wrapper_uncurry_hand.clap`: direct wrapper/uncurry-free wasm
  benchmark fixture
- `bench_wasm_wrapper_uncurry_abstraction.clap`: wrapper/uncurry abstraction
  wasm benchmark fixture
- `bench_wasm_slice_set_reuse.clap`: non-aliased `slice_set_u8` reuse
  benchmark fixture
- `bench_wasm_slice_set_copy.clap`: aliased `slice_set_u8` copy-path benchmark
  fixture
- `wasm_struct_has_tag.clap`: low-level struct helper fixture for tagged
  `__is_*`
- `wasm_struct_has_tag_false.clap`: low-level struct helper fixture for
  false-path tagged `__is_*`
- `wasm_struct_get_ok.clap`: low-level struct helper fixture for matching
  `__get_*`
- `wasm_struct_get_mismatch.clap`: low-level struct helper fixture for
  mismatched-tag `__get_*` trap checks
- `bootstrap_phase1_frontend_primitives.clap`: phase 1 self-host bootstrap
  fixture (ADT + pattern/case frontend primitives)
- `bootstrap_phase2_core_data_structures.clap`: phase 2 self-host bootstrap
  fixture (recursive `List` with `Nil`/`Cons`)
- `bootstrap_phase3_entry.clap`: phase 3 self-host bootstrap entry fixture
  (module graph import/export)
- `bootstrap_phase4_parser_pilot.clap`: phase 4 parser pilot for
  assignment-like byte-slice recognition
- `bootstrap_phase5_dispatch_pilot.clap`: phase 5 dispatch pilot for enum-code
  decode + ADT route dispatch
- `bootstrap_phase6_entry.clap` + `bootstrap6/router.clap`: phase 6
  moduleized decode/dispatch bootstrap fixture
- `bootstrap_phase7_host_capability_pilot.clap`: phase 7 host capability
  import compile pilot (`import host.time`)
- `bootstrap_phase8_pattern_and_operators.clap`: phase 8 syntax/behavior pilot
  (guards + operators + constructor-pattern case)
- `lib/compiler/kernel.clap`: phase 9 compiler ABI kernel pilot
  (`clap_run` command dispatch for `compile`/`format`/`selfhost-artifacts`)
- `bootstrap_phase10_frontend_lexer.clap`: phase 10 lexer pilot
  (token classes + keyword recognizer)
- `bootstrap_phase11_parser_combinator_pilot.clap`: phase 11 parser-combinator
  pilot (`>>=`/`<$>`/`<*>`/`<|>` + `many`/`some`/`sepBy` for top-level
  declaration-shape parsing)
- `parser_layout_pain_points.clap`: parser/formatter conformance fixture for
  nested case chains, multiline parenthesized application, and long chained let
  layout
- `util/math.clap` + `util/base.clap`: phase 3 transitive module graph
  fixtures
- `selfhost_corpus.txt`: manifest used by self-host differential artifact/parity
  gates
- `selfhost_parser_corpus.txt`: manifest used by parser artifact parity harness
  (`merged_module.txt` parity)
- `selfhost_behavior_corpus.json`: manifest used by self-host behavior
  differential wasm execution gates
  - also used by `scripts/selfhost-compile-strategy-report.mjs` to census
    public compile-surface `compile_strategy` usage over deduplicated
    `(entry, export)` pairs
- `compiler_source_corpus.txt`: manifest used by formatter idempotence gate for
  compiler-source files
- `lsp_wasm_fixtures.json`: diagnostics/hover request fixtures for wasm LSP
  runner
- `../scripts/check-selfhost-manifests.mjs`: manifest consistency guard for
  parity coverage drift
  - intentionally excludes `examples/traits.clap` (non-compiling trait-catalog
    fixture)
- `../scripts/selfhost-bench.mjs`: corpus-level parity benchmark (compile+run
  timing per engine)

## Formatting

Format a file to stdout:

```bash
deno run -A scripts/clap.mjs format examples/currying.clap
```

Format in place:

```bash
deno run -A scripts/clap.mjs format --write examples/currying.clap
```

## Compile and run (WASM)

```bash
deno run -A scripts/clap.mjs compile examples/wasm_main.clap out/wasm_main.wasm
deno run -A scripts/run-wasm.mjs out/wasm_main.wasm main 7
```

Each `compile` also emits `out/<name>.d.ts` based on collapsed IR export arity.

Closure/currying compile + run:

```bash
deno run -A scripts/clap.mjs compile examples/wasm_closure.clap out/wasm_closure.wasm
deno run -A scripts/run-wasm.mjs out/wasm_closure.wasm main 7
```

Data constructor/deconstruction compile + run:

```bash
deno run -A scripts/clap.mjs compile examples/data.clap out/wasm_data.wasm
deno run -A scripts/run-wasm.mjs out/wasm_data.wasm main 7
```

String literal compile + run:

```bash
deno run -A scripts/clap.mjs compile examples/strings.clap out/wasm_strings.wasm
deno run -A scripts/run-wasm.mjs out/wasm_strings.wasm main
```

Low-level struct helper smoke (tagged `__is_*` + tag-safe `__get_*`):

```bash
just wasm-struct-helpers-smoke
```

Linear-memory helper smoke:

```bash
just wasm-linear-memory-helpers-smoke
```

Slice interop compile + run:

```bash
deno run -A scripts/clap.mjs compile examples/interop_slice.clap out/interop_slice.wasm
deno run -A examples/interop_slice.mjs out/interop_slice.wasm
```

Game of Life rule compile + smoke:

```bash
deno run -A scripts/clap.mjs compile examples/game_of_life.clap out/game_of_life.wasm
deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 0 3
deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 1 2
deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 1 4
deno run -A scripts/life-slice-smoke.mjs out/game_of_life.wasm
```

The browser demo uses `apply_event` + `event_*` exports so Clap remains the
source of truth for simulation state.

Mario-like ECS compile + smoke:

```bash
deno run -A scripts/clap.mjs compile examples/mario_ecs.clap out/mario_ecs.wasm
deno run -A scripts/mario-ecs-smoke.mjs out/mario_ecs.wasm
```

The browser demo keeps game logic pure in Clap and treats JS as input/render
boundary only.

HTTP request parser-style compile + run:

```bash
deno run -A scripts/clap.mjs compile examples/http_request_parser.clap out/wasm_http_request_parser.wasm
deno run -A scripts/run-wasm.mjs out/wasm_http_request_parser.wasm main 10203
```

Case expression compile + run:

```bash
deno run -A scripts/clap.mjs compile examples/case_of.clap out/wasm_case_of.wasm
deno run -A scripts/run-wasm.mjs out/wasm_case_of.wasm main 7
```

This example also demonstrates multiline case-arm formatting. For
single-scrutinee constructor matches, the final catch-all arm is optional when
constructor coverage is exhaustive.

Maybe/Either monad compile + run:

```bash
deno run -A scripts/clap.mjs compile examples/monads_maybe_either.clap out/wasm_monads_maybe_either.wasm
deno run -A scripts/run-wasm.mjs out/wasm_monads_maybe_either.wasm main 7
```

## Benchmark (WASM runtime)

Single module benchmark:

```bash
deno run -A scripts/clap.mjs compile examples/wasm_main.clap out/wasm_main.wasm
deno run -A scripts/bench-wasm.mjs out/wasm_main.wasm main 2000000 20000
```

Hand vs abstraction benchmark fixtures:

```bash
deno run -A scripts/clap.mjs compile examples/bench_wasm_hand.clap out/bench_wasm_hand.wasm
deno run -A scripts/clap.mjs compile examples/bench_wasm_abstraction.clap out/bench_wasm_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_abstraction.wasm main 2000000 20000
```

Closure/environment-flattening fixture pair:

```bash
deno run -A scripts/clap.mjs compile examples/bench_wasm_closure_env_hand.clap out/bench_wasm_closure_env_hand.wasm
deno run -A scripts/clap.mjs compile examples/bench_wasm_closure_env_abstraction.clap out/bench_wasm_closure_env_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_closure_env_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_closure_env_abstraction.wasm main 2000000 20000
```

Struct-field-flattening fixture pair:

```bash
deno run -A scripts/clap.mjs compile examples/bench_wasm_struct_field_hand.clap out/bench_wasm_struct_field_hand.wasm
deno run -A scripts/clap.mjs compile examples/bench_wasm_struct_field_abstraction.clap out/bench_wasm_struct_field_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_struct_field_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_struct_field_abstraction.wasm main 2000000 20000
```

Wrapper/uncurrying fixture pair:

```bash
deno run -A scripts/clap.mjs compile examples/bench_wasm_wrapper_uncurry_hand.clap out/bench_wasm_wrapper_uncurry_hand.wasm
deno run -A scripts/clap.mjs compile examples/bench_wasm_wrapper_uncurry_abstraction.clap out/bench_wasm_wrapper_uncurry_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_abstraction.wasm main 2000000 20000
```

Slice ownership fixture pair (reuse vs copy):

```bash
deno run -A scripts/clap.mjs compile examples/bench_wasm_slice_set_reuse.clap out/bench_wasm_slice_set_reuse.wasm
deno run -A scripts/clap.mjs compile examples/bench_wasm_slice_set_copy.clap out/bench_wasm_slice_set_copy.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_slice_set_reuse.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_slice_set_copy.wasm main 2000000 20000
```

Self-host bootstrap phase fixtures:

```bash
deno run -A scripts/clap.mjs compile examples/bootstrap_phase1_frontend_primitives.clap out/bootstrap_phase1.wasm
deno run -A scripts/clap.mjs compile examples/bootstrap_phase2_core_data_structures.clap out/bootstrap_phase2.wasm
deno run -A scripts/clap.mjs compile examples/bootstrap_phase3_entry.clap out/bootstrap_phase3.wasm
deno run -A scripts/clap.mjs compile examples/bootstrap_phase4_parser_pilot.clap out/bootstrap_phase4_parser_pilot.wasm
deno run -A scripts/bootstrap-parser-pilot-smoke.mjs out/bootstrap_phase4_parser_pilot.wasm
deno run -A scripts/clap.mjs compile examples/bootstrap_phase5_dispatch_pilot.clap out/bootstrap_phase5_dispatch_pilot.wasm
deno run -A scripts/bootstrap-phase5-dispatch-smoke.mjs out/bootstrap_phase5_dispatch_pilot.wasm
deno run -A scripts/clap.mjs compile examples/bootstrap_phase6_entry.clap out/bootstrap_phase6_entry.wasm
deno run -A scripts/bootstrap-phase6-module-smoke.mjs out/bootstrap_phase6_entry.wasm
deno run -A scripts/clap.mjs compile examples/bootstrap_phase7_host_capability_pilot.clap out/bootstrap_phase7_host_capability_pilot.wasm
deno run -A scripts/clap.mjs compile examples/bootstrap_phase8_pattern_and_operators.clap out/bootstrap_phase8_pattern_and_operators.wasm
deno run -A scripts/run-wasm.mjs out/bootstrap_phase8_pattern_and_operators.wasm main 7
deno run -A scripts/clap.mjs compile lib/compiler/kernel.clap out/clap_compiler.wasm
CLAP_COMPILER_WASM_PATH=out/clap_compiler.wasm deno run -A scripts/run-clap-compiler-wasm.mjs engine-mode
deno run -A scripts/clap.mjs compile examples/bootstrap_phase10_frontend_lexer.clap out/bootstrap_phase10_frontend_lexer.wasm
deno run -A scripts/run-wasm.mjs out/bootstrap_phase10_frontend_lexer.wasm main 0
deno run -A scripts/clap.mjs compile examples/bootstrap_phase11_parser_combinator_pilot.clap out/bootstrap_phase11_parser_combinator_pilot.wasm
deno run -A scripts/run-wasm.mjs out/bootstrap_phase11_parser_combinator_pilot.wasm main 0
```

## Browser canvas demo

```bash
just life-serve 8080
```

Then open `http://localhost:8080/examples/game_of_life.html`.

Mario-like ECS browser demo:

```bash
just mario-serve 8080
```

Then open `http://localhost:8080/examples/mario_ecs.html`.
