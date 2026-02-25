# Examples

Simple sample programs for the current Clapse syntax.

Default source style is operator-first (`+ - * / == &&`). Custom operator
declarations extend or override builtin operator tokens. Any function can also
be used as a backtick operator without a declaration (for example
``a `mod` b``).

## Files

- `identity.clapse`: identity and apply basics
- `currying.clapse`: partial application and saturated calls
- `closures.clapse`: closure creation and use
- `signatures_and_collections.clapse`: function signatures with optional named
  witness constraints and `[]` collection literals
- `let_bindings.clapse`: local `let ... in ...` bindings inside function bodies
- `case_of.clapse`: `case ... of` expression matching over multiple values and
  constructor patterns
- `http_request_parser.clapse`: fixed-width numeric HTTP request parsing
  pipeline with data + let bindings
- `recursion.clapse`: tail-recursive and non-tail recursive patterns
- `operators.clapse`: custom infix operators with precedence/backticks (on top
  of builtin operators)
- `traits.clapse`: builtin trait-oriented expression patterns
- `class_arithmetic_rewrites.clapse`: compile-time class/law/instance rewrites
  for `add`/`sub`/`mul`/`div`
- `class_algebra_rewrites.clapse`: compile-time class/law/instance rewrites for
  monoid/functor/applicative/monad
- `traits_ord_slice.clapse`: compile-time class/law/instance rewrites for
  `ord` and `slice` kinds (`lt/le/gt/ge` + slice method surface)
- `monads_maybe_either.clapse`: `Maybe` and `Either` ADT constructors plus
  law-driven monad instances using HKT-style class/instance declarations
- `data.clapse`: parametric `data` declaration with constructor + let-pattern
  deconstruction
- `strings.clapse`: static string literal compile + runtime rendering
- `interop_slice.clapse`: byte-slice interop (`slice_len`, `slice_get_u8`, `slice_eq_u8`, `slice_set_u8`, `str_to_slice`, `slice_to_string`)
- `util/slice_scan.clapse`: pure byte-slice scanning helpers
  (`slice_find_u8`, `slice_find_seq_u8`)
- `util/string_slice.clapse`: pure bytewise slice/string comparison helpers
  (`slice_eq_u8` builtin, `slice_eq_str`)
- `util/json.clapse`: tiny pure byte-slice JSON command marker helper module
- `interop_slice.mjs`: Deno/Node-compatible runner showing `Uint8Array` ->
  Clapse slice descriptor (`ptr,len`) interop
- `wasm_linear_memory_helpers.clapse`: low-level linear-memory helper usage
  (`slice_new_u8`, `slice_data_ptr`, `slice_len_raw`, `region_*`, `memcpy_u8`,
  `memset_u8`) with explicit dataflow threading to avoid collapse-time dead-code
  elimination
- `game_of_life.clapse`: wasm Life rule + pure `LifeState` plus
  `LifeEvent`/`apply_event` transition API over slice descriptors with per-step
  `region_mark`/`region_reset` cleanup
- `game_of_life.html`: browser canvas demo shell
- `game_of_life.mjs`: browser engine loop where JS only dispatches events
  (`event_tick`, `event_toggle`, `event_clear`, `event_load`) and renders
  wasm-owned state
- `mario_ecs.clapse`: tiny ECS-like pure state machine (`kinds/xs/lanes/active`
  slices) with event-driven stepping
- `mario_ecs.html`: browser canvas shell for the Mario-like ECS demo
- `mario_ecs.mjs`: browser render/input glue that dispatches `event_tick` /
  `event_reset` into wasm
- `assets/sprite_regions.md`: descriptive sprite-sheet region notes used by the
  Mario-like demo atlas
- `wasm_main.clapse`: simple program for wasm compile + node execution
- `wasm_closure.clapse`: closure + currying wasm smoke input
- `bench_wasm_hand.clapse`: direct numeric-expression wasm benchmark fixture
- `bench_wasm_abstraction.clapse`: abstraction-heavy equivalent numeric wasm
  benchmark fixture
- `bench_wasm_closure_env_hand.clapse`: direct closure/environment-warmup wasm
  benchmark fixture
- `bench_wasm_closure_env_abstraction.clapse`: closure/environment abstraction
  wasm benchmark fixture
- `bench_wasm_struct_field_hand.clapse`: direct struct-field-free wasm benchmark
  fixture
- `bench_wasm_struct_field_abstraction.clapse`: struct-field abstraction wasm
  benchmark fixture
- `bench_wasm_wrapper_uncurry_hand.clapse`: direct wrapper/uncurry-free wasm
  benchmark fixture
- `bench_wasm_wrapper_uncurry_abstraction.clapse`: wrapper/uncurry abstraction
  wasm benchmark fixture
- `bench_wasm_slice_set_reuse.clapse`: non-aliased `slice_set_u8` reuse
  benchmark fixture
- `bench_wasm_slice_set_copy.clapse`: aliased `slice_set_u8` copy-path benchmark
  fixture
- `wasm_struct_has_tag.clapse`: low-level struct helper fixture for tagged
  `__is_*`
- `wasm_struct_has_tag_false.clapse`: low-level struct helper fixture for
  false-path tagged `__is_*`
- `wasm_struct_get_ok.clapse`: low-level struct helper fixture for matching
  `__get_*`
- `wasm_struct_get_mismatch.clapse`: low-level struct helper fixture for
  mismatched-tag `__get_*` trap checks
- `bootstrap_phase1_frontend_primitives.clapse`: phase 1 self-host bootstrap
  fixture (ADT + pattern/case frontend primitives)
- `bootstrap_phase2_core_data_structures.clapse`: phase 2 self-host bootstrap
  fixture (recursive `List` with `Nil`/`Cons`)
- `bootstrap_phase3_entry.clapse`: phase 3 self-host bootstrap entry fixture
  (module graph import/export)
- `bootstrap_phase4_parser_pilot.clapse`: phase 4 parser pilot for
  assignment-like byte-slice recognition
- `bootstrap_phase5_dispatch_pilot.clapse`: phase 5 dispatch pilot for enum-code
  decode + ADT route dispatch
- `bootstrap_phase6_entry.clapse` + `bootstrap6/router.clapse`: phase 6
  moduleized decode/dispatch bootstrap fixture
- `bootstrap_phase7_host_capability_pilot.clapse`: phase 7 host capability
  import compile pilot (`import host.time`)
- `bootstrap_phase8_pattern_and_operators.clapse`: phase 8 syntax/behavior pilot
  (guards + operators + constructor-pattern case)
- `lib/compiler/kernel.clapse`: phase 9 compiler ABI kernel pilot
  (`clapse_run` command dispatch for `compile`/`format`/`selfhost-artifacts`)
- `bootstrap_phase10_frontend_lexer.clapse`: phase 10 lexer pilot
  (token classes + keyword recognizer)
- `bootstrap_phase11_parser_combinator_pilot.clapse`: phase 11 parser-combinator
  pilot (`>>=`/`<$>`/`<*>`/`<|>` + `many`/`some`/`sepBy` for top-level
  declaration-shape parsing)
- `parser_layout_pain_points.clapse`: parser/formatter conformance fixture for
  nested case chains, multiline parenthesized application, and long chained let
  layout
- `util/math.clapse` + `util/base.clapse`: phase 3 transitive module graph
  fixtures
- `selfhost_corpus.txt`: manifest used by self-host differential artifact/parity
  gates
- `selfhost_parser_corpus.txt`: manifest used by parser artifact parity harness
  (`merged_module.txt` parity)
- `selfhost_behavior_corpus.json`: manifest used by self-host behavior
  differential wasm execution gates
- `compiler_source_corpus.txt`: manifest used by formatter idempotence gate for
  compiler-source files
- `lsp_wasm_fixtures.json`: diagnostics/hover request fixtures for wasm LSP
  runner
- `../scripts/check-selfhost-manifests.mjs`: manifest consistency guard for
  parity coverage drift
  - intentionally excludes `examples/traits.clapse` (non-compiling trait-catalog
    fixture)
- `../scripts/selfhost-bench.mjs`: corpus-level parity benchmark (compile+run
  timing per engine)

## Formatting

Format a file to stdout:

```bash
deno run -A scripts/clapse.mjs format examples/currying.clapse
```

Format in place:

```bash
deno run -A scripts/clapse.mjs format --write examples/currying.clapse
```

## Compile and run (WASM)

```bash
deno run -A scripts/clapse.mjs compile examples/wasm_main.clapse out/wasm_main.wasm
deno run -A scripts/run-wasm.mjs out/wasm_main.wasm main 7
```

Each `compile` also emits `out/<name>.d.ts` based on collapsed IR export arity.

Closure/currying compile + run:

```bash
deno run -A scripts/clapse.mjs compile examples/wasm_closure.clapse out/wasm_closure.wasm
deno run -A scripts/run-wasm.mjs out/wasm_closure.wasm main 7
```

Data constructor/deconstruction compile + run:

```bash
deno run -A scripts/clapse.mjs compile examples/data.clapse out/wasm_data.wasm
deno run -A scripts/run-wasm.mjs out/wasm_data.wasm main 7
```

String literal compile + run:

```bash
deno run -A scripts/clapse.mjs compile examples/strings.clapse out/wasm_strings.wasm
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
deno run -A scripts/clapse.mjs compile examples/interop_slice.clapse out/interop_slice.wasm
deno run -A examples/interop_slice.mjs out/interop_slice.wasm
```

Game of Life rule compile + smoke:

```bash
deno run -A scripts/clapse.mjs compile examples/game_of_life.clapse out/game_of_life.wasm
deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 0 3
deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 1 2
deno run -A scripts/run-wasm.mjs out/game_of_life.wasm main 1 4
deno run -A scripts/life-slice-smoke.mjs out/game_of_life.wasm
```

The browser demo uses `apply_event` + `event_*` exports so Clapse remains the
source of truth for simulation state.

Mario-like ECS compile + smoke:

```bash
deno run -A scripts/clapse.mjs compile examples/mario_ecs.clapse out/mario_ecs.wasm
deno run -A scripts/mario-ecs-smoke.mjs out/mario_ecs.wasm
```

The browser demo keeps game logic pure in Clapse and treats JS as input/render
boundary only.

HTTP request parser-style compile + run:

```bash
deno run -A scripts/clapse.mjs compile examples/http_request_parser.clapse out/wasm_http_request_parser.wasm
deno run -A scripts/run-wasm.mjs out/wasm_http_request_parser.wasm main 10203
```

Case expression compile + run:

```bash
deno run -A scripts/clapse.mjs compile examples/case_of.clapse out/wasm_case_of.wasm
deno run -A scripts/run-wasm.mjs out/wasm_case_of.wasm main 7
```

This example also demonstrates multiline case-arm formatting. For
single-scrutinee constructor matches, the final catch-all arm is optional when
constructor coverage is exhaustive.

Maybe/Either monad compile + run:

```bash
deno run -A scripts/clapse.mjs compile examples/monads_maybe_either.clapse out/wasm_monads_maybe_either.wasm
deno run -A scripts/run-wasm.mjs out/wasm_monads_maybe_either.wasm main 7
```

## Benchmark (WASM runtime)

Single module benchmark:

```bash
deno run -A scripts/clapse.mjs compile examples/wasm_main.clapse out/wasm_main.wasm
deno run -A scripts/bench-wasm.mjs out/wasm_main.wasm main 2000000 20000
```

Hand vs abstraction benchmark fixtures:

```bash
deno run -A scripts/clapse.mjs compile examples/bench_wasm_hand.clapse out/bench_wasm_hand.wasm
deno run -A scripts/clapse.mjs compile examples/bench_wasm_abstraction.clapse out/bench_wasm_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_abstraction.wasm main 2000000 20000
```

Closure/environment-flattening fixture pair:

```bash
deno run -A scripts/clapse.mjs compile examples/bench_wasm_closure_env_hand.clapse out/bench_wasm_closure_env_hand.wasm
deno run -A scripts/clapse.mjs compile examples/bench_wasm_closure_env_abstraction.clapse out/bench_wasm_closure_env_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_closure_env_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_closure_env_abstraction.wasm main 2000000 20000
```

Struct-field-flattening fixture pair:

```bash
deno run -A scripts/clapse.mjs compile examples/bench_wasm_struct_field_hand.clapse out/bench_wasm_struct_field_hand.wasm
deno run -A scripts/clapse.mjs compile examples/bench_wasm_struct_field_abstraction.clapse out/bench_wasm_struct_field_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_struct_field_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_struct_field_abstraction.wasm main 2000000 20000
```

Wrapper/uncurrying fixture pair:

```bash
deno run -A scripts/clapse.mjs compile examples/bench_wasm_wrapper_uncurry_hand.clapse out/bench_wasm_wrapper_uncurry_hand.wasm
deno run -A scripts/clapse.mjs compile examples/bench_wasm_wrapper_uncurry_abstraction.clapse out/bench_wasm_wrapper_uncurry_abstraction.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_hand.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_abstraction.wasm main 2000000 20000
```

Slice ownership fixture pair (reuse vs copy):

```bash
deno run -A scripts/clapse.mjs compile examples/bench_wasm_slice_set_reuse.clapse out/bench_wasm_slice_set_reuse.wasm
deno run -A scripts/clapse.mjs compile examples/bench_wasm_slice_set_copy.clapse out/bench_wasm_slice_set_copy.wasm
deno run -A scripts/bench-wasm.mjs out/bench_wasm_slice_set_reuse.wasm main 2000000 20000
deno run -A scripts/bench-wasm.mjs out/bench_wasm_slice_set_copy.wasm main 2000000 20000
```

Self-host bootstrap phase fixtures:

```bash
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase1_frontend_primitives.clapse out/bootstrap_phase1.wasm
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase2_core_data_structures.clapse out/bootstrap_phase2.wasm
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase3_entry.clapse out/bootstrap_phase3.wasm
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase4_parser_pilot.clapse out/bootstrap_phase4_parser_pilot.wasm
deno run -A scripts/bootstrap-parser-pilot-smoke.mjs out/bootstrap_phase4_parser_pilot.wasm
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase5_dispatch_pilot.clapse out/bootstrap_phase5_dispatch_pilot.wasm
deno run -A scripts/bootstrap-phase5-dispatch-smoke.mjs out/bootstrap_phase5_dispatch_pilot.wasm
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase6_entry.clapse out/bootstrap_phase6_entry.wasm
deno run -A scripts/bootstrap-phase6-module-smoke.mjs out/bootstrap_phase6_entry.wasm
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase7_host_capability_pilot.clapse out/bootstrap_phase7_host_capability_pilot.wasm
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase8_pattern_and_operators.clapse out/bootstrap_phase8_pattern_and_operators.wasm
deno run -A scripts/run-wasm.mjs out/bootstrap_phase8_pattern_and_operators.wasm main 7
deno run -A scripts/clapse.mjs compile lib/compiler/kernel.clapse out/clapse_compiler.wasm
CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler.wasm deno run -A scripts/run-clapse-compiler-wasm.mjs engine-mode
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase10_frontend_lexer.clapse out/bootstrap_phase10_frontend_lexer.wasm
deno run -A scripts/run-wasm.mjs out/bootstrap_phase10_frontend_lexer.wasm main 0
deno run -A scripts/clapse.mjs compile examples/bootstrap_phase11_parser_combinator_pilot.clapse out/bootstrap_phase11_parser_combinator_pilot.wasm
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
