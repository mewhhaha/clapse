# Examples

Simple sample programs for the current Clapse syntax.

Default source style is operator-first (`+ - * / == &&`). Custom operator declarations extend or override builtin operator tokens.

## Files

- `identity.clapse`: identity and apply basics
- `currying.clapse`: partial application and saturated calls
- `closures.clapse`: closure creation and use
- `signatures_and_collections.clapse`: function signatures with optional named witness constraints and `[]` collection literals
- `let_bindings.clapse`: local `let ... in ...` bindings inside function bodies
- `case_of.clapse`: `case ... of` expression matching over multiple values and constructor patterns
- `http_request_parser.clapse`: fixed-width numeric HTTP request parsing pipeline with data + let bindings
- `recursion.clapse`: tail-recursive and non-tail recursive patterns
- `operators.clapse`: custom infix operators with precedence/backticks (on top of builtin operators)
- `traits.clapse`: builtin trait-oriented expression patterns
- `class_arithmetic_rewrites.clapse`: compile-time class/law/instance rewrites for `add`/`sub`/`mul`/`div`
- `class_algebra_rewrites.clapse`: compile-time class/law/instance rewrites for monoid/functor/applicative/monad
- `monads_maybe_either.clapse`: `Maybe` and `Either` ADT constructors plus law-driven monad instances using HKT-style class/instance declarations
- `data.clapse`: parametric `data` declaration with constructor + let-pattern deconstruction
- `strings.clapse`: static string literal compile + runtime rendering
- `interop_slice.clapse`: byte-slice interop (`slice_len`, `slice_get_u8`)
- `interop_slice.mjs`: Node ESM runner showing `Uint8Array` -> Clapse slice descriptor (`ptr,len`) interop
- `wasm_linear_memory_helpers.clapse`: low-level linear-memory helper usage (`slice_new_u8`, `slice_data_ptr`, `slice_len_raw`, `region_*`, `memcpy_u8`, `memset_u8`) with explicit dataflow threading to avoid collapse-time dead-code elimination
- `game_of_life.clapse`: wasm Life rule + pure `LifeState`/`step_state` transition API over slice descriptors with per-step `region_mark`/`region_reset` cleanup
- `game_of_life.html`: browser canvas demo shell
- `game_of_life.mjs`: browser engine loop + wasm interop glue
- `wasm_main.clapse`: simple program for wasm compile + node execution
- `wasm_closure.clapse`: closure + currying wasm smoke input
- `bench_wasm_hand.clapse`: direct numeric-expression wasm benchmark fixture
- `bench_wasm_abstraction.clapse`: abstraction-heavy equivalent numeric wasm benchmark fixture
- `bench_wasm_closure_env_hand.clapse`: direct closure/environment-warmup wasm benchmark fixture
- `bench_wasm_closure_env_abstraction.clapse`: closure/environment abstraction wasm benchmark fixture
- `bench_wasm_struct_field_hand.clapse`: direct struct-field-free wasm benchmark fixture
- `bench_wasm_struct_field_abstraction.clapse`: struct-field abstraction wasm benchmark fixture
- `bench_wasm_wrapper_uncurry_hand.clapse`: direct wrapper/uncurry-free wasm benchmark fixture
- `bench_wasm_wrapper_uncurry_abstraction.clapse`: wrapper/uncurry abstraction wasm benchmark fixture
- `wasm_struct_has_tag.clapse`: low-level struct helper fixture for tagged `__is_*`
- `wasm_struct_has_tag_false.clapse`: low-level struct helper fixture for false-path tagged `__is_*`
- `wasm_struct_get_ok.clapse`: low-level struct helper fixture for matching `__get_*`
- `wasm_struct_get_mismatch.clapse`: low-level struct helper fixture for mismatched-tag `__get_*` trap checks

## Formatting

Format a file to stdout:

```bash
cabal run clapse -- format examples/currying.clapse
```

Format in place:

```bash
cabal run clapse -- format --write examples/currying.clapse
```

## Compile and run (WASM)

```bash
cabal run clapse -- compile examples/wasm_main.clapse out/wasm_main.wasm
node scripts/run-wasm.mjs out/wasm_main.wasm main 7
```

Closure/currying compile + run:

```bash
cabal run clapse -- compile examples/wasm_closure.clapse out/wasm_closure.wasm
node scripts/run-wasm.mjs out/wasm_closure.wasm main 7
```

Data constructor/deconstruction compile + run:

```bash
cabal run clapse -- compile examples/data.clapse out/wasm_data.wasm
node scripts/run-wasm.mjs out/wasm_data.wasm main 7
```

String literal compile + run:

```bash
cabal run clapse -- compile examples/strings.clapse out/wasm_strings.wasm
node scripts/run-wasm.mjs out/wasm_strings.wasm main
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
cabal run clapse -- compile examples/interop_slice.clapse out/interop_slice.wasm
node examples/interop_slice.mjs out/interop_slice.wasm
```

Game of Life rule compile + smoke:

```bash
cabal run clapse -- compile examples/game_of_life.clapse out/game_of_life.wasm
node scripts/run-wasm.mjs out/game_of_life.wasm main 0 3
node scripts/run-wasm.mjs out/game_of_life.wasm main 1 2
node scripts/run-wasm.mjs out/game_of_life.wasm main 1 4
node scripts/life-slice-smoke.mjs out/game_of_life.wasm
```

HTTP request parser-style compile + run:

```bash
cabal run clapse -- compile examples/http_request_parser.clapse out/wasm_http_request_parser.wasm
node scripts/run-wasm.mjs out/wasm_http_request_parser.wasm main 10203
```

Case expression compile + run:

```bash
cabal run clapse -- compile examples/case_of.clapse out/wasm_case_of.wasm
node scripts/run-wasm.mjs out/wasm_case_of.wasm main 7
```
This example also demonstrates multiline case-arm formatting.
For single-scrutinee constructor matches, the final catch-all arm is optional when constructor coverage is exhaustive.

Maybe/Either monad compile + run:

```bash
cabal run clapse -- compile examples/monads_maybe_either.clapse out/wasm_monads_maybe_either.wasm
node scripts/run-wasm.mjs out/wasm_monads_maybe_either.wasm main 7
```

## Benchmark (WASM runtime)

Single module benchmark:

```bash
cabal run clapse -- compile examples/wasm_main.clapse out/wasm_main.wasm
node scripts/bench-wasm.mjs out/wasm_main.wasm main 2000000 20000
```

Hand vs abstraction benchmark fixtures:

```bash
cabal run clapse -- compile examples/bench_wasm_hand.clapse out/bench_wasm_hand.wasm
cabal run clapse -- compile examples/bench_wasm_abstraction.clapse out/bench_wasm_abstraction.wasm
node scripts/bench-wasm.mjs out/bench_wasm_hand.wasm main 2000000 20000
node scripts/bench-wasm.mjs out/bench_wasm_abstraction.wasm main 2000000 20000
```

Closure/environment-flattening fixture pair:

```bash
cabal run clapse -- compile examples/bench_wasm_closure_env_hand.clapse out/bench_wasm_closure_env_hand.wasm
cabal run clapse -- compile examples/bench_wasm_closure_env_abstraction.clapse out/bench_wasm_closure_env_abstraction.wasm
node scripts/bench-wasm.mjs out/bench_wasm_closure_env_hand.wasm main 2000000 20000
node scripts/bench-wasm.mjs out/bench_wasm_closure_env_abstraction.wasm main 2000000 20000
```

Struct-field-flattening fixture pair:

```bash
cabal run clapse -- compile examples/bench_wasm_struct_field_hand.clapse out/bench_wasm_struct_field_hand.wasm
cabal run clapse -- compile examples/bench_wasm_struct_field_abstraction.clapse out/bench_wasm_struct_field_abstraction.wasm
node scripts/bench-wasm.mjs out/bench_wasm_struct_field_hand.wasm main 2000000 20000
node scripts/bench-wasm.mjs out/bench_wasm_struct_field_abstraction.wasm main 2000000 20000
```

Wrapper/uncurrying fixture pair:

```bash
cabal run clapse -- compile examples/bench_wasm_wrapper_uncurry_hand.clapse out/bench_wasm_wrapper_uncurry_hand.wasm
cabal run clapse -- compile examples/bench_wasm_wrapper_uncurry_abstraction.clapse out/bench_wasm_wrapper_uncurry_abstraction.wasm
node scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_hand.wasm main 2000000 20000
node scripts/bench-wasm.mjs out/bench_wasm_wrapper_uncurry_abstraction.wasm main 2000000 20000
```

## Browser canvas demo

```bash
just life-serve 8080
```

Then open `http://localhost:8080/examples/game_of_life.html`.
