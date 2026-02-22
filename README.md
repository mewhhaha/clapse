# Clapse

Clapse is a toy pure functional language focused on aggressive reduction and collapse into minimal WASM-oriented code.

## Design goals

- Pure core semantics for deterministic rewriting/collapse.
- Compile-time rewrite control (class/law/instance declarations) inspired by Haskell typeclasses but used like comptime rewrite contracts.
- Stack-first lowered IR and verifier-gated collapsed IR.
- WASM-only backend, targeting WebAssembly 3.0 feature set.

## Core syntax

Top-level declarations are line-oriented; guarded functions can use indented continuation lines:

```haskell
module entry
import util.math
export main

identity x = x
data Pair a b = Pair a b
class plus_rules i : add
law plus_rules right_identity = add x 0 => x
instance plus_on_i64 : plus_rules i add=plus
infixl 6 +. = add

id : a -> a
eq_id : (eq_witness : eq a) => a -> a
id x = x
```

Module directives:

- `module <dotted_name>` declares a source module name (e.g. `module util.math`).
- `import <dotted_name>` imports another source module from the compile root.
- `export <name>[, <name> ...]` sets explicit wasm exports for the entry module.
- When `export` is omitted in the entry module, all entry-module functions are exported.
- `clapse compile <entry.clapse>` resolves dotted imports relative to the entry file directory (`util.math` -> `util/math.clapse`), merges modules, rewrites imported names to qualified internal symbols, then compiles a single optimized wasm module.
- Compile also emits a TypeScript sidecar (`<output path with .d.ts extension>`) derived from collapsed IR exports (`name` + `arity`) so JS/TS interop can type exported API calls.

Custom operator declarations:

```haskell
infixl 6 +. = add
infixr 5 <> = append
infix 4 ==. = eq
infixl 6 plus_op = add
```

- Form: `infixl|infixr|infix <precedence> <operator_token> = <target_name>`
- Precedence range: `0..9` (`9` binds tightest).
- `<operator_token>` can be symbolic (`+.` `<>`) or an identifier (`plus_op`).
- Identifier operators are used with backticks in expressions: ``x `plus_op` y``.
- Any function identifier can be used in backticks without a declaration: ``a `mod` b`` (default fixity: `infixl 9`, target function: same identifier).
- Add an explicit declaration only when you need custom precedence/associativity or a different target function.
- Operators are pure source-level sugar that rewrite to normal function calls during parse.

Builtin operators (available without declarations):

- `+` => `add` (left-associative, precedence 6)
- `-` => `sub` (left-associative, precedence 6)
- `*` => `mul` (left-associative, precedence 7)
- `/` => `div` (left-associative, precedence 7)
- `==` => `eq` (non-associative, precedence 4)
- `&&` => `and` (right-associative, precedence 3)

User declarations override builtin fixity/target for the same operator token.

Function attributes:

You can attach compile-time metadata to a function by placing one or more attributes above its declaration:

```haskell
#[memo 100]
#[test "fibonacci memoized"]
#[bench "fib benchmark"]
fib n = add n 1
```

Attribute form:

- `#[name]` no value
- `#[name value]` where `value` is a single token:
  - string: `#[bench "my bench"]`
  - integer: `#[memo 100]`
  - identifier: `#[label fast_path]`
  - function attributes are attached to the entire function group (including multi-clause functions)

Value identifiers are snake_case: `[a-z_][a-z0-9_']*`.
Data type and constructor names are capitalized: `[A-Z][A-Za-z0-9_']*`.
`data` declarations support multi-constructor forms and one-line GADT alternatives:

```haskell
data Maybe a = Just : a -> Maybe a | Nothing : Maybe a
```

For old-style declarations without explicit type parameters (for example `data Event = Tick n | Reset token`), constructor fields do not introduce implicit type parameters on the data type; the result type stays `Event`.

Supported expressions:

- variable: `x`
- integer literal: `42`
- string literal: `"hello"` (escapes: `\"`, `\\`, `\n`, `\t`, `\r`)
- lambda: `\x -> expr`, `\x y -> expr`
- let-expression (desugars to lambda/application): `let x = expr in body`
- let-expression with multiple bindings (left-to-right): `let x = expr; y = expr in body`
- let-expression with multiline bindings (Haskell-style):

  ```haskell
  let x = expr
      y = expr
  in body
  ```
- let local function binding: `let inc y = y + 1 in inc x`
- let constructor deconstruction binding: `let Pair left right = value in left`
- case-expression over one or more values:

  `case a b of 0 0 -> 0; x y -> x + y`

  multiline:

  ```haskell
  case a b of
    0 0 -> 0
    x y -> x + y
  ```
- case constructor deconstruction pattern:

  `case value of Pair left right -> left; _ -> 0`

  multiline:

  ```haskell
  case value of
    Pair left right -> left
    _ -> 0
  ```
- constructor-specific matching works with multi-constructor `data` declarations
- multi-equation functions with pattern arguments (same as Haskell-style equations):

  ```haskell
  sum2 0 y = y
  sum2 x y = add x y
  ```

  (single-function bodies with only plain variable arguments are desugared back to direct form; mixed variable/pattern clauses stay in a `case` representation).
- guarded function equations (desugar to `case`):

  ```haskell
  add x y | x == 0 && y == 0 = 0
          | otherwise = x + y
  ```

  or in prefix form:

  ```haskell
  add_or_zero x y | eq x 0 = 0
                  | otherwise = add x y
  ```
- application (left-associative): `f x y`
- infix operator application using builtin/custom operators: `x + y`, `x +. y`, ``x `plus_op` y``, ``a `mod` b``
- function signatures (with optional named witness constraints): `id : a -> a`, `eq_id : (eq_witness : eq a) => a -> a`
- parenthesized expression: `(expr)`
- collection literal: `[]`, `[expr1, expr2]`

Case-expression notes:

- Arms can be separated with line breaks and indentation after `of`.
- A semicolon-separated single-line form is also supported.
- Final arm is required only when the listed patterns are not exhaustive.
- For single-scrutinee pattern matching, a trailing catch-all is optional when constructor coverage is complete.
- Pattern forms: integer literal, variable, `_`, constructor pattern (`ctor field1 field2 ...`).
- Scrutinees are space-separated terms; parenthesize complex scrutinees: `case (Pair x y) of ...`.

Collection literal lowering model:

- `[]` lowers to `collection_empty 0`
- `[a, b, c]` lowers to repeated `collection_extend` calls
- you can override `collection_empty` / `collection_extend` with your own definitions to customize concrete collection behavior

`do` and `ado` notation are intentionally not supported.

## Fundamental data types

Explicit primitive type inventory (language-level target set):

- `i64`: default numeric literal/arithmetic type today.
- `u64`: reserved primitive (not yet inferred from literals).
- `byte`: primitive for compact binary/host ABI boundaries.
- `string`: contiguous byte-string runtime value.

Current inference/runtime status:

- Integer literals and arithmetic builtins currently infer as `i64`.
- String literals infer as `string`.
- `slice_len` consumes `slice byte`.
- `slice_get_u8`/`slice_set_u8` currently infer numeric payloads as `i64` in the current backend model.
- low-level linear-memory helpers are available:
  - `slice_new_u8 : i64 -> slice byte`
  - `slice_data_ptr : slice byte -> i64` (raw linear-memory pointer handle)
  - `slice_len_raw : slice byte -> i64` (descriptor length load)
  - `region_mark : i64 -> i64` (call as `region_mark 0`)
  - `region_alloc : i64 -> i64 -> i64`
  - `region_reset : i64 -> i64`
  - `memcpy_u8 : i64 -> i64 -> i64 -> i64`
  - `memset_u8 : i64 -> i64 -> i64 -> i64`
  - `struct_tag : a -> i64`
- WASM backend/runtime currently execute numeric values as tagged `i32` values (payload range `[-1073741824, 1073741823]`) while the source type model tracks `i64` names.
- WASM backend encodes string literals to UTF-8 bytes in linear-memory `[ptr,len]` descriptors.
- WASM backend emits string literals as linear-memory descriptors: each literal is placed as a fixed `[ptr,len]` descriptor in the module data section and referenced directly as a raw handle.
- WASM backend lowers `slice_len`/`slice_get_u8`/`slice_set_u8` to direct linear-memory descriptor loads/stores (no per-element JS import hops).
- low-level memory helpers remain expression-level intrinsics, but collapse dead-temp pruning treats effectful ones as live side-effects.
- effectful memory operations (`slice_set_u8`, `memcpy_u8`, `memset_u8`, `region_reset`) are retained even when their result values are not referenced.
- for explicit sequencing, keep data dependencies between effectful operations in source.

## Data declarations

`data` declarations generate low-level constructor functions. Field access is done by deconstruction in `let`:

```haskell
data Pair a b = Pair a b
```

Generated functions (one per constructor):

- constructor: `Pair a b = __mk_Pair#Pair_2_tpar_2_fmap_0_1 a b`
- constructor: `Just a = __mk_Maybe#Just_1_tpar_1_fmap_0 a`
- constructor: `Nothing = __mk_Maybe#Nothing_0_tpar_1_fmap_none`
- no user-facing getter functions are generated

Access/deconstruct fields:

- `let Pair left right = value in ...`

These map to low-level struct runtime calls in the WASM backend.

## Compile-time class/law/instance rewrites

This system is for optimizer/collapse rewriting, not runtime dictionary passing.

### Declarations

- Class declaration:

```haskell
class <class_name> <type_ctor> : <kind>
```

- Law declaration:

```haskell
law <class_name> <law_name> = <lhs_expr> => <rhs_expr>
```

- Instance declaration:

```haskell
instance <instance_name> : <class_name> <type_ctor> <method>=<target> ...
```

Example HKT class/instance declarations:

```haskell
class monad_rules m : monad
instance monad_on_maybe : monad_rules Maybe pure=maybe_pure bind=maybe_bind
```

### Supported class kinds

- `add`
- `sub`
- `mul`
- `div`
- `monoid`
- `functor`
- `applicative`
- `monad`

Each kind enforces a required method set and required law-name set (`Clapse.Laws`).

### Rewrite derivation model

- Class laws are written against required method names for the kind.
- Instance bindings map those method names to concrete target names.
- Parser derives concrete rewrite rules by substituting bindings into class laws.
- Function bodies are normalized with derived rules before lowering/collapse.

This gives a comptime-like "declare rules next to abstraction" flow while keeping runtime pure and minimal.

## Compilation pipeline

1. Parse source (`Clapse.Syntax`).
2. Derive and apply class/law/instance rewrites (compile-time normalization).
3. Lower to stack ops (`Clapse.Lowering`).
4. Collapse to normalized stack-free IR + verify (`Clapse.CollapseIR`).
5. Emit WASM (`Clapse.Wasm`).

Collapsed IR includes currying normalization, immediate-apply collapse, constant-argument direct-call specialization, small non-recursive inlining, root-based dead-function pruning, dead-temp pruning, and self tail-call normalization (`VSelfTailCall`).

## Parallelism roadmap (HVM2-inspired)

- keep collapse/rewrite passes pure and deterministic so independent redexes can be scheduled in parallel
- stage future collapse IR batching around independent call/rewrite regions (work-stealing friendly)
- keep verifier constraints as the correctness gate so parallel scheduling does not change program meaning

## Tooling

Single executable provides compiler + formatter + LSP:

```bash
cabal run clapse -- format <file>
cabal run clapse -- format --write <file>
cabal run clapse -- format --stdin
cabal run clapse -- compile <input.clapse> [output.wasm]
cabal run clapse -- bench [iterations]
cabal run clapse -- lsp --stdio
```

`compile` writes the wasm file at the requested output path and a `.d.ts` sidecar at `replaceExtension(outputPath, "d.ts")`.

Formatter behavior today:

- syntax-validating and mostly source-preserving: collapses redundant internal horizontal whitespace (outside strings/comments), trims trailing horizontal whitespace, normalizes line endings/final newline
- expands single-line semicolon-separated `case ... of` arms to multiline layout
- supports multiline `case` expansion even with indented continuation lines (preserving `let`-arm continuations)
- rewrites multiline `let` blocks in Haskell-style layout (`let x = ...`, aligned bindings, `in ...` on its own aligned line)
- rewrites `case` arms with inline `let` into multiline arm bodies with the same Haskell-style `let` layout
- expands long inline `let ...; ... in ...` chains into multiline Haskell-style `let` blocks
- does not lower/collapse/rewrite declarations during formatting
- `format` exits with a non-zero status on format errors (`--write` never writes on error)

Install:

```bash
cabal install exe:clapse
```

LSP currently provides:

- parse diagnostics
- type diagnostics
- hover inferred types (no function-signature inlay hints)

## Benchmarking

Collapsed IR optimization-bar benchmark:

```bash
just bench 200000
```

`bench` compiles each case from its entry root (`hand` / `abstraction`) so static/runtime bars compare only reachable collapsed code.

WASM runtime benchmark for a compiled module:

```bash
just bench-wasm-main 2000000 20000
```

WASM hand-vs-abstraction fixture comparison:

```bash
just bench-wasm-compare 2000000 20000
```

WASM feature-set hand-vs-abstraction comparisons (numeric baseline + closure/env + struct-field + wrapper/uncurry):

```bash
just bench-wasm-compare-all 2000000 20000
```

## WASM runtime support

Current backend supports:

- numeric literals and local refs
- native wasm scalar lowering for `add/sub/mul/div/eq/and` (tagged-`i32` runtime representation)
- tagged numeric runtime payload range is `[-1073741824, 1073741823]`
- scalar numeric lowering assumes numeric operands at compile/runtime boundaries (no dynamic tag checks in those inlined ops)
- string literals as static contiguous bytes in module memory
- direct calls and direct `if/else` branches
- case/if branch thunks that are single-use saturating closures are lowered to direct branch calls (skipping closure allocation/apply)
- closures and currying (`VClosure`, `VCurryDirect`, `VApply`)
- closure allocation/application are lowered inline in generated wasm (no emitted `rt_make_closure`, `rt_apply`, or `rt_apply2/3/4` helpers)
- struct allocation/field/tag operations are now emitted inline in generated Wasm (no `rt_make_struct`, `rt_get_field`, `rt_has_tag`, `rt_get_tag` helper functions)
- closure and struct runtime records are variable-sized in linear memory (no fixed 8-capture / 8-field backend ceiling)
- constructor tags are lowered through a module-local unique tag-id table (no hash-collision ambiguity)
- self-tail return in tail positions (`return_call`)
- low-level struct values for `data`
- JS byte-slice interop via `slice_len`, `slice_get_u8`, and `slice_set_u8` with slice descriptors (`ptr,len`) in wasm linear memory
- slice + region + bulk memory builtins are lowered inline in generated Wasm (no dedicated `rt_slice_*`, `rt_region_*`, `rt_memcpy`, or `rt_memset` helpers emitted)
- collapsed IR global atoms now lower to wasm globals for known runtime globals (`__heap_ptr` / `__heap`)
- no emitted `rt_*` helper functions; runtime operations are lowered inline in generated wasm
- compiled modules now use wasm-native helpers for closures and do not require `rt_*` function imports
- JS runtime fallback `rt_*` imports have been removed

Node ESM runtime smoke:

```bash
cabal run clapse -- compile examples/wasm_main.clapse out/wasm_main.wasm
node scripts/run-wasm.mjs out/wasm_main.wasm main 7
```

Node ESM slice interop smoke:

```bash
just wasm-interop-slice-smoke
```

Runtime contract smoke (tagged-int bounds + no JS `rt_*` fallback + slice copy isolation):

```bash
just wasm-runtime-contract-smoke
```

WASM struct helper smoke (tagged `__is_*`, tag-safe `__get_*`):

```bash
just wasm-struct-helpers-smoke
```

WASM linear-memory helper smoke (`slice_new_u8` + `memcpy_u8` + `memset_u8` + region helpers):

```bash
just wasm-linear-memory-helpers-smoke
```

Browser canvas Game of Life demo:

```bash
just life-build
just life-smoke
just life-serve 8080
```

Game of Life step timing/profile:

```bash
just life-time 160 100 120 1
```

Then open `http://localhost:8080/examples/game_of_life.html`.
The browser demo keeps simulation transitions in Clapse (`LifeState`, `LifeEvent`, `apply_event`) while JS handles rendering/timing/input and reads generation/alive-count from wasm state.

Mario-like ECS demo:

```bash
just mario-serve 8080
```

Then open `http://localhost:8080/examples/mario_ecs.html`.
This demo keeps tiny ECS-like updates pure in Clapse (`MarioState`, `MarioEvent`, `apply_event`) and uses JS only for keyboard input + sprite rendering.

## Examples

See `examples/README.md`.

Key examples:

- `examples/class_arithmetic_rewrites.clapse`
- `examples/class_algebra_rewrites.clapse`
- `examples/monads_maybe_either.clapse`
- `examples/case_of.clapse`
- `examples/operators.clapse`
- `examples/http_request_parser.clapse`
- `examples/let_bindings.clapse`
- `examples/data.clapse`
- `examples/strings.clapse`
- `examples/wasm_linear_memory_helpers.clapse`
- `examples/interop_slice.clapse`
- `examples/interop_slice.mjs`
- `examples/game_of_life.clapse`
- `examples/game_of_life.html`
- `examples/mario_ecs.clapse`
- `examples/mario_ecs.html`
- `examples/mario_ecs.mjs`
- `examples/assets/sprite_regions.md`
- `examples/wasm_main.clapse`
- `examples/bench_wasm_hand.clapse`
- `examples/bench_wasm_abstraction.clapse`
- `examples/bench_wasm_closure_env_hand.clapse`
- `examples/bench_wasm_closure_env_abstraction.clapse`
- `examples/bench_wasm_struct_field_hand.clapse`
- `examples/bench_wasm_struct_field_abstraction.clapse`
- `examples/bench_wasm_wrapper_uncurry_hand.clapse`
- `examples/bench_wasm_wrapper_uncurry_abstraction.clapse`
- `examples/wasm_struct_has_tag.clapse`
- `examples/wasm_struct_has_tag_false.clapse`
- `examples/wasm_struct_get_ok.clapse`
- `examples/wasm_struct_get_mismatch.clapse`

## AI-first docs

AI-first language documentation lives as a skill at:

- `docs/clapse-language/SKILL.md`
- `docs/clapse-language/references/`

## Tree-sitter and Helix

- Grammar: `tree-sitter-clapse/`
- Helix local setup: `just install`
- Sync local grammar path: `./scripts/setup-helix-local.sh`
- Tree-sitter includes reliable `let ... in ...` expression/binding parsing (`in` must be a real separator, not an identifier prefix like `in_cells`), wildcard `_` pattern nodes, guarded function clauses, robust multi-branch `case ... of` parsing, explicit `module/import/export` declarations, and operator highlighting for symbolic/infix/backtick/custom operator-declaration tokens.

## Status

Implemented now:

- parser for functions, `data`, `class`, `law`, `instance`
- parser support for multi-constructor `data` declarations and one-line GADT constructor alternatives
- parser support for `let ... in ...` expressions (including `;`-separated bindings)
- parser support for indented multiline `let` continuation lines in function bodies
- parser support for guarded function declarations using `|` clauses with `otherwise`
- parser support for constructor deconstruction in `let` bindings
- parser support for `case ... of` expressions with multi-scrutinee matching and constructor patterns
- parser support for top-level function type signatures, including optional named witness constraints
- parser support for top-level function attributes (`#[memo ...]`, `#[test ...]`, `#[bench ...]`) and clause-group propagation
- parser support for HKT-style class/instance declarations (`class <name> <type_ctor> : kind`, `instance <name> : <class> <type_ctor> ...`)
- parser support for collection literals (`[]`, `[a, b, ...]`)
- parser tolerance for `module/import/export` directives in syntax validation/format/lsp paths
- builtin infix operators (`+ - * / == &&`) plus custom operator declarations with fixity/precedence and backtick operators
- compile-time rewrite derivation from class/law/instance declarations
- closure-aware lowering and normalized collapsed IR with verifier
- automatic currying normalization and immediate-apply collapse
- constant-argument direct-call specialization for small wrapper-style functions
- small non-recursive interprocedural inlining for wrapper-style functions
- escape/lifetime analysis over collapsed IR for closure/struct temps
- simple slice ownership analysis for `slice_set_u8` (reuse on linear ownership, copy path on shared targets)
- struct representation flattening for non-escaping constructor values (`__get_*`/`__is_*` rewrite to direct atoms/constants)
- closure capture-layout flattening pass that trims unused captured locals and rewrites call/curry/closure argument layouts
- hot wrapper uncurrying pass that rewrites high-frequency apply-chain wrappers to direct `VApply` chains before final normalization
- deeper post-uncurry normalize+inline rounds that collapse multi-arg lambda towers toward direct numeric code
- root-based dead-function pruning support in collapse pipeline
- pruning keeps parents when reachable lifted descendants remain, preventing orphan lifted-call breakage
- self tail-call optimization in collapsed IR
- pure source + collapsed evaluators with differential tests
- WASM backend with closure/currying/data-struct runtime interop
- wasm struct helpers enforce runtime tag safety for getters and tagged-bool predicates
- wasm tag comparisons now use module-local unique tag ids instead of hashed string tags
- native wasm scalar lowering for numeric builtins (`add/sub/mul/div/eq/and`)
- static string literal lowering into wasm memory/data with shared runtime handles
- byte-slice builtins lowered directly to wasm memory ops (`i32.load`/`i32.load8_u`/`i32.store8`) with JS helpers that allocate/copy slice descriptors in linear memory
- formatter + LSP in same executable
- module-aware compile path for entry files (`module/import/export`, cycle/missing-module checks, explicit export lists)
- tree-sitter grammar + highlight/textobject/indent/tags/rainbow query set
- Node ESM wasm runtime benchmark harness (`scripts/bench-wasm.mjs`) and `just` benchmark targets
- browser canvas Game of Life demo driven by Clapse-compiled wasm rule function

Not implemented yet:

- full numeric-width runtime (`i64/u64/byte`) in backend ABI
- full proof checker
- richer public WASM memory/globals ABI (beyond internal `__heap_ptr` / `__heap` globals)
