# Clapse

Clapse is a toy pure functional language focused on aggressive reduction and
collapse into minimal WASM-oriented code.

## Design goals

- Pure core semantics for deterministic rewriting/collapse.
- Compile-time rewrite control (class/law/instance declarations) inspired by
  Haskell typeclasses but used like comptime rewrite contracts.
- Stack-first lowered IR and verifier-gated collapsed IR.
- WASM-only backend, targeting WebAssembly 3.0 feature set.

## Core syntax

Top-level declarations are line-oriented; function bodies can continue on
indented lines for both guarded clauses and `name ... =` block RHS forms:

```haskell
module entry
import util.math
export main

identity x = x
data Pair a b = Pair a b
class plus_rules i : add
law plus_rules right_identity = add x 0 => x
instance plus_on_i64 : plus_rules i add=plus
infixl 6 +.
id : a -> a
eq_id : (eq_witness : eq a) => a -> a
id x = x
```

Module directives:

- `module <dotted_name>` declares a source module name (e.g.
  `module util.math`).
- `import <dotted_name>` imports another source module from the compile root.
- `import host.<capability>` enables host builtin imports without requiring a
  source module file.
  - currently supported:
    - `import host.io` (enables `read_file` host import in generated wasm import
      section)
    - `import host.time` (enables `unix_time_ms` host import in generated wasm
      import section)
- `export <name>[, <name> ...]` sets explicit wasm exports for the entry module.
- When `export` is omitted in the entry module, all entry-module functions are
  exported.
- `deno run -A scripts/clapse.mjs compile <entry.clapse> [output.wasm]` resolves
  dotted imports relative to the entry file directory (`util.math` ->
  `util/math.clapse`), merges modules, rewrites imported names to qualified
  internal symbols, then compiles a single optimized wasm module.
- Compile also emits a TypeScript sidecar (`<output path with .d.ts extension>`)
  derived from collapsed IR exports (`name` + `arity`) so JS/TS interop can type
  exported API calls.

Unified deno CLI frontend:

```bash
deno run -A scripts/clapse.mjs compile examples/wasm_main.clapse out/wasm_main.wasm
deno run -A scripts/clapse.mjs format --write examples/wasm_main.clapse
deno run -A scripts/clapse.mjs format --stdin
deno run -A scripts/clapse.mjs lsp --stdio
```

- `compile`/`selfhost-artifacts`/`format`/`lsp` route through compiler-wasm mode
  by default.
  - compiler wasm is resolved from `CLAPSE_COMPILER_WASM_PATH`, then
    `out/clapse_compiler.wasm`.
  - transitional bridge artifact (`out/clapse_compiler_bridge.wasm`) is only
    allowed when `CLAPSE_ALLOW_BRIDGE=1`.
- `bench` currently routes through the wasm runner behind the same deno
  frontend.

Release/version pipeline for compiler artifacts:

```bash
# produces a versioned release candidate directory under out/releases/
just release-candidate

# custom output root
just release-candidate out=out/releases-ci
```

`release-candidate` does all release gates in one pass:

- builds `clapse_compiler.wasm` + bridge artifact in a versioned directory
- runs strict wasm selfhost parity (`selfhost-check-wasm`)
- regenerates fixture maps and requires byte-for-byte parity against:
  - `scripts/wasm-behavior-fixture-map.json`
  - `scripts/wasm-selfhost-artifact-fixture-map.json`
- emits:
  - `release-manifest.json` (version, commit, toolchain, artifact hashes/sizes)
  - `checksums.sha256` (reproducible checksum list)

Custom operator declarations:

```haskell
infixl 6 +.
infixr 5 <>
infix 4 ==.
infixl 6 plus_op
```

- Form: `infixl|infixr|infix <precedence> <operator_token>`
- Precedence range: `0..9` (`9` binds tightest).
- `<operator_token>` can be symbolic (`+.` `<>`) or an identifier (`plus_op`).
- Identifier operators are used with backticks in expressions:
  ``x `plus_op` y``.
- Any function identifier can be used in backticks without a declaration:
  ``a `mod` b`` (default fixity: `infixl 9`, target function: same identifier).
- Add an explicit declaration only when you need custom precedence/associativity
  on that operator token.
- Operators are pure source-level sugar that rewrite to normal function calls
  during parse.

Builtin operators (available without declarations):

- `+` => `add` (left-associative, precedence 6)
- `-` => `sub` (left-associative, precedence 6)
- `*` => `mul` (left-associative, precedence 7)
- `/` => `div` (left-associative, precedence 7)
- `==` => `eq` (non-associative, precedence 4)
- `<` => `lt` (non-associative, precedence 4)
- `<=` => `le` (non-associative, precedence 4)
- `>` => `gt` (non-associative, precedence 4)
- `>=` => `ge` (non-associative, precedence 4)
- `not` => `bool_not` (prefix function application, e.g. `not x`; use `not (x == y)` when negating expressions)
- `&&` => `and` (right-associative, precedence 3)
- `||` => `bool_or` (right-associative, precedence 2, declared as `infixr 2 || = bool_or` in prelude/custom modules)

User declarations override builtin fixity/target for the same operator token.

Function attributes:

You can attach compile-time metadata to a function by placing one or more
attributes above its declaration:

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
  - function attributes are attached to the entire function group (including
    multi-clause functions)

Built-in attributes (`memo`, `test`, `bench`) are wired through a default
attribute plugin pipeline and validated during parsing.

- `test` and `bench` are metadata-only in the default pipeline. They are
  attached to the function and can be consumed by external tooling (LSP/test
  runners/bench harnesses) without changing language semantics.
- `memo` is a compiler-consumed performance attribute in the WASM backend:
  - current backend support is for unary top-level functions (`arity == 1`, no
    captures)
  - memo tables are fixed-size hash slots in linear memory (entry stores `key`,
    `value`, `occupied`)
  - misses compute via a generated internal `__memo_body` function, then
    populate the slot
  - memoization is also propagated to eligible generated helper functions in the
    same function family, so recursive worker calls are memoized too
  - self-tail-call optimized functions are rejected for `memo` right now

External tools can also use `parseModuleWithPlugins` with custom
`FunctionAttributePlugin`s to install additional attribute behaviour in the same
parsing pipeline.

LSP hover metadata:

- LSP diagnostics are produced from wasm-compiler `compile` responses.
- Hover currently serves `--|` doc comments attached to declarations, including
  method declarations inside class/instance `where` blocks.
- Attribute hover is not yet implemented in the wasm LSP path.

Tree-sitter attribute syntax:

- Grammar now includes top-level attributed declarations:
  - `attributed_function_declaration`
  - `function_attribute`
- Highlight queries now scope:
  - attribute names as `@attribute`
  - attribute delimiters as bracket punctuation
  - attribute values (`integer`, `string`, `identifier`)

Value identifiers are snake_case: `[a-z_][a-z0-9_']*`. Data type names are
capitalized: `[A-Z][A-Za-z0-9_']*`. Constructor names are capitalized by
default, with literal-backed constructors like `true`/`false` as lowercase
wrappers.
`data` declarations support multi-constructor forms and one-line GADT alternatives:

```haskell
data Maybe a = Just : a -> Maybe a | Nothing : Maybe a
```

For old-style declarations without explicit type parameters (for example
`data Event = Tick n | Reset token`), constructor fields do not introduce
implicit type parameters on the data type; the result type stays `Event`.
Old-style constructor fields are treated as type expressions and support
parenthesized recursive forms (for example
`data List a = Nil | Cons a (List a)`).

`newtype` is supported as a constrained `data` form (exactly one constructor
with one field):

```haskell
newtype UserId = UserId i64
```

Supported expressions:

- variable: `x`
- integer literal: `42`, unary negative literal: `-42`
- string literal: `"hello"` (escapes: `\"`, `\\`, `\n`, `\t`, `\r`)
- char literal: `'a'`, `'\n'`, `'\''` (lowered to integer codepoints)
- lambda: `\x -> expr`, `\x y -> expr`
- let-expression (desugars to lambda/application): `let x = expr in body`
- let-expression with multiple bindings (left-to-right):
  `let x = expr; y = expr in body`
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
- multi-equation functions with pattern arguments (same as Haskell-style
  equations):

  ```haskell
  sum2 0 y = y
  sum2 x y = add x y
  ```

  (single-function bodies with only plain variable arguments are desugared back
  to direct form; mixed variable/pattern clauses stay in a `case`
  representation).
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
- no `if/then/else` expression syntax; use `case` (or guarded function equations)
- application (left-associative): `f x y`
- infix operator application using builtin/custom operators: `x + y`, `x +. y`,
  ``x `plus_op` y``, ``a `mod` b``
- function signatures (with optional named witness constraints): `id : a -> a`,
  `eq_id : (eq_witness : eq a) => a -> a`
- parenthesized expression: `(expr)`
- collection literal: `[]`, `[expr1, expr2]`

Case-expression notes:

- Arms can be separated with line breaks and indentation after `of`.
- A semicolon-separated single-line form is also supported.
- Final arm is required only when the listed patterns are not exhaustive.
- For single-scrutinee pattern matching, a trailing catch-all is optional when
  constructor coverage is complete.
- Pattern forms: integer literal, string literal, char literal, variable, `_`, constructor
  pattern (`ctor field1 field2 ...`).
- Constructor patterns are resolved across direct module imports, so imported
  constructors (for example `true`/`false` from a `bool` data declaration) work in
  `case` arms.
- Scrutinees are space-separated terms; parenthesize complex scrutinees:
  `case (Pair x y) of ...`.

Collection literal lowering model:

- `[]` lowers to `collection_empty 0`
- `[a, b, c]` lowers to repeated `collection_extend` calls
- you can override `collection_empty` / `collection_extend` with your own
  definitions to customize concrete collection behavior

`do` and `ado` notation are intentionally not supported.

## Fundamental data types

Explicit primitive type inventory (language-level target set):

- `i64`: default numeric literal/arithmetic type today.
- `u64`: reserved primitive (not yet inferred from literals).
- `byte`: primitive for compact binary/host ABI boundaries.
- `string`: contiguous byte-string runtime value.

Current inference/runtime status:

- Integer literals and arithmetic builtins infer as `i64`.
- Comparison and boolean builtins infer as `bool`:
  - `eq`, `lt`, `le`, `gt`, `ge`, `str_eq`, `slice_eq_u8`, `and`
- String literals infer as `string`.
- Bridge builtins:
- `str_to_slice : string -> slice byte`
- `slice_to_string : slice byte -> string`
- `slice_len` consumes `slice byte`.
- `slice_get_u8`/`slice_set_u8` currently infer numeric payloads as `i64` in the
  current backend model.
- `slice_eq_u8 : slice byte -> slice byte -> bool` is bytewise equality over two
  byte slices and is lowered inline in wasm.
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
- WASM backend/runtime currently execute numeric values as tagged `i32` values
  (payload range `[-1073741824, 1073741823]`) while the source type model tracks
  `i64` names.
- WASM backend encodes string literals to UTF-8 bytes in linear-memory
  `[ptr,len]` descriptors.
- WASM backend emits string literals as linear-memory descriptors: each literal
  is placed as a fixed `[ptr,len]` descriptor in the module data section and
  referenced directly as a raw handle.
- `str_eq` performs bytewise content equality over string descriptors (not
  handle/pointer identity).
- `slice_eq_u8 : slice byte -> slice byte -> bool` performs bytewise equality over
  slice payloads and is lowered inline in wasm.
- WASM backend lowers `slice_len`/`slice_get_u8`/`slice_set_u8`/`slice_eq_u8`
  to direct linear-memory descriptor loads/stores (no per-element JS import
  hops).
- low-level memory helpers remain expression-level intrinsics, but collapse
  dead-temp pruning treats effectful ones as live side-effects.
- effectful memory operations (`slice_set_u8`, `memcpy_u8`, `memset_u8`,
  `region_reset`) are retained even when their result values are not referenced.
- for explicit sequencing, keep data dependencies between effectful operations
  in source.

## Data declarations

`data` declarations generate low-level constructor functions. Field access is
done by deconstruction in `let`:

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
class <class_name> <type_ctor> where
  <method_name> : <signature>
```

- Law declaration:

```haskell
law <class_name> <law_name> = <lhs_expr> => <rhs_expr>
```

- Instance declaration:

```haskell
instance <instance_name> : <class_name> <type_ctor> where
  <method> = <target>
  ...
```

Example class/instance declarations:

```haskell
class monad_rules m where
  pure : a -> m a
  bind : m a -> (a -> m b) -> m b

instance monad_on_maybe : monad_rules Maybe where
  pure = maybe_pure
  bind = maybe_bind
```

### Supported class kinds

- `add`
- `sub`
- `mul`
- `div`
- `ord`
- `slice`
- `monoid`
- `functor`
- `applicative`
- `monad`

Each kind enforces a required method set and required law-name set
(`Clapse.Laws`).

### Rewrite derivation model

- Class laws are written against required method names for the kind.
- Instance bindings map those method names to concrete target names.
- Parser derives concrete rewrite rules by substituting bindings into class
  laws.
- Function bodies are normalized with derived rules before lowering/collapse.

This gives a comptime-like "declare rules next to abstraction" flow while
keeping runtime pure and minimal.

## Compilation pipeline

1. Parse source (`Clapse.Syntax`).
2. Derive and apply class/law/instance rewrites (compile-time normalization).
3. Lower to stack ops (`Clapse.Lowering`).
4. Collapse to normalized stack-free IR + verify (`Clapse.CollapseIR`).
5. Emit WASM (`Clapse.Wasm`).

Collapsed IR includes currying normalization, immediate-apply collapse,
constant-argument direct-call specialization, small non-recursive inlining,
root-based dead-function pruning, dead-temp pruning, and self tail-call
normalization (`VSelfTailCall`).

## Parallelism roadmap (HVM2-inspired)

- keep collapse/rewrite passes pure and deterministic so independent redexes can
  be scheduled in parallel
- stage future collapse IR batching around independent call/rewrite regions
  (work-stealing friendly)
- keep verifier constraints as the correctness gate so parallel scheduling does
  not change program meaning

## Tooling

Single executable provides compiler + formatter + LSP:

```bash
deno run -A scripts/clapse.mjs format <file>
deno run -A scripts/clapse.mjs format --write <file>
deno run -A scripts/clapse.mjs format --stdin
deno run -A scripts/clapse.mjs compile <input.clapse> [output.wasm]
deno run -A scripts/clapse.mjs bench [iterations]
deno run -A scripts/clapse.mjs lsp --stdio
```

`compile` writes the wasm file at the requested output path and a `.d.ts`
sidecar at `replaceExtension(outputPath, "d.ts")`.

Formatter behavior today:

- syntax-validating and mostly source-preserving: collapses redundant internal
  horizontal whitespace (outside strings/comments), trims trailing horizontal
  whitespace, normalizes line endings/final newline
- expands single-line semicolon-separated `case ... of` arms to multiline layout
- supports multiline `case` expansion even with indented continuation lines
  (preserving `let`-arm continuations)
- rewrites multiline `let` blocks in Haskell-style layout (`let x = ...`,
  aligned bindings, `in ...` on its own aligned line)
- rewrites `case` arms with inline `let` into multiline arm bodies with the same
  Haskell-style `let` layout
- expands long inline `let ...; ... in ...` chains into multiline Haskell-style
  `let` blocks
- preserves class/instance `where` block declarations
- does not lower/collapse/rewrite declarations during formatting
- `format` exits with a non-zero status on format errors (`--write` never writes
  on error)

No host install step is required; use the Deno CLI directly.

LSP currently provides:

- compile diagnostics
- hover for `--|` doc comments on declarations (including class/instance
  `where` method lines)
- no inlay hints

## Benchmarking

Collapsed IR optimization-bar benchmark:

```bash
just bench 200000
```

`bench` compiles each case from its entry root (`hand` / `abstraction`) so
static/runtime bars compare only reachable collapsed code.

WASM runtime benchmark for a compiled module:

```bash
just bench-wasm-main 2000000 20000
```

WASM hand-vs-abstraction fixture comparison:

```bash
just bench-wasm-compare 2000000 20000
```

WASM feature-set hand-vs-abstraction comparisons (numeric baseline +
closure/env + struct-field + wrapper/uncurry):

```bash
just bench-wasm-compare-all 2000000 20000
```

## WASM runtime support

Current backend supports:

- numeric literals and local refs
- native wasm scalar lowering for arithmetic and boolean builtins
  (`add/sub/mul/div` + `eq/and`, tagged-`i32` runtime representation)
- tagged numeric runtime payload range is `[-1073741824, 1073741823]`
- compile rejects integer literals outside that tagged payload range (no silent
  wraparound)
- scalar numeric lowering assumes numeric operands at compile/runtime boundaries
  (no dynamic tag checks in those inlined ops)
- string literals as static contiguous bytes in module memory
- direct calls and direct branch lowering for case/guard-generated conditionals
- case/guard branch thunks that are single-use saturating closures are lowered to
  direct branch calls (skipping closure allocation/apply)
- closures and currying (`VClosure`, `VCurryDirect`, `VApply`)
- closure allocation/application are lowered inline in generated wasm (no
  emitted `rt_make_closure`, `rt_apply`, or `rt_apply2/3/4` helpers)
- struct allocation/field/tag operations are now emitted inline in generated
  Wasm (no `rt_make_struct`, `rt_get_field`, `rt_has_tag`, `rt_get_tag` helper
  functions)
- closure and struct runtime records are variable-sized in linear memory (no
  fixed 8-capture / 8-field backend ceiling)
- constructor tags are lowered through a module-local unique tag-id table (no
  hash-collision ambiguity)
- self-tail return in tail positions (`return_call`)
- low-level struct values for `data`
- JS byte-slice interop via `slice_len`, `slice_get_u8`, `slice_set_u8`, and
  `slice_eq_u8` with slice descriptors (`ptr,len`) in wasm linear memory
- slice + region + bulk memory builtins are lowered inline in generated Wasm (no
  dedicated `rt_slice_*`, `rt_region_*`, `rt_memcpy`, or `rt_memset` helpers
  emitted)
- collapsed IR global atoms now lower to wasm globals for known runtime globals
  (`__heap_ptr` / `__heap`)
- no emitted `rt_*` helper functions; runtime operations are lowered inline in
  generated wasm
- compiled modules now use wasm-native helpers for closures and do not require
  `rt_*` function imports
- JS runtime fallback `rt_*` imports have been removed

Deno runtime smoke:

```bash
deno run -A scripts/clapse.mjs compile examples/wasm_main.clapse out/wasm_main.wasm
deno run -A scripts/run-wasm.mjs out/wasm_main.wasm main 7
```

Deno slice interop smoke:

```bash
just wasm-interop-slice-smoke
```

Optional: compile standalone Deno CLI helpers for local/runtime use:

```bash
just deno-tools
```

`deno compile` downloads `denort` on first use.

Runtime contract smoke (tagged-int bounds + no JS `rt_*` fallback + slice copy
isolation):

```bash
just wasm-runtime-contract-smoke
```

WASM struct helper smoke (tagged `__is_*`, tag-safe `__get_*`):

```bash
just wasm-struct-helpers-smoke
```

WASM linear-memory helper smoke (`slice_new_u8` + `memcpy_u8` + `memset_u8` +
region helpers):

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

Then open `http://localhost:8080/examples/game_of_life.html`. The browser demo
keeps simulation transitions in Clapse (`LifeState`, `LifeEvent`, `apply_event`)
while JS handles rendering/timing/input and reads generation/alive-count from
wasm state.

Mario-like ECS demo:

```bash
just mario-serve 8080
```

Then open `http://localhost:8080/examples/mario_ecs.html`. This demo keeps tiny
ECS-like updates pure in Clapse (`MarioState`, `MarioEvent`, `apply_event`) and
uses JS only for keyboard input + sprite rendering.

## Examples

See `examples/README.md`.

Key examples:

- `examples/bootstrap_phase1_frontend_primitives.clapse`
- `examples/bootstrap_phase2_core_data_structures.clapse`
- `examples/bootstrap_phase3_entry.clapse`
- `examples/bootstrap_phase4_parser_pilot.clapse`
- `examples/bootstrap_phase5_dispatch_pilot.clapse`
- `examples/bootstrap_phase6_entry.clapse`
- `examples/bootstrap_phase7_host_capability_pilot.clapse`
- `examples/bootstrap_phase8_pattern_and_operators.clapse`
- `lib/compiler/kernel.clapse`
- `examples/bootstrap_phase10_frontend_lexer.clapse`
- `examples/bootstrap_phase11_parser_combinator_pilot.clapse`
- `examples/parser_layout_pain_points.clapse`
- `examples/bootstrap6/router.clapse`
- `examples/util/math.clapse`
- `examples/util/base.clapse`
- `examples/util/slice_scan.clapse`
- `examples/util/string_slice.clapse`
- `examples/util/json.clapse`
- `examples/class_arithmetic_rewrites.clapse`
- `examples/class_algebra_rewrites.clapse`
- `examples/traits_ord_slice.clapse`
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
- `examples/bench_wasm_slice_set_reuse.clapse`
- `examples/bench_wasm_slice_set_copy.clapse`
- `examples/wasm_struct_has_tag.clapse`
- `examples/wasm_struct_has_tag_false.clapse`
- `examples/wasm_struct_get_ok.clapse`
- `examples/wasm_struct_get_mismatch.clapse`

Self-host bootstrap checkpoint (`1/2/3/4/5/6/7/8/9/10/11`) now has concrete fixtures:

1. Frontend primitives: `data` constructors + `case` matching
   (`examples/bootstrap_phase1_frontend_primitives.clapse`)
2. Core data structures: recursive `List` with `Nil`/`Cons`
   (`examples/bootstrap_phase2_core_data_structures.clapse`)
3. Module graph: dotted imports/exports across transitive modules
   (`examples/bootstrap_phase3_entry.clapse`, `examples/util/*.clapse`)
4. Parser pilot: assignment-like byte-slice parser in Clapse
   (`examples/bootstrap_phase4_parser_pilot.clapse`)
5. Dispatch pilot: enum-code decode + ADT route dispatch
   (`examples/bootstrap_phase5_dispatch_pilot.clapse`)
6. Module dispatch pilot: routed decode/dispatch split across entry and imported
   module (`examples/bootstrap_phase6_entry.clapse`,
   `examples/bootstrap6/router.clapse`)
7. Host capability pilot: host import compilation path for time capability
   (`examples/bootstrap_phase7_host_capability_pilot.clapse`)
8. Syntax/behavior pilot: guards + operators + constructor-pattern case
   (`examples/bootstrap_phase8_pattern_and_operators.clapse`)
9. Native compiler ABI kernel: exported `clapse_run` response path in Clapse
   source (`lib/compiler/kernel.clapse`)
   - command dispatch now handles `compile`/`format`/`selfhost-artifacts`
     request shapes at protocol level
   - command detection now uses request-byte scanning for `"command":"..."`
     and is resilient to request key reordering
   - `format` now echoes the request `source` payload into `formatted`
     (kernel placeholder behavior before parser-aware formatting lands)
   - `selfhost-artifacts` now returns the full artifact key set and mirrors
     request `input_source` into `artifacts["merged_module.txt"]` (JSON-escaped)
   - `selfhost-artifacts` now requires `input_source` (legacy `source` fallback
     was removed for strict request-shape parity)
   - `compile` now expects `input_source` only (legacy `source` fallback was
     removed for strict request-shape parity)
   - `compile` currently returns a structured "not implemented yet" error
10. Frontend lexer pilot: token-class scanner and keyword recognizer in Clapse
    source (`examples/bootstrap_phase10_frontend_lexer.clapse`)
11. Parser-combinator pilot: parsec-style state-threaded primitives with
    `Monad`/`Applicative`/`Alternative`-style operators (`>>=`, `<$>`, `<*>`,
    `<|>`) plus `many`/`some`/`sepBy` for top-level declaration-shape parsing
    (`module`/`import`/`export`/`infix`/`data`/`newtype`/`class`/`law`/`instance`/signature/function,
    including guarded function declaration shape `name ... | ... = ...`)
    with committed reserved-keyword dispatch and blank/comment/attribute-line
    tolerant scanning (`#[...]` blocks before signatures/functions), multiline
    plain-function block RHS parsing (`name ... =` followed by indented body
    lines), indented guarded continuation parsing (`| ... = ...` on subsequent
    lines), module-optional declaration-artifact entry parsing (sources may
    start directly with imports/declarations), explicit `bool` + `MaybePos`
    adapters for parser entrypoint control flow (now canonicalized to
    polymorphic `Maybe`), shared `Maybe`-first parser helpers for function-head
    and decl-line parsing (`parse_function_head_m`, `parse_function_head_eq_m`,
    `parse_decl_line_m`, `parse_line_from_payload_m`), and function-shape
    tagging for plain vs guarded declaration lines in artifact mode;
    emits deterministic declaration artifacts via three exports:
    combined (`main`), aggregate stats (`main_stats`), and order stream (`main_stream`)
    (`examples/bootstrap_phase11_parser_combinator_pilot.clapse`)

Fast bootstrap gate:

```bash
just bootstrap-check
just bootstrap-phase9-smoke
just bootstrap-phase10-smoke
just bootstrap-phase11-smoke
```

Three-stage compiler chain proof (Haskell -> wasm compiler A -> wasm compiler B -> wasm compiler C):

```bash
just bootstrap-chain-proof
```

This runs Stage B and Stage C with a wasm-only command path and
requires identical wasm hashes for A/B/C.

Compiler parity gate:

```bash
just parity-check
```

Self-host differential artifacts/parity gates:

```bash
# emit parse/type/lower/collapse/export/wasm stats artifacts for one entry module
deno run -A scripts/run-clapse-compiler-wasm.mjs -- selfhost-artifacts examples/bootstrap_phase6_entry.clapse out/selfhost-artifacts

# compare left/right compiler engines over corpus manifest
deno run -A scripts/selfhost-diff.mjs --manifest examples/selfhost_corpus.txt --out out/selfhost-diff

# parser artifact parity harness (`merged_module.txt`) over compiler-source corpus
deno run -A scripts/selfhost-parser-parity.mjs --manifest examples/selfhost_parser_corpus.txt --out out/selfhost-parser-parity

# compare left/right compiler engines on wasm execution scenarios
deno run -A scripts/selfhost-behavior-diff.mjs --manifest examples/selfhost_behavior_corpus.json --out out/selfhost-behavior-diff

# formatter idempotence gate for compiler-source corpus
deno run -A scripts/formatter-idempotence-corpus.mjs --manifest examples/compiler_source_corpus.txt --out out/formatter-idempotence

# wasm LSP fixtures (diagnostics + hover request path)
CLAPSE_ALLOW_BRIDGE=1 CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler_bridge.wasm deno run -A scripts/lsp-wasm-fixtures.mjs

# stage A/B/C bootstrap orchestration + report
deno run -A scripts/selfhost-bootstrap-abc.mjs --manifest examples/selfhost_corpus.txt --behavior-manifest examples/selfhost_behavior_corpus.json --out out/selfhost-bootstrap

# one-shot local gate
just selfhost-check

# strict gate: requires distinct left/right engine commands
just selfhost-check-strict

# strict gate with explicit engine commands
SELFHOST_LEFT_CMD='CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler.wasm deno run -A scripts/run-clapse-compiler-wasm.mjs --' \
SELFHOST_RIGHT_CMD='CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler.wasm deno run -A scripts/run-clapse-compiler-wasm.mjs --' \
just selfhost-check-strict
```

Strict bootstrap now also rejects compiler corpus entries that import
`host.clapse` (enforced via `--forbid-host-clapse-imports 1` in
`selfhost-bootstrap-abc-strict`).

Manifest consistency guard:

```bash
deno run -A scripts/check-selfhost-manifests.mjs
```

Release candidate reproducibility guard:

```bash
just release-candidate
```

This is the release-time proof we use before deprecating the Haskell path: if
artifact hashes or fixture maps diverge, the release fails.

Additional manifests/fixtures used by the new parser/formatter/LSP gates:

- `examples/selfhost_parser_corpus.txt`
- `examples/compiler_source_corpus.txt`
- `examples/lsp_wasm_fixtures.json`

Parity performance benchmark:

```bash
just selfhost-bench
just selfhost-bench-wasm
just selfhost-bench-wasm-fresh
just selfhost-bench 3
just selfhost-bench-wasm 3
# optional: force fresh compile on every repeat for both sides
deno run -A scripts/selfhost-bench.mjs --manifest examples/selfhost_behavior_corpus.json --repeats 3 --reuse-compiles-across-repeats 0 --out out/selfhost-bench-fresh
```

Wasm compiler runner:

- `scripts/run-clapse-compiler-wasm.mjs` is the strict right-engine entrypoint.
- Strict parity gates require `engine-mode` to report a wasm mode (`wasm-native`
  or `wasm-bridge`; `wasm` checks accept both for compatibility).
- Required environment:

```bash
CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler.wasm
```

Compile and `selfhost-artifacts` are strict wasm execution paths.
If the compiler artifact is missing/invalid, or responses are malformed,
commands fail immediately.

Current phase9 kernel wiring:

- `lib/compiler/kernel.clapse` imports `host.clapse` and
  routes `compile` and `selfhost-artifacts` requests through
  `clapse_host_run`.
- This keeps strict wasm-right parity free of fixture substitution while the
  full compiler-in-clapse pipeline is still being completed.

Runner ABI contract:

- compiler wasm must export `memory` or `__memory`
- compiler wasm must export
  `clapse_run(request_slice_handle: i32) -> response_slice_handle: i32`
- request/response payloads are UTF-8 JSON stored in slice descriptors
- supported commands: `compile`, `selfhost-artifacts`, `engine-mode`

Expected JSON payloads:

```json
{ "command": "compile", "input_path": "path.clapse", "input_source": "..." }
```

```json
{
  "command": "selfhost-artifacts",
  "input_path": "path.clapse",
  "input_source": "..."
}
```

Compile success response:

```json
{
  "ok": true,
  "wasm_base64": "<base64 wasm bytes>",
  "exports": [{ "name": "main", "arity": 1 }],
  "dts": "optional pre-rendered .d.ts text"
}
```

Compile failure response:

```json
{ "ok": false, "error": "compile error message" }
```

Selfhost-artifacts response:

```json
{
  "ok": true,
  "artifacts": {
    "merged_module.txt": "...",
    "type_info.txt": "...",
    "type_info_error.txt": "...",
    "lowered_ir.txt": "...",
    "collapsed_ir.txt": "...",
    "exports.txt": "...",
    "wasm_stats.txt": "..."
  }
}
```

```bash
CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler.wasm just selfhost-check-wasm
```

Bridge artifact for immediate strict-path validation:

```bash
just selfhost-build-wasm-bridge
CLAPSE_ALLOW_BRIDGE=1 \
just selfhost-check-wasm-bridge
```

`selfhost-check-wasm-bridge` runs strict wasm-right parity using a minimal wasm
module that exports `clapse_run`/`memory` and forwards requests through host
capability imports. This validates the runner path end-to-end while the full
compiler-in-clapse wasm artifact is still in progress.

Strict gates can be wired explicitly:

```bash
SELFHOST_LEFT_CMD='CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler.wasm deno run -A scripts/run-clapse-compiler-wasm.mjs --' \
SELFHOST_RIGHT_CMD='CLAPSE_COMPILER_WASM_PATH=out/clapse_compiler.wasm deno run -A scripts/run-clapse-compiler-wasm.mjs --' \
just selfhost-check-strict
```

## Self-hosting roadmap (remaining)

Goal: compile Clapse with Clapse (Haskell becomes bootstrap host only, then
optional fallback).

1. Define stable compiler IR/ABI contract

- Freeze typed AST and collapsed IR schemas used by
  parser/type/lowering/collapse/wasm stages.
- Freeze runtime value ABI (tagged ints, struct/slice layouts, closure layout,
  region semantics).
- Add golden serialization/parity tests for IR and exported wasm API metadata.

2. Build compiler frontend in Clapse

- Implement tokenizer + parser in Clapse for current syntax subset (modules,
  data, case, let, operators, signatures, attributes).
- Keep Haskell parser as oracle; add differential tests (same source =>
  equivalent AST/normalized form).
- Acceptance: Clapse parser can parse and format phase fixtures + example
  corpus.

3. Build type + rewrite pipeline in Clapse

- Implement inference/checking for current core types and class/law/instance
  rewrite derivation.
- Port trait/law normalization pipeline and keep rewrite parity with Haskell
  implementation.
- Acceptance: type/rewrite outputs match Haskell for corpus fixtures.

4. Build lowering + collapse pipeline in Clapse

- Port lowering (closures, currying, case desugaring) and collapse passes
  (normalize, inline, specialize, prune, tail-opt, region/slice/escape passes).
- Lock pass order and invariants with parity tests.
- Acceptance: collapsed IR for fixtures is equivalent (modulo temp
  renaming/order where valid).

5. Build wasm emitter in Clapse

- Port wasm codegen (numeric builtins, closures, struct helpers, slice/region
  ops, exports).
- Keep wasm behavior parity via runtime smoke tests and differential execution.
- Acceptance: generated wasm passes existing smoke/tests and benchmark harness.

6. Add self-host bootstrap stages (compiler-in-clapse)

- Stage A: compile Clapse compiler sources with Haskell compiler into wasm
  artifact.
- Stage B: run compiled clapse-compiler wasm to compile examples and phase
  fixtures.
- Stage C: compare Stage B outputs vs Haskell compiler outputs (exports,
  behavior, perf envelope).
- Acceptance: stage B passes `just parity-check` equivalent gates.

7. Introduce CLI host wrapper for production use

- Keep thin host runner (Rust or Deno) for file IO, process args, and host
  capability bridging.
- Clapse compiler logic remains pure and wasm-executed.
- Acceptance: `clapse` CLI uses wasm compiler path by default behind a flag,
  then becomes default.

8. Cutover and deprecate Haskell backend path

- Default to self-hosted compiler path once parity and perf bars are met.
- Keep Haskell path as fallback for a transition window; remove when stable.
- Acceptance: release criteria met on correctness, performance, and DX.
- Release gate: `just release-candidate` is green with stable fixture-map parity
  and checksumed compiler artifacts.

Recommended immediate next execution order:

1. Add AST parity test fixtures (Haskell parser vs clapse parser prototype).
2. Implement minimal Clapse parser module that covers bootstrap phases 1-11.
3. Add Stage A/B driver script to compile and run compiler-wasm against example
   corpus.

Detailed execution checklist:

- `docs/clapse-language/references/self-hosting-roadmap.md`
- `docs/clapse-language/references/selfhost-pain-points.md`

## AI-first docs

AI-first language documentation lives as a skill at:

- `docs/clapse-language/SKILL.md`
- `docs/clapse-language/references/`

## Tree-sitter and Helix

- Grammar: `tree-sitter-clapse/`
- Helix local setup: `just install`
- Sync local grammar path: `./scripts/setup-helix-local.sh`
- Tree-sitter includes reliable `let ... in ...` expression/binding parsing
  (`in` must be a real separator, not an identifier prefix like `in_cells`),
  wildcard `_` pattern nodes, guarded function clauses, robust multi-branch
  `case ... of` parsing, explicit `module/import/export` declarations, and
  operator highlighting for symbolic/infix/backtick/custom operator-declaration
  tokens.

## Status

Implemented now:

- parser for functions, `data`, `class`, `law`, `instance`
- parser support for multi-constructor `data` declarations and one-line GADT
  constructor alternatives
- parser support for parenthesized recursive old-style constructor field type
  expressions (`data List a = Nil | Cons a (List a)`)
- parser support for `let ... in ...` expressions (including `;`-separated
  bindings)
- parser support for indented multiline `let` continuation lines in function
  bodies
- parser support for guarded function declarations using `|` clauses with
  `otherwise`
- parser support for constructor deconstruction in `let` bindings
- parser support for `case ... of` expressions with multi-scrutinee matching and
  constructor patterns
- parser support for top-level function type signatures, including optional
  named witness constraints
- parser support for top-level function attributes (`#[memo ...]`,
  `#[test ...]`, `#[bench ...]`) and clause-group propagation
- parser support for Haskell-style class/instance declarations with `where`
  blocks (`class <name> <type_ctor> where ...`,
  `instance <name> : <class> <type_ctor> where ...`)
- parser support for collection literals (`[]`, `[a, b, ...]`)
- parser tolerance for `module/import/export` directives in syntax
  validation/format/lsp paths
- module compilation now module-qualifies constructor helper tags
  (`__mk_*`/`__get_*`/`__is_*`) to avoid cross-module constructor tag collisions
- builtin infix operators (`+ - * / == < <= > >= && ||`) plus custom operator declarations
  with fixity/precedence and backtick operators
- string literal case-pattern matching via `str_eq`-based desugaring
- compile-time rewrite derivation from class/law/instance declarations
- closure-aware lowering and normalized collapsed IR with verifier
- lowering support for top-level function values via zero-capture closures (for
  example `twice inc x`)
- lowering support for nullary constructor values in expression position
  (module-aware zero-arity constructor calls)
- automatic currying normalization and immediate-apply collapse
- constant-argument direct-call specialization for small wrapper-style functions
- small non-recursive interprocedural inlining for wrapper-style functions
- escape/lifetime analysis over collapsed IR for closure/struct temps
- simple slice ownership analysis for `slice_set_u8` (reuse on linear ownership,
  copy path on shared targets)
- struct representation flattening for non-escaping constructor values
  (`__get_*`/`__is_*` rewrite to direct atoms/constants)
- closure capture-layout flattening pass that trims unused captured locals and
  rewrites call/curry/closure argument layouts
- hot wrapper uncurrying pass that rewrites high-frequency apply-chain wrappers
  to direct `VApply` chains before final normalization
- deeper post-uncurry normalize+inline rounds that collapse multi-arg lambda
  towers toward direct numeric code
- root-based dead-function pruning support in collapse pipeline
- pruning keeps parents when reachable lifted descendants remain, preventing
  orphan lifted-call breakage
- self tail-call optimization in collapsed IR
- pure source + collapsed evaluators with differential tests
- WASM backend with closure/currying/data-struct runtime interop
- wasm struct helpers enforce runtime tag safety for getters and tagged-bool
  predicates
- wasm tag comparisons now use module-local unique tag ids instead of hashed
  string tags
- native wasm scalar lowering for numeric builtins (`add/sub/mul/div/eq/and`)
- static string literal lowering into wasm memory/data with shared runtime
  handles
- byte-slice builtins lowered directly to wasm memory ops
  (`i32.load`/`i32.load8_u`/`i32.store8`) with JS helpers that allocate/copy
  slice descriptors in linear memory
- formatter + LSP in same executable
- module-aware compile path for entry files (`module/import/export`,
  cycle/missing-module checks, explicit export lists)
- tree-sitter grammar + highlight/textobject/indent/tags/rainbow query set
- Deno wasm runtime benchmark harness (`scripts/bench-wasm.mjs`) and `just`
  benchmark targets
- browser canvas Game of Life demo driven by Clapse-compiled wasm rule function

Not implemented yet:

- full numeric-width runtime (`i64/u64/byte`) in backend ABI
- full proof checker
- richer public WASM memory/globals ABI (beyond internal `__heap_ptr` / `__heap`
  globals)
- full host capability surface (currently `import host.io`/`read_file` and
  `import host.time`/`unix_time_ms`)
