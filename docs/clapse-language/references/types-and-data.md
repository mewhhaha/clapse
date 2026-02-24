# Types and Data Model

## Primitive Inventory

Current language-level target set:

- `i64` (default numeric literal/arithmetic type in source model)
- `u64` (reserved)
- `byte` (reserved for compact binary/host ABI boundaries)

Conventional core data wrappers:

- `data string = string<slice byte>`
- `data bool = true<1> | false<0>`

`type` aliases may define literal unions (for example `type Digit = <0 | 1 | 2>`) and may not own constructors. Constructor ownership belongs to `data`.

## Current Inference Behavior

- Integer literals and arithmetic builtins infer as `i64`.
- String literals infer as `string`.
- Runtime backend still represents numeric values as tagged `i32` in WASM runtime ABI.

## Data Declarations

`data` declarations compile to constructor functions.

Example:

```haskell
data Pair a b = Pair a b
```

Conceptually becomes constructor builtins (`__mk_*`) in lowered/backend form.

## Field Access

- Do not rely on generated user-facing getters.
- Deconstruct through patterns in `let` / `case`.

```haskell
let Pair left right = value in left
```

## Constructor Matching

- Constructor patterns enforce arity by declaration.
- Exhaustive single-scrutinee constructor cases may omit trailing `_` catch-all when coverage is complete.

## Collections

Collection literals are abstract syntax and currently lower to:

- `collection_empty`
- repeated `collection_extend`

This is currently a pure collection encoding, not a contiguous buffer/slice ABI.
