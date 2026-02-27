# Syntax Reference

## Naming

- Value/function identifiers: `snake_case`
- Data type names: `PascalCase`
- Constructor names are usually `PascalCase`, with literal-backed constructors
  commonly written as lowercase (for example `true`/`false`):

```haskell
data bool = true<1> | false<0>
```

## Functions

Single equation:

```haskell
inc x = x + 1
```

Pattern-style multiple equations:

```haskell
add 0 0 = 0
add x y = x + y
```

## Literals

- Integer: `42`, `-1`
- String: `"hello\nworld"`
- Char: `'a'`, `'\n'`, `'\''` (parsed as integer codepoints)

## Lambda

```haskell
make_adder x = \y -> x + y
```

## Let Bindings

```haskell
main x =
  let y = x + 1
      z = y * 2
  in z
```

Constructor deconstruction in `let`:

```haskell
let Pair left right = p in left
```

## Case Expressions

Single scrutinee:

```haskell
case m of
  Nothing -> 0
  Just n -> n + 1
```

Multiple scrutinees:

```haskell
case a b of
  0 0 -> 0
  x y -> x + y
```

## Data Declarations

Simple constructor form:

```haskell
data Pair a b = Pair a b
```

Multi-constructor GADT-like one-line form:

```haskell
data Maybe a = Just : a -> Maybe a | Nothing : Maybe a
```

Newtype form (single constructor + single field):

```haskell
newtype UserId = UserId i64
```

## Type Signatures and Constraints

```haskell
combine : (m : monoid_rules t) => t -> t -> t
combine x y = append x y
```

## Classes/Laws/Instances (Rewrite Metadata)

```haskell
class plus_rules i where
  add : i -> i -> i
law plus_rules right_identity = add x 0 => x
instance plus_rules i where
  add = plus
```

Functional dependencies can be written on the class header using
`|` and `->` in minimal one-way form:

```haskell
class eq_by_id a | a -> add where
  eq : a -> a -> bool
```

Compiler prelude classes now follow Haskell-style `where` blocks:

```haskell
class Functor f where
  fmap : (a -> b) -> f a -> f b
  <$> : (a -> b) -> f a -> f b
  <$ : a -> f b -> f a
  <$> = fmap
  <$ = map_replace

class Applicative f where
  pure : a -> f a
  ap : f (a -> b) -> f a -> f b
  <*> : f (a -> b) -> f a -> f b
  keep_left : f a -> f b -> f a
  keep_right : f a -> f b -> f b
  <* : f a -> f b -> f a
  *> : f a -> f b -> f b
  <*> = ap
  <* = keep_left
  *> = keep_right

class Monad m where
  bind : m a -> (a -> m b) -> m b
  then_m : m a -> m b -> m b
  >>= : m a -> (a -> m b) -> m b
  >> : m a -> m b -> m b
  >>= = bind
  >> = then_m

class Alternative f where
  empty : f a
  append : f a -> f a -> f a
  alt : f a -> f a -> f a
  <|> : f a -> f a -> f a
  <|> = alt

instance Functor parser where
  fmap = parser_map

instance Applicative parser where
  pure = parser_pure
  ap = parser_ap
  keep_left = keep_left_default
  keep_right = keep_right_default

instance Monad parser where
  pure = parser_pure
  bind = parser_bind
  then_m = then_m_default

instance Alternative parser where
  empty = parser_empty
  append = parser_or
  alt = alt_default
```

The compiler now uses a local evidence model for dispatch:
`ClassEvidenceResolved`, `ClassEvidenceUnknown`, and `ClassEvidenceAmbiguous`.
Static dispatch remains the default when inference resolves to a single target, and
`ClassFundepInfoAmbiguous` forces dynamic dispatch.

For applicative form `a <*> b`, dispatch is inferred from the combined evidence of both
operands and fundep metadata, so type-directed static dispatch can be chosen without
explicit signatures. Explicit signatures are only required when that inference remains
ambiguous.

In the compiler prelude, operator-facing methods (`ap`, `then_m`, `keep_left`,
`keep_right`, `alt`) are class members, and their default equations are expressed in
terms of `bind`/`pure`/`append` so custom instances can override while preserving law shape.
`<$` is mapped to `map_replace`, which defaults to `fmap (\_ -> x)`.
Default helper functions are `map_replace_default`, `ap_default`, `then_m_default`,
`keep_left_default`, `keep_right_default`, and `alt_default`.

## Class Dispatch Witness (Kernel)

Class method resolution in the compiler kernel is controlled by an explicit witness:

```haskell
data ClassDispatchMode = ClassDispatchStatic | ClassDispatchDynamic
kernel_class_dispatch = class_dispatch_default

resolve_class_method dispatch static_method dynamic_method =
  case dispatch of
    ClassDispatchStatic -> static_method
    ClassDispatchDynamic -> dynamic_method

let selected_method =
      resolve_class_method kernel_class_dispatch
        law_static
        law_dynamic
in ...
```

`class_dispatch_default` is defined as `ClassDispatchStatic`, so the default bootstrap path resolves to static method implementations and enables static law simplification after rewrite selection.

This is separate from parser branch selection (`ParserDispatch`), which still governs parser alternatives in `lib/compiler/parser.clapse`.

Declaration parsing for `class`, `law`, and `instance` forms in compiler pathways feeds class declarations through the same class dispatch/law pipeline as `lib/compiler/kernel.clapse` using the same dispatch evidence model.

## Operators

Builtin operators (no declaration required):

```haskell
x + y
x * y
x < y && y >= z
not x && y || z
```

- `+` => `add` (infixl 6)
- `-` => `sub` (infixl 6)
- `*` => `mul` (infixl 7)
- `/` => `div` (infixl 7)
- `==` => `eq` (infix 4)
- `<` => `lt` (infix 4)
- `<=` => `le` (infix 4)
- `>` => `gt` (infix 4)
- `>=` => `ge` (infix 4)
- `not` => `bool_not` via the compiler-prelude alias `not`
- `&&` (infixr 3)
- `||` (infixr 2)

Custom operators:

```haskell
infixl 6 +.
infixr 2 ||
infixr 5 <>
infixl 6 plus_op
```

The compiler prelude also defines the shared boolean class in Haskell-like naming:

```haskell
data bool = true<1> | false<0>
data Unit = Unit
data Lazy a = Lazy : (Unit -> a) -> Lazy a

-- `~a` is the source-level signature marker for lazy parameters.
-- Current runtime lowering still uses the internal `Lazy a` box.

force : Lazy a -> a
force l = case l of Lazy thunk -> thunk Unit

lazy : (Unit -> a) -> Lazy a
lazy thunk = Lazy thunk

class Boolean b where
  not : b -> b
  and : b -> b -> b
  or : b -> b -> b
  xor : b -> b -> b
  implies : b -> b -> b
  && : b -> b -> b
  || : b -> b -> b
  xor a b = or (and a (not b)) (and (not a) b)
  implies a b = or (not a) b

instance Boolean bool where
  not true = false
  not false = true

  and false _ = false
  and _ false = false
  and _ _ = true

  or true _ = true
  or _ true = true
  or _ _ = false

  xor true false = true
  xor false true = true
  xor _ _ = false

  implies true false = false
  implies _ _ = true

  && false _ = false
  && _ false = false
  && _ _ = true

  || true _ = true
  || _ true = true
  || _ _ = false

infixr 3 &&
infixr 2 ||
```

`Boolean` supplies the `bool` laws (identity, annihilation, and double-negation) with the default `bool` instance. `&&` and `||` are class methods with signatures `b -> b -> b`, and the default `bool` instance defines short-circuit-like behavior through constructor-driven clauses. `xor`/`implies` are available as boolean methods.

Pattern-demand semantics with `_`

- `_` is a wildcard binder (it does not bind a variable name).
- In clause matching, wildcard positions introduce no demand for that argument position.
- The matcher may reorder argument tests to reduce demand, but must remain deterministic:
  - preserve clause priority (top-to-bottom),
  - choose the next demanded argument by a fixed strategy,
  - break ties by source argument order (left-to-right).
- This wildcard-demand behavior is separate from explicit laziness:
  `~` and `force` remain the surface for guaranteed lazy API boundaries.

Custom declarations override builtins for matching operator tokens.

Use symbolic operator:

```haskell
x +. y
```

Use identifier operator:

```haskell
x `plus_op` y
```

Monadic-style chaining

These operators are typically available with low precedence and left associativity:

`>>=` and `>>` are `infixl 1`, so they parse left-to-right.

```haskell
infixl 1 >>=
infixl 1 >>
m >>= \x ->
  stepA x >>
  stepB x
```

- `>>=` and `>>` are weaker than arithmetic/comparison/logical operators (`+`, `==`, `&&`, `||`, ...), so they usually go at the outer level: write `m1 >> m2 >>= \x -> ...` only if that order is intended.
- Parentheses are only needed to override this order, e.g. to force sequencing before a larger continuation: `m >>= (\x -> f x >> g)`.
- Formatter strategy: normalize multiline bind/sequence chains to a procedural left-rail layout:

  ```haskell
  pipeline input =
    open_tx input
    >>= \tx ->
      step_a tx
      >> step_b tx
    >>= \a ->
      finish a
  ```

Applicative/Functor shorthand operators map to named functions from the prelude:

- `<$` => `map_replace` (`infixl 4`)
- `<$>` => `fmap` (`infixl 4`)
- `<*>` => `ap` (`infixl 4`)
- `<*` => `keep_left` (`infixl 4`)
- `*>` => `keep_right` (`infixl 4`)

## Collection Literals

```haskell
[]
[1, 2, 3]
```

Current lowering model uses `collection_empty`/`collection_extend`; this is not a packed linear buffer ABI.
