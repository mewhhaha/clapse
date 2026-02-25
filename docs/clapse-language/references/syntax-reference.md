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
instance plus_on_i64 : plus_rules i where
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

class Applicative f where
  pure : a -> f a
  ap : f (a -> b) -> f a -> f b

class Monad m where
  bind : m a -> (a -> m b) -> m b

class Alternative f where
  empty : f a
  append : f a -> f a -> f a

instance ParserFunctor : Functor parser where
  fmap = parser_map

instance ParserApplicative : Applicative parser where
  pure = parser_pure
  ap = parser_ap

instance ParserMonad : Monad parser where
  pure = parser_pure
  bind = parser_bind

instance ParserAlternative : Alternative parser where
  empty = parser_empty
  append = parser_or
```

The compiler now uses a local evidence model for dispatch:
`ClassEvidenceResolved`, `ClassEvidenceUnknown`, and `ClassEvidenceAmbiguous`.
Static dispatch remains the default when inference resolves to a single target, and
`ClassFundepInfoAmbiguous` forces dynamic dispatch.

For applicative form `a <*> b`, dispatch is inferred from the combined evidence of both
operands and fundep metadata, so type-directed static dispatch can be chosen without
explicit signatures. Explicit signatures are only required when that inference remains
ambiguous.

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
- `&&` => `and` (infixr 3)
- `||` => `or` (infixr 2, declaration usually `infixr 2 || = or` in prelude/custom modules)

Custom operators:

```haskell
infixl 6 +. = add
infixr 2 || = or
infixr 5 <> = append
infixl 6 plus_op = add
```

The compiler prelude also defines the shared boolean class in Haskell-like naming:

```haskell
data bool = true<1> | false<0>
class Boolean b where
  not : b -> b
  and : b -> b -> b
  or : b -> b -> b

instance BooleanBool : Boolean bool where
  not = bool_not
  and = bool_and
  or = bool_or

infixr 3 && = and
infixr 2 || = or
```

`Boolean` supplies the `bool` laws (identity, annihilation, and double-negation) with the default `bool` instance.

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
infixl 1 >>= = bind
infixl 1 >> = then_m

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

## Collection Literals

```haskell
[]
[1, 2, 3]
```

Current lowering model uses `collection_empty`/`collection_extend`; this is not a packed linear buffer ABI.
