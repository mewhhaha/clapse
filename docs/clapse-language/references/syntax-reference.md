# Syntax Reference

## Naming

- Value/function identifiers: `snake_case`
- Data type and constructor names: `PascalCase`

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

## Type Signatures and Constraints

```haskell
combine : (m : monoid_rules t) => t -> t -> t
combine x y = append x y
```

## Classes/Laws/Instances (Rewrite Metadata)

```haskell
class plus_rules i : add
law plus_rules right_identity = add x 0 => x
instance plus_on_i64 : plus_rules i add=plus
```

## Operators

Builtin operators (no declaration required):

```haskell
x + y
x * y
x == y && y == z
```

- `+` => `add` (infixl 6)
- `-` => `sub` (infixl 6)
- `*` => `mul` (infixl 7)
- `/` => `div` (infixl 7)
- `==` => `eq` (infix 4)
- `&&` => `and` (infixr 3)

Custom operators:

```haskell
infixl 6 +. = add
infixr 5 <> = append
infixl 6 plus_op = add
```

Custom declarations override builtins for matching operator tokens.

Use symbolic operator:

```haskell
x +. y
```

Use identifier operator:

```haskell
x `plus_op` y
```

## Collection Literals

```haskell
[]
[1, 2, 3]
```

Current lowering model uses `collection_empty`/`collection_extend`; this is not a packed linear buffer ABI.
