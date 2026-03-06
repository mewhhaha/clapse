# Layout and Formatting Rules

## Canonical Module Header

`module` declarations are deprecated and rejected in this branch.

Use standard top-level declarations directly; modules are resolved by source name.

## Import Forms

Import lines are layout-insensitive except in `case`, `let`, and function clauses:

- No parenthesized wrapping is required for single-line import declarations.
- Keep imported names one-per-line for long symbol lists.

```text
import "prelude"
import "prelude" { foldl, Filterable }
```

## Function and `let` Layout

Use consistent indentation for guarded clauses and `let` blocks.

```clapse
main x =
  let selected =
        case eq x 0 of
          true -> 0
          _ -> x
  in selected
```

## Case Layout

`case` scrutinee arms and guard arms follow indentation-driven grouping.

```text
case xs of
  ListNil -> 0
  ListCons x rest ->
    case rest of
      ListNil -> x
      ListCons y _ -> x + y
```

## Export Layout

Export lists stay in brace form and are comma-separated.

```text
export {
  main,
  helper,
}
```

## Spacing

- Tabs and form feeds are not special; use spaces.
- Blank lines separate logical blocks.
