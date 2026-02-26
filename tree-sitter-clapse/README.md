# tree-sitter-clapse

Tree-sitter grammar scaffold for current Clapse syntax.

## Covered syntax

- Function declarations: `name args = expr`
- Function signatures: `name : type_expr` (including constraint forms like `name : (m : trait t) => t -> t`)
- Data declarations: `data Type = Constructor field ...`
- Data declarations with multiple constructors: `data Type = Ctor args | GadtCtor : type_expr`
- Class declarations: `class class_name : kind`
- Law declarations: `law class_name law_name = lhs => rhs`
- Instance declarations: `instance class_name type_arg... where method=target ...`
- Class/instance heads can include optional type arguments: `class ClassName TypeArg ... : kind`, `instance inst : ClassName TypeArg ... method=target`
- Expressions:
  - variables (`x`)
  - integer literals (`42`)
  - string literals (`"hello"`, escaped as `\"`, `\\`, `\n`, `\t`, `\r`)
  - lambda (`\\x -> expr`, `\\x y -> expr`)
  - application (`f x y`, left-associative)
- case expressions, including multiline/indented arms
  - `case a b of Pair x y -> add x y; 0 0 -> 0`
  - multiline with indented arms:
    - `case a b of`
    - `  Pair x y -> add x y; 0 0 -> 0`
- case branch patterns with multi-token patterns (including constructor patterns such as `Pair x y`)
  - parenthesized expressions (`(expr)`)
  - collection literals (`[]`, `[a, b]`)
- Line comments: `-- ...`

## Query support

Queries shipped in `queries/`:

- `highlights.scm`
- `textobjects.scm`
- `indents.scm`
- `tags.scm`
- `rainbows.scm`

## Strict behavior

- Declarations are newline-delimited.
- Application requires whitespace separation; `f(x)` is rejected.
- Decimal integer tokens are non-negative; unary minus is parsed as an operator (for example `-42`).
- Value identifiers are snake_case: `[a-z_][a-z0-9_']*`.
- Type names are capitalized identifiers: `[A-Z][A-Za-z0-9_']*` (for example `Pair`).
- Constructors are usually capitalized, but literal-backed lowercase constructors are also valid (for example `true` and `false`).

## Development

```bash
npm install
npx tree-sitter generate
npx tree-sitter test
```

From the repo root:

```bash
cd tree-sitter-clapse
tree-sitter generate
XDG_CACHE_HOME=/tmp tree-sitter test
```

## Helix integration

From repo root:

```bash
just install
```

Manual equivalent:

```bash
./scripts/setup-helix-local.sh
hx --health clapse
```
