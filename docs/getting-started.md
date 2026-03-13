# Getting Started

This document uses executable Clap examples.

## Simple function

```clap skip
--| Add one to an integer.
inc x = x + 1

main n = inc n
```

## Tagged booleans

```clap skip
literal bool = true<1> | false<0>

--| Convert bool to i64 payload.
to_i64 b = case b of
  true -> 1
  false -> 0

main x = to_i64 (case x == 0 of
  true -> true
  _ -> false)
```

## String wrapper

```clap skip
literal string = string<slice u8>

--| Wrap a byte slice as string.
from_slice bytes = string bytes

main n = n
```
