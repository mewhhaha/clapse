# Getting Started

This document uses executable Clapse examples.

## Simple function

```clapse skip
module getting_started

--| Add one to an integer.
inc x = x + 1

main n = inc n
```

## Tagged booleans

```clapse skip
module tagged_bool

primitive bool = true<1> | false<0>

--| Convert bool to i64 payload.
to_i64 b = case b of
  true -> 1
  false -> 0

main x = to_i64 (case x == 0 of
  true -> true
  _ -> false)
```

## String wrapper

```clapse skip
module string_wrapper

primitive string = string<slice u8>

--| Wrap a byte slice as string.
from_slice bytes = string bytes

main n = n
```
