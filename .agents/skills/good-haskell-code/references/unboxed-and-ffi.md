# Unboxed and FFI Hot-Path Patterns (GHC 9.14.1)

## Contents
- 1. Use unboxed features only in measured hotspots
- 2. Strict/unpacked data layout
- 3. Choose ByteArray/PrimArray/Vector intentionally
- 4. FFI boundary safety
- 5. Pinned memory guidance
- 6. Profiling-first rollout

## 1) Use unboxed features only in measured hotspots

`UnboxedTuples`, `UnboxedSums`, and primops are for leaf hotspots where profiling proves benefit.

```haskell
-- GOOD
{-# LANGUAGE UnboxedTuples #-}
hotStep# :: Int# -> Int# -> (# Int#, Int# #)
hotStep# x y = (# x +# y, x -# y #)
```

```haskell
-- BAD
-- introduce unboxed internals in high-level orchestration code first
appMain# :: ...
```

Why: unboxed code increases complexity; keep it local and measured.

## 2) Strict/unpacked data layout

```haskell
-- GOOD
data Vec2 = Vec2
  { x :: {-# UNPACK #-} !Double
  , y :: {-# UNPACK #-} !Double
  }
```

```haskell
-- BAD
data Vec2 = Vec2
  { x :: Double
  , y :: Double
  }
```

Why: strict/unpacked fields reduce indirections and improve cache locality in hot loops.

## 3) Choose ByteArray/PrimArray/Vector intentionally

```txt
GOOD:
- ByteArray: raw bytes and binary codecs.
- PrimArray: immutable unboxed primitive values.
- MutablePrimArray/ST or mutable vectors: scoped mutation before freeze.
```

```txt
BAD:
- list-based numeric hot paths;
- repeated conversions list <-> vector in tight loops.
```

Why: representation choice drives allocation and traversal costs.

## 4) FFI boundary safety

```haskell
-- GOOD
foreign import ccall unsafe "sum_ints"
  c_sum_ints :: Ptr CInt -> CSize -> IO CInt
```

```haskell
-- BAD
foreign import ccall "sum_ints"
  c_sum_ints_bad :: Ptr Int -> Int -> IO Int
```

Why: use `C*` types at boundaries and explicit conversions at call sites.

Rules:
1. Keep boundary modules small and isolated.
2. Convert to domain types immediately after boundary call.
3. Document ownership/lifetime rules for pointers.

## 5) Pinned memory guidance

```haskell
-- GOOD
withBytesForC :: ByteString -> (Ptr Word8 -> CSize -> IO a) -> IO a
withBytesForC bs k =
  BS.useAsCStringLen bs $ \(ptr, len) ->
    k (castPtr ptr) (fromIntegral len)
```

```haskell
-- BAD
escapePtr :: ByteString -> IO (Ptr Word8)
escapePtr bs =
  BS.useAsCStringLen bs $ \(ptr, _) ->
    pure (castPtr ptr)
```

Why: pointers from callback-based helpers must not outlive the callback scope.

Use pinned memory only when the foreign API requires stable addresses across calls.

## 6) Profiling-first rollout

1. Baseline with non-unboxed clear implementation.
2. Profile to identify true hotspot.
3. Introduce one low-level change (layout/unboxed/FFI copy strategy).
4. Re-measure time + allocation + GC.
5. Keep low-level code behind small APIs and test equivalence.
