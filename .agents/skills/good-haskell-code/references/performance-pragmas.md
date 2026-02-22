# Performance Pragmas (GHC 9.14.1)

For validation, also use `references/benchmarking-and-profiling.md`.
For realtime loops, also use `references/low-latency.md`.

## Contents
- 1. Measure before pragma changes
- 2. INLINE: tiny hot functions only
- 3. INLINABLE for generic hot helpers
- 4. NOINLINE for intended boundaries
- 5. SPECIALIZE polymorphic bottlenecks
- 6. RULES for local, safe rewrites
- 7. Control RULES phase
- 8. Remove dead pragmas fast

## 1) Measure before pragma changes

```haskell
-- GOOD
main = defaultMain [bench "decode" $ nfIO (decodePayload fixture)]
```

```haskell
-- BAD
{-# INLINE decodePayload #-}
-- benchmark not rerun
```

Why: every pragma should be paired with before/after numbers, not folklore.

## 2) INLINE: tiny hot functions only

```haskell
-- GOOD
{-# INLINE isTransient #-}
isTransient :: Status -> Bool
isTransient s = s `elem` [Busy, Waiting]
```

```haskell
-- BAD
{-# INLINE parsePayload #-}
parsePayload :: ByteString -> Value
parsePayload = parseWithSchema fullSchema
```

Why: forcing inlining on large bodies increases code size and can hurt cache behavior.

## 3) INLINABLE for generic hot helpers

```haskell
-- GOOD
{-# INLINABLE decodeJSON #-}
decodeJSON :: FromJSON a => ByteString -> Either String a
```

```haskell
-- BAD
decodeJSON :: FromJSON a => ByteString -> Either String a
```

Why: `INLINABLE` enables specialisation opportunities where polymorphism is actually hot.

## 4) NOINLINE for intended boundaries

```haskell
-- GOOD
{-# NOINLINE globalConfig #-}
globalConfig :: Config
globalConfig = unsafePerformIO loadConfig
```

```haskell
-- BAD
{-# INLINE globalConfig #-}
globalConfig :: Config
```

Why: singleton-like values should not be duplicated or re-created by callers.

## 5) SPECIALIZE polymorphic bottlenecks

```haskell
-- GOOD
{-# SPECIALIZE parseItems :: ByteString -> [Order] -> Either Text [Order] #-}
parseItems :: FromJSON a => ByteString -> [a] -> Either Text [a]
```

```haskell
-- BAD
parseItems :: FromJSON a => ByteString -> [a] -> Either Text [a]
```

Why: explicit specialisation removes dictionary work on known hot types.

## 6) RULES for local, safe rewrites

```haskell
{-# RULES "filter/map" forall p f xs. filter p (map f xs) = map f (filter (p . f) xs) #-}
```

```haskell
{-# RULES "magic" forall x. dangerous x = dangerous x #-}
```

Why: only semantics-preserving rewrites with obvious intent should be added.

## 7) Control RULES phase

```haskell
{-# RULES "map/comp [1]" [~1] forall f g xs. map f (map g xs) = map (f . g) xs #-}
```

```haskell
{-# RULES "map/comp" forall f g xs. map f (map g xs) = map (f . g) xs #-}
```

Why: phase control avoids rewrite feedback loops and unwanted late-stage effects.

## 8) Remove dead pragmas fast

```haskell
-- GOOD
-- keep INLINE only where benchmark flags show a win
```

```haskell
{-# INLINE oldNormalizer #-}
-- kept only for API stability, never used in hot path
```

Why: stale pragmas erode trust in future tuning decisions.
