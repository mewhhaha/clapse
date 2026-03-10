# Laziness, Time Travel, Dynamic Programming, and Concurrency (GHC 9.14.1)

For render/logic coordination, STM queues, and process-boundary sharing, use `references/stm-and-parallelism.md`.
For Arrow-style signal composition and FRP migration patterns, use `references/arrows-and-frp.md`.

## Contents
- 1. Decide between strictness and laziness
- 2. Good laziness examples
- 3. Time-travel patterns (tying the knot)
- 4. Lazy dynamic programming
- 5. Parallel pure work correctly
- 6. Async IO correctly
- 7. Practical rollout checklist

## 1) Decide between strictness and laziness

Use strictness for hot accumulators and long-running loops.
Use laziness for demand-driven pipelines and recursive definitions where only part of the result is needed.

```haskell
-- GOOD
import Data.List (foldl')

sumEdges :: [Double] -> Double
sumEdges = foldl' (+) 0
```

```haskell
-- BAD
sumEdges :: [Double] -> Double
sumEdges = foldl (+) 0
```

Why: strict folds avoid thunk chains and reduce heap growth.

```haskell
-- GOOD
data Stats = Stats
  { glyphs   :: !Int
  , segments :: !Int
  }
```

```haskell
-- BAD
data Stats = Stats
  { glyphs   :: Int
  , segments :: Int
  }
```

Why: strict fields are a safe default in hot data structures.

```haskell
-- GOOD
firstTenMatches :: [Int] -> [Int]
firstTenMatches = take 10 . filter isInteresting
```

```haskell
-- BAD
firstTenMatches :: [Int] -> [Int]
firstTenMatches xs =
  let ys = filter isInteresting xs
  in length ys `seq` take 10 ys
```

Why: laziness avoids needless work when the consumer only needs a prefix.

## 2) Good laziness examples

```haskell
-- GOOD
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib100 :: Integer
fib100 = fibs !! 100
```

```haskell
-- BAD
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

Why: memoized lazy streams avoid exponential recomputation.

```haskell
-- GOOD
anyLarge :: [Int] -> Bool
anyLarge = foldr (\x rest -> x > 10_000 || rest) False
```

```haskell
-- BAD
anyLarge :: [Int] -> Bool
anyLarge = foldl' (\acc x -> acc || x > 10_000) False
```

Why: `foldr` with `||` can short-circuit on infinite or long inputs.

```haskell
-- GOOD
firstActiveUser :: [User] -> Maybe User
firstActiveUser = find (\u -> u.active)
```

```haskell
-- BAD
firstActiveUser :: [User] -> User
firstActiveUser users = head (filter (\u -> u.active) users)
```

Why: avoid partial functions while preserving lazy search behavior.

## 3) Time-travel patterns (tying the knot)

In Haskell, "time travel" usually means defining values in terms of earlier results from the same output structure.

```haskell
-- GOOD
runningMin :: [Int] -> [Int]
runningMin [] = []
runningMin (x : xs) = mins
  where
    mins = x : zipWith min xs mins
```

```haskell
-- BAD
runningMin :: [Int] -> [Int]
runningMin xs = [minimum (take (i + 1) xs) | i <- [0 .. length xs - 1]]
```

Why: knot-tying keeps the recurrence linear and avoids repeated prefix scans.

```haskell
-- GOOD
smoothed :: Double -> [Double] -> [Double]
smoothed _ [] = []
smoothed alpha (x0 : xs) = ys
  where
    ys = x0 : zipWith step xs ys
    step x prev = alpha * x + (1 - alpha) * prev
```

```haskell
-- BAD
smoothed :: Double -> [Double] -> [Double]
smoothed alpha xs = map (\i -> naiveSmoothing alpha (take i xs)) [1 .. length xs]
```

Why: use feedback recurrence directly, not repeated recomputation from prefixes.

## 4) Lazy dynamic programming

Lazy arrays and lists let you define DP tables in terms of themselves.

```haskell
-- GOOD
import Data.Array (Array, (!), array, listArray)

lcsLen :: String -> String -> Int
lcsLen as bs = table ! (n, m)
  where
    n = length as
    m = length bs
    a = listArray (1, n) as
    b = listArray (1, m) bs
    table :: Array (Int, Int) Int
    table =
      array ((0, 0), (n, m))
        [ ((i, j), cell i j) | i <- [0 .. n], j <- [0 .. m] ]
    cell 0 _ = 0
    cell _ 0 = 0
    cell i j
      | a ! i == b ! j = 1 + table ! (i - 1, j - 1)
      | otherwise = max (table ! (i - 1, j)) (table ! (i, j - 1))
```

```haskell
-- BAD
lcsLen :: String -> String -> Int
lcsLen [] _ = 0
lcsLen _ [] = 0
lcsLen (x : xs) (y : ys)
  | x == y = 1 + lcsLen xs ys
  | otherwise = max (lcsLen (x : xs) ys) (lcsLen xs (y : ys))
```

Why: naive recursion is exponential; lazy table definitions are explicit and reusable.

```haskell
-- GOOD
coinWays :: [Int] -> Int -> Integer
coinWays coins target = table !! target
  where
    table = [ways n | n <- [0 .. target]]
    ways 0 = 1
    ways n = sum [table !! (n - c) | c <- coins, c <= n]
```

```haskell
-- BAD
coinWays :: [Int] -> Int -> Integer
coinWays _ 0 = 1
coinWays coins n
  | n < 0 = 0
  | otherwise = sum [coinWays coins (n - c) | c <- coins]
```

Why: reuse subproblem results explicitly instead of recomputing.

## 5) Parallel pure work correctly

Keep sequential logic as source of truth; parallelize with a thin wrapper.

```haskell
-- GOOD
import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)

buildAllSeq :: [GlyphReq] -> [GlyphOut]
buildAllSeq = map buildOne

buildAllPar :: NFData GlyphOut => Int -> [GlyphReq] -> [GlyphOut]
buildAllPar chunkSize requests =
  withStrategy (parListChunk chunkSize rdeepseq) (map buildOne requests)
```

```haskell
-- BAD
buildAll :: [GlyphReq] -> IO [GlyphOut]
buildAll reqs = do
  -- algorithm rewrite + mutable state + parallelism in one change
  ...
```

Why: keep the diff non-invasive and preserve an easy sequential fallback.

```haskell
-- GOOD
-- Chunking keeps task size above scheduling overhead.
```

```haskell
-- BAD
-- One tiny spark per element for very small tasks.
```

Why: tiny parallel work units often lose to scheduling and GC overhead.

## 6) Async IO correctly

Prefer structured concurrency with `withAsync`, `race`, and explicit cancellation behavior.

```haskell
-- GOOD
import Control.Concurrent.Async (waitBoth, withAsync)

fetchPair :: IO a -> IO b -> IO (a, b)
fetchPair left right =
  withAsync left $ \a ->
    withAsync right $ \b ->
      waitBoth a b
```

```haskell
-- BAD
import Control.Concurrent.Async (async, wait)

fetchPair :: IO a -> IO b -> IO (a, b)
fetchPair left right = do
  a <- async left
  b <- async right
  x <- wait a
  y <- wait b
  pure (x, y)
```

Why: if `wait a` throws, `b` may continue running unmanaged; `withAsync` scopes cancellation.

```haskell
-- GOOD
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)

withTimeoutMicros :: Int -> IO a -> IO (Either () a)
withTimeoutMicros micros action = do
  result <- race (threadDelay micros) action
  pure (either (const (Left ())) Right result)
```

```haskell
-- BAD
withTimeoutMicros :: Int -> IO a -> IO a
withTimeoutMicros _ action = action
```

Why: use `race` for explicit, composable timeout semantics.

```haskell
-- GOOD
import Control.Concurrent.Async (mapConcurrently)

mapConcurrentlyChunked :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyChunked chunkSize f xs =
  fmap concat (traverse (mapConcurrently f) (chunked chunkSize xs))

chunked :: Int -> [a] -> [[a]]
chunked n _
  | n <= 0 = []
chunked _ [] = []
chunked n ys =
  let (prefix, suffix) = splitAt n ys
  in prefix : chunked n suffix
```

```haskell
-- BAD
mapAll :: (a -> IO b) -> [a] -> IO [b]
mapAll = mapConcurrently
```

Why: unbounded concurrency over huge inputs can overload resources.

## 7) Practical rollout checklist

1. Start with a simple sequential implementation.
2. Add type-level invariants before micro-optimizing.
3. Choose strictness in hot loops and lazy composition where demand-driven behavior matters.
4. Add parallel wrappers without rewriting domain logic.
5. Benchmark `+RTS -N1` vs `+RTS -N` repeatedly and compare medians.
6. Keep the sequential path available as fallback.
