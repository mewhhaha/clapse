# Performance Patterns (GHC 9.14.1)

For frame-time-sensitive systems (games/realtime), also use `references/low-latency.md`.
For low-level layout and FFI leaf hotspots, also use `references/unboxed-and-ffi.md`.
For container selection by workload shape, also use `references/pure-data-structures.md`.
For pragma-level optimization policy, also use `references/performance-pragmas.md`.
For retention diagnosis workflow, also use `references/space-leak-debugging.md`.

## Contents
- 1. Use strict folds for accumulators
- 2. Make hot fields strict
- 3. Fuse traversals when possible
- 4. Pick containers by access pattern
- 5. Avoid repeated list append
- 6. Use Text/ByteString over String on hot paths
- 7. Choose strict map/state variants
- 8. Parse once, reuse structured values
- 9. Use builders for large text output
- 10. Keep pure loops pure
- 11. Measure before and after every optimization
- 12. Guard regressions with stable benchmarks
- 13. Use LinearTypes for performance-safe ownership

## 1) Use strict folds for accumulators

```haskell
-- GOOD
import Data.List (foldl')

sumSquares :: [Int] -> Int
sumSquares = foldl' (\acc x -> acc + x * x) 0
```

```haskell
-- BAD
sumSquares :: [Int] -> Int
sumSquares = foldl (\acc x -> acc + x * x) 0
```

Why: lazy left folds retain thunks and can cause space leaks.

## 2) Make hot fields strict

```haskell
-- GOOD
data Stats = Stats
  { count :: !Int
  , bytes :: !Int
  }
```

```haskell
-- BAD
data Stats = Stats
  { count :: Int
  , bytes :: Int
  }
```

Why: strict fields reduce heap pressure in frequently updated records.

## 3) Fuse traversals when possible

```haskell
-- GOOD
positiveEvenSum :: [Int] -> Int
positiveEvenSum =
  foldl' (\acc x -> if x > 0 && even x then acc + x else acc) 0
```

```haskell
-- BAD
positiveEvenSum :: [Int] -> Int
positiveEvenSum xs = sum (filter even (filter (> 0) xs))
```

Why: a single pass typically allocates less and is easier to profile.

## 4) Pick containers by access pattern

```haskell
-- GOOD
import Data.IntMap.Strict qualified as IntMap

type UserTable = IntMap.IntMap User

lookupUser :: Int -> UserTable -> Maybe User
lookupUser = IntMap.lookup
```

```haskell
-- BAD
type UserTable = [(Int, User)]

lookupUser :: Int -> UserTable -> Maybe User
lookupUser userId = lookup userId
```

Why: asymptotics matter; linear scans become bottlenecks quickly.

## 5) Avoid repeated list append

```haskell
-- GOOD
buildSquares :: [Int] -> [Int]
buildSquares xs = reverse (foldl' (\acc x -> (x * x) : acc) [] xs)
```

```haskell
-- BAD
buildSquares :: [Int] -> [Int]
buildSquares = foldl (\acc x -> acc ++ [x * x]) []
```

Why: repeated `++` in a fold is quadratic.

## 6) Use Text/ByteString over String on hot paths

```haskell
-- GOOD
import Data.Text (Text)
import Data.Text qualified as Text

normalizeName :: Text -> Text
normalizeName = Text.toCaseFold . Text.strip
```

```haskell
-- BAD
normalizeName :: String -> String
normalizeName = map toLower . dropWhile isSpace
```

Why: boxed linked lists of `Char` are expensive for real workloads.

## 7) Choose strict map/state variants

```haskell
-- GOOD
import Control.Monad.State.Strict (State, modify')
```

```haskell
-- BAD
import Control.Monad.State.Lazy (State, modify)
```

Why: strict state updates avoid long chains of deferred updates.

## 8) Parse once, reuse structured values

```haskell
-- GOOD
data Request = Request
  { customerId :: !CustomerId
  , amount     :: !Money
  }

handle :: ByteString -> Either RequestError Result
handle raw = do
  request <- decodeRequest raw
  validateRequest request
  pure (runBusinessLogic request)
```

```haskell
-- BAD
handle :: ByteString -> Either RequestError Result
handle raw = do
  cid <- parseCustomerId raw
  amt <- parseAmount raw
  -- parse same payload repeatedly in deeper calls
  pure (runBusinessLogicRaw raw cid amt)
```

Why: repeated parsing wastes cycles and often repeats allocations.

## 9) Use builders for large text output

```haskell
-- GOOD
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Builder.Int qualified as Builder

renderIds :: [Int] -> Text
renderIds ids =
  toStrict (Builder.toLazyText (foldMap (\i -> Builder.decimal i <> Builder.fromString "\n") ids))
```

```haskell
-- BAD
renderIds :: [Int] -> Text
renderIds ids = foldl' (\acc i -> acc <> Text.pack (show i) <> "\n") "" ids
```

Why: builders avoid repeated reallocation during concatenation-heavy rendering.

## 10) Keep pure loops pure

```haskell
-- GOOD
scoreBatch :: Vector Event -> Int
scoreBatch = Vector.foldl' scoreOne 0

runScoring :: Vector Event -> IO Int
runScoring events = do
  logInfo "scoring"
  pure (scoreBatch events)
```

```haskell
-- BAD
scoreBatch :: Vector Event -> IO Int
scoreBatch events = Vector.foldM' (\acc e -> logEvent e >> pure (scoreOne acc e)) 0 events
```

Why: effect-free inner loops are easier for GHC to optimize and benchmark.

## 11) Measure before and after every optimization

```haskell
-- GOOD
import Criterion.Main

main :: IO ()
main =
  defaultMain
    [ bench "encode/original" $ nf encodeOriginal fixture
    , bench "encode/new" $ nf encodeNew fixture
    ]
```

```haskell
-- BAD
-- "This should be faster" with no benchmark evidence.
```

Why: intuition is unreliable for performance.

## 12) Guard regressions with stable benchmarks

```haskell
-- GOOD
-- keep fixed fixtures and compare allocations/time across commits
```

```haskell
-- BAD
-- run benchmark once with random input shape and call it done
```

Why: consistency beats noisy one-off numbers when deciding merges.

## 13) Use LinearTypes for performance-safe ownership

Linear types (`%1 ->`) are useful when you want in-place style performance and aliasing safety.
They encode single-use ownership so APIs can prevent accidental duplication of mutable-like resources.

```haskell
-- GOOD
{-# LANGUAGE LinearTypes #-}

data Buffer = Buffer InternalRep

newBuffer :: Int -> Buffer
pushByte :: Buffer %1 -> Word8 -> Buffer
freezeBuffer :: Buffer %1 -> ByteString

fillBuffer :: Buffer %1 -> [Word8] -> Buffer
fillBuffer buf [] = buf
fillBuffer buf (x:xs) = fillBuffer (pushByte buf x) xs

encodePacket :: [Word8] -> ByteString
encodePacket bytes =
  freezeBuffer (fillBuffer (newBuffer 256) bytes)
```

```haskell
-- BAD
-- unrestricted API permits aliasing/duplicate writes accidentally
data Buffer = Buffer InternalRep

pushByte :: Buffer -> Word8 -> Buffer

badEncode :: [Word8] -> ByteString
badEncode bytes =
  let b0 = newBuffer 256
      b1 = foldl' pushByte b0 bytes
      b2 = pushByte b0 0xFF
  in freezeBuffer b2
```

Why: unrestricted ownership allows stale aliases and invalid mutation order.

```haskell
-- GOOD
{-# LANGUAGE LinearTypes #-}

data Token = Token Handle

readChunk :: Token %1 -> IO (Token, ByteString)
closeToken :: Token %1 -> IO ()
```

```haskell
-- BAD
data Token = Token Handle

readChunk :: Token -> IO ByteString
closeToken :: Token -> IO ()
```

Why: linear resource tokens make \"use-after-close\" and forgotten handoff paths harder to represent.

When to use LinearTypes:
1. Hot leaf modules where ownership discipline removes defensive copying.
2. Resource APIs where linear handoff makes lifecycle explicit.
3. Internal boundaries first; keep top-level application APIs mostly non-linear unless necessary.
