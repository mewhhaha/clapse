# Pure Data Structures: What To Use When (GHC 9.14.1)

For strictness and allocation tactics, also use `references/performance.md`.
For low-latency frame-budget constraints, also use `references/low-latency.md`.

## Contents
- 1. Choose by operation profile first
- 2. List and NonEmpty
- 3. Seq for FIFO/deque workloads
- 4. Vector family for dense indexed data
- 5. Map, IntMap, and HashMap
- 6. Set, IntSet, and HashSet
- 7. Text, ByteString, and String
- 8. Array and PrimArray in pure code
- 9. Builder and DList for append-heavy output
- 10. Strict vs lazy container modules
- 11. Complexity cheat sheet
- 12. Decision checklist

## 1) Choose by operation profile first

Pick the container from dominant operations, not habit.

```haskell
-- GOOD
-- Int key lookups + inserts in large tables.
import Data.IntMap.Strict qualified as IntMap

type UserTable = IntMap.IntMap User

findUsers :: UserTable -> [Int] -> [Maybe User]
findUsers table ids = fmap (`IntMap.lookup` table) ids
```

```haskell
-- BAD
type UserTable = [(Int, User)]

findUsers :: UserTable -> [Int] -> [Maybe User]
findUsers table ids = fmap (`lookup` table) ids
```

Why: data structure choice should follow access pattern and complexity budget.

## 2) List and NonEmpty

Use list for cheap cons + sequential traversal.
Use `NonEmpty` when empty is illegal.

```haskell
-- GOOD
import Data.List.NonEmpty (NonEmpty(..))

firstItem :: NonEmpty a -> a
firstItem (x :| _) = x

mapEvents :: (Event -> Out) -> [Event] -> [Out]
mapEvents f = map f
```

```haskell
-- BAD
firstItem :: [a] -> a
firstItem = head
```

Why: list is excellent for stream-like traversal, but `head`/indexing on plain list is unsafe and slow.

## 3) Seq for FIFO/deque workloads

Use `Seq` when you need both-end operations or frequent front pops.

```haskell
-- GOOD
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

enqueue :: a -> Seq a -> Seq a
enqueue x q = q Seq.|> x

dequeue :: Seq a -> Maybe (a, Seq a)
dequeue q = case Seq.viewl q of
  Seq.EmptyL -> Nothing
  x Seq.:< q' -> Just (x, q')
```

```haskell
-- BAD
enqueue :: a -> [a] -> [a]
enqueue x q = q ++ [x]
```

Why: list append in queues is O(n), while `Seq` gives near O(1) deque-style operations.

## 4) Vector family for dense indexed data

Use `Vector` for dense immutable indexed data and tight loops.

```haskell
-- GOOD
import Data.Vector.Unboxed qualified as U

dot :: U.Vector Double -> U.Vector Double -> Double
dot xs ys = U.sum (U.zipWith (*) xs ys)

sumVec :: U.Vector Int -> Int
sumVec = U.foldl' (+) 0
```

```haskell
-- BAD
sumVec :: [Int] -> Int
sumVec xs = sum [xs !! i | i <- [0 .. length xs - 1]]
```

Why: vectors give O(1) indexing and better locality for numeric/index-heavy workloads.

Variant rule:
1. `Data.Vector` for boxed values.
2. `Data.Vector.Unboxed` for primitive element types.
3. `Data.Vector.Storable` for FFI-friendly layouts.
4. Mutable vectors in `ST`/`IO` for batch construction, then freeze.

## 5) Map, IntMap, and HashMap

Choose by key type and ordering needs.

```haskell
-- GOOD
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.HashMap.Strict qualified as HashMap

type ById = IntMap.IntMap User
type ByName = Map.Map Text User
type ByToken = HashMap.HashMap Token Session
```

```haskell
-- BAD
type ById = [(Int, User)]
type ByName = [(Text, User)]
```

Why: maps avoid repeated linear scans and make lookup/update costs predictable.

Heuristic:
1. `IntMap` for `Int` keys.
2. `Map` when ordered traversal/range ops matter.
3. `HashMap` for large hash/equality workloads with no ordering requirement.

## 6) Set, IntSet, and HashSet

Use set types for membership and deduplication, not lists.

```haskell
-- GOOD
import Data.IntSet qualified as IntSet
import Data.HashSet qualified as HashSet
import Data.Set qualified as Set

isSeen :: Int -> IntSet.IntSet -> Bool
isSeen key seen = IntSet.member key seen

sortedTags :: Set.Set Text
sortedTags = Set.fromList ["perf", "render", "tooling"]

uniqueTokens :: HashSet.HashSet Text
uniqueTokens = HashSet.fromList ["a", "b", "a"]
```

```haskell
-- BAD
isSeen :: Int -> [Int] -> Bool
isSeen key seen = key `elem` seen
```

Why: set membership is typically much cheaper and clearer than repeated list scans.

## 7) Text, ByteString, and String

Use `Text` for Unicode text and `ByteString` for raw bytes/wire formats.
Reserve `String` for small boundary glue.

```haskell
-- GOOD
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString (ByteString)

normalizeName :: Text -> Text
normalizeName = Text.toCaseFold . Text.strip

decodePacket :: ByteString -> Either DecodeError Packet
decodePacket = ...
```

```haskell
-- BAD
import Data.Char (toLower, isSpace)

normalizeName :: String -> String
normalizeName = map toLower . dropWhile isSpace
```

Why: `String` is a linked list of `Char` and is costly for hot data paths.

## 8) Array and PrimArray in pure code

For fixed-size indexed tables and numeric data, arrays avoid linked-list overhead.

```haskell
-- GOOD
import Data.Array.Unboxed (UArray, listArray, (!))
import Control.Monad.ST (runST)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as Prim

lut :: UArray Int Int
lut = listArray (0, 3) [10, 20, 30, 40]

lookupLut :: Int -> Int
lookupLut i = lut ! i

weights :: PrimArray Int
weights = runST $ do
  m <- Prim.newPrimArray 5
  Prim.writePrimArray m 0 1
  Prim.writePrimArray m 1 2
  Prim.writePrimArray m 2 3
  Prim.writePrimArray m 3 5
  Prim.writePrimArray m 4 8
  Prim.unsafeFreezePrimArray m

weightAt :: Int -> Int
weightAt i = Prim.indexPrimArray weights i
```

```haskell
-- BAD
lutList :: [Int]
lutList = [10, 20, 30, 40]

lookupLut :: Int -> Int
lookupLut i = lutList !! i
```

Why: array-based structures provide explicit bounds/indexing and better locality than lists.

## 9) Builder and DList for append-heavy output

Avoid repeated left-associated append when constructing large outputs.

```haskell
-- GOOD
import Data.Text (Text)
import Data.List (foldl')
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.DList qualified as DL

renderIds :: [Int] -> Text
renderIds ids =
  TL.toStrict $
    TB.toLazyText $
      foldMap (\i -> TBI.decimal i <> TB.singleton '\n') ids

accumulateLines :: [Text] -> [Text]
accumulateLines xs =
  DL.toList (foldl' (flip DL.snoc) DL.empty xs)
```

```haskell
-- BAD
import Data.Text qualified as Text
import Data.List (foldl')

renderIds :: [Int] -> Text
renderIds = foldl' (\acc i -> acc <> Text.pack (show i) <> "\n") ""
```

Why: builders and difference lists avoid quadratic reallocation in append-heavy code.

## 10) Strict vs lazy container modules

Prefer strict variants in hot update paths.

```haskell
-- GOOD
import Data.Map.Strict qualified as Map
import Data.List (foldl')
import Data.Text (Text)

countWords :: [Text] -> Map.Map Text Int
countWords = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty
```

```haskell
-- BAD
import Data.Map.Lazy qualified as Map

countWords :: [Text] -> Map.Map Text Int
countWords = foldl (\m k -> Map.insertWith (+) k 1 m) Map.empty
```

Why: strict containers plus strict folds prevent thunk buildup in long-running updates.

## 11) Complexity cheat sheet

```txt
List: cons/head O(1), append/index O(n)
NonEmpty: list costs + non-empty invariant
Seq: push/pop both ends amortized O(1), index/split O(log n)
Vector: index O(1), map/fold O(n), structural append usually O(n)
Map/Set: O(log n)
IntMap/IntSet: O(log n) with low constants for Int keys
HashMap/HashSet: average O(1), worst-case depends on hashing/collisions
Text/ByteString append: linear in bytes/chunks, prefer Builder for many appends
```

```txt
BAD:
Treat complexity tables as absolute truth without profiling your real workload.
```

Why: asymptotics guide the first choice, then benchmarks confirm constant-factor reality.

## 12) Decision checklist

```txt
GOOD:
1. List dominant operations (lookup, insert, append, index, split, merge).
2. Pick the container matching those operations.
3. Pick strict module variants for hot mutable-like updates.
4. Encode invariants with structure (`NonEmpty`, set/map key type, fixed-size arrays).
5. Benchmark with fixed fixtures before and after any container swap.
```

```txt
BAD:
Switch containers for style reasons without measuring.
```

Why: matching workload shape and validating with measurements gives reliable wins.
