# Code Review Checklist (GHC 9.14.1)

Use this as a severity-ordered rubric for reviews.
For testing and benchmark evidence, also use `references/testing-and-tooling.md` and `references/benchmarking-and-profiling.md`.

## Contents
- 1. Correctness and totality
- 2. Invariants and type contracts
- 3. Error handling and boundary behavior
- 4. Performance and allocation risks
- 5. API shape and compatibility
- 6. Tests and validation evidence
- 7. Docs, migration, and deprecation
- 8. Review output format

## 1) Correctness and totality

```haskell
-- GOOD
firstItem :: [a] -> Maybe a
firstItem [] = Nothing
firstItem (x:_) = Just x
```

```haskell
-- BAD
firstItem :: [a] -> a
firstItem = head
```

Why: correctness regressions and partial functions are highest-severity review findings.

## 2) Invariants and type contracts

```haskell
-- GOOD
newtype NonEmptyText = NonEmptyText Text

mkNonEmptyText :: Text -> Either Text NonEmptyText
mkNonEmptyText t
  | Text.null t = Left "must be non-empty"
  | otherwise = Right (NonEmptyText t)
```

```haskell
-- BAD
type UserName = Text
```

Why: invariants should be encoded at boundaries, not remembered implicitly.

## 3) Error handling and boundary behavior

```haskell
-- GOOD
decodeConfig :: ByteString -> Either ConfigErr Config
decodeConfig = ...
```

```haskell
-- BAD
decodeConfig :: ByteString -> Config
decodeConfig = fromJust . decode
```

Why: reviewers should require explicit failure paths and boundary safety.

## 4) Performance and allocation risks

```haskell
-- GOOD
sumIds :: [Int] -> Int
sumIds = foldl' (+) 0
```

```haskell
-- BAD
sumIds :: [Int] -> Int
sumIds = foldl (+) 0
```

Why: obvious space/time risks should be called out before merge.

## 5) API shape and compatibility

```haskell
-- GOOD
newFn :: Config -> Input -> Result
newFn cfg input = ...
```

```haskell
-- BAD
newFn :: (Int, Int, Bool, String) -> Result
newFn = ...
```

Why: API shape determines long-term maintainability and migration cost.

## 6) Tests and validation evidence

```haskell
-- GOOD
prop_roundTrip :: Payload -> Property
prop_roundTrip payload =
  decodePayload (encodePayload payload) === Right payload
```

```haskell
-- BAD
-- behavior changed with no new tests and no benchmark evidence
```

Why: claims need deterministic proof via tests and (for perf work) measured benchmarks.

## 7) Docs, migration, and deprecation

```haskell
-- GOOD
{-# DEPRECATED oldFn "Use newFn; oldFn will be removed in v1.2" #-}
oldFn :: Input -> Result
oldFn = ...
```

```haskell
-- BAD
-- silent semantic change with stale docs
```

Why: consumers need migration guidance to adopt changes safely.

## 8) Review output format

```txt
GOOD:
1. Findings first, ordered by severity, each with file path reference.
2. Open questions/assumptions second.
3. Summary last (or explicit \"no findings\").
```

```txt
BAD:
1. High-level summary only.
2. No concrete file-level evidence.
```

Why: consistent review format keeps risk communication actionable.
