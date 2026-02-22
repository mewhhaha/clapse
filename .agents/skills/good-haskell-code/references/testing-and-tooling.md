# Testing and Tooling Patterns (GHC 9.14.1)

For deeper performance measurement workflows, use `references/benchmarking-and-profiling.md`.
For persistence compatibility checks, use `references/parsing-and-serialization.md`.
For dependency/build setup, use `references/tooling-and-deps.md`.
For reproducibility guardrails, use `references/determinism-and-reproducibility.md`.
For reviewer rubric and severity ordering, use `references/code-review-checklist.md`.

## Contents
- 1. Encode laws as properties
- 2. Add edge-case generators
- 3. Keep regression tests for fixed bugs
- 4. Use golden tests for render/format output
- 5. Benchmark hot paths with criterion
- 6. Fail builds on warning drift
- 7. Keep partial functions behind explicit wrappers
- 8. Prefer deterministic fixtures in perf tests
- 9. Validate error paths explicitly
- 10. Keep tests close to domain invariants

## 1) Encode laws as properties

```haskell
-- GOOD
prop_reverseInvolutive :: [Int] -> Bool
prop_reverseInvolutive xs = reverse (reverse xs) == xs
```

```haskell
-- BAD
test_reverseExample :: Bool
test_reverseExample = reverse [1,2,3] == [3,2,1]
```

Why: one example is not a behavioral contract.

## 2) Add edge-case generators

```haskell
-- GOOD
newtype SmallPositive = SmallPositive Int

instance Arbitrary SmallPositive where
  arbitrary = SmallPositive <$> chooseInt (1, 10_000)
```

```haskell
-- BAD
-- only default Arbitrary Int, no control over pathological input
```

Why: targeted generators expose boundary failures faster.

## 3) Keep regression tests for fixed bugs

```haskell
-- GOOD
test_issue_482_empty_discount_code :: Assertion
test_issue_482_empty_discount_code =
  applyDiscountCode "" @?= Left EmptyDiscountCode
```

```haskell
-- BAD
-- bug fixed in code, no locked regression test
```

Why: fixed bugs without tests eventually reappear.

## 4) Use golden tests for render/format output

```haskell
-- GOOD
test_invoiceRender_golden :: Assertion
test_invoiceRender_golden =
  renderInvoice fixture @?= expectedGolden
```

```haskell
-- BAD
test_invoiceRender_smoke :: Assertion
test_invoiceRender_smoke =
  assertBool "non-empty output" (not (Text.null (renderInvoice fixture)))
```

Why: smoke tests miss subtle format regressions.

## 5) Benchmark hot paths with criterion

```haskell
-- GOOD
main :: IO ()
main = defaultMain
  [ bench "decode/old" $ nf decodeOld fixture
  , bench "decode/new" $ nf decodeNew fixture
  ]
```

```haskell
-- BAD
-- merge performance-sensitive refactor with no benchmark
```

Why: performance work needs quantitative verification.

## 6) Fail builds on warning drift

```txt
-- GOOD
ghc-options: -Wall -Wcompat -Werror -Wincomplete-uni-patterns -Wincomplete-record-updates
```

```txt
-- BAD
ghc-options: -Wall
```

Why: permissive warning policy lets correctness debt accumulate silently.

## 7) Keep partial functions behind explicit wrappers

```haskell
-- GOOD
headOr :: a -> [a] -> a
headOr fallback = \case
  []    -> fallback
  x : _ -> x
```

```haskell
-- BAD
headOr :: a -> [a] -> a
headOr _ = head
```

Why: explicit handling documents behavior and avoids runtime exceptions.

## 8) Prefer deterministic fixtures in perf tests

```haskell
-- GOOD
fixture :: ByteString
fixture = loadFixture "fixtures/invoice-large.json"
```

```haskell
-- BAD
fixture :: IO ByteString
fixture = randomPayload
```

Why: random inputs make benchmark comparisons noisy and hard to trust.

## 9) Validate error paths explicitly

```haskell
-- GOOD
test_parseAmount_invalid :: Assertion
test_parseAmount_invalid =
  parseAmount "abc" @?= Left (InvalidAmount "abc")
```

```haskell
-- BAD
test_parseAmount_happyPath :: Assertion
test_parseAmount_happyPath =
  parseAmount "42" @?= Right (Money 42)
```

Why: error branches are often where production failures happen.

## 10) Keep tests close to domain invariants

```haskell
-- GOOD
prop_mkEmail_roundtrip :: Text -> Property
prop_mkEmail_roundtrip raw =
  isValidEmailText raw ==>
    case mkEmail raw of
      Left _ -> counterexample "expected valid email" False
      Right email -> unEmail email === raw
```

```haskell
-- BAD
prop_mkEmail_smoke :: Text -> Bool
prop_mkEmail_smoke _ = True
```

Why: tests should enforce invariants, not just execute code paths.
