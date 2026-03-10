# Determinism and Reproducibility (GHC 9.14.1)

For benchmark rigor, also use `references/benchmarking-and-profiling.md`.

## Contents
- 1. Define deterministic input spaces
- 2. Fix random seeds in tests and benches
- 3. Pin fixture versions and formats
- 4. Enforce stable ordering
- 5. Remove locale/time dependency
- 6. Control parallelism for reproducible checks
- 7. Keep builds and runs reproducible
- 8. CI reproducibility gate

## 1) Define deterministic input spaces

```haskell
-- GOOD
testPipeline :: FilePath -> IO Result
testPipeline fp = do
  input <- loadFixture fp
  pure (runPipeline (normalizeInput input))
```

```haskell
-- BAD
testPipeline :: IO Result
testPipeline = do
  input <- randomInput
  pure (runPipeline input)
```

Why: deterministic inputs make failures replayable.

## 2) Fix random seeds in tests and benches

```haskell
-- GOOD
let seed = mkStdGen 42
quickCheckWith stdArgs { replay = Just (seed, 0) } prop_roundTrip
```

```haskell
-- BAD
seed <- newStdGen
quickCheck prop_roundTrip
```

Why: fixed seeds keep CI results stable and debugging fast.

## 3) Pin fixture versions and formats

```haskell
-- GOOD
fixturePath :: FilePath
fixturePath = "fixtures/v3/request-small.json"
```

```haskell
-- BAD
fixturePath :: IO FilePath
fixturePath = getLine
```

Why: versioned fixtures prevent accidental data drift.

## 4) Enforce stable ordering

```haskell
-- GOOD
orderedPairs :: Map Text Value -> [(Text, Value)]
orderedPairs = Map.toAscList
```

```haskell
-- BAD
orderedPairs :: HashMap Text Value -> [(Text, Value)]
orderedPairs = HashMap.toList
```

Why: unstable ordering is a common source of flaky golden tests.

## 5) Remove locale/time dependency

```haskell
-- GOOD
formatTimestamp :: UTCTime -> Text
formatTimestamp = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
```

```haskell
-- BAD
formatTimestamp :: IO String
formatTimestamp = formatTime defaultTimeLocale "%c" <$> getZonedTime
```

Why: locale/time-zone dependent formats create environment-specific behavior.

## 6) Control parallelism for reproducible checks

```bash
# GOOD
cabal test --test-show-details=direct --jobs=1
```

```bash
# BAD
cabal test --jobs=16
```

Why: serial test runs are often preferable for deterministic failure triage.

## 7) Keep builds and runs reproducible

```bash
# GOOD
cabal build --offline
cabal test --test-option=--seed=42
```

```bash
# BAD
cabal update
cabal build
```

Why: uncontrolled dependency refreshes can invalidate comparisons.

## 8) CI reproducibility gate

```txt
GOOD:
1. Re-run critical suites twice in fresh processes.
2. Persist failing seed and runtime flags as CI artifacts.
3. Check deterministic fixture outputs (goldens or hashes).
4. Fail CI when reproducibility checks diverge.
```

```txt
BAD:
1. Accepting one green run without rerun.
2. Dropping seeds and runtime flags from logs.
```

Why: reproducibility is only real when it survives rerun and environment variation.
