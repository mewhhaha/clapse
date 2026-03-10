# Space Leak Debugging (GHC 9.14.1)

For profiling workflows, also use `references/benchmarking-and-profiling.md`.
For strictness fixes, also use `references/performance.md`.

## Contents
- 1. Reproduce the issue deterministically
- 2. Confirm residency growth
- 3. Use heap profiles to find retainers
- 4. Use eventlog to align leaks with phases
- 5. Add phase markers
- 6. Remove thunk accumulation
- 7. Reduce closure retention in async workers
- 8. Re-validate with repeated fixed runs

## 1) Reproduce the issue deterministically

```haskell
-- GOOD
main :: IO ()
main = runScenario fixedFixture 10_000
```

```haskell
-- BAD
main :: IO ()
main = runWithRandomInputs
```

Why: deterministic workloads make leak diagnosis repeatable.

## 2) Confirm residency growth

```bash
# GOOD
./service +RTS -s -RTS
```

```bash
# BAD
./service
```

Why: `+RTS -s` shows whether maximum residency rises abnormally over the same workload.

## 3) Use heap profiles to find retainers

```bash
# GOOD
./service +RTS -hc -hd -hr -RTS
```

```bash
# BAD
./service +RTS -p -RTS
```

Why: heap profiles identify what is retained, not just where CPU time is spent.

## 4) Use eventlog to align leaks with phases

```bash
# GOOD
./service +RTS -N2 -l-au -T -RTS
```

```bash
# BAD
./service +RTS -RTS
```

Why: timeline data helps connect memory growth to specific pipeline stages.

## 5) Add phase markers

```haskell
-- GOOD
markPhase :: String -> IO ()
markPhase label = traceEventIO ("phase:" ++ label)
```

```haskell
-- BAD
-- no trace markers around major stages
```

Why: markers make heap/eventlog traces actionable during diagnosis.

## 6) Remove thunk accumulation

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

Why: lazy left folds are a common source of avoidable retention.

## 7) Reduce closure retention in async workers

```haskell
-- GOOD
runStep :: Text -> Item -> IO ()
runStep idPrefix item = worker idPrefix item
```

```haskell
-- BAD
runStep :: Config -> Item -> IO ()
runStep cfg item = worker cfg item
```

Why: capturing large configs in many closures can pin memory unexpectedly.

## 8) Re-validate with repeated fixed runs

```bash
# GOOD
for i in 1 2 3; do ./service +RTS -s -RTS; done
```

```bash
# BAD
# one run, then immediate conclusion
./service +RTS -s -RTS
```

Why: repeated fixed runs separate true improvements from scheduler noise.
