# Benchmarking and Profiling (GHC 9.14.1)

For pragma-tuning policy, also use `references/performance-pragmas.md`.
For space-leak triage playbook, also use `references/space-leak-debugging.md`.
For production telemetry patterns, also use `references/observability.md`.
For reproducibility discipline, also use `references/determinism-and-reproducibility.md`.

## Contents
- 1. Measurement workflow (best practice)
- 2. Build trustworthy benchmarks
- 3. Run criterion effectively
- 4. Compare sequential and parallel modes
- 5. Use RTS stats for allocation/GC budgets
- 6. Use eventlog and ThreadScope
- 7. CPU/time profiling with cost centres
- 8. Heap profiling for leaks and retention
- 9. Low-latency frame profiling method
- 10. Regression gates for CI

## 1) Measurement workflow (best practice)

Use this loop for every performance change:

1. Pick one target function/workload.
2. Record baseline metrics (time, allocs, GC).
3. Make one focused change.
4. Re-run with identical inputs and RTS flags.
5. Keep only changes that improve median and tail behavior.

```txt
GOOD: baseline -> one change -> remeasure -> decide
BAD: broad refactor + guessed perf claims
```

Why: single-variable experiments are reproducible and reversible.

## 2) Build trustworthy benchmarks

```haskell
-- GOOD
main :: IO ()
main = do
  fixtures <- loadFixtures "bench/fixtures"
  defaultMain
    [ env (pure fixtures.small) $ \input ->
        bench "decode/small" $ nf decodePayload input
    , env (pure fixtures.large) $ \input ->
        bench "decode/large" $ nf decodePayload input
    ]
```

```haskell
-- BAD
main :: IO ()
main =
  defaultMain
    [ bench "decode" $ nf decodePayload (randomFixtureFromDiskEveryRun)
    ]
```

Rules:
- Use fixed fixtures (checked into repo or artifact store).
- Separate setup from measured work (`env` in criterion).
- Name benchmarks by behavior and size, not by implementation.

## 3) Run criterion effectively

```bash
# GOOD: stable runtime and output artifact
cabal bench my-bench --benchmark-options="--time-limit 20 --csv bench.csv"
```

```bash
# BAD: one quick run used as final truth
cabal bench my-bench
```

Guidance:
- Prefer `nf` over `whnf` unless partial evaluation is intentional.
- Run multiple times and compare medians, not a single outlier.
- Keep machine conditions stable (power mode, background load).

## 4) Compare sequential and parallel modes

Always compare `-N1` vs `-N` for the same workload.

```bash
# GOOD
./dist-newstyle/build/.../my-bench/my-bench +RTS -N1 -s -T -RTS
./dist-newstyle/build/.../my-bench/my-bench +RTS -N  -s -T -RTS
```

```bash
# BAD
# only runs multicore and assumes scaling
./dist-newstyle/build/.../my-bench/my-bench +RTS -N -RTS
```

Interpretation:
- If `-N` improves wall time but doubles allocation/GC, reassess.
- Keep sequential implementation as fallback for correctness and stability.

## 5) Use RTS stats for allocation/GC budgets

`+RTS -s` gives allocation rate and GC time.
`+RTS -T` enables runtime counters for in-process sampling via `GHC.Stats`.

```bash
# GOOD
./my-bench +RTS -s -T -RTS
```

```bash
# BAD
# no GC/allocation visibility
./my-bench
```

Watch for:
- bytes allocated per run/frame;
- percent time in GC;
- max residency growth;
- productivity collapse after a refactor.

## 6) Use eventlog and ThreadScope

Build with eventlog support and inspect timeline-level behavior.

```bash
# Build benchmark executable with eventlog support
cabal build my-bench --ghc-options="-threaded -rtsopts -eventlog"
```

```bash
# Capture eventlog + summary
./my-bench +RTS -N2 -l-au -s -T -RTS
threadscope my-bench.eventlog
```

```haskell
-- GOOD: add markers to correlate stages
import Debug.Trace (traceEventIO)

markPhase :: String -> IO ()
markPhase label = traceEventIO ("phase:" ++ label)
```

```haskell
-- BAD: no markers, no stage boundaries in traces
```

Why: timeline traces reveal pauses, contention, and scheduler artifacts hidden by aggregate numbers.

## 7) CPU/time profiling with cost centres

Use profiling builds to identify call-site hotspots.

```bash
# GOOD
cabal build my-exe --enable-profiling --ghc-options="-fprof-auto -rtsopts"
./my-exe +RTS -p -RTS
# output: my-exe.prof
```

```bash
# BAD
# passing compile-time flags at runtime
./my-exe +RTS -p -fprof-auto -RTS
```

Notes:
- Use profiling builds for diagnosis, not final speed claims.
- Profiled binaries have different performance characteristics.

## 8) Heap profiling for leaks and retention

```bash
# GOOD: profile by closure type / description / retainer
./my-exe +RTS -hc -RTS
./my-exe +RTS -hd -RTS
./my-exe +RTS -hr -RTS
```

```bash
# Convert profile to graph
hp2ps -c my-exe.hp
```

```bash
# BAD
# no heap profile, guessing leak causes
./my-exe
```

Heuristic:
- growing residency with steady workload usually indicates retention;
- investigate large constructors/retainers first, then closure captures.

## 9) Low-latency frame profiling method

For games/realtime loops, optimize tail frame time and GC spikes.

```haskell
-- GOOD
frameLoop :: Int -> IO ()
frameLoop n =
  forM_ [1 .. n] $ \i -> do
    traceEventIO ("frame-start:" ++ show i)
    stepFrame
    traceEventIO ("frame-end:" ++ show i)
```

```haskell
-- BAD
-- random workload and no frame markers
frameLoop _ = stepFrame
```

```bash
# GOOD
./game --replay fixtures/scene-A +RTS -N2 -A32m -s -T -l-au -RTS
```

Why: deterministic replay plus frame markers enables p95/p99 comparison across commits.

## 10) Regression gates for CI

Use automated thresholds on stable inputs.

```bash
# GOOD
cabal bench my-bench --benchmark-options="--csv bench.csv --time-limit 20"
python3 <repo-local>/scripts/check_bench_regression.py \
  --baseline bench-baseline.csv \
  --current bench.csv \
  --max-mean-regress-pct 10 \
  --max-alloc-regress-pct 8
```

```bash
# BAD
# no thresholding, manual eyeballing only
cabal bench my-bench
```

Gate at least:
- mean/median runtime;
- allocation delta;
- p95/p99 for latency-sensitive paths.
