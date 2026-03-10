# Low-Latency Haskell (Games and Realtime) (GHC 9.14.1)

For full benchmark/profile workflows, use `references/benchmarking-and-profiling.md`.
For STM, foreign-store, and render/logic data-sharing patterns, use `references/stm-and-parallelism.md`.
For cancellation/cleanup discipline, use `references/resource-safety-and-exceptions.md`.
For bounded streaming and ingress backpressure, use `references/streaming-and-backpressure.md`.
For end-to-end loop scaffolding, use `references/render-logic-template.md`.

## Contents
- 1. Latency mindset: p99 over average
- 2. Allocation budget per frame
- 3. Keep hot loops allocation-light
- 4. Keep data layout simple and strict
- 5. Design for GC-friendly lifetimes
- 6. Parallel/async without latency explosions
- 7. Measure and tune RTS flags
- 8. Pre-ship checklist

## 1) Latency mindset: p99 over average

For games/realtime apps, tail latency matters more than throughput.

- 60 FPS budget: `16.67ms`
- 120 FPS budget: `8.33ms`
- 240 FPS budget: `4.17ms`

Track `p50`, `p95`, and `p99` frame times.
A fast average with periodic 40ms spikes still feels bad.

```haskell
-- GOOD
-- Keep explicit frame budget and fixed simulation step.
fixedStep :: Double
fixedStep = 1 / 120
```

```haskell
-- BAD
-- No explicit budget target; only average FPS is tracked.
```

Why: explicit budgets drive sane tradeoffs around allocations and GC.

## 2) Allocation budget per frame

Most GC pause problems are allocation-rate problems first.

```haskell
-- GOOD
-- Allocate heavy structures at startup, reuse in frame loop.
data Runtime = Runtime
  { worldRef   :: !(IORef World)
  , scratchRef :: !(IORef Scratch)
  }
```

```haskell
-- BAD
-- Recreate scratch buffers each frame.
frame :: World -> IO World
frame world = do
  scratch <- newScratch
  pure (step scratch world)
```

Why: predictable allocation rate gives predictable GC cadence.

```haskell
-- GOOD
-- Throttle debug formatting in hot path.
import Data.Text qualified as Text

updateHudText :: Int -> World -> Text
updateHudText frameIx world
  | frameIx `mod` 30 == 0 = renderHud world
  | otherwise = Text.empty
```

```haskell
-- BAD
-- Build fresh debug strings every frame.
updateHudText :: World -> Text
updateHudText world =
  "fps=" <> Text.pack (show world.fps) <> ", entities=" <> Text.pack (show world.entityCount)
```

Why: repeated `show`/`Text.pack` in per-frame code creates avoidable churn.

## 3) Keep hot loops allocation-light

```haskell
-- GOOD
import Data.List (foldl')

sumLen :: [Edge] -> Double
sumLen = foldl' (\acc edge -> acc + edgeLen edge) 0
```

```haskell
-- BAD
sumLen :: [Edge] -> Double
sumLen edges = sum (map edgeLen edges)
```

Why: one-pass reductions often allocate less than map-then-reduce chains.

```haskell
-- GOOD
import Control.Monad (forM_)
import Data.Vector.Mutable qualified as VM

advancePositions :: Double -> VM.IOVector Vec2 -> IO ()
advancePositions !dt positions =
  forM_ [0 .. VM.length positions - 1] $ \i -> do
    pos <- VM.read positions i
    VM.write positions i (stepVec dt pos)
```

```haskell
-- BAD
advancePositions :: Double -> [Vec2] -> [Vec2]
advancePositions dt = map (stepVec dt)
```

Why: mutable vector updates can reuse memory across frames instead of allocating a new list each step.

```haskell
-- GOOD
import Control.Monad.State.Strict (State, modify')
```

```haskell
-- BAD
import Control.Monad.State.Lazy (State, modify)
```

Why: strict state prevents chains of deferred updates in tight loops.

## 4) Keep data layout simple and strict

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

Why: strict/unpacked scalar fields reduce indirections in hot data.

```haskell
-- GOOD
data Particle = Particle
  { pos :: !Vec2
  , vel :: !Vec2
  , ttl :: {-# UNPACK #-} !Float
  }
```

```haskell
-- BAD
data Particle = Particle
  { pos :: Vec2
  , vel :: Vec2
  , ttl :: Maybe Float
  }
```

Why: optional/boxed fields in hot structs create extra allocation and branch noise.

## 5) Design for GC-friendly lifetimes

Separate truly long-lived data from per-frame transient data.
Avoid retaining old frame worlds by accident.

```haskell
-- GOOD
-- Keep only a bounded history for rewind/debug.
keepLastN :: Int -> [a] -> [a]
keepLastN n = take n
```

```haskell
-- BAD
-- Unbounded history keeps entire world graph alive.
appendHistory :: World -> [World] -> [World]
appendHistory world history = world : history
```

Why: accidental retention inflates live set and major GC pauses.

```haskell
-- GOOD
-- Keep closure captures tight in callbacks.
onInput :: IORef World -> InputEvent -> IO ()
onInput worldRef ev = modifyIORef' worldRef (applyInput ev)
```

```haskell
-- BAD
-- Captures large runtime context in every callback closure.
onInput :: Runtime -> InputEvent -> IO ()
onInput runtime ev = do
  let _ = runtime.bigAssetCache
  ...
```

Why: large captures increase survivor set and hurt pause predictability.

## 6) Parallel/async without latency explosions

Parallelize only coarse, CPU-bound pure work with bounded fanout.
Keep the sequential path available.

```haskell
-- GOOD
import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)

buildVisibleSeq :: [Chunk] -> [Mesh]
buildVisibleSeq = map buildMesh

buildVisiblePar :: NFData Mesh => Int -> [Chunk] -> [Mesh]
buildVisiblePar chunkSize =
  withStrategy (parListChunk chunkSize rdeepseq) . map buildMesh
```

```haskell
-- BAD
-- Tiny sparks for tiny jobs can increase jitter.
buildVisiblePar :: [Chunk] -> [Mesh]
buildVisiblePar = withStrategy (parList rseq) . map buildMesh
```

Why: task granularity must amortize scheduling/GC overhead.

```haskell
-- GOOD
import Control.Concurrent.Async (mapConcurrently)

fetchAssetsBounded :: Int -> [Url] -> IO [Asset]
fetchAssetsBounded chunkSize urls =
  fmap concat (traverse (mapConcurrently fetchAsset) (chunked chunkSize urls))

chunked :: Int -> [a] -> [[a]]
chunked n _
  | n <= 0 = []
chunked _ [] = []
chunked n xs =
  let (prefix, suffix) = splitAt n xs
  in prefix : chunked n suffix
```

```haskell
-- BAD
fetchAssets :: [Url] -> IO [Asset]
fetchAssets = mapConcurrently fetchAsset
```

Why: unbounded async over huge inputs can explode memory and produce long pauses.

## 7) Measure and tune RTS flags

Start from measurement, not folklore.

Runtime visibility:
- `+RTS -s`: GC/alloc summary.
- `+RTS -T`: enable `GHC.Stats` counters for runtime sampling.
- `+RTS -l-au`: eventlog (requires build with `-eventlog`).

Typical low-latency tuning candidates to test:
- `-N`: capabilities.
- `-A<size>`: nursery size (bigger can reduce minor GC frequency).
- `-n<size>`: nursery chunking for multicore allocation.
- `-H<size>`: suggested heap size to reduce resizing churn.
- `-I0`: disable idle GC if it causes visible hitching after idle periods.
- `--nonmoving-gc`: test for large old-generation heaps; keep behind config and benchmark.

```bash
# GOOD: compare sequential vs multicore with same workload.
./game +RTS -N1 -A32m -H1G -s -T -l-au -RTS
./game +RTS -N  -A32m -H1G -s -T -l-au -RTS
```

```bash
# BAD: tune flags without stable workload or repeated runs.
./game +RTS -N -RTS
```

Why: latency tuning requires repeated, comparable measurements.

## 8) Pre-ship checklist

1. Keep a sequential baseline path for correctness and fallback.
2. Bound async/parallel fanout and queue sizes.
3. Remove avoidable per-frame allocations in hot code.
4. Keep hot structs strict; unpack small numeric fields where useful.
5. Check `+RTS -s` allocation rate and GC time under worst-case scenes.
6. Inspect eventlog for pause spikes, not just average frame rate.
7. Compare p95/p99 frame time before/after each optimization.
