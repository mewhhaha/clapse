# Observability (GHC 9.14.1)

For profiling details, also use `references/benchmarking-and-profiling.md`.
For low-latency loops, also use `references/low-latency.md`.

## Contents
- 1. Prefer structured logs over ad-hoc strings
- 2. Gate debug logs explicitly
- 3. Use bounded asynchronous log pipelines
- 4. Keep metric cardinality bounded
- 5. Use stable enums for labels
- 6. Trace phase boundaries, not every line
- 7. Keep hot-path counters cheap
- 8. Observe queue pressure early

## 1) Prefer structured logs over ad-hoc strings

```haskell
-- GOOD
logInfo "request.complete" [("method", method), ("status", "ok")]
```

```haskell
-- BAD
putStrLn ("request done: " <> show request)
```

Why: structured logs are easier to query and alert on at scale.

## 2) Gate debug logs explicitly

```haskell
-- GOOD
when cfg.debug $
  logDebug "cache.hit" [("key", key)]
```

```haskell
-- BAD
logDebug "cache.hit" [("key", key)]
```

Why: always-on debug logging increases allocation and latency overhead.

## 3) Use bounded asynchronous log pipelines

```haskell
-- GOOD
newtype LogQueue = LogQueue (TBQueue LogLine)

newLogQueue :: IO LogQueue
newLogQueue = LogQueue <$> newTBQueueIO 4096
```

```haskell
-- BAD
newtype LogQueue = LogQueue (TQueue LogLine)
```

Why: bounded queues prevent logging bursts from turning into unbounded memory growth.

## 4) Keep metric cardinality bounded

```haskell
-- GOOD
incCounter "http.requests" [("status_class", "2xx")]
```

```haskell
-- BAD
incCounter "http.requests" [("path", rawPath), ("user_id", userIdText)]
```

Why: high-cardinality labels can overload metrics backends.

## 5) Use stable enums for labels

```haskell
-- GOOD
data RequestResult = ReqOk | ReqTimeout | ReqDecodeErr

resultLabel :: RequestResult -> Text
resultLabel = \case
  ReqOk -> "ok"
  ReqTimeout -> "timeout"
  ReqDecodeErr -> "decode_err"
```

```haskell
-- BAD
resultLabel :: Text -> Text
resultLabel = id
```

Why: finite enums keep label space bounded and predictable.

## 6) Trace phase boundaries, not every line

```haskell
-- GOOD
withSpan "batch.process" processBatch
```

```haskell
-- BAD
withSpan "loop.iteration" processOneItem
```

Why: too many tiny spans add overhead and reduce signal-to-noise ratio.

## 7) Keep hot-path counters cheap

```haskell
-- GOOD
incCounterFast :: Counter -> IO ()
incCounterFast = atomicIncCounter
```

```haskell
-- BAD
recordCounter :: Map Text Int -> Text -> Map Text Int
recordCounter counters key = Map.insertWith (+) key 1 counters
```

Why: hot-path telemetry should avoid expensive dynamic aggregation.

## 8) Observe queue pressure early

```haskell
-- GOOD
setGauge "queue.inflight" inflight
setGauge "queue.wait_ns_p95" waitNsP95
```

```haskell
-- BAD
-- only latency metrics, no queue pressure visibility
```

Why: queue pressure usually rises before visible latency failures.
