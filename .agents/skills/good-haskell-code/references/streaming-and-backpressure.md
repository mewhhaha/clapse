# Streaming and Backpressure Patterns (GHC 9.14.1)

## Contents
- 1. Bound every stage
- 2. Prefer incremental parsers
- 3. Use bounded queues between threads
- 4. Avoid full materialization
- 5. Conduit/Pipes/Streaming guidance
- 6. Checklist

## 1) Bound every stage

Backpressure exists only when producers cannot outrun consumers indefinitely.

```haskell
-- GOOD
newEventBus :: IO (TBQueue Event)
newEventBus = newTBQueueIO 1024
```

```haskell
-- BAD
newEventBus :: IO (TQueue Event)
newEventBus = newTQueueIO
```

Why: unbounded queues turn bursts into unbounded memory growth.

## 2) Prefer incremental parsers

```haskell
-- GOOD
decodeStreamed :: ByteString -> Either ParseError [Record]
decodeStreamed = runIncrementalParser
```

```haskell
-- BAD
decodeWholeFile :: FilePath -> IO (Either ParseError [Record])
decodeWholeFile path = do
  bytes <- BS.readFile path
  pure (decodeAll bytes)
```

Why: incremental parsing keeps memory bounded and starts work earlier.

## 3) Use bounded queues between threads

```haskell
-- GOOD
pushEventDropOldest :: TBQueue Event -> Event -> STM ()
pushEventDropOldest q event = do
  full <- isFullTBQueue q
  when full (void readTBQueue q)
  writeTBQueue q event
```

```haskell
-- BAD
pushEvent :: TQueue Event -> Event -> STM ()
pushEvent q event = writeTQueue q event
```

Why: explicit backpressure/drop policy is better than implicit memory blowup.

## 4) Avoid full materialization

```haskell
-- GOOD
processLines :: Handle -> IO ()
processLines h = do
  eof <- hIsEOF h
  unless eof $ do
    line <- BS.hGetLine h
    processLine line
    processLines h
```

```haskell
-- BAD
processLines :: FilePath -> IO ()
processLines path = do
  bs <- BS.readFile path
  mapM_ processLine (BS.lines bs)
```

Why: full-file loads remove flow control and spike residency on large payloads.

## 5) Conduit/Pipes/Streaming guidance

Use whichever stack your repo already uses; apply the same rules:
1. bounded chunk sizes;
2. bounded cross-thread queues;
3. incremental decode;
4. no hidden whole-stream conversion.

```txt
GOOD:
- Conduit: source .| bounded transform .| sink
- Pipes: bounded producer/consumer channels
- Streaming: chunked folds and bounded windows
```

```txt
BAD:
- convert stream to list/ByteString and process afterward
- random chunk sizing per run
- unbounded worker fanout without queue limits
```

## 6) Checklist

1. Choose explicit max chunk size and keep it stable.
2. Bound every queue crossing thread boundaries.
3. Ensure parser works incrementally for large inputs.
4. Measure allocation/residency under worst-case payloads.
5. Keep a fallback serial path with strict bounds for degraded mode.
