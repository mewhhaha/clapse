# Resource Safety and Async Exception Discipline (GHC 9.14.1)

For layered typed error policy at service boundaries, use `references/error-architecture.md`.

## Contents
- 1. Exception model for concurrent systems
- 2. Use bracketed resource lifetimes
- 3. Use mask only for critical registration windows
- 4. Keep cancellation structured with async
- 5. Catch policy: rethrow async, classify sync
- 6. Loop and shutdown patterns
- 7. Checklist

## 1) Exception model for concurrent systems

Concurrent Haskell code must handle:
- synchronous exceptions (IO/parsing/logic failures);
- asynchronous exceptions (thread cancellation/interruption).

Design around cleanup determinism and fast cancellation.

## 2) Use bracketed resource lifetimes

```haskell
-- GOOD
withConn :: IO Conn -> (Conn -> IO a) -> IO a
withConn acquire use =
  bracket acquire closeConn use
```

```haskell
-- BAD
withConn :: IO Conn -> (Conn -> IO a) -> IO a
withConn acquire use = do
  conn <- acquire
  use conn
```

Why: unbracketed resources leak on exceptions or cancellation.

```haskell
-- GOOD
withTempFileSafe :: FilePath -> (Handle -> IO a) -> IO a
withTempFileSafe path use =
  bracket (openBinaryFile path WriteMode) hClose use
```

Why: always tie acquisition and release in one lexical place.

## 3) Use mask only for critical registration windows

```haskell
-- GOOD
spawnTracked :: IO () -> (ThreadId -> IO ()) -> IO ThreadId
spawnTracked body registerTid =
  mask $ \restore -> do
    tid <- forkIO (restore body)
    registerTid tid
    pure tid
```

```haskell
-- BAD
spawnTracked :: IO () -> (ThreadId -> IO ()) -> IO ThreadId
spawnTracked body registerTid = do
  tid <- forkIO body
  registerTid tid
  pure tid
```

Why: without masked registration, cancellation can leave orphan threads untracked.

Avoid long masked regions:

```haskell
-- BAD
mask_ $ forever $ do
  stepGame
  threadDelay 16_666
```

Why: overly broad masking makes cancellation/shutdown unresponsive.

## 4) Keep cancellation structured with async

```haskell
-- GOOD
withAsync logicLoop $ \logicA ->
  withAsync renderLoop $ \renderA -> do
    _ <- waitEitherCatch logicA renderA
    cancel logicA
    cancel renderA
```

```haskell
-- BAD
_ <- forkIO logicLoop
_ <- forkIO renderLoop
pure ()
```

Why: structured lifetimes prevent zombie workers and lost failures.

## 5) Catch policy: rethrow async, classify sync

```haskell
-- GOOD
runWorker :: IO () -> IO ()
runWorker action =
  action `catch` \e ->
    case fromException e of
      Just (asyncE :: AsyncException) -> throwIO asyncE
      Nothing -> do
        logSyncFailure e
        recover
```

```haskell
-- BAD
runWorker :: IO () -> IO ()
runWorker action = action `catch` \(_ :: SomeException) -> pure ()
```

Why: swallowing async exceptions breaks cancellation semantics.

## 6) Loop and shutdown patterns

```haskell
-- GOOD
loop :: TVar Bool -> IO ()
loop runningVar = do
  keepGoing <- readTVarIO runningVar
  when keepGoing $ do
    step
    loop runningVar
```

```haskell
-- BAD
loop :: IO ()
loop = forever step
```

Why: explicit shutdown state makes stop logic testable and deterministic.

```haskell
-- GOOD
stepWithCleanup :: IO ()
stepWithCleanup =
  update `onException` rollbackTemporaryState
```

Why: define rollback policy for half-applied steps.

## 7) Checklist

1. Every resource acquisition uses `bracket`/`bracketOnError`.
2. Every spawned thread has ownership and cancellation path.
3. Mask only around minimal non-interruptible registration/critical windows.
4. Do not swallow `AsyncException`.
5. Shutdown path is tested under cancellation and failures.
