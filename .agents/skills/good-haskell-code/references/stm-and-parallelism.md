# STM and Parallelism Strategies (Render + Logic Architectures) (GHC 9.14.1)

For concrete wiring, use `references/render-logic-template.md`.
For package/build setup, use `references/tooling-and-deps.md`.
For Arrowized signal-flow and FRP loop patterns, use `references/arrows-and-frp.md`.

## Contents
- 1. Choose the right concurrency primitive
- 2. Share state between render and logic threads (STM)
- 3. Use command/event queues with backpressure
- 4. Keep STM transactions short and deterministic
- 5. Parallelism strategies beyond STM
- 6. Process boundary design (render process + logic process)
- 7. Low-latency checklist for render/logic pipelines
- 8. Use foreign-store for reload-stable runtime state
- 9. GHCi and hot-reload workflow

## 1) Choose the right concurrency primitive

```txt
GOOD:
- TVar + STM: shared state snapshots, atomic multi-variable updates.
- TQueue/TBQueue: producer/consumer channels.
- async/withAsync: structured lifecycle and cancellation.
- parListChunk/strategies: pure CPU-bound batch parallelism.
- Process IPC (pipes/sockets): cross-process communication.
```

```txt
BAD:
- One global MVar as an application-wide lock.
- Unbounded queues for bursty streams.
- Ad-hoc forkIO without ownership/cancellation.
- Sharing STM vars across OS processes (not possible).
```

Why: different workloads need different coordination semantics.

## 2) Share state between render and logic threads (STM)

Prefer immutable world snapshots with a generation counter.

```haskell
-- GOOD
data Snapshot = Snapshot
  { world      :: !World
  , generation :: !Int
  }

newtype SnapshotBus = SnapshotBus (TVar Snapshot)

publishSnapshot :: SnapshotBus -> World -> STM ()
publishSnapshot (SnapshotBus bus) newWorld = do
  snap <- readTVar bus
  writeTVar bus Snapshot
    { world = newWorld
    , generation = snap.generation + 1
    }

readFreshSnapshot :: SnapshotBus -> TVar Int -> STM World
readFreshSnapshot (SnapshotBus bus) seenGen = do
  snap <- readTVar bus
  seen <- readTVar seenGen
  if snap.generation == seen
    then retry
    else do
      writeTVar seenGen snap.generation
      pure snap.world
```

```haskell
-- BAD
-- render and logic both mutate same world with interleaved updates
logicStep :: TVar World -> Input -> STM ()
logicStep worldVar input = modifyTVar' worldVar (stepWorld input)

renderStep :: TVar World -> STM RenderOutput
renderStep worldVar = do
  world <- readTVar worldVar
  pure (buildRenderOutput world)
```

Why: snapshot handoff avoids render observing partially updated state.

## 3) Use command/event queues with backpressure

Model flow as channels, not incidental shared mutable fields.

```haskell
-- GOOD
data Buses = Buses
  { cmdQ :: TQueue Command
  , evtQ :: TBQueue Event
  }

newBuses :: IO Buses
newBuses = do
  commands <- newTQueueIO
  events <- newTBQueueIO 256
  pure Buses { cmdQ = commands, evtQ = events }

pushEventDropOldest :: TBQueue Event -> Event -> STM ()
pushEventDropOldest q event = do
  full <- isFullTBQueue q
  when full (void readTBQueue q)
  writeTBQueue q event
```

```haskell
-- BAD
-- unbounded queue => runaway memory on producer bursts
newtype EventBus = EventBus (TQueue Event)
```

Why: bounded queues enforce backpressure and cap memory growth.

```haskell
-- GOOD
awaitWork :: TQueue Command -> TBQueue Event -> STM (Either Command Event)
awaitWork commands events =
      (Left <$> readTQueue commands)
  `orElse`
      (Right <$> readTBQueue events)
```

```haskell
-- BAD
pollLoop :: TQueue Command -> IO ()
pollLoop q = forever $ do
  m <- atomically (tryReadTQueue q)
  case m of
    Nothing -> threadDelay 1000
    Just c -> handleCommand c
```

Why: `retry`/`orElse` avoids busy polling and reduces jitter.

## 4) Keep STM transactions short and deterministic

Do expensive work outside `atomically`; commit minimal state in STM.

```haskell
-- GOOD
tickLogic :: TVar World -> [Command] -> IO ()
tickLogic worldVar commands = do
  oldWorld <- atomically (readTVar worldVar)
  let newWorld = applyCommands oldWorld commands
  atomically (writeTVar worldVar newWorld)
```

```haskell
-- BAD
tickLogic :: TVar World -> [Command] -> IO ()
tickLogic worldVar commands =
  atomically $ do
    world <- readTVar worldVar
    let newWorld = expensivePhysicsAndAiStep commands world
    writeTVar worldVar newWorld
```

Why: long transactions increase contention and transaction retries.

```haskell
-- BAD
-- side effects hidden in STM
atomically $ do
  _ <- unsafeIOToSTM (appendFile "log.txt" "tick\n")
  ...
```

Why: keep IO effects outside STM; use effect queues if needed.

## 5) Parallelism strategies beyond STM

### Strategy A: Pure data parallelism

```haskell
-- GOOD
import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)

buildMeshesSeq :: [Chunk] -> [Mesh]
buildMeshesSeq = map buildMesh

buildMeshesPar :: NFData Mesh => Int -> [Chunk] -> [Mesh]
buildMeshesPar chunkSize =
  withStrategy (parListChunk chunkSize rdeepseq) . map buildMesh
```

```haskell
-- BAD
-- one spark per tiny item
buildMeshesPar :: [Chunk] -> [Mesh]
buildMeshesPar = withStrategy (parList rseq) . map buildMesh
```

Why: chunking amortizes scheduling overhead and reduces GC pressure.

### Strategy B: Structured async IO

```haskell
-- GOOD
import Control.Concurrent.Async (waitBoth, withAsync)

runSystems :: IO Physics -> IO Audio -> IO (Physics, Audio)
runSystems physicsIO audioIO =
  withAsync physicsIO $ \a ->
    withAsync audioIO $ \b ->
      waitBoth a b
```

```haskell
-- BAD
runSystems :: IO Physics -> IO Audio -> IO (Physics, Audio)
runSystems physicsIO audioIO = do
  _ <- forkIO physicsIO
  _ <- forkIO audioIO
  pure undefined
```

Why: structured async gives cancellation and exception propagation semantics.

## 6) Process boundary design (render process + logic process)

STM/TVar is in-process only. For separate processes, share data with explicit messages over IPC.

```haskell
-- GOOD
data LogicToRender
  = FrameSnapshot !FrameId !RenderPacket
  | RenderConfig !RenderConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data RenderToLogic
  = InputBatch !FrameId ![InputEvent]
  | RenderAck !FrameId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
```

```haskell
-- BAD
-- assumes process-shared mutable variable
sharedWorld :: TVar World
sharedWorld = unsafePerformIO (newTVarIO initialWorld)
```

Why: processes need a wire protocol, not shared heap references.

Use framing for IPC payloads (length-prefix + binary payload), plus versioning:

```haskell
-- GOOD
data WireEnvelope a = WireEnvelope
  { protocolVersion :: !Word16
  , payload         :: !a
  }
```

Why: protocol versioning enables rolling upgrades and compatibility checks.

## 7) Low-latency checklist for render/logic pipelines

1. Keep logic as source of truth; render consumes immutable snapshots.
2. Use bounded queues (`TBQueue`) for event bursts.
3. Keep STM transactions short and side-effect free.
4. Parallelize only coarse pure stages; keep sequential fallback.
5. Prefer `withAsync` for subsystem lifecycles and cancellation.
6. For multi-process setups, use versioned IPC messages with framing.
7. Track p95/p99 frame times after each concurrency change.

## 8) Use foreign-store for reload-stable runtime state

Use `foreign-store` in development loops where `:reload` should keep live runtime state.
Do not use it as a primary runtime state system for production concurrency.

```haskell
-- GOOD
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Word (Word32)
import Foreign.Store (Store(..), lookupStore, readStore, writeStore)
import System.IO.Unsafe (unsafePerformIO)

storeKey :: Word32
storeKey = 0

runtimeStore :: Store (TVar Runtime)
runtimeStore = Store storeKey

{-# NOINLINE runtimeVar #-}
runtimeVar :: TVar Runtime
runtimeVar = unsafePerformIO $ do
  existing <- lookupStore storeKey
  case existing of
    Just st -> readStore st
    Nothing -> do
      tv <- newTVarIO initialRuntime
      writeStore runtimeStore tv
      pure tv
```

```haskell
-- BAD
-- repeatedly mutate via foreign-store itself in hot path
tick :: Input -> IO ()
tick input = do
  world <- readStore worldStore
  writeStore worldStore (stepWorld input world)
```

Why: keep `foreign-store` as reload-time bootstrap only; use `TVar`/`MVar` for concurrent mutation.

Additional rules:
1. Initialize store once (`lookupStore` then `writeStore` only if missing).
2. Keep a stable numeric key and `NOINLINE` binding.
3. Store synchronization primitives (`TVar`/`MVar`) rather than large mutable graphs directly.
4. For full reset in dev, explicitly clear/reinitialize store keys.

## 9) GHCi and hot-reload workflow

Hot reload works best when state and behavior are separated:
- state lives in `TVar` (possibly rooted via `foreign-store`);
- behavior (`step` function) is replaceable after `:reload`.

```haskell
-- GOOD
data Runtime = Runtime
  { worldVar :: TVar World
  , stepVar  :: TVar (World -> Input -> World)
  }

logicLoop :: Runtime -> IO ()
logicLoop runtime = forever $ do
  input <- pollInput
  stepFn <- readTVarIO runtime.stepVar
  atomically $ modifyTVar' runtime.worldVar (\world -> stepFn world input)

installStep :: Runtime -> (World -> Input -> World) -> IO ()
installStep runtime newStep =
  atomically $ writeTVar runtime.stepVar newStep
```

```haskell
-- BAD
-- state and loop are recreated on every reload
main :: IO ()
main = loop initialWorld defaultStep
```

Why: preserving runtime state across reload reduces restart cost and speeds iteration.

Practical dev loop:
```txt
1. Start ghci/ghcid entrypoint that initializes runtime once.
2. Run loop in background thread.
3. Edit logic module.
4. :reload (or ghcid auto-reload).
5. Reinstall latest step function into stepVar.
```

For separate render and logic processes:
1. Keep hot reload scoped per process.
2. Preserve state via protocol messages/snapshots.
3. Do not depend on `foreign-store` across process boundaries.

Reload failure and recovery playbook:
1. Keep previous `step` function in memory.
2. Compile/load candidate function.
3. Validate candidate on a smoke input before install.
4. If validation fails, keep previous step and emit a typed reload error event.
5. If logic loop throws, restart loop with last known-good step and same world snapshot.

```haskell
-- GOOD
reloadStep :: TVar (World -> Input -> World) -> (World -> Input -> World) -> IO ()
reloadStep stepVar candidate = do
  previous <- readTVarIO stepVar
  let smoke = candidate initialWorld NoopInput
  smoke `seq` atomically (writeTVar stepVar candidate)
    `catch` \(_ :: SomeException) ->
      atomically (writeTVar stepVar previous)
```

```haskell
-- BAD
reloadStep :: TVar (World -> Input -> World) -> (World -> Input -> World) -> IO ()
reloadStep stepVar candidate =
  atomically (writeTVar stepVar candidate) -- no fallback, no validation
```
