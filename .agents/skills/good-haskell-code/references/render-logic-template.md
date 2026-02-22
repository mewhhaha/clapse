# Render + Logic Template (STM, Async, Hot Reload) (GHC 9.14.1)

For Arrow/FRP step composition and deterministic time-step patterns, use `references/arrows-and-frp.md`.

## Contents
- 1. Architecture
- 2. Minimal skeleton
- 3. Hot reload install/fallback
- 4. Process split template
- 5. Good/Bad checklist

## 1) Architecture

Baseline shape:
1. Logic loop owns authoritative `World`.
2. Render loop consumes immutable snapshots.
3. Input/events flow through bounded queues.
4. Step function is swappable for hot reload.
5. Lifecycle is structured with `withAsync`.

## 2) Minimal skeleton

```haskell
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Monad (forever)

data Input = Tick | Quit deriving stock (Eq, Show)
data World = World { tick :: !Int } deriving stock (Eq, Show)
data Snapshot = Snapshot { snapTick :: !Int } deriving stock (Eq, Show)

type Step = World -> Input -> Either String World

data Runtime = Runtime
  { worldVar    :: TVar World
  , snapshotVar :: TVar Snapshot
  , inputQ      :: TBQueue Input
  , stepVar     :: TVar Step
  , runningVar  :: TVar Bool
  }

defaultStep :: Step
defaultStep world Tick = Right world { tick = world.tick + 1 }
defaultStep world Quit = Right world

newRuntime :: IO Runtime
newRuntime = do
  world <- newTVarIO (World 0)
  snap <- newTVarIO (Snapshot 0)
  input <- newTBQueueIO 256
  step <- newTVarIO defaultStep
  running <- newTVarIO True
  pure Runtime
    { worldVar = world
    , snapshotVar = snap
    , inputQ = input
    , stepVar = step
    , runningVar = running
    }

logicLoop :: Runtime -> IO ()
logicLoop runtime = forever $ do
  running <- readTVarIO runtime.runningVar
  if not running
    then threadDelay 1_000
    else do
      input <- atomically $ readTBQueue runtime.inputQ
      step <- readTVarIO runtime.stepVar
      world <- readTVarIO runtime.worldVar
      case step world input of
        Left _err -> pure ()  -- keep old world on failure
        Right world' ->
          atomically $ do
            writeTVar runtime.worldVar world'
            writeTVar runtime.snapshotVar (Snapshot world'.tick)
      threadDelay 16_666

renderLoop :: Runtime -> IO ()
renderLoop runtime = forever $ do
  snap <- readTVarIO runtime.snapshotVar
  render snap
  threadDelay 16_666

render :: Snapshot -> IO ()
render _ = pure ()

main :: IO ()
main = do
  runtime <- newRuntime
  withAsync (logicLoop runtime) $ \_logic ->
    withAsync (renderLoop runtime) $ \_render ->
      forever $ threadDelay 1_000_000
```

Notes:
- Keep render pure-read if possible.
- Keep only one writer for authoritative world.
- Use `TBQueue` for bounded ingress.

## 3) Hot reload install/fallback

```haskell
installStep :: Runtime -> Step -> IO ()
installStep runtime newStep =
  atomically $ writeTVar runtime.stepVar newStep

installStepSafe :: Runtime -> Step -> IO ()
installStepSafe runtime newStep = do
  oldStep <- readTVarIO runtime.stepVar
  atomically $ writeTVar runtime.stepVar $ \world input ->
    case newStep world input of
      Left _ -> oldStep world input
      ok -> ok
```

Why: failed reload should degrade gracefully, not kill the loop.

## 4) Process split template

When render and logic run in separate processes:
- process A (logic) publishes versioned snapshots/messages;
- process B (render) consumes snapshots and sends input acks/events.

```haskell
data LogicToRender
  = Frame !Word64 !RenderPacket
  | Config !RenderConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data RenderToLogic
  = InputBatch !Word64 ![Input]
  | FrameAck !Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
```

Use length-prefixed framing and protocol version envelope.
Do not attempt cross-process sharing via `TVar`/STM.

## 5) Good/Bad checklist

```txt
GOOD:
- one authoritative world owner;
- bounded queues for burst control;
- immutable snapshots to renderer;
- structured async lifecycle;
- reload fallback path.
```

```txt
BAD:
- render and logic both mutating world;
- unbounded queues;
- restart entire process on every code change;
- no protocol versioning in process-split mode.
```
