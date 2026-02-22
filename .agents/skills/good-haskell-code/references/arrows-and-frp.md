# Arrows and Functional Reactive Programming (GHC 9.14.1)

For deterministic runtime boundaries, also use `references/determinism-and-reproducibility.md` and `references/effect-boundaries.md`.

## Contents
- 1. Use arrows when arrows model intent, not when functions are enough
- 2. Core operator guidance: `arr`, `>>>`, `first`, `loop`
- 3. Build `SF` pipelines with pure signal transforms and explicit IO boundaries
- 4. Enforce fixed-step time discipline for stability and replay
- 5. Handle state and mode switching explicitly in pure transitions
- 6. Watch FRP performance traps in loops
- 7. Interop with STM/async without leaking nondeterminism
- 8. Adopt FRP incrementally with a low-risk rollout

## 1) Use arrows when arrows model intent, not when functions are enough

```haskell
-- GOOD
combine2D :: (Double, Double) -> Double
combine2D (x, y) = x * x + y * y

stepBoard :: Board -> Input -> Board
stepBoard = runPureUpdate
```

```haskell
-- BAD
stepBoardM :: Board -> Input -> IO Board
stepBoardM board input = pure (stepBoard board input)

-- BAD (over-abstracted)
step :: (Arrow a, Monad m) => Board -> m Input -> a (Board, Input) Board
step = undefined
```

Why: pick arrows for composition-heavy signal/flow shapes, and keep plain functions when you only need direct data transformation.

## 2) Core operator guidance: `arr`, `>>>`, `first`, `loop`

```haskell
-- GOOD
{-# LANGUAGE Arrows #-}
import Control.Arrow

pureGain :: Arrow a => Double -> a Double Double
pureGain k = arr (* k)

blend :: Arrow a => a (Double, Double) Double
blend = first (pureGain 0.25) >>> arr (uncurry (+))

integrator :: ArrowLoop a => a (Double, Double) Double
integrator = loop $ proc (x, acc) -> do
  let acc' = x + acc
  returnA -< (acc', acc')
```

```haskell
-- BAD
-- Manual tuple threading duplicates flow structure and makes feedback harder to audit.
blend' :: (Double, Double) -> Double
blend' (x, y) = (x * 0.25) + y

integrator' :: Double -> Double -> (Double, Double)
integrator' x acc = (x + acc, x + acc)
```

Why: `arr`, `>>>`, `first`, and `loop` express graph wiring and feedback explicitly, so topology errors are easier to reason about.

## 3) Build `SF` pipelines with pure signal transforms and explicit IO boundaries

```haskell
-- GOOD
-- Pseudotype (generic):
type SF i o = i -> (o, SF i o)

type In = InputTick

type Out = (Pos, Vel)

physicsPure :: SF In Out
physicsPure = ...

renderBoundary :: Out -> IO ()
renderBoundary _ = pure ()

stepTick :: In -> IO (Out, SF In Out)
stepTick i = do
  let (o, nxt) = physicsPure i
  renderBoundary o
  pure (o, nxt)
```

```haskell
-- BAD
-- Everything in IO makes replay/reasoning hard and leaks side effects into pure math.
physicsAndRender :: In -> IO Out
physicsAndRender i = do
  putStrLn "frame"
  pure (integrate i)
```

Why: keep deterministic physics/math in pure signal functions and move environment effects to one explicit boundary.

## 4) Enforce fixed-step time discipline for stability and replay

```haskell
-- GOOD
-- Fixed-step integrator with replay log
stepWithFixedDt :: SF (Time, Input) World
stepWithFixedDt = ...

advance :: Rational -> World -> Input -> (World, Rational, Output)
advance dt world input =
  let world' = rk4 dt (world, input)
   in (world', dt, project world')

snapshotForReplay :: Time -> World -> ReplayFrame
snapshotForReplay t w = ReplayFrame t w
```

```haskell
-- BAD
-- Frame-time-coupled logic amplifies instability and breaks deterministic re-sim.
advanceBad :: Double -> World -> Input -> World
advanceBad dt world input = if dt > 0 then integrate dt world input else world
```

Why: fixed `dt`, bounded integrator strategy, and explicit snapshotting are what make FRP time behavior reproducible.

## 5) Handle state and mode switching explicitly in pure transitions

```haskell
-- GOOD
data Mode = Idle | Move | Dash deriving (Eq, Show)

type GameSF i o = (i, Mode) -> (o, Mode)

stepMode :: GameSF Input World
stepMode (input, m) =
  let (o, m') = case m of
        Idle -> (idleOutput, if wantMove input then Move else Idle)
        Move -> (moveOutput, if wantDash input then Dash else Move)
        Dash -> (dashOutput, if dashDone then Move else Dash)
   in (o, m')
```

```haskell
-- BAD
-- Implicit mode in separate hidden mutable cells creates accidental transitions.
data BadWorld = BadWorld
  { position :: Pos
  , internalMode :: IORef Mode
  }
```

Why: explicit mode state in the signal graph prevents hidden transitions and makes switching behavior inspectable and testable.

## 6) Watch FRP performance traps in loops

```haskell
-- GOOD
-- Keep closures small; avoid rebuilding large functions per sample.
mkImpulseGate :: Double -> SF In Bool
mkImpulseGate threshold = ...

step :: SF In Out -> SF In Out
step core = core -- shared, constant-shaped pipeline
```

```haskell
-- BAD
-- Alloc-heavy switching + capture-in-loop churn in hot paths.
step' :: Env -> SF In Out
step' env = proc inp -> do
  let heavy = expensiveCapture env  -- rebuilt every frame
  returnA -< heavy inp
```

Why: stable function/object shape and low-capture loops reduce GC pressure and prevent stepwise stalls.

## 7) Interop with STM/async without leaking nondeterminism

```haskell
-- GOOD
-- Treat STM as an input producer, then fold into pure updates.
spawnSampler :: TQueue Input -> IO (IO (Maybe Input))
spawnSampler q = pure $ atomically (Just <$> readTQueue q)

stepFromAsync :: SF (Maybe Input, World) World
stepFromAsync = arr $ \(mb, w) -> maybe w (applyInput w) mb
```

```haskell
-- BAD
-- Reading async sources inside every pure transform breaks deterministic composition.
unsafeStep :: SF (Input, AsyncEvents) World
unsafeStep = arr $ \(i, a) -> ...  -- consumes async at wrong layer
```

Why: isolate `STM/async` to explicit ingestion points and keep the FRP core deterministic from queue snapshots.

## 8) Adopt FRP incrementally with a low-risk rollout

```txt
GOOD:
1. Identify one narrow signal domain (e.g., UI progress, animation, or timer-driven telemetry).
2. Re-implement only that domain as pure SF style with one IO boundary.
3. Keep old path in place with an adapter and compare outputs in tests.
4. Add fixed-step replay harness before broader migration.
5. Promote mode transitions and state shape only after benchmarks and property tests pass.
6. Expand to additional domains only when interfaces stay stable.

BAD:
1. Replacing everything at once without an equivalent old-path oracle.
2. Letting async/stateful side-effects seep into signal logic.
3. Switching time base from fixed step before validation.
4. Changing control-flow (`switch`) behavior without explicit transition tests.
```

Why: incremental migration reduces architectural risk while preserving correctness and performance invariants.
