# Effect Boundaries (GHC 9.14.1)

For IO-at-the-edge style and invariants, also use `references/invariants.md`.

## Contents
- 1. Keep core domain logic pure
- 2. Use concrete `AppM` at wiring edges
- 3. Prefer small capability constraints over deep stacks
- 4. Use polymorphic interfaces only when reuse is real
- 5. Keep interpreters thin
- 6. Inject effects for deterministic tests
- 7. Avoid transformer sprawl
- 8. Checklist

## 1) Keep core domain logic pure

```haskell
-- GOOD
stepTurn :: Board -> Input -> Either DomainErr Board
stepTurn board input = ...
```

```haskell
-- BAD
stepTurn :: MonadIO m => Board -> Input -> m (Either DomainErr Board)
stepTurn board input = ...
```

Why: pure core logic is simpler to test and reason about than effectful core logic.

## 2) Use concrete `AppM` at wiring edges

```haskell
-- GOOD
newtype AppM a = AppM { unAppM :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runAppM :: Env -> AppM a -> IO a
runAppM env app = runReaderT app.unAppM env
```

```haskell
-- BAD
type AppM = ReaderT Env (StateT Runtime (ExceptT AppErr IO))
```

Why: keep the runtime wiring monad concrete and as small as possible.

## 3) Prefer small capability constraints over deep stacks

```haskell
-- GOOD
class Monad m => MonadClock m where
  now :: m UTCTime

class Monad m => MonadLogger m where
  logInfo :: Text -> m ()

process :: (MonadClock m, MonadLogger m) => Input -> m Output
process input = ...
```

```haskell
-- BAD
process :: ReaderT Env (StateT Runtime (ExceptT AppErr IO)) Output
process = ...
```

Why: focused constraints preserve readability and composability.

## 4) Use polymorphic interfaces only when reuse is real

```haskell
-- GOOD
cacheLookup :: MonadCache m => Key -> m (Maybe Value)
cacheLookup = ...
```

```haskell
-- BAD
renderFrame :: (MonadCache m, MonadClock m, MonadHTTP m, MonadRandom m) => FrameInput -> m Frame
renderFrame = ...
```

Why: over-generalized signatures can obscure required behavior instead of improving reuse.

## 5) Keep interpreters thin

```haskell
-- GOOD
runClockIO :: IO UTCTime
runClockIO = getCurrentTime
```

```haskell
-- BAD
runClockIO :: IO UTCTime
runClockIO = do
  -- business rules mixed into infra adapter
  t <- getCurrentTime
  pure (normalizeBusinessTime t)
```

Why: interpreters should adapt effects, not own domain policy.

## 6) Inject effects for deterministic tests

```haskell
-- GOOD
newtype FakeClock a = FakeClock { runFakeClock :: State UTCTime a }
  deriving newtype (Functor, Applicative, Monad, MonadState UTCTime)

instance MonadClock FakeClock where
  now = get
```

```haskell
-- BAD
testProcess :: IO Output
testProcess = process sampleInput
```

Why: fake interpreters remove nondeterminism from domain-level tests.

## 7) Avoid transformer sprawl

```haskell
-- GOOD
newtype App a = App
  { unApp :: ReaderT Env (ExceptT AppErr IO) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadError AppErr, MonadIO)
```

```haskell
-- BAD
newtype App a = App
  { unApp :: ReaderT Env (StateT S (ExceptT AppErr (WriterT [Log] IO))) a
  }
```

Why: every extra layer increases cognitive load and maintenance risk.

## 8) Checklist

```txt
GOOD:
1. Core algorithms are pure.
2. `AppM` is concrete and thin.
3. Capabilities are small and explicit.
4. Effect interpreters are adapters, not policy engines.
5. Tests run against fake interpreters where practical.
```
