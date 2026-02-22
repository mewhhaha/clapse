# Typeclass Hygiene (GHC 9.14.1)

For deriving strategy details, also use `references/deriving-and-template-haskell.md`.

## Contents
- 1. State laws for every class
- 2. Avoid orphan instances
- 3. Keep defaults explicit and intentional
- 4. Prefer concrete functions when abstraction is not needed
- 5. Avoid overlap and incoherence
- 6. Split capability classes by concern
- 7. Keep constraints minimal
- 8. Checklist

## 1) State laws for every class

```haskell
-- GOOD
class Semigroup a => IdempotentMerge a where
  merge :: a -> a -> a
  -- law: merge x x == x
```

```haskell
-- BAD
class Semigroup a => IdempotentMerge a where
  merge :: a -> a -> a
-- no laws documented
```

Why: undocumented laws make instance behavior unpredictable and hard to review.

## 2) Avoid orphan instances

```haskell
-- GOOD
newtype AppText = AppText Text
  deriving stock (Eq, Show)
```

```haskell
-- BAD
instance Show Text where
  show = Text.unpack
```

Why: orphan instances create global behavior changes that are difficult to track.

## 3) Keep defaults explicit and intentional

```haskell
-- GOOD
class Parse a where
  parseStrict :: ByteString -> Either ParseErr a
  parseLenient :: ByteString -> Either ParseErr a
  parseLenient = parseStrict
```

```haskell
-- BAD
class Parse a where
  parseStrict :: ByteString -> Either ParseErr a
  parseLenient = parseStrict
```

Why: explicit signatures on defaults make intended behavior clear during review.

## 4) Prefer concrete functions when abstraction is not needed

```haskell
-- GOOD
renderInvoice :: Invoice -> Text
renderInvoice = ...
```

```haskell
-- BAD
class Render a where
  render :: a -> Text
```

Why: a class with one practical instance is usually needless abstraction.

## 5) Avoid overlap and incoherence

```haskell
-- GOOD
class Logger m where
  logMsg :: Text -> m ()

instance Logger IO where
  logMsg = putStrLn . Text.unpack
```

```haskell
-- BAD
instance {-# OVERLAPPABLE #-} Logger m where
  logMsg _ = pure ()
```

Why: overlapping instances hide dispatch policy inside compiler resolution rules.

## 6) Split capability classes by concern

```haskell
-- GOOD
class Monad m => MonadClock m where
  now :: m UTCTime

class Monad m => MonadLogger m where
  logErr :: Text -> m ()
```

```haskell
-- BAD
class Monad m => Runtime m where
  now :: m UTCTime
  logErr :: Text -> m ()
  readConfig :: m Config
  sendHttp :: Request -> m Response
```

Why: smaller classes are easier to mock, compose, and evolve.

## 7) Keep constraints minimal

```haskell
-- GOOD
save :: (MonadIO m, MonadError AppErr m) => State -> m ()
save = ...
```

```haskell
-- BAD
save :: (MonadIO m, MonadError AppErr m, MonadReader Env m, MonadLogger m, MonadClock m, MonadRandom m) => State -> m ()
save = ...
```

Why: over-constrained signatures reduce reuse and increase coupling.

## 8) Checklist

```txt
GOOD:
1. Every custom class includes law comments.
2. No orphan instances unless explicitly justified and isolated.
3. Defaults are typed and documented.
4. No overlap pragmas in normal application code.
5. Constraints express only required capabilities.
```
