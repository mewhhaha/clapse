# Error Architecture (GHC 9.14.1)

For exception and cancellation discipline, also use `references/resource-safety-and-exceptions.md`.

## Contents
- 1. Separate domain, application, and infrastructure errors
- 2. Wrap context at boundaries
- 3. Use typed recoverable errors in pure core logic
- 4. Reserve exceptions for exceptional runtime failures
- 5. Log once at the boundary
- 6. Preserve causality in concurrent paths
- 7. Checklist

## 1) Separate domain, application, and infrastructure errors

```haskell
-- GOOD
data DomainErr
  = ErrNotFound
  | ErrRuleBroken Text
  deriving stock (Eq, Show)

data AppErr
  = AppDomain DomainErr
  | AppValidation Text
  deriving stock (Eq, Show)

data InfraErr
  = InfraIo IOException
  | InfraDecode Text
  deriving stock (Show)
```

```haskell
-- BAD
type AppErr = String
```

Why: layered error types make handling policy explicit and maintainable.

## 2) Wrap context at boundaries

```haskell
-- GOOD
loadSave :: FilePath -> IO (Either AppErr SaveDoc)
loadSave fp = do
  eBytes <- try (BS.readFile fp)
  pure $ case eBytes of
    Left ioErr -> Left (AppValidation ("io read failed: " <> Text.pack (show (ioErr :: IOException))))
    Right bytes -> first AppDomain (parseSave bytes)
```

```haskell
-- BAD
loadSave :: FilePath -> IO (Either Text SaveDoc)
loadSave fp = first (Text.pack . show) <$> parseFromDisk fp
```

Why: boundary context should tell callers what operation failed, not just that something failed.

## 3) Use typed recoverable errors in pure core logic

```haskell
-- GOOD
applyCoupon :: Coupon -> Cart -> Either DomainErr Cart
applyCoupon coupon cart
  | invalid coupon = Left (ErrRuleBroken "invalid coupon")
  | otherwise = Right (applyValidCoupon coupon cart)
```

```haskell
-- BAD
applyCoupon :: Coupon -> Cart -> Cart
applyCoupon coupon cart
  | invalid coupon = error "invalid coupon"
  | otherwise = applyValidCoupon coupon cart
```

Why: recoverable failures should be explicit in types, not hidden as runtime crashes.

## 4) Reserve exceptions for exceptional runtime failures

```haskell
-- GOOD
withConn :: IO Conn -> (Conn -> IO a) -> IO a
withConn acquire use = bracket acquire closeConn use
```

```haskell
-- BAD
withConn :: IO Conn -> (Conn -> IO a) -> IO a
withConn acquire use = do
  conn <- acquire
  use conn
```

Why: exceptions are appropriate for failure cleanup paths where resource safety matters.

## 5) Log once at the boundary

```haskell
-- GOOD
handleRequest :: Request -> IO Response
handleRequest req = do
  result <- runRequest req
  case result of
    Left err -> do
      logErr req err
      pure (renderErr err)
    Right ok -> pure (renderOk ok)
```

```haskell
-- BAD
parseRequest :: Request -> IO (Either AppErr Parsed)
parseRequest req = do
  logDebug "parse start"
  ...
  logDebug "parse end"
```

Why: centralized logging prevents duplicated noise and preserves clear failure narratives.

## 6) Preserve causality in concurrent paths

```haskell
-- GOOD
runJobs :: [IO a] -> IO (Either AppErr [a])
runJobs jobs = do
  results <- traverse try jobs
  pure $
    case sequence results of
      Left (e :: SomeException) -> Left (AppValidation (Text.pack (show e)))
      Right xs -> Right xs
```

```haskell
-- BAD
runJobs :: [IO a] -> IO [Either AppErr a]
runJobs = traverse (\job -> (Right <$> job) `catch` (\(_ :: SomeException) -> pure (Left (AppValidation "failed"))))
```

Why: a single coherent error path is easier to retry, alert, and reason about.

## 7) Checklist

```txt
GOOD:
1. Domain logic returns typed recoverable errors.
2. Infra failures are wrapped with operation context.
3. Exceptions are used for exceptional runtime failures and cleanup.
4. Logging happens at request/job boundaries, not deep in pure core helpers.
5. Concurrent flows preserve error causality and do not silently drop failures.
```
