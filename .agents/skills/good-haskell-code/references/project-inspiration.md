# Project Inspiration (Good Haskell in Practice) (GHC 9.14.1)

Use these projects as style signals. Do not cargo-cult internals; borrow the underlying discipline.

## Contents
- 1. `aeson`: explicit parse errors and compositional decoders
- 2. `optparse-applicative`: compositional APIs
- 3. `servant`: types as API contracts
- 4. `text` and `bytestring`: specialization and strictness
- 5. `rio`: explicit imports and practical defaults
- 6. `QuickCheck` and `hedgehog`: law-first testing
- 7. `warp/wai`: tight boundaries between pure and effectful code

## 1) `aeson`: explicit parse errors and compositional decoders

```haskell
-- GOOD
decodeCustomer :: Value -> Parser Customer
decodeCustomer = withObject "Customer" $ \o ->
  Customer
    <$> o .: "id"
    <*> o .: "email"
```

```haskell
-- BAD
decodeCustomer :: Value -> Customer
decodeCustomer = fromJust . parseMaybe (\v -> ...)
```

Pattern: keep parsing failures explicit and local.

## 2) `optparse-applicative`: compositional APIs

```haskell
-- GOOD
commandParser :: Parser Command
commandParser =
  subparser
    ( command "serve" (info serveParser mempty)
   <> command "migrate" (info migrateParser mempty)
    )
```

```haskell
-- BAD
parseArgs :: [String] -> Either String Command
parseArgs ("serve":_) = Right Serve
parseArgs ("migrate":_) = Right Migrate
parseArgs _ = Left "unknown command"
```

Pattern: build APIs from small combinators, not ad-hoc branching.

## 3) `servant`: types as API contracts

```haskell
-- GOOD
type Api =
       "users" :> Capture "id" UserId :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] NewUser :> PostCreated '[JSON] User
```

```haskell
-- BAD
type Api = ServerRequest -> IO ServerResponse
```

Pattern: encode protocol shape in types so refactors are compiler-assisted.

## 4) `text` and `bytestring`: specialization and strictness

```haskell
-- GOOD
normalize :: Text -> Text
normalize = Text.toCaseFold . Text.strip
```

```haskell
-- BAD
normalize :: String -> String
normalize = map toLower . trim
```

Pattern: use data representations that match workload constraints.

## 5) `rio`: explicit imports and practical defaults

```haskell
-- GOOD
import RIO
import RIO.Text qualified as Text
```

```haskell
-- BAD
import Prelude
import Data.List
import Data.Text
import Data.Map
```

Pattern: control namespace and defaults for predictable codebases.

## 6) `QuickCheck` and `hedgehog`: law-first testing

```haskell
-- GOOD
prop_roundTrip :: Payload -> Property
prop_roundTrip payload =
  decodePayload (encodePayload payload) === Right payload
```

```haskell
-- BAD
test_roundTripExample :: Assertion
test_roundTripExample =
  decodePayload (encodePayload fixture) @?= Right fixture
```

Pattern: enforce invariants as properties, not isolated examples.

## 7) `warp/wai`: tight boundaries between pure and effectful code

```haskell
-- GOOD
routeRequest :: Request -> Either RouteError Route
routeRequest = ...

app :: Application
app req respond =
  case routeRequest req of
    Left err -> respond (renderRouteError err)
    Right route -> runRoute route respond
```

```haskell
-- BAD
app :: Application
app req respond = do
  putStrLn "routing"
  if pathInfo req == ["users"]
    then ...
    else ...
```

Pattern: keep effectful boundaries thin and pure decision logic testable.
