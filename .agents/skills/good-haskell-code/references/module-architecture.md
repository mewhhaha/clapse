# Module Architecture (GHC 9.14.1)

## Contents
- 1. Draw hard internal/public boundaries
- 2. Export only what is stable
- 3. Re-export as a policy, not convenience
- 4. Keep dependency direction acyclic
- 5. Group modules by ownership and lifecycle
- 6. Control type surface across module edges
- 7. Use module boundaries for testing seams

## 1) Draw hard internal/public boundaries

```haskell
-- GOOD
module MyLib.Internal.Cache (CacheKey, CacheState, newCache) where

module MyLib
  ( Cache
  , lookup
  , insert
  ) where
```

```haskell
-- BAD
module MyLib where
import MyLib.Internal.Cache
```

Why: explicit boundary modules prevent accidental API commitments from internal utilities.

## 2) Export only what is stable

```haskell
-- GOOD
module MyLib
  ( Session
  , newSession
  , closeSession
  )
```

```haskell
-- BAD
module MyLib (module MyLib.Internal.Session) where
```

Why: re-exporting internal modules locks in internals and slows future refactors.

## 3) Re-export as a policy, not convenience

```haskell
-- GOOD
module MyLib.Prelude
  ( module Data.Text
  , module MyLib.Core
  ) where
```

```haskell
-- BAD
module MyLib
  ( module MyLib.Core
  , module MyLib.IO
  , module MyLib.Extra
  ) where
```

Why: central re-exports should be intentional and versioned, otherwise they become hidden dependency coupling.

## 4) Keep dependency direction acyclic

```haskell
-- GOOD
-- Domain -> App -> Runtime -> CLI
-- no backwards edge from Runtime to Domain
```

```haskell
-- BAD
-- Domain imports Runtime for logging helpers
```

Why: cycles increase rebuild cost and make dependency upgrades harder to reason about.

## 5) Group modules by ownership and lifecycle

```haskell
src/
  MyLib/
    Domain/
    Infra/
    Api/
    Internal/
    Migration/
```

```txt
GOOD:
- Domain has pure core types and invariants
- Infra implements IO/FFI/adapters
- Api exposes serialization and public entrypoints
BAD:
- all modules in one file for "speed", then export everything later
```

Why: ownership boundaries simplify code review and migration ownership.

## 6) Control type surface across module edges

```haskell
-- GOOD
newtype Connection = Connection { unConnection :: STM (Maybe Handle) }
runConnection :: Connection -> ...
```

```haskell
-- BAD
data Connection = Connection { handle :: Handle, state :: TVar InternalState }
```

Why: exposing internal fields in public types raises coupling and prevents changing internals safely.

## 7) Use module boundaries for testing seams

```haskell
-- GOOD
class Clock m where now :: m UTCTime
module MyLib.Test.Clock (mockClock) where
```

```haskell
-- BAD
-- hard-coded `getCurrentTime` calls in every module under test
```

Why: stable interfaces and dedicated test modules allow deterministic tests without internals.
