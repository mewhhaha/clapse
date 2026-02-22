# Deriving and Template Haskell (Including Compile-Time Caching) (GHC 9.14.1)

## Contents
- 1. Deriving strategy rules
- 2. Common deriving patterns
- 3. Generic-derived instance performance rules
- 4. Template Haskell boundaries
- 5. Compile-time caching patterns
- 6. Determinism, invalidation, and regeneration policy
- 7. Good/Bad checklist

## 1) Deriving strategy rules

Prefer explicit deriving strategy so instance intent is clear during review.

```haskell
-- GOOD
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype UserId = UserId Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)
```

```haskell
-- BAD
newtype UserId = UserId Int
  deriving (Show, Eq, Ord, Num)
```

Why: explicit strategy prevents accidental behavior changes when classes/instances evolve.

## 2) Common deriving patterns

### `DerivingVia` for policy reuse

```haskell
-- GOOD
{-# LANGUAGE DerivingVia #-}

newtype Duration = Duration Int
  deriving stock (Show, Eq)

deriving via (Sum Int) instance Semigroup Duration
deriving via (Sum Int) instance Monoid Duration
```

```haskell
-- BAD
newtype Dollars = Dollars Int
  deriving stock (Show, Eq)

deriving via (Product Int) instance Semigroup Dollars
```

Why: choose `via` wrappers that match domain semantics, not convenience.

### `DeriveAnyClass` only when defaults make sense

```haskell
-- GOOD
{-# LANGUAGE DeriveAnyClass #-}

class Taggable a where
  isTaggable :: a -> Bool
  isTaggable _ = True

data Config = Config { timeoutMs :: Int }
  deriving stock (Show, Eq)
  deriving anyclass (Taggable)
```

```haskell
-- BAD
class NeedsMethod a where
  encodeIt :: a -> String

data Broken = Broken
  deriving anyclass (NeedsMethod)
```

Why: `anyclass` does not invent non-default method implementations.

### `StandaloneDeriving` for constrained/external cases

```haskell
-- GOOD
{-# LANGUAGE StandaloneDeriving #-}

data Pair a = Pair a a
deriving stock instance Eq a => Eq (Pair a)
deriving stock instance Show a => Show (Pair a)
```

Why: use standalone deriving when declaration-site deriving is insufficient.

## 3) Generic-derived instance performance rules

Generic deriving is usually fine for readability and non-hot paths.
For hot codecs/inner loops, benchmark before keeping generic defaults.

```haskell
-- GOOD
{-# LANGUAGE DeriveGeneric #-}

data SaveState = SaveState
  { level :: !Int
  , score :: !Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SaveState where
  toEncoding = genericToEncoding saveJsonOptions
  toJSON = genericToJSON saveJsonOptions
```

```haskell
-- BAD
data HotPacket = HotPacket
  { tag   :: !Word8
  , value :: !Int64
  }
  deriving stock (Generic)
  deriving anyclass (Binary)
```

Why: auto-derived generic instances can allocate more or encode less predictably in hot paths.

```haskell
-- GOOD
data HotPacket = HotPacket
  { tag   :: !Word8
  , value :: !Int64
  }
  deriving stock (Eq, Show)

instance Binary HotPacket where
  put (HotPacket t v) = putWord8 t >> putInt64be v
  get = HotPacket <$> getWord8 <*> getInt64be
```

Why: hand-written leaf instances are often easier to optimize and reason about for performance-critical formats.

## 4) Template Haskell boundaries

Use TH for compile-time generation and embedding, not core runtime logic.

```haskell
-- GOOD
{-# LANGUAGE TemplateHaskell #-}

module App.BuildTables where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
```

```haskell
-- BAD
-- business logic embedded in large TH splices
myRuntimeLogic = $(...)
```

Why: large logic splices hurt readability, compile speed, and debugging.

## 5) Compile-time caching patterns

### Pattern A: Embed immutable assets

```haskell
-- GOOD
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)

schemaJson :: ByteString
schemaJson = $(embedFile "assets/schema.json")
```

Why: removes runtime file IO/parsing for static blobs.

### Pattern B: Parse once at compile time and generate lookup table

```haskell
-- GOOD
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import Data.List (sortOn)

keywords :: [(String, Int)]
keywords = $(do
  let fp = "assets/keywords.csv"
  addDependentFile fp
  raw <- runIO (readFile fp)
  let rows = parseKeywords raw
      normalized = sortOn fst (dedupe rows)
  lift normalized)
```

```haskell
-- BAD
keywords :: IO [(String, Int)]
keywords = do
  raw <- readFile "assets/keywords.csv"
  pure (parseKeywords raw)
```

Why: compile-time caching avoids repeated startup parse costs and runtime failure points.

### Pattern C: Generated cache + validation

```haskell
-- GOOD
cachedRules :: [(String, Rule)]
cachedRules = $(do
  let fp = "assets/rules.json"
  addDependentFile fp
  rules <- runIO (loadAndValidateRules fp)  -- fail build on invalid data
  lift (sortOn fst rules))
```

Why: fail-fast during build is better than shipping invalid runtime tables.

## 6) Determinism, invalidation, and regeneration policy

```txt
GOOD:
- call addDependentFile for every external file read in TH;
- sort generated lists/maps before lifting;
- reject duplicates/inconsistent schema with fail;
- avoid clocks/random/network/env-dependent generation;
- keep a documented regenerate command for checked-in generated artifacts.
```

```txt
BAD:
- runIO getCurrentTime/random in splices;
- directory walks without deterministic ordering;
- generated output that depends on machine-local state;
- no explicit regeneration workflow for generated modules/files.
```

Why: deterministic generation keeps incremental builds and cache behavior predictable.

Regeneration policy:
1. If generated output is checked in, document one canonical regenerate command in README/cabal comments.
2. Run regeneration in CI verification and fail on dirty tree drift.
3. Keep generated-module boundaries narrow so review diffs stay understandable.
4. Prefer TH-only embedding when checked-in generated files are unnecessary.

## 7) Good/Bad checklist

```txt
GOOD:
- explicit deriving strategies (`stock` / `newtype` / `via` / `anyclass`);
- benchmark generic-derived instances before using them in hot paths;
- narrow TH modules dedicated to generation;
- compile-time caching for static parse-heavy data;
- deterministic, dependency-tracked TH generation with explicit regeneration policy.
```

```txt
BAD:
- implicit deriving in mixed/newtype-heavy modules;
- generic derivation in critical wire/hot paths with no measurement;
- TH-heavy business logic in application modules;
- runtime parsing of static datasets used every startup;
- nondeterministic build-time codegen;
- generated artifacts with no repeatable regenerate command.
```
