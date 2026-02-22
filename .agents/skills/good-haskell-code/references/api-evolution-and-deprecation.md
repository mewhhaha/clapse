# API Evolution and Deprecation (GHC 9.14.1)

## Contents
- 1. Track API as a compatibility contract
- 2. Additive changes first
- 3. Deprecate with timeline and intent
- 4. Keep shims thin and targeted
- 5. Migration windows with measurable risk
- 6. Versioned release notes that are testable
- 7. Keep deprecation warnings actionable

## 1) Track API as a compatibility contract

```haskell
-- GOOD
module MyLib.API
  ( Foo
  , bar
  , barCompat
  ) where

-- BAD
-- Exposing random internal helpers as stable public API
module MyLib.API where
import MyLib.Internal.FastPath -- re-exports too much
```

Why: a public module boundary defines the contract you must keep stable, not a convenience dump of internal names.

## 2) Additive changes first

```haskell
-- GOOD
addFoo :: Foo -> Foo

addFooWithEnv :: Foo -> Env -> Foo
```

```haskell
-- BAD
oldFoo :: Foo -> Foo
oldFoo _ = error "signature changed"
```

Why: adding capabilities preserves existing callers and gives migration time, while breaking signature churn shifts risk to all downstream packages.

## 3) Deprecate with timeline and intent

```haskell
-- GOOD
{-# DEPRECATED "Use parseConfigV2 with explicit schema" #-}
parseConfig :: ByteString -> Either Text Config

parseConfigV2 :: ByteString -> Either Text Config
```

```haskell
-- BAD
-- remove silently in minor release
parseConfig :: ByteString -> Either Text Config
parseConfig = parseConfigV2
```

Why: deprecation warnings without a planned removal window create ambiguity for users and downstream maintainers.

## 4) Keep shims thin and targeted

```haskell
-- GOOD
toV1Request :: V2Request -> V1Request
toV1Request r = V1Request { id = r.id, payload = r.payload }

parseLegacyRequest :: ByteString -> Either Text V2Request
```

```haskell
-- BAD
parseLegacyRequest :: ByteString -> Either Text V2Request
parseLegacyRequest = parseLegacyPath >> parseCurrentPath
```

Why: migration shims should isolate legacy behavior instead of masking current decoding flow with ambiguous fallbacks.

## 5) Migration windows with measurable risk

```txt
GOOD:
- Version 1.0: introduce new API + WARN
- Version 1.1: update docs and examples
- Version 1.2: remove old API, keep adapter tests

BAD:
- remove and document in a single bullet with no migration guidance
```

Why: explicit migration windows let consumers batch upgrades and lower coordinated breakage.

## 6) Versioned release notes as an API map

```markdown
### 2.4.0
- Added: `decodeStrict :: ByteString -> Either ConfigError Config`
- Deprecated: `decode` (use `decodeStrict`)
- Removed: none
```

```markdown
### 2.3.0
- Added: legacy field support for `Config` migration
```

Why: structured notes become the source of truth for compatibility checks and downstream release planning.

## 7) Keep deprecation warnings actionable

```haskell
{-# DEPRECATED "Use newConnect (renamed from connectNoTimeout) for explicit timeout args" #-}
connectNoTimeout :: Host -> IO Conn
connectNoTimeout = newConnect defaultConnectParams
```

```haskell
{-# DEPRECATED "Use new name" #-}
legacyFoo :: A -> B
```

Why: warnings should include replacement guidance; generic text slows down migrations and increases misuse.
