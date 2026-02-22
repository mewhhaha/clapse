# Unsafe Boundary Policy (GHC 9.14.1)

For FFI specifics, also use `references/unboxed-and-ffi.md`.

## Contents
- 1. Default to safe APIs and explicit boundaries
- 2. Restrict `unsafePerformIO` to audited singletons
- 3. Prefer `coerce` and newtypes over `unsafeCoerce`
- 4. Wrap low-level primitives behind safe helpers
- 5. Use unsafe FFI imports only for proven non-blocking calls
- 6. Isolate unsafe code in dedicated modules
- 7. Require audit checklist before merge
- 8. Test boundary behavior explicitly

## 1) Default to safe APIs and explicit boundaries

```haskell
-- GOOD
loadConfig :: FilePath -> IO Config
loadConfig = ...

buildService :: Config -> Service
buildService = ...
```

```haskell
-- BAD
loadConfigPure :: FilePath -> Config
loadConfigPure = unsafePerformIO . loadConfig
```

Why: explicit effects keep caller semantics predictable.

## 2) Restrict `unsafePerformIO` to audited singletons

```haskell
-- GOOD
{-# NOINLINE parserTable #-}
parserTable :: ParserTable
parserTable = unsafePerformIO loadParserTable
```

```haskell
-- BAD
parserTable :: ParserTable
parserTable = unsafePerformIO loadParserTable
```

Why: without `NOINLINE`, work can duplicate and violate single-initialization assumptions.

## 3) Prefer `coerce` and newtypes over `unsafeCoerce`

```haskell
-- GOOD
newtype UserId = UserId Int

userIdToInt :: UserId -> Int
userIdToInt = coerce
```

```haskell
-- BAD
userIdToInt :: UserId -> Int
userIdToInt = unsafeCoerce
```

Why: `coerce` is type-safe and compile-time checked; `unsafeCoerce` is not.

## 4) Wrap low-level primitives behind safe helpers

```haskell
-- GOOD
copyBytesSafe :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
copyBytesSafe dst src n
  | n < 0 = fail "negative length"
  | otherwise = copyBytes dst src n
```

```haskell
-- BAD
copyBytesUnsafe :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
copyBytesUnsafe dst src n = copyBytes dst src n
```

Why: wrappers are where you enforce preconditions and document invariants.

## 5) Use unsafe FFI imports only for proven non-blocking calls

```haskell
-- GOOD
foreign import ccall safe "sha256"
  c_sha256 :: Ptr Word8 -> CSize -> Ptr Word8 -> IO ()
```

```haskell
-- BAD
foreign import ccall unsafe "read_large_file"
  c_read_large_file :: Ptr Word8 -> CSize -> IO CInt
```

Why: blocking calls must use `safe` imports to avoid stalling the runtime scheduler.

## 6) Isolate unsafe code in dedicated modules

```haskell
-- GOOD
module App.Unsafe.Native
  ( decodeFastUnsafe
  ) where
```

```haskell
-- BAD
module App.Domain where
import Unsafe.Coerce
```

Why: containment makes audits, tests, and ownership clear.

## 7) Require audit checklist before merge

```txt
GOOD:
1. Unsafe usage has a short rationale comment.
2. Preconditions are checked or documented.
3. Module exports a safe wrapper API.
4. Equivalent safe path exists for verification/testing.
5. Reviewer confirms why safer alternatives are insufficient.
```

```txt
BAD:
1. Unsafe code added for style/convenience.
2. No rationale, tests, or ownership.
```

Why: unsafe code needs explicit risk accounting, not implicit trust.

## 8) Test boundary behavior explicitly

```haskell
-- GOOD
prop_safe_vs_unsafe_decode_agree :: ByteString -> Property
prop_safe_vs_unsafe_decode_agree bs =
  classify (BS.null bs) "empty" $
    decodeSafe bs === decodeUnsafeWrapped bs
```

```haskell
-- BAD
-- no differential tests around unsafe wrappers
```

Why: differential tests catch undefined-behavior regressions early.
