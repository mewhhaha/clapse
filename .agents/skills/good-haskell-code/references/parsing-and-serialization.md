# Parsing and Serialization (Arbitrary Data and Save Files) (GHC 9.14.1)

For compile-time cache/codegen patterns, use `references/deriving-and-template-haskell.md`.
For long-term protocol and compatibility lifecycle policy, use `references/protocol-versioning-lifecycle.md`.

## Contents
- 1. Use a versioned envelope
- 2. Separate raw decode from validated domain
- 3. Use typed error taxonomy
- 4. Keep encoding deterministic
- 5. Design explicit migration paths
- 6. Parse untrusted input with hard bounds
- 7. Choose strict vs lazy bytestring intentionally
- 8. Write save files atomically
- 9. Test round-trip and compatibility
- 10. Binary vs JSON tradeoffs
- 11. Optional crypto/authentication layer

## 1) Use a versioned envelope

```haskell
-- GOOD
data SaveHeader = SaveHeader
  { magic      :: !Word32
  , version    :: !Word16
  , codec      :: !Word8
  , payloadLen :: !Word32
  , payloadCrc :: !Word32
  }
```

```haskell
-- BAD
-- file starts with payload directly, no metadata
decodeSave :: ByteString -> Either Text SaveState
decodeSave = ...
```

Why: envelope metadata enables early rejection, version routing, and integrity checks.

## 2) Separate raw decode from validated domain

```haskell
-- GOOD
data SaveRaw = SaveRaw
  { rawPlayerName :: !Text
  , rawLevel      :: !Int
  }

data SaveState = SaveState
  { playerName :: !PlayerName
  , level      :: !Level
  }

decodeAndValidate :: ByteString -> Either SaveError SaveState
decodeAndValidate bytes = do
  raw <- decodeRaw bytes
  validateRaw raw
```

```haskell
-- BAD
decodeSave :: ByteString -> Either SaveError SaveState
decodeSave bytes = do
  state <- decodeRaw bytes
  pure state
```

Why: decoded data is still untrusted; semantic validation belongs after syntactic parse.

## 3) Use typed error taxonomy

```haskell
-- GOOD
data SaveError
  = ErrBadMagic Word32
  | ErrUnsupportedVersion Word16
  | ErrPayloadTooLarge Word32
  | ErrChecksumMismatch Word32 Word32
  | ErrDecode Text
  | ErrInvalidDomain Text
  deriving stock (Eq, Show)
```

```haskell
-- BAD
decodeSave :: ByteString -> Either String SaveState
```

Why: typed errors support recovery policy, logging, and stable tests.

## 4) Keep encoding deterministic

```haskell
-- GOOD
encodeInventory :: [Item] -> Put
encodeInventory items = do
  let sorted = sortOn itemId items
  putWord16le (fromIntegral (length sorted))
  forM_ sorted putItem
```

```haskell
-- BAD
encodeInventory :: HashMap ItemId Item -> Put
encodeInventory items =
  forM_ (HashMap.toList items) (putItem . snd)
```

Why: deterministic ordering avoids flaky hashes, diffs, and migration tests.

## 5) Design explicit migration paths

```haskell
-- GOOD
data ParsedSave
  = ParsedV1 SaveV1
  | ParsedV2 SaveV2
  | ParsedV3 SaveV3

toCurrent :: ParsedSave -> SaveState
toCurrent = \case
  ParsedV1 v1 -> migrateV1toV3 v1
  ParsedV2 v2 -> migrateV2toV3 v2
  ParsedV3 v3 -> fromV3 v3
```

```haskell
-- BAD
decodeVersioned :: Word16 -> ByteString -> Either SaveError SaveState
decodeVersioned 3 bytes = decodeV3 bytes
decodeVersioned _ _ = Left (ErrDecode "unsupported")
```

Why: explicit migration preserves old saves and prevents accidental data loss.

## 6) Parse untrusted input with hard bounds

```haskell
-- GOOD
maxPayloadBytes :: Word32
maxPayloadBytes = 32 * 1024 * 1024

parsePayload :: Get ByteString
parsePayload = do
  n <- getWord32le
  when (n > maxPayloadBytes) $
    fail "payload too large"
  getByteString (fromIntegral n)
```

```haskell
-- BAD
parsePayload :: Get ByteString
parsePayload = do
  n <- getWord32le
  getByteString (fromIntegral n)
```

Why: attacker-controlled lengths can trigger huge allocations/OOM without bounds.

## 7) Choose strict vs lazy bytestring intentionally

```haskell
-- GOOD
-- Small bounded save files
readSaveFile :: FilePath -> IO ByteString
readSaveFile = BS.readFile
```

```haskell
-- GOOD
-- Very large archives/streaming payloads
readReplayArchive :: FilePath -> IO LBS.ByteString
readReplayArchive = LBS.readFile
```

```haskell
-- BAD
readSaveFile :: FilePath -> IO ByteString
readSaveFile path = LBS.toStrict <$> LBS.readFile path
```

Why: avoid lazy-indirection costs when strict bounded input is enough.

## 8) Write save files atomically

```haskell
-- GOOD
writeSaveAtomic :: FilePath -> ByteString -> IO ()
writeSaveAtomic path bytes =
  withBinaryFile (path <> ".tmp") WriteMode $ \h -> do
    BS.hPut h bytes
    hFlush h
  >> renameFile (path <> ".tmp") path
```

```haskell
-- BAD
writeSave :: FilePath -> ByteString -> IO ()
writeSave = BS.writeFile
```

Why: atomic replace avoids partially written saves on crash/power loss.

## 9) Test round-trip and compatibility

```haskell
-- GOOD
prop_roundTrip :: SaveState -> Property
prop_roundTrip state =
  decodeSave (encodeSave state) === Right (normalizeSave state)
```

```haskell
-- GOOD
test_decode_v1_golden :: Assertion
test_decode_v1_golden = do
  bytes <- BS.readFile "test/golden/save-v1.bin"
  decodeSave bytes @?= Right expectedCurrentState
```

```haskell
-- BAD
-- only test current encoder + current decoder together
```

Why: round-trip catches encoder/decoder drift; golden fixtures catch compatibility breaks.

## 10) Binary vs JSON tradeoffs

```txt
GOOD:
- Binary for frequent saves/checkpoints (smaller, faster, more stable layout).
- JSON for tooling/interchange/debug where readability matters.
- If using JSON for saves, fix field names/order/precision policy and version envelope anyway.
```

```txt
BAD:
- Use ad-hoc JSON with optional fields and no versioning for long-lived save files.
```

Why: readability alone is not enough for durable persistence formats.

## 11) Optional crypto/authentication layer

Use authentication (MAC/signature) when tamper detection matters.
Use encryption only if confidentiality is required.

```haskell
-- GOOD
data SaveEnvelope = SaveEnvelope
  { header    :: !SaveHeader
  , ciphertextOrPayload :: !ByteString
  , macTag    :: !ByteString
  }

loadAndVerify :: SecretKey -> ByteString -> Either SaveError SaveState
loadAndVerify key bytes = do
  env <- decodeEnvelope bytes
  verifyMac key env.ciphertextOrPayload env.macTag
  decodeAndValidate env.ciphertextOrPayload
```

```haskell
-- BAD
-- checksum only protects against corruption, not malicious tampering
loadSave :: ByteString -> Either SaveError SaveState
loadSave bytes = do
  env <- decodeEnvelope bytes
  verifyCrc env
  decodeAndValidate env.payload
```

Why: CRC detects random corruption; it does not authenticate data origin.

Key points:
1. Version the crypto envelope too (algorithm, key-id, nonce/iv policy).
2. Verify before decode when using authenticated encryption.
3. Separate key-management policy from codec logic.
4. Keep deterministic non-secret fields explicit for debugging/migration.
