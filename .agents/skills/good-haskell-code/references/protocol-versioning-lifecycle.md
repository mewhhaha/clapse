# Protocol Versioning Lifecycle (GHC 9.14.1)

## Contents
- 1. Version every wire format explicitly
- 2. Separate schema, compatibility, and migration layers
- 3. Classify changes as additive or breaking
- 4. Define forward and backward compatibility
- 5. Keep migration between protocol generations explicit
- 6. Use schema tests as first-class assets
- 7. Validate cross-version behavior in CI

## 1) Version every wire format explicitly

```haskell
-- GOOD
data PacketEnvelope = PacketEnvelope
  { protocolVersion :: !Word16
  , schemaHash      :: !Word32
  , payload         :: !ByteString
  }
```

```haskell
-- BAD
decodePacket :: ByteString -> Either Text Packet
decodePacket = parseNoHeader
```

Why: version fields are mandatory for future upgrade safety and for clear failure diagnostics.

## 2) Separate schema, compatibility, and migration layers

```haskell
data WireV1 = WireV1 { ... }
data WireV2 = WireV2 { ... }

data ProtocolMessage
  = P1 WireV1
  | P2 WireV2

migrateV1toV2 :: WireV1 -> WireV2
```

```haskell
-- BAD
decodePacket :: Word16 -> ByteString -> Either Text WireV2
decodePacket v = if v < 2 then fail "old version unsupported" else ...
```

Why: parsing, compatibility checks, and migration should be independently testable and auditable.

## 3) Classify changes as additive or breaking

```txt
GOOD:
- Add field with default.
- Add new optional operation.
- Add new message type under unknown-mode handling.
  
BAD:
- Rename serialized field.
- Change enum numeric value semantics.
- Change required field type.
```

Why: explicit taxonomy makes review and release decisions traceable instead of ad hoc.

## 4) Define forward and backward compatibility

```haskell
-- GOOD
decodeUnknown :: Schema -> ByteString -> Either UnknownFieldError DecodedMessage
decodeUnknown schema bs = ...
```

```haskell
-- BAD
decodeMessage schema bs = parseStrict schema bs -- no tolerance path
```

Why: a protocol should reject malformed inputs while still handling unknown future extensions predictably.

## 5) Keep migration between protocol generations explicit

```haskell
migrateMessage :: DecodedMessage -> Either MigrationError CanonicalMessage
migrateMessage = \case
  FromV1 v1 -> Right (toCanonical v1)
  FromV2 v2 -> Right (toCanonical v2)
```

```haskell
-- BAD
-- in-place mutation of canonical struct during parse
parseAndMutate :: ByteString -> CanonicalMessage
```

Why: one-way, explicit migrations are easier to reason about than parser side effects and implicit branching.

## 6) Use schema tests as first-class assets

```haskell
prop_decode_roundtrip :: PacketEnvelope -> Bool
prop_decode_roundtrip env = encode . normalize . decode . encode === Right (normalize env)
```

```txt
BAD:
- only test current producer/consumer pair and skip old versions
- no golden fixtures for historical protocol versions
```

Why: protocol evolution regressions often appear at old-version edges, not current edges.

## 7) Validate cross-version behavior in CI

```txt
GOOD matrix:
- parser v1 reads v1-v3 payloads
- parser v2 reads v1-v2 payloads
- parser v3 reads v2-v3 payloads
- round-trip canonicalization from old to current
```

```txt
BAD:
- run a single end-to-end test against latest artifacts only
```

Why: CI matrices catch breakages in both forward and backward compatibility before release.
