# Invariant Patterns (GHC 9.14.1)

For save-file schema and migration invariants, also use `references/parsing-and-serialization.md`.

## Contents
- 0. Identify invariants that simplify code
- 1. Prefer newtypes over aliases for domain values
- 2. Use smart constructors for validation
- 3. Use NonEmpty for guaranteed presence
- 4. Replace boolean state flags with sum types
- 5. Encode workflow state in types
- 6. Use phantom types for unit/domain separation
- 7. Avoid nullable field combinations
- 8. Return typed domain errors
- 9. Hide constructors when invariants matter
- 10. Separate unvalidated and validated data
- 11. Keep illegal transitions untypeable
- 12. Prefer total pattern matches

## 0) Identify invariants that simplify code

Use invariants as a simplification tool, not just a safety mechanism.

Workflow:
1. Find repeated runtime checks (`null`, bounds checks, `Bool` flags, `String`ly-typed identifiers).
2. Ask which checks are really domain guarantees.
3. Encode those guarantees in types (`newtype`, ADT, phase type, `NonEmpty`).
4. Validate once at boundaries, then remove redundant checks from core logic.

```haskell
-- GOOD
newtype PxRange = PxRange Double

mkPxRange :: Double -> Either Text PxRange
mkPxRange x
  | x > 0 = Right (PxRange x)
  | otherwise = Left "PxRange must be > 0"

renderGlyph :: PxRange -> Glyph -> Raster
renderGlyph pxRange glyph = ...
```

```haskell
-- BAD
type PxRange = Double

renderGlyph :: PxRange -> Glyph -> Either Text Raster
renderGlyph pxRange glyph
  | pxRange <= 0 = Left "bad pxRange"
  | otherwise = ...
```

Why: move validation to construction to keep downstream code branch-free.

```haskell
-- GOOD
data BuildState = Unnormalized | Normalized

data Contours state = Contours
  { values :: NonEmpty Contour
  }

normalizeContours :: Contours Unnormalized -> Contours Normalized
normalizeContours = ...
```

```haskell
-- BAD
data Contours = Contours
  { values :: [Contour]
  , normalized :: Bool
  }
```

Why: phase-typed data removes invalid state combinations and simplifies call flow.

```haskell
-- GOOD
data EdgeStats = EdgeStats
  { edgeCount :: !Int
  , totalLen  :: !Double
  }
```

```haskell
-- BAD
data EdgeStats = EdgeStats
  { edgeCount :: Maybe Int
  , totalLen  :: Maybe Double
  }
```

Why: if values are always present after construction, model them as always present.

## 1) Prefer newtypes over aliases for domain values

```haskell
-- GOOD
newtype Email = Email Text
  deriving stock (Eq, Ord, Show)
```

```haskell
-- BAD
type Email = Text
```

Why: newtypes prevent accidental mixups and enable local instances safely.

## 2) Use smart constructors for validation

```haskell
-- GOOD
mkEmail :: Text -> Either EmailError Email
mkEmail raw
  | "@" `Text.isInfixOf` raw = Right (Email raw)
  | otherwise = Left (InvalidEmail raw)
```

```haskell
-- BAD
mkEmail :: Text -> Email
mkEmail = Email
```

Why: enforce domain rules once at construction boundaries.

## 3) Use NonEmpty for guaranteed presence

```haskell
-- GOOD
createInvoice :: CustomerId -> NonEmpty LineItem -> Invoice
createInvoice customerId items = ...
```

```haskell
-- BAD
createInvoice :: CustomerId -> [LineItem] -> Invoice
createInvoice customerId items = ...
```

Why: if emptiness is invalid, encode that in the type.

## 4) Replace boolean state flags with sum types

```haskell
-- GOOD
data InvoiceStatus = Draft | Issued | Paid | Voided
```

```haskell
-- BAD
data InvoiceStatus = InvoiceStatus
  { isDraft :: Bool
  , isPaid  :: Bool
  }
```

Why: booleans allow contradictory states and ambiguous meaning.

## 5) Encode workflow state in types

```haskell
-- GOOD
data Draft
data Finalized

data Quote phase = Quote
  { quoteId :: QuoteId
  , lines   :: NonEmpty LineItem
  }

finalize :: Quote Draft -> Quote Finalized
finalize = ...
```

```haskell
-- BAD
data Quote = Quote
  { quoteId   :: QuoteId
  , lines     :: [LineItem]
  , finalized :: Bool
  }
```

Why: typed phases remove entire classes of illegal transitions.

## 6) Use phantom types for unit/domain separation

```haskell
-- GOOD
newtype Quantity unit = Quantity Int
newtype Kg
newtype Lb

addKg :: Quantity Kg -> Quantity Kg -> Quantity Kg
addKg (Quantity a) (Quantity b) = Quantity (a + b)
```

```haskell
-- BAD
type Kilograms = Int
type Pounds = Int
addMass :: Int -> Int -> Int
```

Why: phantom parameters make cross-unit mistakes unrepresentable.

## 7) Avoid nullable field combinations

```haskell
-- GOOD
data PaymentMethod
  = Card CardInfo
  | BankTransfer BankInfo
  | Cash
```

```haskell
-- BAD
data PaymentMethod = PaymentMethod
  { cardInfo        :: Maybe CardInfo
  , bankTransferInfo :: Maybe BankInfo
  }
```

Why: sum types encode valid alternatives without invalid combinations.

## 8) Return typed domain errors

```haskell
-- GOOD
data CheckoutError
  = EmptyCart
  | OutOfStock ItemId
  | PaymentDeclined DeclineCode
  deriving stock (Eq, Show)

checkout :: Cart -> Either CheckoutError Receipt
checkout = ...
```

```haskell
-- BAD
checkout :: Cart -> Either Text Receipt
checkout = ...
```

Why: typed errors support exhaustive handling and stable refactors.

## 9) Hide constructors when invariants matter

```haskell
-- GOOD
module Domain.Username
  ( Username
  , mkUsername
  , unUsername
  ) where
```

```haskell
-- BAD
module Domain.Username (Username(..), mkUsername) where
```

Why: hidden constructors force callers through validation paths.

## 10) Separate unvalidated and validated data

```haskell
-- GOOD
data RawUser = RawUser
  { rawEmail :: Text
  , rawName  :: Text
  }

data User = User
  { email :: Email
  , name  :: NonEmptyText
  }

validateUser :: RawUser -> Either UserValidationError User
validateUser = ...
```

```haskell
-- BAD
data User = User
  { email :: Text
  , name  :: Text
  }
```

Why: parsing and validation are distinct steps with distinct failure modes.

## 11) Keep illegal transitions untypeable

```haskell
-- GOOD
cancel :: Quote Draft -> Quote Draft
cancel = ...

-- no function: cancel :: Quote Finalized -> ...
```

```haskell
-- BAD
cancel :: Quote -> Quote
cancel quote = quote { finalized = False }
```

Why: transition rules belong in the type surface, not comments.

## 12) Prefer total pattern matches

```haskell
-- GOOD
statusLabel :: InvoiceStatus -> Text
statusLabel = \case
  Draft  -> "draft"
  Issued -> "issued"
  Paid   -> "paid"
  Voided -> "voided"
```

```haskell
-- BAD
statusLabel :: InvoiceStatus -> Text
statusLabel Draft = "draft"
statusLabel Paid = "paid"
```

Why: incomplete matches are latent crashes and maintenance traps.
