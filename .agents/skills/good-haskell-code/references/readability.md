# Readability Patterns (GHC 9.14.1)

For public/internal boundaries and re-export policy, also use `references/module-architecture.md`.

## Contents
- 1. Export lists define module API
- 2. Use explicit, qualified imports
- 3. Prefer domain names over abbreviations (modules/types/functions)
- 4. Always annotate exported bindings
- 5. Prefer record-dot for nested reads
- 6. Prefer focused record updates
- 7. Treat field selectors as API
- 8. Keep pure logic separate from effects
- 9. Prefer total functions
- 10. Use typed errors, not strings
- 11. Keep expressions direct, avoid clever point-free
- 12. Use deriving strategies for clarity
- 13. Use short type-scoped field names with modern records

## 1) Export lists define module API

```haskell
-- GOOD
module Billing.Invoice
  ( Invoice
  , InvoiceId
  , mkInvoice
  , invoiceTotal
  ) where
```

```haskell
-- BAD
module Billing.Invoice where
```

Why: explicit exports keep internals private and make the public surface reviewable.

## 2) Use explicit, qualified imports

```haskell
-- GOOD
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict qualified as Map
```

```haskell
-- BAD
import Data.Text
import Data.Map.Strict
```

Why: qualified imports make call sites unambiguous and avoid namespace drift.

## 3) Prefer domain names over abbreviations (for modules/types/functions)

```haskell
-- GOOD
fetchInvoiceByCustomerId :: CustomerId -> AppM (Either InvoiceError [Invoice])
```

```haskell
-- BAD
fInv :: CId -> M (Either E [Inv])
```

Why: long-lived code is read far more than written.

Note: this rule is about module/type/function naming. For record fields, prefer concise type-scoped names with modern record extensions (see section 13).

## 4) Always annotate exported bindings

```haskell
-- GOOD
buildMonthlyReport :: LocalTime -> NonEmpty Invoice -> Report
buildMonthlyReport cutoff invoices = ...
```

```haskell
-- BAD
buildMonthlyReport cutoff invoices = ...
```

Why: signatures are executable documentation and reduce accidental inference changes.

## 5) Prefer record-dot for nested reads

```haskell
-- GOOD
{-# LANGUAGE OverloadedRecordDot #-}

formatContact :: User -> Text
formatContact user =
  user.profile.displayName <> " <" <> user.profile.email <> ">"
```

```haskell
-- BAD
formatContact user =
  displayName (profile user) <> " <" <> email (profile user) <> ">"
```

Why: dot syntax removes visual noise in nested projections.

## 6) Prefer focused record updates

```haskell
-- GOOD
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

markPaid :: UTCTime -> Invoice -> Invoice
markPaid paidAt invoice = invoice{ status = Paid, paidAt = Just paidAt }
```

```haskell
-- BAD
markPaid paidAt invoice =
  invoice
    { status = if isOverdue invoice then Paid else status invoice
    , paidAt = Just paidAt
    }
```

Why: updates should be predictable and side-condition free unless required by domain rules.

## 7) Treat field selectors as API

```haskell
-- GOOD
{-# LANGUAGE NoFieldSelectors #-}
module Billing.Customer
  ( Customer
  , customerEmail
  ) where

data Customer = Customer { email :: Text, name :: Text }

customerEmail :: Customer -> Text
customerEmail customer = customer.email
```

```haskell
-- BAD
module Billing.Customer (Customer(..)) where
```

Why: exporting raw selectors commits you to field names as a stable public API.

## 8) Keep pure logic separate from effects

```haskell
-- GOOD
computeLateFee :: Day -> Invoice -> Money
computeLateFee today invoice = ...

applyLateFee :: Day -> InvoiceId -> AppM (Either BillingError Invoice)
applyLateFee today invoiceId = do
  invoice <- loadInvoice invoiceId
  pure (fmap (withFee (computeLateFee today)) invoice)
```

```haskell
-- BAD
applyLateFee :: Day -> InvoiceId -> AppM Invoice
applyLateFee today invoiceId = do
  invoice <- fromJust <$> loadInvoiceUnsafe invoiceId
  let fee = computeLateFee today invoice
  saveInvoice (withFee fee invoice)
```

Why: pure cores are easier to test and reason about than effectful monoliths.

## 9) Prefer total functions

```haskell
-- GOOD
headEither :: NonEmpty a -> a
headEither (x :| _) = x
```

```haskell
-- BAD
unsafeHead :: [a] -> a
unsafeHead = head
```

Why: total APIs remove hidden runtime failure paths.

## 10) Use typed errors, not strings

```haskell
-- GOOD
data ParseAmountError
  = EmptyInput
  | InvalidAmount Text
  deriving stock (Eq, Show)

parseAmount :: Text -> Either ParseAmountError Money
parseAmount input = ...
```

```haskell
-- BAD
parseAmount :: Text -> Either String Money
parseAmount input = ...
```

Why: typed errors are discoverable, exhaustively matchable, and refactor-safe.

## 11) Keep expressions direct, avoid clever point-free

```haskell
-- GOOD
invoiceIdsForCustomer :: CustomerId -> [Invoice] -> [InvoiceId]
invoiceIdsForCustomer customerId invoices =
  [ invoice.invoiceId | invoice <- invoices, invoice.customerId == customerId ]
```

```haskell
-- BAD
invoiceIdsForCustomer =
  map (.invoiceId) . filter . ((==) . (.customerId))
```

Why: a direct expression is often clearer than point-free gymnastics.

## 12) Use deriving strategies for clarity

```haskell
-- GOOD
newtype Email = Email Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)
```

```haskell
-- BAD
newtype Email = Email Text
  deriving (Eq, Ord, Show, IsString)
```

Why: explicit strategies make derivation intent obvious during reviews.

## 13) Use short type-scoped field names with modern records

When using modern record extensions, prefer short field names scoped by type (`id`, `adv`, `pos`, `bbox`) instead of long prefixed fields.

```haskell
-- GOOD
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

data Glyph = Glyph { id :: !Int, adv :: !Double }
data Run   = Run   { id :: !Int, adv :: !Double }

scaleAdvance :: Double -> Glyph -> Glyph
scaleAdvance k glyph = glyph { adv = glyph.adv * k }

glyphId :: Glyph -> Int
glyphId glyph = glyph.id
```

```haskell
-- BAD
data Glyph = Glyph
  { glyphIdentifier :: Int
  , glyphAdvanceInFontUnitsAlongX :: Double
  }

data Run = Run
  { runIdentifier :: Int
  , runAdvanceInFontUnitsAlongX :: Double
  }

scaleAdvance :: Double -> Glyph -> Glyph
scaleAdvance k glyph =
  glyph
    { glyphAdvanceInFontUnitsAlongX =
        glyphAdvanceInFontUnitsAlongX glyph * k
    }
```

Why: modern record ergonomics remove the old need for verbose prefixing and keep call sites compact.

```haskell
-- GOOD
module Domain.Glyph
  ( Glyph
  , scaleAdvance
  , glyphId
  ) where
```

```haskell
-- BAD
module Domain.Glyph (Glyph(..), scaleAdvance) where
```

Why: keep constructor/field details internal unless you explicitly want them in the public API.
