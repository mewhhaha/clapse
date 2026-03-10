# Type-Level Modeling with GADTs, DataKinds, TypeFamilies, and HLists (GHC 9.14.1)

## Contents
- 1. Use GADTs for typed state transitions
- 2. Use DataKinds for protocol phases and capabilities
- 3. Use TypeFamilies for compile-time shape computation
- 4. Use TypeLits for sized APIs
- 5. Use indexed/phantom types for capability safety
- 6. HList/extensible records tradeoffs
- 7. Adoption checklist

## 1) Use GADTs for typed state transitions

```haskell
-- GOOD
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

data ConnState = Closed | Open

data Conn :: ConnState -> Type where
  MkClosed :: Socket -> Conn 'Closed
  MkOpen   :: Socket -> Conn 'Open

openConn :: Conn 'Closed -> IO (Conn 'Open)
openConn (MkClosed s) = pure (MkOpen s)

sendPacket :: Conn 'Open -> ByteString -> IO ()
sendPacket (MkOpen s) payload = ...
```

```haskell
-- BAD
data ConnState = Closed | Open
data Conn = Conn Socket ConnState

sendPacket :: Conn -> ByteString -> IO ()
sendPacket (Conn s _) payload = ...
```

Why: typed states remove illegal operations at compile time.

## 2) Use DataKinds for protocol phases and capabilities

```haskell
-- GOOD
{-# LANGUAGE DataKinds #-}

data Phase = Handshake | Ready | Done

data Msg (from :: Phase) (to :: Phase) where
  AuthOk   :: Msg 'Handshake 'Ready
  SendData :: ByteString -> Msg 'Ready 'Ready
  Close    :: Msg 'Ready 'Done
```

```haskell
-- BAD
data Msg = AuthOk | SendData ByteString | Close
```

Why: phase-indexed messages make protocol order explicit and verifiable.

## 3) Use TypeFamilies for compile-time shape computation

```haskell
-- GOOD
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

type family OutRows (n :: Nat) :: Nat where
  OutRows n = n + 1

data Grid (rows :: Nat) (cols :: Nat) = Grid ...

padGrid :: Grid r c -> Grid (OutRows r) c
padGrid = ...
```

```haskell
-- BAD
data Grid = Grid Int Int ...
padGrid :: Grid -> Grid
```

Why: encode shape transforms in types when dimension bugs are expensive.

## 4) Use TypeLits for sized APIs

```haskell
-- GOOD
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

newtype Vec (n :: Nat) a = Vec [a]

vecLen :: forall n a. KnownNat n => Vec n a -> Integer
vecLen _ = natVal (Proxy @n)
```

```haskell
-- BAD
newtype Vec a = Vec [a]

headUnsafe :: Vec a -> a
headUnsafe (Vec xs) = head xs
```

Why: sized vectors make length invariants explicit and reducible by compiler checks.

## 5) Use indexed/phantom types for capability safety

```haskell
-- GOOD
data Mode = ReadOnly | ReadWrite
newtype Handle (m :: Mode) = Handle FD

readBytes :: Handle 'ReadOnly -> IO ByteString
readBytes = ...

writeBytes :: Handle 'ReadWrite -> ByteString -> IO ()
writeBytes = ...
```

```haskell
-- BAD
data Handle = Handle FD Bool
```

Why: booleans for permissions are fragile compared to indexed capabilities.

## 6) HList/extensible records tradeoffs

HLists are useful for narrow heterogenous plumbing and typed interpreter internals.
They are usually a poor default for domain models.

```haskell
-- GOOD
data HList :: [Type] -> Type where
  HNil  :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)
```

```haskell
-- BAD
-- replacing normal domain records with HList everywhere
type User = HList '[Int, Text, Bool, Day, ByteString]
```

Why: HLists reduce named-field ergonomics and increase error-message complexity for app-level models.

Prefer:
1. normal records for domain types;
2. HList/row-style structures only where open heterogeneity is core to the problem.

## 7) Adoption checklist

1. Start with one high-value invariant (state machine, capability, or size).
2. Keep type-level encoding local to a module boundary.
3. Expose ergonomic non-type-level wrappers for most callers.
4. Add compile-time examples + property tests around boundary conversions.
5. Stop when type errors become harder than the bug class you’re preventing.
