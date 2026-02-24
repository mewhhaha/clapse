module Clapse.Laws
  ( Name
  , ClassKind(..)
  , Law(..)
  , ClassDef(..)
  , mkClassDef
  , requiredMethodNames
  , requiredLawNames
  , parseClassKind
  ) where

type Name = String

data ClassKind
  = AddClass
  | SubClass
  | MulClass
  | DivClass
  | OrdClass
  | SliceClass
  | MonoidClass
  | FunctorClass
  | ApplicativeClass
  | MonadClass
  deriving (Eq, Show)

data Law expr = Law
  { lawName :: Name
  , lhs :: expr
  , rhs :: expr
  }
  deriving (Eq, Show)

data ClassDef expr = ClassDef
  { className :: Name
  , kind :: ClassKind
  , methods :: [Name]
  , laws :: [Law expr]
  }
  deriving (Eq, Show)

mkClassDef :: Name -> ClassKind -> [Name] -> [Law expr] -> Either String (ClassDef expr)
mkClassDef className kind methods laws = do
  case duplicates methods of
    [] -> Right ()
    d:_ -> Left ("duplicate class method: " <> d)

  case missingMethods kind methods of
    [] -> Right ()
    missing ->
      Left
        ( "missing required methods for "
            <> classKindLabel kind
            <> ": "
            <> commaSep missing
        )

  case unexpectedMethods kind methods of
    [] -> Right ()
    unexpected ->
      Left
        ( "unexpected methods for "
            <> classKindLabel kind
            <> ": "
            <> commaSep unexpected
        )

  case duplicates (map lawName laws) of
    [] -> Right ()
    d:_ -> Left ("duplicate class law: " <> d)

  case missingLaws kind laws of
    [] -> Right ()
    missing ->
      Left
        ( "missing required laws for "
            <> classKindLabel kind
            <> ": "
            <> commaSep missing
        )

  pure ClassDef {className = className, kind = kind, methods = methods, laws = laws}

requiredMethodNames :: ClassKind -> [Name]
requiredMethodNames classKind =
  case classKind of
    AddClass ->
      ["add"]
    SubClass ->
      ["sub"]
    MulClass ->
      ["mul"]
    DivClass ->
      ["div"]
    OrdClass ->
      ["lt", "le", "gt", "ge", "eq"]
    SliceClass ->
      ["slice_len", "slice_get_u8", "slice_set_u8"]
    MonoidClass ->
      ["empty", "append"]
    FunctorClass ->
      ["fmap", "id", "compose"]
    ApplicativeClass ->
      ["pure", "ap", "fmap"]
    MonadClass ->
      ["pure", "bind"]

requiredLawNames :: ClassKind -> [Name]
requiredLawNames classKind =
  case classKind of
    AddClass ->
      ["left_identity", "right_identity", "associativity"]
    SubClass ->
      ["right_identity", "self_inverse"]
    MulClass ->
      ["left_identity", "right_identity", "left_annihilation", "right_annihilation", "associativity"]
    DivClass ->
      ["right_identity"]
    OrdClass ->
      ["irreflexive_lt", "reflexive_le", "reflexive_ge", "antisymmetry", "duality"]
    SliceClass ->
      ["set_preserves_len", "get_after_set_same_index"]
    MonoidClass ->
      ["left_identity", "right_identity", "associativity"]
    FunctorClass ->
      ["identity", "composition"]
    ApplicativeClass ->
      ["identity", "homomorphism", "interchange", "composition"]
    MonadClass ->
      ["left_identity", "right_identity", "associativity"]

parseClassKind :: Name -> Maybe ClassKind
parseClassKind n =
  case n of
    "add" -> Just AddClass
    "sub" -> Just SubClass
    "mul" -> Just MulClass
    "div" -> Just DivClass
    "ord" -> Just OrdClass
    "slice" -> Just SliceClass
    "monoid" -> Just MonoidClass
    "functor" -> Just FunctorClass
    "applicative" -> Just ApplicativeClass
    "monad" -> Just MonadClass
    _ -> Nothing

missingMethods :: ClassKind -> [Name] -> [Name]
missingMethods classKind allMethods =
  let present = allMethods
   in filter (`notElem` present) (requiredMethodNames classKind)

unexpectedMethods :: ClassKind -> [Name] -> [Name]
unexpectedMethods classKind allMethods =
  let required = requiredMethodNames classKind
   in filter (`notElem` required) allMethods

missingLaws :: ClassKind -> [Law expr] -> [Name]
missingLaws classKind allLaws =
  let present = map lawName allLaws
   in filter (`notElem` present) (requiredLawNames classKind)

classKindLabel :: ClassKind -> Name
classKindLabel classKind =
  case classKind of
    AddClass -> "add"
    SubClass -> "sub"
    MulClass -> "mul"
    DivClass -> "div"
    OrdClass -> "ord"
    SliceClass -> "slice"
    MonoidClass -> "monoid"
    FunctorClass -> "functor"
    ApplicativeClass -> "applicative"
    MonadClass -> "monad"

duplicates :: [Name] -> [Name]
duplicates = reverse . go [] []
  where
    go :: [Name] -> [Name] -> [Name] -> [Name]
    go _seen dups [] = dups
    go seen dups (x:xs)
      | x `elem` seen && x `notElem` dups = go seen (x : dups) xs
      | x `elem` seen = go seen dups xs
      | otherwise = go (x : seen) dups xs

commaSep :: [String] -> String
commaSep [] = ""
commaSep [x] = x
commaSep (x:xs) = x <> ", " <> commaSep xs
