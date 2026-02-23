module Clapse.AttributeManifest
  ( AttributeArgKind(..)
  , AttributeKind(..)
  , AttributeManifestEntry(..)
  , AttributeManifest
  , defaultAttributeManifest
  , lookupAttributeManifest
  , mergeAttributeManifest
  ) where

import Clapse.Syntax (Name)

data AttributeKind
  = AttributeBuiltin
  | AttributeCustom
  deriving (Eq, Show)

data AttributeArgKind
  = AttributeArgNone
  | AttributeArgInt
  | AttributeArgString
  | AttributeArgIdentifier
  | AttributeArgSingleToken
  deriving (Eq, Show)

data AttributeManifestEntry = AttributeManifestEntry
  { name :: Name
  , kind :: AttributeKind
  , argKind :: AttributeArgKind
  , summary :: String
  , details :: String
  }
  deriving (Eq, Show)

type AttributeManifest = [(Name, AttributeManifestEntry)]

defaultAttributeManifest :: AttributeManifest
defaultAttributeManifest =
  [ entry "memo" AttributeBuiltin AttributeArgInt "Memoize function calls." "WASM backend memoization. Current lowering supports unary function-family memoization."
  , entry "test" AttributeBuiltin AttributeArgString "Mark a test case." "Metadata-only tag for test tooling."
  , entry "bench" AttributeBuiltin AttributeArgString "Mark a benchmark case." "Metadata-only tag for benchmark tooling."
  ]
  where
    entry :: Name -> AttributeKind -> AttributeArgKind -> String -> String -> (Name, AttributeManifestEntry)
    entry entryName entryKind entryArgKind entrySummary entryDetails =
      ( entryName
      , AttributeManifestEntry
          { name = entryName
          , kind = entryKind
          , argKind = entryArgKind
          , summary = entrySummary
          , details = entryDetails
          }
      )

lookupAttributeManifest :: Name -> AttributeManifest -> Maybe AttributeManifestEntry
lookupAttributeManifest key manifest =
  lookup key manifest

mergeAttributeManifest :: AttributeManifest -> AttributeManifest -> AttributeManifest
mergeAttributeManifest base extra =
  foldr insert base extra
  where
    insert :: (Name, AttributeManifestEntry) -> AttributeManifest -> AttributeManifest
    insert (entryName, entryValue) acc =
      (entryName, entryValue) : filter (\(k, _) -> k /= entryName) acc
