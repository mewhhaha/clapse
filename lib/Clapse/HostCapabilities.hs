module Clapse.HostCapabilities
  ( isHostImportModule
  , hostBuiltinsForImportModule
  , hostBuiltinsFromImports
  , hostBuiltinArity
  , buildHostBuiltinImportSigs
  ) where

import Data.List (isPrefixOf)

import Clapse.Syntax (Name)

hostCapabilityCatalog :: [(Name, [(Name, Int)])]
hostCapabilityCatalog =
  [ ("host.io", [("read_file", 1)])
  , ("host.time", [("unix_time_ms", 1)])
  ]

isHostImportModule :: Name -> Bool
isHostImportModule moduleName = "host." `isPrefixOf` moduleName

hostBuiltinsForImportModule :: Name -> [Name]
hostBuiltinsForImportModule moduleName =
  case lookup moduleName hostCapabilityCatalog of
    Nothing -> []
    Just builtins -> map fst builtins

hostBuiltinsFromImports :: [Name] -> [Name]
hostBuiltinsFromImports imports =
  dedupeNames
    ( concatMap hostBuiltinsForImportModule imports
    )

hostBuiltinArity :: Name -> Maybe Int
hostBuiltinArity builtinName =
  lookup builtinName (concatMap snd hostCapabilityCatalog)

buildHostBuiltinImportSigs :: [Name] -> Either String [(Name, (Int, Int))]
buildHostBuiltinImportSigs hostBuiltins = do
  withArities <- traverse withArity (dedupeNames hostBuiltins)
  pure (zipWith addIx [0 :: Int ..] withArities)
  where
    withArity :: Name -> Either String (Name, Int)
    withArity builtinName =
      case hostBuiltinArity builtinName of
        Nothing ->
          Left ("wasm backend: unknown host builtin capability: " <> builtinName)
        Just arityN ->
          Right (builtinName, arityN)

    addIx :: Int -> (Name, Int) -> (Name, (Int, Int))
    addIx ix (builtinName, arityN) = (builtinName, (ix, arityN))

dedupeNames :: [Name] -> [Name]
dedupeNames = reverse . foldl step []
  where
    step :: [Name] -> Name -> [Name]
    step acc name0
      | name0 `elem` acc = acc
      | otherwise = name0 : acc
