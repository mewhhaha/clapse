module Clapse.Modules
  ( compileEntryModuleToWasm
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (foldM)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))

import Clapse.CollapseIR (collapseAndVerifyModule)
import Clapse.Lowering (lowerModule)
import Clapse.Syntax
  ( CasePattern(..)
  , Expr(..)
  , Function(..)
  , Module(..)
  , CaseArm(..)
  , Name
  , parseModule
  )
import Clapse.Wasm (compileModuleToWasmWithExports)

data ParsedSource = ParsedSource
  { parsedBody :: String
  , parsedModule :: Maybe Name
  , parsedImports :: [Name]
  , parsedExports :: Maybe [Name]
  }

data RawModule = RawModule
  { rawModule :: Module
  , rawImports :: [Name]
  , rawExports :: Maybe [Name]
  }

data QualifiedModule = QualifiedModule
  { qualifiedFunctions :: [Function]
  , qualifiedExports :: M.Map Name Name
  }

compileEntryModuleToWasm :: FilePath -> IO (Either String BS.ByteString)
compileEntryModuleToWasm entryPath = do
  source <- readFile entryPath
  let entryDir = takeDirectory entryPath
  case parseSourceHeader source of
    Left err ->
      pure (Left err)
    Right entryHeader ->
      case parseModule (parsedBody entryHeader) of
        Left err ->
          pure (Left err)
        Right entryModule -> do
          rawModules <- loadRawModules entryDir (parsedImports entryHeader)
          case rawModules of
            Left err ->
              pure (Left err)
            Right loadedRaw ->
              case qualifyImportedModules loadedRaw (parsedImports entryHeader) of
                Left err ->
                  pure (Left err)
                Right qualifiedImports ->
                  case importEnvFromModules qualifiedImports (parsedImports entryHeader) of
                    Left err ->
                      pure (Left err)
                    Right entryImportEnv -> do
                      case resolveEntryExports (functions entryModule) (parsedExports entryHeader) of
                        Left err ->
                          pure (Left err)
                        Right exportNames -> do
                          let entryLocalNames =
                                M.fromList [(name fn, name fn) | fn <- functions entryModule]
                              entryQualified =
                                map
                                  (qualifyEntryFunction entryImportEnv entryLocalNames)
                                  (functions entryModule)
                              importOrder =
                                collectImportOrder loadedRaw (parsedImports entryHeader)
                              importedFunctions =
                                concatMap
                                  (\name -> qualifiedFunctions (qualifiedImports M.! name))
                                  importOrder
                              mergedFunctions = importedFunctions ++ entryQualified
                          case lowerModule (Module {signatures = [], functions = mergedFunctions}) >>= collapseAndVerifyModule of
                            Left err ->
                              pure (Left err)
                            Right collapsed ->
                              pure (compileModuleToWasmWithExports collapsed exportNames)

loadRawModules
  :: FilePath
  -> [Name]
  -> IO (Either String (M.Map Name RawModule))
loadRawModules root imports =
  loadRawModulesWithCache root M.empty S.empty imports

loadRawModulesWithCache
  :: FilePath
  -> M.Map Name RawModule
  -> S.Set Name
  -> [Name]
  -> IO (Either String (M.Map Name RawModule))
loadRawModulesWithCache _ loaded _ [] = pure (Right loaded)
loadRawModulesWithCache root loaded visiting (nextImport : remaining) = do
  loadedNow <- loadRawModule root loaded visiting nextImport
  case loadedNow of
    Left err ->
      pure (Left err)
    Right loadedOut ->
      loadRawModulesWithCache root loadedOut (S.insert nextImport visiting) remaining

loadRawModule
  :: FilePath
  -> M.Map Name RawModule
  -> S.Set Name
  -> Name
  -> IO (Either String (M.Map Name RawModule))
loadRawModule _ loaded _ name | M.member name loaded = pure (Right loaded)
loadRawModule _ _ visiting name | S.member name visiting = pure (Left ("cyclic module import detected: " <> name))
loadRawModule root loaded visiting name = do
  let sourcePath = moduleFilePath root name
  exists <- doesFileExist sourcePath
  if not exists
    then pure (Left ("missing module file: " <> sourcePath))
    else do
      source <- readFile sourcePath
      case parseSourceHeader source of
        Left err ->
          pure (Left err)
        Right parsed ->
          case parseModule (parsedBody parsed) of
            Left err ->
              pure (Left err)
            Right parsedModule ->
              case validateParsedModuleName name parsed of
                Left err ->
                  pure (Left err)
                Right _ -> do
                  let raw =
                        RawModule
                          { rawModule = parsedModule
                          , rawImports = parsedImports parsed
                          , rawExports = parsedExports parsed
                          }
                  let loadedWithCurrent = M.insert name raw loaded
                  loadRawModulesWithCache root loadedWithCurrent (S.insert name visiting) (parsedImports parsed)

validateParsedModuleName :: Name -> ParsedSource -> Either String ()
validateParsedModuleName expectedName parsed =
  case parsedModule parsed of
    Nothing ->
      Right ()
    Just declared
      | declared == expectedName -> Right ()
      | otherwise ->
          Left
            ( "module declaration mismatch: expected "
                <> expectedName
                <> ", declared "
                <> declared
            )

qualifyImportedModules
  :: M.Map Name RawModule
  -> [Name]
  -> Either String (M.Map Name QualifiedModule)
qualifyImportedModules loaded initialImports =
  foldM (qualifyOne loaded S.empty) M.empty initialImports
  where
    qualifyOne
      :: M.Map Name RawModule
      -> S.Set Name
      -> M.Map Name QualifiedModule
      -> Name
      -> Either String (M.Map Name QualifiedModule)
    qualifyOne loadedModules visiting cache moduleName
      | M.member moduleName cache = Right cache
      | S.member moduleName visiting = Left ("cyclic module import detected: " <> moduleName)
      | otherwise = do
          raw <- maybe (Left ("module import not loaded: " <> moduleName)) Right (M.lookup moduleName loadedModules)
          cacheWithImports <-
            foldM
              (qualifyOne loadedModules (S.insert moduleName visiting))
              cache
              (rawImports raw)
          let localNames =
                M.fromList
                  [ (rawName, qualifyModuleName moduleName rawName)
                  | rawName <- map name (functions (rawModule raw))
                  ]
          importEnv <- importEnvFromModules cacheWithImports (rawImports raw)
          exportMap <-
            case rawExports raw of
              Just exports ->
                case firstMissing (M.keys localNames) exports of
                  Just missing ->
                    Left ("unknown export in module " <> moduleName <> ": " <> missing)
                  Nothing ->
                    Right
                      (M.fromList
                        [ (exportName, qualifyModuleName moduleName exportName)
                        | exportName <- exports
                        ]
                      )
              Nothing ->
                Right localNames
          let qualifiedFunctions =
                map (renameFunction localNames importEnv) (functions (rawModule raw))
          Right (M.insert moduleName (QualifiedModule qualifiedFunctions exportMap) cacheWithImports)

importEnvFromModules
  :: M.Map Name QualifiedModule
  -> [Name]
  -> Either String (M.Map Name Name)
importEnvFromModules qualifiedModules imports =
  foldM addFromImport M.empty imports
  where
    addFromImport :: M.Map Name Name -> Name -> Either String (M.Map Name Name)
    addFromImport acc importName =
      case M.lookup importName qualifiedModules of
        Nothing ->
          Left ("module import not loaded: " <> importName)
        Just qModule ->
          foldM addAlias acc (M.toList (qualifiedExports qModule))

    addAlias :: M.Map Name Name -> (Name, Name) -> Either String (M.Map Name Name)
    addAlias acc (unqualifiedName, qualifiedName) =
      case M.lookup unqualifiedName acc of
        Just existing ->
          if existing == qualifiedName
            then Right acc
            else Left ("ambiguous import for " <> unqualifiedName)
        Nothing ->
          Right (M.insert unqualifiedName qualifiedName acc)

qualifyEntryFunction
  :: M.Map Name Name
  -> M.Map Name Name
  -> Function
  -> Function
qualifyEntryFunction importEnv localNames fn =
  fn
    { body = renameExpr (args fn) localNames importEnv (body fn)
    }

renameFunction
  :: M.Map Name Name
  -> M.Map Name Name
  -> Function
  -> Function
renameFunction localNames importEnv fn =
  fn
    { name = fromMaybeName (name fn) localNames
    , body = renameExpr (args fn) localNames importEnv (body fn)
    }

renameExpr
  :: [Name]
  -> M.Map Name Name
  -> M.Map Name Name
  -> Expr
  -> Expr
renameExpr bound localNames importEnv expr =
  case expr of
    Var name ->
      if name `elem` bound
        then Var name
        else
          case M.lookup name localNames of
            Just renamed -> Var renamed
            Nothing ->
              case M.lookup name importEnv of
                Just renamed -> Var renamed
                Nothing -> Var name
    IntLit n -> IntLit n
    StringLit s -> StringLit s
    CollectionLit values ->
      CollectionLit (map (renameExpr bound localNames importEnv) values)
    App lhs rhs ->
      App (renameExpr bound localNames importEnv lhs) (renameExpr bound localNames importEnv rhs)
    Lam name bodyExpr ->
      Lam name (renameExpr (name : bound) localNames importEnv bodyExpr)
    Case scruts arms ->
      Case
        (map (renameExpr bound localNames importEnv) scruts)
        (map (renameCaseArm bound localNames importEnv) arms)

renameCaseArm
  :: [Name]
  -> M.Map Name Name
  -> M.Map Name Name
  -> CaseArm
  -> CaseArm
renameCaseArm bound localNames importEnv arm =
  arm
    { armBody =
        renameExpr
          (bound ++ patternBoundNames (armPatterns arm))
          localNames
          importEnv
          (armBody arm)
    }

patternBoundNames :: [CasePattern] -> [Name]
patternBoundNames = concatMap
  ( \case
      PatWildcard -> []
      PatVar name -> [name]
      PatInt _ -> []
      PatConstructor _ _ names -> filter (/= "_") names
  )

resolveEntryExports
  :: [Function]
  -> Maybe [Name]
  -> Either String [Name]
resolveEntryExports entryFunctions maybeExports =
  case maybeExports of
    Nothing -> Right (map name entryFunctions)
    Just exportNames ->
      let functionNames = map name entryFunctions
       in case firstMissing functionNames exportNames of
            Just missing ->
              Left ("unknown export in entry module: " <> missing)
            Nothing -> Right exportNames

firstMissing :: [Name] -> [Name] -> Maybe Name
firstMissing _ [] = Nothing
firstMissing available (target : rest)
  | target `elem` available = firstMissing available rest
  | otherwise = Just target

collectImportOrder
  :: M.Map Name RawModule
  -> [Name]
  -> [Name]
collectImportOrder loaded imports =
  snd (foldl visit (S.empty, []) imports)
  where
    visit :: (S.Set Name, [Name]) -> Name -> (S.Set Name, [Name])
    visit (seen, order) name =
      if S.member name seen
        then (seen, order)
        else
          case M.lookup name loaded of
            Nothing ->
              (seen, order)
            Just raw ->
              let (seenAfterChildren, orderAfterChildren) =
                    foldl visit (S.insert name seen, order) (rawImports raw)
               in (seenAfterChildren, orderAfterChildren ++ [name])

parseSourceHeader :: String -> Either String ParsedSource
parseSourceHeader src =
  go 1 [] Nothing [] [] (lines src)
  where
    go
      :: Int
      -> [String]
      -> Maybe Name
      -> [Name]
      -> [Name]
      -> [String]
      -> Either String ParsedSource
    go _lineNo body moduleDecl imports exports [] =
      Right
        ParsedSource
          { parsedBody = unlines (reverse body)
          , parsedModule = moduleDecl
          , parsedImports = reverse imports
          , parsedExports = if null exports then Nothing else Just (reverse exports)
          }
    go lineNo body moduleDecl imports exports (rawLine : remainingLines) = do
      let nextLineNo = lineNo + 1
          stripped = stripComment rawLine
          cleaned = trim stripped
      if cleaned == ""
        then go nextLineNo ("" : body) moduleDecl imports exports remainingLines
        else if startsWithWhitespace rawLine
          then go nextLineNo (rawLine : body) moduleDecl imports exports remainingLines
          else
            case words cleaned of
              ["module", modName] -> do
                validateModulePath lineNo modName
                case moduleDecl of
                  Just _ ->
                    Left ("line " <> show lineNo <> ": duplicate module declaration")
                  Nothing ->
                    go nextLineNo ("" : body) (Just modName) imports exports remainingLines
              "module" : _ ->
                Left ("line " <> show lineNo <> ": module declaration must be `module <name>`")
              ["import", importName] -> do
                validateModulePath lineNo importName
                go nextLineNo ("" : body) moduleDecl (importName : imports) exports remainingLines
              "import" : _ ->
                Left ("line " <> show lineNo <> ": import directive must be `import <module>`")
              "export" : exportTail -> do
                exportNames <- parseExportLine lineNo (unwords exportTail)
                go nextLineNo ("" : body) moduleDecl imports (reverse exportNames ++ exports) remainingLines
              _ ->
                go nextLineNo (rawLine : body) moduleDecl imports exports remainingLines

parseExportLine :: Int -> String -> Either String [Name]
parseExportLine lineNo tailText = do
  let names = words (map commaToSpace tailText)
  if null names
    then Left ("line " <> show lineNo <> ": export directive must include at least one name")
    else do
      traverse_ (validateIdentifier lineNo) names
      Right names
  where
    commaToSpace ' ' = ' '
    commaToSpace ',' = ' '
    commaToSpace c = c

validateModulePath :: Int -> Name -> Either String ()
validateModulePath lineNo name =
  case filter (not . isValidPathSegment) (splitModulePath name) of
    [] -> Right ()
    _ -> Left ("line " <> show lineNo <> ": invalid module path: " <> name)
  where
    isValidPathSegment seg =
      case seg of
        [] -> False
        firstChar : restChars ->
          isIdentifierStart firstChar
            && all isIdentifierChar restChars

validateIdentifier :: Int -> Name -> Either String ()
validateIdentifier lineNo name
  | isValidIdentifier name = Right ()
  | otherwise = Left ("line " <> show lineNo <> ": invalid identifier: " <> name)
  where
    isValidIdentifier n =
      case n of
        [] -> False
        firstChar : restChars ->
          isIdentifierStart firstChar
            && all isIdentifierChar restChars

traverse_ :: (a -> Either e b) -> [a] -> Either e ()
traverse_ _ [] = Right ()
traverse_ f (x : xs) = do
  _ <- f x
  traverse_ f xs

moduleFilePath :: FilePath -> Name -> FilePath
moduleFilePath root moduleName =
  root </> intercalate "/" (splitModulePath moduleName) <.> "clapse"

splitModulePath :: String -> [String]
splitModulePath src = go [] src
  where
    go :: [String] -> String -> [String]
    go segments "" = reverse segments
    go segments rest =
      case break (== '.') rest of
        (segment, "") ->
          reverse (segment : segments)
        (segment, _dot : restSegments) ->
          go (segment : segments) restSegments

qualifyModuleName :: Name -> Name -> Name
qualifyModuleName moduleName name = moduleName <> "$" <> name

fromMaybeName :: Name -> M.Map Name Name -> Name
fromMaybeName defaultName map0 = fromMaybe defaultName (M.lookup defaultName map0)

isIdentifierStart :: Char -> Bool
isIdentifierStart c = isAsciiLower c || isAsciiUpper c || c == '_'

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isIdentifierStart c || isDigit c || c == '_' || c == '\''

startsWithWhitespace :: String -> Bool
startsWithWhitespace str =
  case str of
    c : _ -> isSpace c
    [] -> False

stripComment :: String -> String
stripComment [] = []
stripComment ('-' : '-' : _) = []
stripComment (c : cs) = c : stripComment cs

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
