module Clapse.Modules
  ( ExportApi(..)
  , CompileArtifact(..)
  , CompileDebugArtifact(..)
  , compileEntryModule
  , compileEntryModuleDebug
  , compileEntryModuleToWasm
  , renderTypeScriptBindings
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))

import qualified Clapse.CollapseIR as CIR
import Clapse.HostCapabilities
  ( hostBuiltinsFromImports
  , isHostImportModule
  )
import Clapse.Lowering (FlatFunction, lowerModule)
import Clapse.Syntax
  ( CasePattern(..)
  , ConstructorInfo(..)
  , Expr(..)
  , FunctionAttribute(..)
  , FunctionAttributeValue(..)
  , Function(..)
  , Module(..)
  , CaseArm(..)
  , Name
  , parseModuleWithConstructorImportsInfo
  )
import Clapse.Wasm (compileModuleToWasmWithExportsAndMemoHintsAndHostBuiltins)
import Clapse.TypeInfo
  ( FunctionTypeInfo
  , inferModuleTypes
  )

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
  , rawConstructors :: M.Map Name ConstructorInfo
  }

data QualifiedModule = QualifiedModule
  { qualifiedFunctions :: [Function]
  , qualifiedExports :: M.Map Name Name
  , qualifiedConstructorExports :: M.Map Name ConstructorInfo
  }

data ExportApi = ExportApi
  { exportName :: Name
  , exportArity :: Int
  }
  deriving (Eq, Show)

data CompileArtifact = CompileArtifact
  { artifactWasm :: BS.ByteString
  , artifactExports :: [ExportApi]
  }
  deriving (Eq, Show)

data CompileDebugArtifact = CompileDebugArtifact
  { debugMergedModule :: Module
  , debugTypeInfo :: Maybe [FunctionTypeInfo]
  , debugTypeInfoError :: Maybe String
  , debugLowered :: [FlatFunction]
  , debugCollapsed :: [CIR.CollapsedFunction]
  , debugExports :: [ExportApi]
  , debugWasm :: BS.ByteString
  }
  deriving (Eq, Show)

compileEntryModule :: FilePath -> IO (Either String CompileArtifact)
compileEntryModule entryPath = do
  result <- compileEntryModuleDebug entryPath
  pure
    ( fmap
        ( \dbg ->
            CompileArtifact
              { artifactWasm = debugWasm dbg
              , artifactExports = debugExports dbg
              }
        )
        result
    )

compileEntryModuleDebug :: FilePath -> IO (Either String CompileDebugArtifact)
compileEntryModuleDebug entryPath = do
  source <- readFile entryPath
  let entryDir = takeDirectory entryPath
  case parseSourceHeader source of
    Left err ->
      pure (Left err)
    Right entryHeader -> do
      let sourceImports = sourceModuleImports (parsedImports entryHeader)
          hostBuiltins = hostBuiltinsFromImports (parsedImports entryHeader)
      rawModules <- loadRawModules entryDir sourceImports
      case rawModules of
        Left err ->
          pure (Left err)
        Right loadedRaw ->
          case qualifyImportedModules loadedRaw sourceImports of
            Left err ->
              pure (Left err)
            Right qualifiedImports ->
              case constructorEnvFromModules qualifiedImports sourceImports of
                Left err ->
                  pure (Left err)
                Right importedConstructors ->
                  case parseModuleWithConstructorImportsInfo (M.toList importedConstructors) (parsedBody entryHeader) of
                    Left err ->
                      pure (Left err)
                    Right (entryModule, _entryConstructors) ->
                      case importEnvFromModules qualifiedImports sourceImports of
                        Left err ->
                          pure (Left err)
                        Right entryImportEnv ->
                          case resolveEntryExports (functions entryModule) (parsedExports entryHeader) of
                            Left err ->
                              pure (Left err)
                            Right exportNames -> do
                              let entryLocalNames =
                                    M.fromList [(name fn, name fn) | fn <- functions entryModule]
                                  entryModuleName = fromMaybe "__entry" (parsedModule entryHeader)
                                  entryQualified =
                                    map
                                      (qualifyEntryFunction entryModuleName entryImportEnv entryLocalNames)
                                      (functions entryModule)
                                  importOrder =
                                    collectImportOrder loadedRaw sourceImports
                                  importedFunctions =
                                    concatMap
                                      (\name -> qualifiedFunctions (qualifiedImports M.! name))
                                      importOrder
                                  mergedFunctions = importedFunctions ++ entryQualified
                                  mergedModule = Module {signatures = [], functions = mergedFunctions}
                                  memoHints = collectMemoHints mergedFunctions
                                  (typeInfo, typeInfoErr) =
                                    case inferModuleTypes mergedModule of
                                      Left inferErr -> (Nothing, Just inferErr)
                                      Right infos -> (Just infos, Nothing)
                              case lowerModule mergedModule of
                                Left lowerErr ->
                                  pure (Left lowerErr)
                                Right lowered ->
                                  case CIR.collapseAndVerifyModule lowered of
                                    Left collapseErr ->
                                      pure (Left collapseErr)
                                    Right collapsed ->
                                      case collectExportApi collapsed exportNames of
                                        Left exportErr ->
                                          pure (Left exportErr)
                                        Right api ->
                                          case compileModuleToWasmWithExportsAndMemoHintsAndHostBuiltins collapsed exportNames memoHints hostBuiltins of
                                            Left wasmErr ->
                                              pure (Left wasmErr)
                                            Right wasmBytes ->
                                              pure
                                                ( Right
                                                    CompileDebugArtifact
                                                      { debugMergedModule = mergedModule
                                                      , debugTypeInfo = typeInfo
                                                      , debugTypeInfoError = typeInfoErr
                                                      , debugLowered = lowered
                                                      , debugCollapsed = collapsed
                                                      , debugExports = api
                                                      , debugWasm = wasmBytes
                                                      }
                                                )

compileEntryModuleToWasm :: FilePath -> IO (Either String BS.ByteString)
compileEntryModuleToWasm entryPath = do
  result <- compileEntryModule entryPath
  pure (fmap artifactWasm result)

renderTypeScriptBindings :: [ExportApi] -> String
renderTypeScriptBindings exports =
  unlines
    ( [ "/* This file is auto-generated by `clapse compile`. */"
      , "/* ClapseValue uses the wasm runtime encoding (tagged ints or heap handles). */"
      , "export type ClapseValue = number;"
      , ""
      , "export interface ClapseWasmExports {"
      , "  readonly \"__memory\": WebAssembly.Memory;"
      , "  readonly \"__table\": WebAssembly.Table;"
      ]
        <> map renderExportFunction exports
        <> [ "}"
           , ""
           , "export interface ClapseExportArities {"
           ]
        <> map renderExportArity exports
        <> [ "}" ]
    )
  where
    renderExportFunction :: ExportApi -> String
    renderExportFunction exportFn =
      "  readonly "
        <> quoteTs (exportName exportFn)
        <> ": "
        <> functionType (exportArity exportFn)
        <> ";"

    renderExportArity :: ExportApi -> String
    renderExportArity exportFn =
      "  readonly "
        <> quoteTs (exportName exportFn)
        <> ": "
        <> show (exportArity exportFn)
        <> ";"

    functionType :: Int -> String
    functionType arityN =
      "(" <> intercalate ", " (map argDecl [0 .. arityN - 1]) <> ") => ClapseValue"

    argDecl :: Int -> String
    argDecl idx = "arg" <> show idx <> ": ClapseValue"

    quoteTs :: String -> String
    quoteTs src = "\"" <> concatMap escape src <> "\""
      where
        escape :: Char -> String
        escape c =
          case c of
            '"' -> "\\\""
            '\\' -> "\\\\"
            '\n' -> "\\n"
            '\r' -> "\\r"
            '\t' -> "\\t"
            _ -> [c]

loadRawModules
  :: FilePath
  -> [Name]
  -> IO (Either String (M.Map Name RawModule))
loadRawModules root imports =
  loadRawModulesWithCache root M.empty S.empty (sourceModuleImports imports)

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
          case validateParsedModuleName name parsed of
            Left err ->
              pure (Left err)
            Right _ -> do
              let imports = sourceModuleImports (parsedImports parsed)
              loadedDepsEither <- loadRawModulesWithCache root loaded (S.insert name visiting) imports
              case loadedDepsEither of
                Left err ->
                  pure (Left err)
                Right loadedWithDeps ->
                  case rawConstructorEnvFromModules loadedWithDeps imports of
                    Left err ->
                      pure (Left err)
                    Right importedConstructors ->
                      case parseModuleWithConstructorImportsInfo (M.toList importedConstructors) (parsedBody parsed) of
                        Left err ->
                          pure (Left err)
                        Right (parsedModule, localConstructors) -> do
                          let raw =
                                RawModule
                                  { rawModule = parsedModule
                                  , rawImports = parsedImports parsed
                                  , rawExports = parsedExports parsed
                                  , rawConstructors = M.fromList localConstructors
                                  }
                          pure (Right (M.insert name raw loadedWithDeps))

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
              localConstructors =
                M.map (qualifyCtorInfo moduleName) (rawConstructors raw)
          importEnv <- importEnvFromModules cacheWithImports (rawImports raw)
          exportMap <-
            case rawExports raw of
              Just exports ->
                case firstMissing (M.keys localNames ++ M.keys localConstructors) exports of
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
          let exportedConstructors =
                case rawExports raw of
                  Nothing ->
                    localConstructors
                  Just exports ->
                    M.filterWithKey (\ctorName _ -> ctorName `elem` exports) localConstructors
          let qualifiedFunctions =
                map (renameFunction moduleName localNames importEnv) (functions (rawModule raw))
          Right (M.insert moduleName (QualifiedModule qualifiedFunctions exportMap exportedConstructors) cacheWithImports)

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

constructorEnvFromModules
  :: M.Map Name QualifiedModule
  -> [Name]
  -> Either String (M.Map Name ConstructorInfo)
constructorEnvFromModules qualifiedModules imports =
  foldM addFromImport M.empty imports
  where
    addFromImport :: M.Map Name ConstructorInfo -> Name -> Either String (M.Map Name ConstructorInfo)
    addFromImport acc importName =
      case M.lookup importName qualifiedModules of
        Nothing ->
          Left ("module import not loaded: " <> importName)
        Just qModule ->
          foldM addAlias acc (M.toList (qualifiedConstructorExports qModule))

    addAlias :: M.Map Name ConstructorInfo -> (Name, ConstructorInfo) -> Either String (M.Map Name ConstructorInfo)
    addAlias acc (ctorName, ctorInfo) =
      case M.lookup ctorName acc of
        Just _ ->
          Left ("ambiguous constructor import for " <> ctorName)
        Nothing ->
          Right (M.insert ctorName ctorInfo acc)

rawConstructorEnvFromModules
  :: M.Map Name RawModule
  -> [Name]
  -> Either String (M.Map Name ConstructorInfo)
rawConstructorEnvFromModules loaded imports =
  foldM addFromImport M.empty imports
  where
    addFromImport :: M.Map Name ConstructorInfo -> Name -> Either String (M.Map Name ConstructorInfo)
    addFromImport acc importName =
      case M.lookup importName loaded of
        Nothing ->
          Left ("module import not loaded: " <> importName)
        Just raw ->
          let exportedConstructors = rawExportedConstructors importName raw
           in foldM addAlias acc (M.toList exportedConstructors)

    addAlias :: M.Map Name ConstructorInfo -> (Name, ConstructorInfo) -> Either String (M.Map Name ConstructorInfo)
    addAlias acc (ctorName, ctorInfo) =
      case M.lookup ctorName acc of
        Just _ ->
          Left ("ambiguous constructor import for " <> ctorName)
        Nothing ->
          Right (M.insert ctorName ctorInfo acc)

rawExportedConstructors :: Name -> RawModule -> M.Map Name ConstructorInfo
rawExportedConstructors moduleName raw =
  let qualifiedConstructors = M.map (qualifyCtorInfo moduleName) (rawConstructors raw)
   in case rawExports raw of
        Nothing ->
          qualifiedConstructors
        Just exports ->
          M.filterWithKey (\ctorName _ -> ctorName `elem` exports) qualifiedConstructors

qualifyEntryFunction
  :: Name
  -> M.Map Name Name
  -> M.Map Name Name
  -> Function
  -> Function
qualifyEntryFunction moduleName importEnv localNames fn =
  fn
    { body = renameExpr moduleName (args fn) localNames importEnv (body fn)
    }

renameFunction
  :: Name
  -> M.Map Name Name
  -> M.Map Name Name
  -> Function
  -> Function
renameFunction moduleName localNames importEnv fn =
  fn
    { name = fromMaybeName (name fn) localNames
    , body = renameExpr moduleName (args fn) localNames importEnv (body fn)
    }

renameExpr
  :: Name
  -> [Name]
  -> M.Map Name Name
  -> M.Map Name Name
  -> Expr
  -> Expr
renameExpr moduleName bound localNames importEnv expr =
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
                Nothing -> Var (fromMaybe name (qualifyCtorBuiltinName moduleName name))
    IntLit n -> IntLit n
    StringLit s -> StringLit s
    CollectionLit values ->
      CollectionLit (map (renameExpr moduleName bound localNames importEnv) values)
    App lhs rhs ->
      App (renameExpr moduleName bound localNames importEnv lhs) (renameExpr moduleName bound localNames importEnv rhs)
    Lam name bodyExpr ->
      Lam name (renameExpr moduleName (name : bound) localNames importEnv bodyExpr)
    Case scruts arms ->
      Case
        (map (renameExpr moduleName bound localNames importEnv) scruts)
        (map (renameCaseArm moduleName bound localNames importEnv) arms)

renameCaseArm
  :: Name
  -> [Name]
  -> M.Map Name Name
  -> M.Map Name Name
  -> CaseArm
  -> CaseArm
renameCaseArm moduleName bound localNames importEnv arm =
  let renamedPatterns = map (renameCasePattern moduleName) (armPatterns arm)
   in arm
        { armPatterns = renamedPatterns
        , armBody =
            renameExpr
              moduleName
              (bound ++ patternBoundNames renamedPatterns)
              localNames
              importEnv
              (armBody arm)
        }

renameCasePattern :: Name -> CasePattern -> CasePattern
renameCasePattern moduleName pat =
  case pat of
    PatConstructor ctorName ctorInfo fieldNames ->
      PatConstructor ctorName (qualifyCtorInfo moduleName ctorInfo) fieldNames
    _ ->
      pat

qualifyCtorInfo :: Name -> ConstructorInfo -> ConstructorInfo
qualifyCtorInfo moduleName ctorInfo =
  ctorInfo {ctorTypeName = qualifyCtorTypeName moduleName (ctorTypeName ctorInfo)}

qualifyCtorTypeName :: Name -> Name -> Name
qualifyCtorTypeName moduleName typeName0
  | '$' `elem` typeName0 = typeName0
  | otherwise = moduleName <> "$" <> typeName0

qualifyCtorBuiltinName :: Name -> Name -> Maybe Name
qualifyCtorBuiltinName moduleName builtinName =
  qualifyMk <|> qualifyGet <|> qualifyIs
  where
    qualifyMk :: Maybe Name
    qualifyMk = do
      rest <- stripPrefixExact "__mk_" builtinName
      case splitOnToken "_tpar_" rest of
        Just (prefix0, suffix0) -> do
          (tag0, fieldCount0) <- splitLastUnderscore prefix0
          if all isDigit fieldCount0
            then Just ("__mk_" <> qualifyCtorTag moduleName tag0 <> "_" <> fieldCount0 <> "_tpar_" <> suffix0)
            else Nothing
        Nothing -> do
          (tag0, fieldCount0) <- splitLastUnderscore rest
          if all isDigit fieldCount0
            then Just ("__mk_" <> qualifyCtorTag moduleName tag0 <> "_" <> fieldCount0)
            else Nothing

    qualifyGet :: Maybe Name
    qualifyGet = do
      rest <- stripPrefixExact "__get_" builtinName
      case splitOnToken "_tpar_" rest of
        Just (prefix0, suffix0) -> do
          (tag0, idx0) <- splitLastUnderscore prefix0
          if all isDigit idx0
            then Just ("__get_" <> qualifyCtorTag moduleName tag0 <> "_" <> idx0 <> "_tpar_" <> suffix0)
            else Nothing
        Nothing -> do
          (tag0, idx0) <- splitLastUnderscore rest
          if all isDigit idx0
            then Just ("__get_" <> qualifyCtorTag moduleName tag0 <> "_" <> idx0)
            else Nothing

    qualifyIs :: Maybe Name
    qualifyIs = do
      rest <- stripPrefixExact "__is_" builtinName
      case splitOnToken "_tpar_" rest of
        Just (tag0, suffix0) ->
          Just ("__is_" <> qualifyCtorTag moduleName tag0 <> "_tpar_" <> suffix0)
        Nothing ->
          if null rest
            then Nothing
            else Just ("__is_" <> qualifyCtorTag moduleName rest)

qualifyCtorTag :: Name -> Name -> Name
qualifyCtorTag moduleName tag0
  | '$' `elem` tag0 = tag0
  | otherwise = moduleName <> "$" <> tag0

stripPrefixExact :: String -> String -> Maybe String
stripPrefixExact prefix0 src =
  if take (length prefix0) src == prefix0
    then Just (drop (length prefix0) src)
    else Nothing

splitOnToken :: String -> String -> Maybe (String, String)
splitOnToken token src =
  go [] src
  where
    go :: String -> String -> Maybe (String, String)
    go acc remaining
      | take (length token) remaining == token =
          Just (reverse acc, drop (length token) remaining)
    go _ [] =
      Nothing
    go acc (c:cs) =
      go (c : acc) cs

splitLastUnderscore :: String -> Maybe (String, String)
splitLastUnderscore src =
  case break (== '_') (reverse src) of
    (_, []) -> Nothing
    (revSuffix, _:revPrefix) ->
      let prefix0 = reverse revPrefix
          suffix0 = reverse revSuffix
       in if null prefix0 || null suffix0
            then Nothing
            else Just (prefix0, suffix0)

patternBoundNames :: [CasePattern] -> [Name]
patternBoundNames = concatMap
  ( \case
      PatWildcard -> []
      PatVar name -> [name]
      PatInt _ -> []
      PatString _ -> []
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

collectExportApi :: [CIR.CollapsedFunction] -> [Name] -> Either String [ExportApi]
collectExportApi collapsedFns = traverse lookupExport
  where
    lookupExport :: Name -> Either String ExportApi
    lookupExport exportFnName =
      case findByName exportFnName collapsedFns of
        Nothing ->
          Left ("internal compile error: missing collapsed export function: " <> exportFnName)
        Just fn ->
          case fn of
            CIR.CollapsedFunction fnName fnArity fnCaptureArity _ _ _ ->
              if fnCaptureArity /= 0
                then
                  Left
                    ( "internal compile error: exported function unexpectedly has captures: "
                        <> fnName
                    )
                else Right ExportApi {exportName = fnName, exportArity = fnArity}

    findByName :: Name -> [CIR.CollapsedFunction] -> Maybe CIR.CollapsedFunction
    findByName _target [] = Nothing
    findByName target (fn : rest)
      | collapsedName fn == target = Just fn
      | otherwise = findByName target rest

    collapsedName :: CIR.CollapsedFunction -> Name
    collapsedName fn =
      case fn of
        CIR.CollapsedFunction fnName _ _ _ _ _ -> fnName

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

sourceModuleImports :: [Name] -> [Name]
sourceModuleImports = filter (not . isHostImportModule)

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

collectMemoHints :: [Function] -> [(Name, Int)]
collectMemoHints = foldr collectOne []
  where
    collectOne :: Function -> [(Name, Int)] -> [(Name, Int)]
    collectOne fn acc =
      case memoSizes fn of
        [] -> acc
        sizeN:_ -> (name fn, sizeN) : acc

    memoSizes :: Function -> [Int]
    memoSizes fn =
      [ sizeN
      | attr <- attributes fn
      , attributeName attr == "memo"
      , Just (AttributeInt sizeN) <- [attributeValue attr]
      ]
