module Clapse.Wasm
  ( compileModuleToWasm
  , compileModuleToWasmWithExports
  , compileSourceToWasm
  ) where

import Control.Monad (foldM, zipWithM)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Int (Int32)
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Clapse.CollapseIR
  ( Atom(..)
  , Bind(..)
  , CollapsedFunction(..)
  , Value(..)
  , collapseAndVerifyModule
  )
import Clapse.Lowering (lowerModule)
import Clapse.Syntax (Name, parseModule)

type StringLayout = [(String, (Int, Int))]
type DataSegment = (Int, [Word8])
type TagIdMap = [(Name, Int)]

compileSourceToWasm :: String -> Either String BS.ByteString
compileSourceToWasm src = do
  parsed <- parseModule src
  lowered <- lowerModule parsed
  collapsed <- collapseAndVerifyModule lowered
  compileModuleToWasm collapsed

compileModuleToWasm :: [CollapsedFunction] -> Either String BS.ByteString
compileModuleToWasm topLevelFns =
  compileModuleToWasmWithExports topLevelFns (map name topLevelFns)

compileModuleToWasmWithExports :: [CollapsedFunction] -> [Name] -> Either String BS.ByteString
compileModuleToWasmWithExports topLevelFns exports = do
  let allFns = flattenFunctions topLevelFns
  verifyUniqueNames allFns
  verifyReservedExportNames allFns
  (stringLayout, dataSegments, heapStart, memoryPageCount) <- buildStringLayout allFns
  let runtimeHelperArities = runtimeHelperSpecs
      runtimeWasmHelperCount = length runtimeHelperArities
      runtimeFunctionBase = runtimeImportCount + runtimeWasmHelperCount
      paramCountEnv = map (\fn -> (name fn, localParamCount fn)) allFns
      definedFuncMap = zip (map name allFns) [runtimeFunctionBase ..]
      signatureArities =
        unique
          ( [1, 2, 3]
              <> runtimeHelperArities
              <> map localParamCount allFns
          )
      typeMap = zip signatureArities [0 :: Int ..]
      tagIdMap = buildTagIdMap allFns
  closureCallTypeCases <- buildClosureCallTypeCases typeMap allFns
  let runtimeWasmHelperBodies = buildRuntimeHelperBodies
  traverse_ (verifyFunctionShape paramCountEnv) allFns
  userCodeBodies <- zipWithM (compileFunction stringLayout tagIdMap closureCallTypeCases paramCountEnv definedFuncMap) [runtimeFunctionBase ..] allFns
  importEntries <- runtimeImports typeMap
  typeIndices <- traverse (typeIndexFor typeMap . localParamCount) allFns
  helperTypeIndices <- traverse (typeIndexFor typeMap) runtimeHelperArities
  exportFns <- filterByExportName exports topLevelFns
  exportEntries <- traverse (encodeFunctionExport definedFuncMap) exportFns
  let typeSectionPayload = encodeVecRaw (map encodeFunctionType signatureArities)
      importSectionPayload = encodeVecRaw importEntries
      functionSectionPayload = encodeVecU32 (helperTypeIndices <> typeIndices)
      tableMin = runtimeFunctionBase + length allFns
      tableSectionPayload = encodeVecRaw [[refTypeFuncref] <> encodeLimitsMin tableMin]
      memorySectionPayload = encodeVecRaw [encodeLimitsMin memoryPageCount]
      globalSectionPayload = encodeVecRaw [encodeMutableI32Global heapStart]
      exportSectionPayload =
        encodeVecRaw
          ( exportEntries
              <> [ encodeName "__memory" <> [exportKindMemory] <> encodeU32 0
                 , encodeName "__table" <> [exportKindTable] <> encodeU32 0
                 , encodeName "__heap_ptr" <> [exportKindGlobal] <> encodeU32 runtimeHeapGlobal
                 ]
          )
      elementSectionPayload =
        encodeVecRaw
          [ encodeActiveElementSegment
              runtimeFunctionBase
              [runtimeFunctionBase .. runtimeFunctionBase + length allFns - 1]
          ]
      codeSectionPayload = encodeVecRaw (runtimeWasmHelperBodies <> userCodeBodies)
      dataSectionPayload = encodeVecRaw (map (uncurry encodeActiveDataSegment) dataSegments)
      moduleBytes =
        wasmMagicVersion
          <> encodeSection sectionType typeSectionPayload
          <> encodeSection sectionImport importSectionPayload
          <> encodeSection sectionFunction functionSectionPayload
          <> encodeSection sectionTable tableSectionPayload
          <> encodeSection sectionMemory memorySectionPayload
          <> encodeSection sectionGlobal globalSectionPayload
          <> encodeSection sectionExport exportSectionPayload
          <> encodeSection sectionElement elementSectionPayload
          <> encodeSection sectionCode codeSectionPayload
          <> encodeSection sectionData dataSectionPayload
  pure (BS.pack moduleBytes)

filterByExportName :: [Name] -> [CollapsedFunction] -> Either String [CollapsedFunction]
filterByExportName exports allFns =
  case filterExport exports of
    Left missing -> Left ("unknown export name: " <> missing)
    Right selected -> Right selected
  where
    filterExport :: [Name] -> Either String [CollapsedFunction]
    filterExport [] = Right []
    filterExport (target : rest) =
      case findFn target allFns of
        Nothing -> Left target
        Just fn -> do
          remaining <- filterExport rest
          Right (fn : remaining)

    findFn :: Name -> [CollapsedFunction] -> Maybe CollapsedFunction
    findFn _ [] = Nothing
    findFn target (fn : fns)
      | name fn == target = Just fn
      | otherwise = findFn target fns

flattenFunctions :: [CollapsedFunction] -> [CollapsedFunction]
flattenFunctions = concatMap go
  where
    go :: CollapsedFunction -> [CollapsedFunction]
    go fn = fn : flattenFunctions (lifted fn)

buildStringLayout :: [CollapsedFunction] -> Either String (StringLayout, [DataSegment], Int, Int)
buildStringLayout allFns = go 0 [] [] (collectStringLiterals allFns)
  where
    go :: Int -> StringLayout -> [DataSegment] -> [String] -> Either String (StringLayout, [DataSegment], Int, Int)
    go nextOffset layout segments literals =
      case literals of
        [] ->
          let heapStart = alignTo 4 (max nextOffset minLinearHeapStart)
           in Right (reverse layout, reverse segments, heapStart, memoryPagesForBytes heapStart)
        literalText:rest -> do
          literalBytes <- encodeLiteralBytes literalText
          let literalLength = length literalBytes
              descriptorOffset = nextOffset
              dataOffset = nextOffset + 8
          descriptorBytes <- descriptorToBytes dataOffset literalLength
          let
              nextLayout = (literalText, (descriptorOffset, literalLength)) : layout
              nextSegments = (nextOffset, descriptorBytes <> literalBytes) : segments
          go (nextOffset + 8 + literalLength) nextLayout nextSegments rest

alignTo :: Int -> Int -> Int
alignTo align n
  | align <= 0 = n
  | otherwise =
      ((n + align - 1) `div` align) * align

collectStringLiterals :: [CollapsedFunction] -> [String]
collectStringLiterals = uniqueStrings . concatMap collectFromFunction
  where
    collectFromFunction :: CollapsedFunction -> [String]
    collectFromFunction fn =
      concatMap collectFromBind (binds fn)
        <> collectFromAtom (result fn)
        <> collectStringLiterals (lifted fn)

    collectFromBind :: Bind -> [String]
    collectFromBind b = collectFromValue (value b)

    collectFromValue :: Value -> [String]
    collectFromValue val =
      case val of
        VClosure _ captures ->
          concatMap collectFromAtom captures
        VCallDirect _ args ->
          concatMap collectFromAtom args
        VCurryDirect _ args ->
          concatMap collectFromAtom args
        VCallClosure callee args ->
          concatMap collectFromAtom (callee : args)
        VApply callee arg ->
          concatMap collectFromAtom [callee, arg]
        VSelfTailCall args ->
          concatMap collectFromAtom args

    collectFromAtom :: Atom -> [String]
    collectFromAtom atom =
      case atom of
        AConstString s -> [s]
        _ -> []

uniqueStrings :: [String] -> [String]
uniqueStrings = go []
  where
    go :: [String] -> [String] -> [String]
    go _seen [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs

encodeLiteralBytes :: String -> Either String [Word8]
encodeLiteralBytes s = Right (BS.unpack (TE.encodeUtf8 (T.pack s)))

descriptorToBytes :: Int -> Int -> Either String [Word8]
descriptorToBytes dataPtr len =
  do
    dataPtrU32 <- toU32 "string descriptor pointer" dataPtr
    lenU32 <- toU32 "string descriptor length" len
    pure
      [ fromIntegral (dataPtrU32 .&. 0xff)
      , fromIntegral ((dataPtrU32 `shiftR` 8) .&. 0xff)
      , fromIntegral ((dataPtrU32 `shiftR` 16) .&. 0xff)
      , fromIntegral ((dataPtrU32 `shiftR` 24) .&. 0xff)
      , fromIntegral (lenU32 .&. 0xff)
      , fromIntegral ((lenU32 `shiftR` 8) .&. 0xff)
      , fromIntegral ((lenU32 `shiftR` 16) .&. 0xff)
      , fromIntegral ((lenU32 `shiftR` 24) .&. 0xff)
      ]

toU32 :: String -> Int -> Either String Int
toU32 label n
  | n < 0 =
      Left ("wasm backend: " <> label <> " must be >= 0, got " <> show n)
  | n > 0xffffffff =
      Left ("wasm backend: " <> label <> " exceeds u32 range, got " <> show n)
  | otherwise =
      Right n

memoryPagesForBytes :: Int -> Int
memoryPagesForBytes totalBytes
  | totalBytes <= 0 = 1
  | otherwise = (totalBytes + wasmPageSize - 1) `div` wasmPageSize

minLinearHeapStart :: Int
minLinearHeapStart = 4096

verifyUniqueNames :: [CollapsedFunction] -> Either String ()
verifyUniqueNames fns =
  case duplicates (map name fns) of
    [] ->
      Right ()
    dup:_ ->
      Left ("wasm backend: duplicate function name: " <> dup)

verifyReservedExportNames :: [CollapsedFunction] -> Either String ()
verifyReservedExportNames fns =
  let reserved = ["__memory", "__table", "__heap_ptr"]
      names = map name fns
   in case filter (`elem` reserved) names of
        [] ->
          Right ()
        bad:_ ->
          Left ("wasm backend: function name reserved for runtime export: " <> bad)

buildTagIdMap :: [CollapsedFunction] -> TagIdMap
buildTagIdMap allFns =
  zip tags [1 ..]
  where
    tags =
      sort
        ( uniqueTagNames
            ( ["collection_empty", "collection_node"]
                <> foldMap collectFunctionTags allFns
            )
        )

    collectFunctionTags :: CollapsedFunction -> [Name]
    collectFunctionTags fn =
      foldMap collectValueTags (map value (binds fn))
        <> foldMap collectFunctionTags (lifted fn)

    collectValueTags :: Value -> [Name]
    collectValueTags val =
      case val of
        VCallDirect callee _ ->
          case parseMkBuiltin callee of
            Just (tag, _fieldCount) ->
              [tag]
            Nothing ->
              case parseGetBuiltin callee of
                Just (tag, _idx) ->
                  [tag]
                Nothing ->
                  case parseIsBuiltin callee of
                    Just tag -> [tag]
                    Nothing -> []
        _ ->
          []

    uniqueTagNames :: [Name] -> [Name]
    uniqueTagNames = reverse . go [] []
      where
        go :: [Name] -> [Name] -> [Name] -> [Name]
        go _seen acc [] = acc
        go seen acc (x:xs)
          | x `elem` seen = go seen acc xs
          | otherwise = go (x : seen) (x : acc) xs

runtimeHelperSpecs :: [Int]
runtimeHelperSpecs = []

buildRuntimeHelperBodies :: [[Word8]]
buildRuntimeHelperBodies = []

buildClosureCallTypeCases :: [(Int, Int)] -> [CollapsedFunction] -> Either String [(Int, Int)]
buildClosureCallTypeCases typeMap allFns = traverse withType supportedArities
  where
    supportedArities =
      sort
        ( unique
            (filter (> 0) (map localParamCount allFns))
        )

    withType :: Int -> Either String (Int, Int)
    withType arity = do
      typeIx <- typeIndexFor typeMap arity
      pure (arity, typeIx)

verifyFunctionShape :: [(Name, Int)] -> CollapsedFunction -> Either String ()
verifyFunctionShape paramCountEnv fn = do
  traverse_ verifyBind (binds fn)
  verifyAtom (result fn)
  where
    verifyBind :: Bind -> Either String ()
    verifyBind b =
      case value b of
        VClosure callee captures -> do
          _ <- lookupRequired ("wasm backend: unknown closure callee: " <> callee) callee paramCountEnv
          verifyCaptureCount "closure capture count" captures
        VCurryDirect callee args -> do
          totalArity <- lookupRequired ("wasm backend: unknown curry callee: " <> callee) callee paramCountEnv
          if length args < totalArity
            then verifyCaptureCount "curry argument count" args
            else Left
              ( "wasm backend: invalid curry arity for "
                  <> callee
                  <> ": got "
                  <> show (length args)
                  <> " args, expected fewer than "
                  <> show totalArity
              )
        VCallDirect _ args ->
          traverse_ verifyAtom args
        VCallClosure callee args -> do
          verifyAtom callee
          traverse_ verifyAtom args
        VApply callee arg -> do
          verifyAtom callee
          verifyAtom arg
        VSelfTailCall args ->
          traverse_ verifyAtom args

    verifyCaptureCount :: String -> [Atom] -> Either String ()
    verifyCaptureCount _label captures =
      traverse_ verifyAtom captures

    verifyAtom :: Atom -> Either String ()
    verifyAtom atom =
      case atom of
        AConstI32 _ -> Right ()
        AConstString _ -> Right ()
        ALocal i ->
          if i < 0
            then Left ("wasm backend: local index must be >= 0, got " <> show i)
            else Right ()
        ATemp t ->
          if t < 0
            then Left ("wasm backend: temp index must be >= 0, got " <> show t)
            else Right ()
        AGlobal g ->
          case globalIndexFor g of
            Nothing -> Left ("wasm backend: unknown global atom: " <> g)
            Just _ -> Right ()

compileFunction :: StringLayout -> TagIdMap -> [(Int, Int)] -> [(Name, Int)] -> [(Name, Int)] -> Int -> CollapsedFunction -> Either String [Word8]
compileFunction stringLayout tagIdMap closureCallTypeCases paramCountEnv funcMap selfIndex fn = do
  let ifThunkMap = detectDirectIfBranchThunks paramCountEnv fn
  bindInstrs <-
    fmap concat
      ( traverse
          (compileBind stringLayout tagIdMap closureCallTypeCases paramCountEnv funcMap selfIndex fn ifThunkMap)
          (binds fn)
      )
  resultInstrs <- compileAtom stringLayout fn (result fn)
  let localCount = runtimeScratchLocalCount + length (binds fn)
      localsDecl =
        if localCount == 0
          then encodeU32 0
          else encodeU32 1 <> encodeU32 localCount <> [valTypeI32]
      body = localsDecl <> bindInstrs <> resultInstrs <> [opcodeEnd]
  pure (encodeU32 (length body) <> body)

compileBind :: StringLayout -> TagIdMap -> [(Int, Int)] -> [(Name, Int)] -> [(Name, Int)] -> Int -> CollapsedFunction -> IntMap IfBranchThunk -> Bind -> Either String [Word8]
compileBind stringLayout tagIdMap closureCallTypeCases paramCountEnv funcMap selfIndex fn ifThunkMap b
  | IM.member (temp b) ifThunkMap =
      Right []
  | otherwise = do
      valInstrs <- compileValue stringLayout tagIdMap closureCallTypeCases paramCountEnv funcMap selfIndex fn ifThunkMap (value b)
      let localIx = tempLocalIx fn (temp b)
      pure (valInstrs <> [opcodeLocalSet] <> encodeU32 localIx)

compileValue :: StringLayout -> TagIdMap -> [(Int, Int)] -> [(Name, Int)] -> [(Name, Int)] -> Int -> CollapsedFunction -> IntMap IfBranchThunk -> Value -> Either String [Word8]
compileValue stringLayout tagIdMap closureCallTypeCases paramCountEnv funcMap selfIndex fn ifThunkMap val =
  case val of
    VCallDirect callee args ->
      case lookup callee funcMap of
        Just callIx -> do
          argInstrs <- fmap concat (traverse (compileAtom stringLayout fn) args)
          pure (argInstrs <> [opcodeCall] <> encodeU32 callIx)
        Nothing ->
          compileRuntimeBuiltin stringLayout tagIdMap closureCallTypeCases funcMap ifThunkMap fn callee args
    VClosure callee captures ->
      compileClosureCreate stringLayout paramCountEnv funcMap fn callee captures
    VCurryDirect callee args ->
      compileClosureCreate stringLayout paramCountEnv funcMap fn callee args
    VApply callee arg -> do
      calleeInstrs <- compileAtom stringLayout fn callee
      argInstrs <- compileAtom stringLayout fn arg
      pure (compileInlineApplyInstrs closureCallTypeCases fn calleeInstrs argInstrs)
    VCallClosure callee args ->
      compileCallClosureChain stringLayout closureCallTypeCases fn callee args
    VSelfTailCall args -> do
      argInstrs <- fmap concat (traverse (compileAtom stringLayout fn) args)
      pure (argInstrs <> [opcodeReturnCall] <> encodeU32 selfIndex)

compileRuntimeBuiltin :: StringLayout -> TagIdMap -> [(Int, Int)] -> [(Name, Int)] -> IntMap IfBranchThunk -> CollapsedFunction -> Name -> [Atom] -> Either String [Word8]
compileRuntimeBuiltin stringLayout tagIdMap closureCallTypeCases funcMap ifThunkMap fn callee args =
  case compileInlineNumericBuiltin stringLayout fn callee args of
    Just out ->
      out
    Nothing ->
      case compileInlineSliceBuiltin stringLayout fn callee args of
        Just out ->
          out
        Nothing ->
          case compileCoreBuiltin stringLayout tagIdMap closureCallTypeCases funcMap ifThunkMap fn callee args of
            Just out ->
              out
            Nothing ->
              case builtinImportSig callee of
                Just (builtinIx, builtinArity) -> do
                  if length args == builtinArity
                    then do
                      argInstrs <- fmap concat (traverse (compileAtom stringLayout fn) args)
                      pure (argInstrs <> [opcodeCall] <> encodeU32 builtinIx)
                    else
                      Left
                        ( "wasm backend: builtin "
                            <> callee
                            <> " expects "
                            <> show builtinArity
                            <> " args, got "
                            <> show (length args)
                        )
                Nothing ->
                  case parseMkBuiltin callee of
                    Just (tag, ctorArity) ->
                      compileStructMake stringLayout tagIdMap fn tag ctorArity args
                    Nothing ->
                      case parseGetBuiltin callee of
                        Just (expectedTag, idx) ->
                          compileStructGet stringLayout tagIdMap fn expectedTag idx args
                        Nothing ->
                          case parseIsBuiltin callee of
                            Just expectedTag ->
                              compileStructTagEq stringLayout tagIdMap fn expectedTag args
                            Nothing ->
                              Left ("wasm backend: unknown direct callee: " <> callee)

compileInlineNumericBuiltin :: StringLayout -> CollapsedFunction -> Name -> [Atom] -> Maybe (Either String [Word8])
compileInlineNumericBuiltin stringLayout fn callee args =
  case (callee, args) of
    ("add", [x, y]) ->
      Just (compileTaggedIntBinOp stringLayout fn x y opcodeI32Add)
    ("sub", [x, y]) ->
      Just (compileTaggedIntBinOp stringLayout fn x y opcodeI32Sub)
    ("mul", [x, y]) ->
      Just (compileTaggedIntBinOp stringLayout fn x y opcodeI32Mul)
    ("div", [x, y]) ->
      Just (compileTaggedIntBinOp stringLayout fn x y opcodeI32DivS)
    ("mod", [x, y]) ->
      Just (compileTaggedIntBinOp stringLayout fn x y opcodeI32RemS)
    ("eq", [x, y]) ->
      Just (compileTaggedEq stringLayout fn x y)
    ("lt", [x, y]) ->
      Just (compileTaggedCmp stringLayout fn x y opcodeI32LtS)
    ("gt", [x, y]) ->
      Just (compileTaggedCmp stringLayout fn x y opcodeI32GtS)
    ("le", [x, y]) ->
      Just (compileTaggedCmp stringLayout fn x y opcodeI32LeS)
    ("ge", [x, y]) ->
      Just (compileTaggedCmp stringLayout fn x y opcodeI32GeS)
    ("and", [x, y]) ->
      Just (compileTaggedAnd stringLayout fn x y)
    _ ->
      Nothing

compileTaggedIntBinOp :: StringLayout -> CollapsedFunction -> Atom -> Atom -> Word8 -> Either String [Word8]
compileTaggedIntBinOp stringLayout fn x y op = do
  xInstrs <- compileTaggedIntAtom stringLayout fn x
  yInstrs <- compileTaggedIntAtom stringLayout fn y
  pure (xInstrs <> yInstrs <> [op] <> retagI32Instrs)

compileTaggedEq :: StringLayout -> CollapsedFunction -> Atom -> Atom -> Either String [Word8]
compileTaggedEq stringLayout fn x y = do
  xInstrs <- compileTaggedIntAtom stringLayout fn x
  yInstrs <- compileTaggedIntAtom stringLayout fn y
  pure (xInstrs <> yInstrs <> [opcodeI32Eq] <> retagBoolInstrs)

compileTaggedCmp :: StringLayout -> CollapsedFunction -> Atom -> Atom -> Word8 -> Either String [Word8]
compileTaggedCmp stringLayout fn x y cmpOp = do
  xInstrs <- compileTaggedIntAtom stringLayout fn x
  yInstrs <- compileTaggedIntAtom stringLayout fn y
  pure (xInstrs <> yInstrs <> [cmpOp] <> retagBoolInstrs)

compileTaggedAnd :: StringLayout -> CollapsedFunction -> Atom -> Atom -> Either String [Word8]
compileTaggedAnd stringLayout fn x y = do
  xInstrs <- compileTaggedIntAtom stringLayout fn x
  yInstrs <- compileTaggedIntAtom stringLayout fn y
  pure
    ( xInstrs
        <> rawI32Const 0
        <> [opcodeI32Ne]
        <> yInstrs
        <> rawI32Const 0
        <> [opcodeI32Ne]
        <> [opcodeI32And]
        <> retagBoolInstrs
    )

compileIfBuiltin ::
  StringLayout ->
  [(Int, Int)] ->
  [(Name, Int)] ->
  IntMap IfBranchThunk ->
  CollapsedFunction ->
  Atom ->
  Atom ->
  Atom ->
  Either String [Word8]
compileIfBuiltin stringLayout closureCallTypeCases funcMap ifThunkMap fn cond whenTrue whenFalse = do
  condInstrs <- compileTaggedIntAtom stringLayout fn cond
  whenTrueInstrs <- compileIfBranchArg stringLayout closureCallTypeCases funcMap ifThunkMap fn whenTrue
  whenFalseInstrs <- compileIfBranchArg stringLayout closureCallTypeCases funcMap ifThunkMap fn whenFalse
  pure
    ( condInstrs
        <> [opcodeIf, valTypeI32]
        <> whenTrueInstrs
        <> [opcodeElse]
        <> whenFalseInstrs
        <> [opcodeEnd]
    )

data IfBranchThunk = IfBranchThunk
  { ifBranchThunkCallee :: !Name
  , ifBranchThunkCaptures :: ![Atom]
  }

data IfThunkUseContext
  = IfThunkUseContextBranchArg
  | IfThunkUseContextOther

compileIfBranchArg ::
  StringLayout ->
  [(Int, Int)] ->
  [(Name, Int)] ->
  IntMap IfBranchThunk ->
  CollapsedFunction ->
  Atom ->
  Either String [Word8]
compileIfBranchArg stringLayout closureCallTypeCases funcMap ifThunkMap fn atom =
  case atom of
    ATemp temp ->
      case IM.lookup temp ifThunkMap of
        Nothing ->
          compileIfBranchApply stringLayout closureCallTypeCases fn atom
        Just thunk ->
          compileIfBranchDirect stringLayout funcMap fn thunk
    _ ->
      compileIfBranchApply stringLayout closureCallTypeCases fn atom

compileIfBranchApply ::
  StringLayout ->
  [(Int, Int)] ->
  CollapsedFunction ->
  Atom ->
  Either String [Word8]
compileIfBranchApply stringLayout closureCallTypeCases fn atom = do
  atomInstrs <- compileAtom stringLayout fn atom
  pure (compileInlineApplyInstrs closureCallTypeCases fn atomInstrs (rawI32Const 0))

compileIfBranchDirect ::
  StringLayout ->
  [(Name, Int)] ->
  CollapsedFunction ->
  IfBranchThunk ->
  Either String [Word8]
compileIfBranchDirect stringLayout funcMap fn thunk = do
  calleeIx <- lookupRequired "wasm backend: missing direct branch thunk callee" (ifBranchThunkCallee thunk) funcMap
  captureInstrs <- fmap concat (traverse (compileAtom stringLayout fn) (ifBranchThunkCaptures thunk))
  pure (captureInstrs <> rawI32Const 0 <> [opcodeCall] <> encodeU32 calleeIx)

detectDirectIfBranchThunks :: [(Name, Int)] -> CollapsedFunction -> IntMap IfBranchThunk
detectDirectIfBranchThunks paramCountEnv fn =
  let closureDefs =
        [ (temp b, IfBranchThunk callee captures)
        | b <- binds fn
        , VClosure callee captures <- [value b]
        ]
      useCounts =
        collectAtomUse
          IfThunkUseContextOther
          (foldl' collectValueUse IM.empty (map value (binds fn)))
          (result fn)
      candidates =
        [ (tempIx, thunk)
        | (tempIx, thunk) <- closureDefs
        , let (branchArgUses, otherUses) = IM.findWithDefault (0, 0) tempIx useCounts
        , branchArgUses == 1
        , otherUses == 0
        , isSaturatingThunk thunk
        ]
   in IM.fromList candidates
  where
    isSaturatingThunk :: IfBranchThunk -> Bool
    isSaturatingThunk thunk =
      case lookup (ifBranchThunkCallee thunk) paramCountEnv of
        Nothing -> False
        Just totalArity -> totalArity == length (ifBranchThunkCaptures thunk) + 1

    collectValueUse :: IntMap (Int, Int) -> Value -> IntMap (Int, Int)
    collectValueUse counts val =
      case val of
        VClosure _ captures ->
          foldl' (collectAtomUse IfThunkUseContextOther) counts captures
        VCallDirect callee args ->
          case (callee, args) of
            ("if", [cond, whenTrue, whenFalse]) ->
              collectAtomUse
                IfThunkUseContextBranchArg
                (collectAtomUse
                  IfThunkUseContextBranchArg
                  (collectAtomUse IfThunkUseContextOther counts cond)
                  whenFalse
                )
                whenTrue
            _ ->
              foldl' (collectAtomUse IfThunkUseContextOther) counts args
        VCurryDirect _ args ->
          foldl' (collectAtomUse IfThunkUseContextOther) counts args
        VCallClosure callee args ->
          foldl' (collectAtomUse IfThunkUseContextOther) counts (callee : args)
        VApply callee arg ->
          foldl' (collectAtomUse IfThunkUseContextOther) counts [callee, arg]
        VSelfTailCall args ->
          foldl' (collectAtomUse IfThunkUseContextOther) counts args

    collectAtomUse ::
      IfThunkUseContext ->
      IntMap (Int, Int) ->
      Atom ->
      IntMap (Int, Int)
    collectAtomUse context counts atom =
      case atom of
        ATemp temp ->
          let updater (branchUses, otherUses) =
                case context of
                  IfThunkUseContextBranchArg -> (branchUses + 1, otherUses)
                  IfThunkUseContextOther -> (branchUses, otherUses + 1)
              updateCounts = case IM.lookup temp counts of
                Nothing -> Just (updater (0, 0))
                Just prev -> Just (updater prev)
           in IM.alter (\_ -> updateCounts) temp counts
        _ ->
          counts


compileInlineSliceBuiltin :: StringLayout -> CollapsedFunction -> Name -> [Atom] -> Maybe (Either String [Word8])
compileInlineSliceBuiltin stringLayout fn callee args =
  case (callee, args) of
    ("slice_len", [sliceHandle]) ->
      Just (compileInlineSliceLen stringLayout fn sliceHandle)
    ("slice_get_u8", [sliceHandle, index]) ->
      Just (compileInlineSliceGetU8 stringLayout fn sliceHandle index)
    ("slice_set_u8", [sliceHandle, index, value]) ->
      Just (compileInlineSliceSetU8 stringLayout fn sliceHandle index value)
    _ ->
      Nothing

-- Slice handle layout in wasm linear memory:
--   [0..3]  i32 data pointer
--   [4..7]  i32 length (untagged)
compileInlineSliceLen :: StringLayout -> CollapsedFunction -> Atom -> Either String [Word8]
compileInlineSliceLen stringLayout fn sliceHandle = do
  handleInstrs <- compileAtom stringLayout fn sliceHandle
  pure
    ( handleInstrs
        <> [opcodeI32Load]
        <> encodeMemArg 2 4
        <> retagI32Instrs
    )

compileInlineSliceGetU8 :: StringLayout -> CollapsedFunction -> Atom -> Atom -> Either String [Word8]
compileInlineSliceGetU8 stringLayout fn sliceHandle index = do
  handleInstrs <- compileAtom stringLayout fn sliceHandle
  indexInstrs <- compileTaggedIntAtom stringLayout fn index
  pure
    ( handleInstrs
        <> [opcodeI32Load]
        <> encodeMemArg 2 0
        <> indexInstrs
        <> [opcodeI32Add]
        <> [opcodeI32Load8U]
        <> encodeMemArg 0 0
        <> retagI32Instrs
    )

compileInlineSliceSetU8 :: StringLayout -> CollapsedFunction -> Atom -> Atom -> Atom -> Either String [Word8]
compileInlineSliceSetU8 stringLayout fn sliceHandle index value = do
  handleInstrs <- compileAtom stringLayout fn sliceHandle
  indexInstrs <- compileTaggedIntAtom stringLayout fn index
  valueInstrs <- compileTaggedIntAtom stringLayout fn value
  pure
    ( handleInstrs
        <> [opcodeI32Load]
        <> encodeMemArg 2 0
        <> indexInstrs
        <> [opcodeI32Add]
        <> valueInstrs
        <> [opcodeI32Store8]
        <> encodeMemArg 0 0
        <> handleInstrs
    )

compileTaggedIntAtom :: StringLayout -> CollapsedFunction -> Atom -> Either String [Word8]
compileTaggedIntAtom stringLayout fn atom = do
  atomInstrs <- compileAtom stringLayout fn atom
  pure (atomInstrs <> untagI32Instrs)

untagI32Instrs :: [Word8]
untagI32Instrs =
  rawI32Const 1
    <> [opcodeI32ShrS]

retagI32Instrs :: [Word8]
retagI32Instrs =
  rawI32Const 1
    <> [opcodeI32Shl]
    <> rawI32Const 1
    <> [opcodeI32Or]

retagBoolInstrs :: [Word8]
retagBoolInstrs =
  rawI32Const 1
    <> [opcodeI32Shl]
    <> rawI32Const 1
    <> [opcodeI32Or]

compileCoreBuiltin :: StringLayout -> TagIdMap -> [(Int, Int)] -> [(Name, Int)] -> IntMap IfBranchThunk -> CollapsedFunction -> Name -> [Atom] -> Maybe (Either String [Word8])
compileCoreBuiltin stringLayout tagIdMap closureCallTypeCases funcMap ifThunkMap fn callee args =
  case (callee, args) of
    ("if", [cond, whenTrue, whenFalse]) ->
      Just (compileIfBuiltin stringLayout closureCallTypeCases funcMap ifThunkMap fn cond whenTrue whenFalse)
    ("if", _) ->
      Just
        ( Left
            ( "wasm backend: builtin if expects 3 args, got "
                <> show (length args)
            )
        )
    ("pure", [x]) ->
      Just (compileAtom stringLayout fn x)
    ("fmap", [f, x]) ->
      Just (compileApplyAtoms stringLayout closureCallTypeCases fn f x)
    ("ap", [f, x]) ->
      Just (compileApplyAtoms stringLayout closureCallTypeCases fn f x)
    ("bind", [m, f]) ->
      Just (compileApplyAtoms stringLayout closureCallTypeCases fn f m)
    ("collection_empty", [_ignored]) ->
      Just (compileStructMake stringLayout tagIdMap fn "collection_empty" 0 [])
    ("collection_extend", [collectionVal, x]) ->
      Just (compileStructMake stringLayout tagIdMap fn "collection_node" 2 [collectionVal, x])
    ("slice_new_u8", [len]) ->
      Just
        ( do
            lenInstrs <- compileTaggedIntAtom stringLayout fn len
            let lenLocal = scratchPtrLocalIx fn
                dataPtrLocal = scratchAuxLocalIx fn
                descLocal = scratchTmpLocalIx fn
                allocSizeLocal = applySizeLocalIx fn
                allocAlignLocal = applyAlignLocalIx fn
            pure
              ( lenInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 lenLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 lenLocal
                        <> rawI32Const 0
                        <> [opcodeI32LtS]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 lenLocal
                  <> [opcodeLocalSet]
                  <> encodeU32 allocSizeLocal
                  <> rawI32Const 1
                  <> [opcodeLocalSet]
                  <> encodeU32 allocAlignLocal
                  <> emitInlineAllocFromLocals allocSizeLocal allocAlignLocal dataPtrLocal
                  <> [opcodeLocalSet]
                  <> encodeU32 dataPtrLocal
                  <> rawI32Const 8
                  <> [opcodeLocalSet]
                  <> encodeU32 allocSizeLocal
                  <> rawI32Const 4
                  <> [opcodeLocalSet]
                  <> encodeU32 allocAlignLocal
                  <> emitInlineAllocFromLocals allocSizeLocal allocAlignLocal descLocal
                  <> [opcodeLocalSet]
                  <> encodeU32 descLocal
                  <> storeI32LocalAt descLocal dataPtrLocal 0
                  <> storeI32LocalAt descLocal lenLocal 4
                  <> [opcodeLocalGet]
                  <> encodeU32 dataPtrLocal
                  <> rawI32Const 0
                  <> [opcodeLocalGet]
                  <> encodeU32 lenLocal
                  <> memoryFillInstr
                  <> [opcodeLocalGet]
                  <> encodeU32 descLocal
              )
        )
    ("slice_new_u8", _) ->
      Just (builtinArityError "slice_new_u8" 1 (length args))
    ("slice_data_ptr", [sliceHandle]) ->
      Just
        ( do
            handleInstrs <- compileAtom stringLayout fn sliceHandle
            let ptrLocal = scratchPtrLocalIx fn
            pure
              ( handleInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 ptrLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 ptrLocal
                        <> rawI32Const 1
                        <> [opcodeI32And]
                        <> rawI32Const 0
                        <> [opcodeI32Ne]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 ptrLocal
                  <> [opcodeI32Load]
                  <> encodeMemArg 2 0
              )
        )
    ("slice_data_ptr", _) ->
      Just (builtinArityError "slice_data_ptr" 1 (length args))
    ("slice_len_raw", [sliceHandle]) ->
      Just
        ( do
            handleInstrs <- compileAtom stringLayout fn sliceHandle
            let ptrLocal = scratchPtrLocalIx fn
            pure
              ( handleInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 ptrLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 ptrLocal
                        <> rawI32Const 1
                        <> [opcodeI32And]
                        <> rawI32Const 0
                        <> [opcodeI32Ne]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 ptrLocal
                  <> [opcodeI32Load]
                  <> encodeMemArg 2 4
                  <> retagI32Instrs
              )
        )
    ("slice_len_raw", _) ->
      Just (builtinArityError "slice_len_raw" 1 (length args))
    ("region_mark", [_ignored]) ->
      Just (Right ([opcodeGlobalGet] <> encodeU32 runtimeHeapGlobal))
    ("region_mark", _) ->
      Just (builtinArityError "region_mark" 1 (length args))
    ("region_alloc", [sizeBytes, alignBytes]) ->
      Just
        ( do
            sizeInstrs <- compileTaggedIntAtom stringLayout fn sizeBytes
            alignInstrs <- compileTaggedIntAtom stringLayout fn alignBytes
            let sizeLocal = scratchPtrLocalIx fn
                alignLocal = scratchAuxLocalIx fn
                ptrLocal = scratchTmpLocalIx fn
            pure
              ( sizeInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 sizeLocal
                  <> alignInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 alignLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 sizeLocal
                        <> rawI32Const 0
                        <> [opcodeI32LtS]
                    )
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 alignLocal
                        <> rawI32Const 0
                        <> [opcodeI32LeS]
                    )
                  <> emitInlineAllocFromLocals sizeLocal alignLocal ptrLocal
                  <> [opcodeLocalSet]
                  <> encodeU32 ptrLocal
                  <> [opcodeLocalGet]
                  <> encodeU32 ptrLocal
              )
        )
    ("region_alloc", _) ->
      Just (builtinArityError "region_alloc" 2 (length args))
    ("region_reset", [mark]) ->
      Just
        ( do
            markInstrs <- compileAtom stringLayout fn mark
            let markLocal = scratchPtrLocalIx fn
            pure
              ( markInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 markLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 markLocal
                        <> rawI32Const 0
                        <> [opcodeI32LtS]
                    )
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 markLocal
                        <> [opcodeGlobalGet]
                        <> encodeU32 runtimeHeapGlobal
                        <> [opcodeI32GtU]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 markLocal
                  <> [opcodeGlobalSet]
                  <> encodeU32 runtimeHeapGlobal
                  <> [opcodeLocalGet]
                  <> encodeU32 markLocal
              )
        )
    ("region_reset", _) ->
      Just (builtinArityError "region_reset" 1 (length args))
    ("memcpy_u8", [destPtr, srcPtr, lenBytes]) ->
      Just
        ( do
            destInstrs <- compileAtom stringLayout fn destPtr
            srcInstrs <- compileAtom stringLayout fn srcPtr
            lenInstrs <- compileTaggedIntAtom stringLayout fn lenBytes
            let destLocal = scratchPtrLocalIx fn
                srcLocal = scratchAuxLocalIx fn
                lenLocal = scratchTmpLocalIx fn
            pure
              ( destInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 destLocal
                  <> srcInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 srcLocal
                  <> lenInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 lenLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 lenLocal
                        <> rawI32Const 0
                        <> [opcodeI32LtS]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 destLocal
                  <> [opcodeLocalGet]
                  <> encodeU32 srcLocal
                  <> [opcodeLocalGet]
                  <> encodeU32 lenLocal
                  <> memoryCopyInstr
                  <> [opcodeLocalGet]
                  <> encodeU32 destLocal
              )
        )
    ("memcpy_u8", _) ->
      Just (builtinArityError "memcpy_u8" 3 (length args))
    ("memset_u8", [destPtr, value, lenBytes]) ->
      Just
        ( do
            destInstrs <- compileAtom stringLayout fn destPtr
            valueInstrs <- compileTaggedIntAtom stringLayout fn value
            lenInstrs <- compileTaggedIntAtom stringLayout fn lenBytes
            let destLocal = scratchPtrLocalIx fn
                valueLocal = scratchAuxLocalIx fn
                lenLocal = scratchTmpLocalIx fn
            pure
              ( destInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 destLocal
                  <> valueInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 valueLocal
                  <> lenInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 lenLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 lenLocal
                        <> rawI32Const 0
                        <> [opcodeI32LtS]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 destLocal
                  <> [opcodeLocalGet]
                  <> encodeU32 valueLocal
                  <> rawI32Const 255
                  <> [opcodeI32And]
                  <> [opcodeLocalGet]
                  <> encodeU32 lenLocal
                  <> memoryFillInstr
                  <> [opcodeLocalGet]
                  <> encodeU32 destLocal
              )
        )
    ("memset_u8", _) ->
      Just (builtinArityError "memset_u8" 3 (length args))
    ("struct_tag", [target]) ->
      Just
        ( do
            targetInstrs <- compileAtom stringLayout fn target
            let ptrLocal = scratchPtrLocalIx fn
            pure
              ( targetInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 ptrLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 ptrLocal
                        <> rawI32Const 1
                        <> [opcodeI32And]
                        <> rawI32Const 0
                        <> [opcodeI32Ne]
                    )
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 ptrLocal
                        <> [opcodeI32Load]
                        <> encodeMemArg 2 0
                        <> rawI32Const runtimeStructMagic
                        <> [opcodeI32Ne]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 ptrLocal
                  <> [opcodeI32Load]
                  <> encodeMemArg 2 4
                  <> retagI32Instrs
              )
        )
    ("struct_tag", _) ->
      Just (builtinArityError "struct_tag" 1 (length args))
    _ ->
      Nothing

builtinArityError :: Name -> Int -> Int -> Either String a
builtinArityError builtinName expected got =
  Left
    ( "wasm backend: builtin "
        <> builtinName
        <> " expects "
        <> show expected
        <> " args, got "
        <> show got
    )

compileApplyAtoms :: StringLayout -> [(Int, Int)] -> CollapsedFunction -> Atom -> Atom -> Either String [Word8]
compileApplyAtoms stringLayout closureCallTypeCases fn f x = do
  fInstrs <- compileAtom stringLayout fn f
  xInstrs <- compileAtom stringLayout fn x
  pure (compileInlineApplyInstrs closureCallTypeCases fn fInstrs xInstrs)

compileStructMake :: StringLayout -> TagIdMap -> CollapsedFunction -> Name -> Int -> [Atom] -> Either String [Word8]
compileStructMake stringLayout tagIdMap fn tag ctorArity args
  | ctorArity /= length args =
      Left
        ( "wasm backend: constructor arity mismatch for __mk_"
            <> tag
            <> "_"
            <> show ctorArity
            <> ", got "
            <> show (length args)
        )
  | otherwise = do
      tagId <- lookupTagId tagIdMap tag
      fieldStores <- fmap concat (zipWithM compileField [0 .. length args - 1] args)
      let ptrLocal = scratchPtrLocalIx fn
          sizeLocal = scratchAuxLocalIx fn
          alignLocal = scratchTmpLocalIx fn
          recordSize = runtimeStructHeaderSize + length args * 4
      pure
        ( rawI32Const recordSize
            <> [opcodeLocalSet]
            <> encodeU32 sizeLocal
            <> rawI32Const 4
            <> [opcodeLocalSet]
            <> encodeU32 alignLocal
            <> emitInlineAllocFromLocals sizeLocal alignLocal ptrLocal
            <> [opcodeLocalSet]
            <> encodeU32 ptrLocal
            <> storeI32ConstAt ptrLocal runtimeStructMagic 0
            <> storeI32ConstAt ptrLocal tagId 4
            <> storeI32ConstAt ptrLocal (length args) 8
            <> fieldStores
            <> [opcodeLocalGet]
            <> encodeU32 ptrLocal
        )
  where
    compileField :: Int -> Atom -> Either String [Word8]
    compileField fieldIx atom = do
      valueInstrs <- compileAtom stringLayout fn atom
      pure
        ( [opcodeLocalGet]
            <> encodeU32 (scratchPtrLocalIx fn)
            <> valueInstrs
            <> [opcodeI32Store]
            <> encodeMemArg 2 (runtimeStructHeaderSize + fieldIx * 4)
        )

compileStructGet :: StringLayout -> TagIdMap -> CollapsedFunction -> Name -> Int -> [Atom] -> Either String [Word8]
compileStructGet stringLayout tagIdMap fn expectedTag idx args =
  case args of
    [target] -> do
      expectedTagId <- lookupTagId tagIdMap expectedTag
      targetInstrs <- compileAtom stringLayout fn target
      if idx < 0
        then
          Left
            ( "wasm backend: struct getter index out of range for "
                <> expectedTag
                <> ": "
                <> show idx
            )
        else
          do
            let ptrLocal = scratchPtrLocalIx fn
                fieldCountLocal = scratchAuxLocalIx fn
            pure
              ( targetInstrs
                  <> [opcodeLocalSet]
                  <> encodeU32 ptrLocal
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 ptrLocal
                        <> rawI32Const 1
                        <> [opcodeI32And]
                        <> rawI32Const 0
                        <> [opcodeI32Ne]
                    )
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 ptrLocal
                        <> [opcodeI32Load]
                        <> encodeMemArg 2 0
                        <> rawI32Const runtimeStructMagic
                        <> [opcodeI32Ne]
                    )
                  <> trapIfTrue
                    ( [opcodeLocalGet]
                        <> encodeU32 ptrLocal
                        <> [opcodeI32Load]
                        <> encodeMemArg 2 4
                        <> rawI32Const expectedTagId
                        <> [opcodeI32Ne]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 ptrLocal
                  <> [opcodeI32Load]
                  <> encodeMemArg 2 8
                  <> [opcodeLocalSet]
                  <> encodeU32 fieldCountLocal
                  <> trapIfTrue
                    ( rawI32Const idx
                        <> [opcodeLocalGet]
                        <> encodeU32 fieldCountLocal
                        <> [opcodeI32GtU]
                        <> rawI32Const idx
                        <> [opcodeLocalGet]
                        <> encodeU32 fieldCountLocal
                        <> [opcodeI32Eq]
                        <> [opcodeI32Or]
                    )
                  <> [opcodeLocalGet]
                  <> encodeU32 ptrLocal
                  <> rawI32Const (runtimeStructHeaderSize + idx * 4)
                  <> [opcodeI32Add]
                  <> [opcodeI32Load]
                  <> encodeMemArg 2 0
              )
    _ ->
      Left ("wasm backend: struct getter expects 1 arg, got " <> show (length args))

compileStructTagEq :: StringLayout -> TagIdMap -> CollapsedFunction -> Name -> [Atom] -> Either String [Word8]
compileStructTagEq stringLayout tagIdMap fn expectedTag args =
  case args of
    [target] -> do
      expectedTagId <- lookupTagId tagIdMap expectedTag
      targetInstrs <- compileAtom stringLayout fn target
      let ptrLocal = scratchPtrLocalIx fn
          matchesLocal = scratchAuxLocalIx fn
      pure
        ( targetInstrs
            <> [opcodeLocalSet]
            <> encodeU32 ptrLocal
            <> rawI32Const 0
            <> [opcodeLocalSet]
            <> encodeU32 matchesLocal
            <> [opcodeLocalGet]
            <> encodeU32 ptrLocal
            <> rawI32Const 1
            <> [opcodeI32And]
            <> [opcodeI32Eqz]
            <> [opcodeIf, blockTypeEmpty]
            <> [opcodeLocalGet]
            <> encodeU32 ptrLocal
            <> [opcodeI32Load]
            <> encodeMemArg 2 0
            <> rawI32Const runtimeStructMagic
            <> [opcodeI32Eq]
            <> [opcodeIf, blockTypeEmpty]
            <> [opcodeLocalGet]
            <> encodeU32 ptrLocal
            <> [opcodeI32Load]
            <> encodeMemArg 2 4
            <> rawI32Const expectedTagId
            <> [opcodeI32Eq]
            <> [opcodeLocalSet]
            <> encodeU32 matchesLocal
            <> [opcodeEnd]
            <> [opcodeEnd]
            <> [opcodeLocalGet]
            <> encodeU32 matchesLocal
            <> retagBoolInstrs
        )
    _ ->
      Left ("wasm backend: struct tag predicate expects 1 arg, got " <> show (length args))

compileClosureCreate :: StringLayout -> [(Name, Int)] -> [(Name, Int)] -> CollapsedFunction -> Name -> [Atom] -> Either String [Word8]
compileClosureCreate stringLayout paramCountEnv funcMap fn callee captures = do
  calleeIx <- lookupRequired ("wasm backend: unknown closure callee: " <> callee) callee funcMap
  totalArity <- lookupRequired ("wasm backend: missing callee arity: " <> callee) callee paramCountEnv
  fieldStores <- fmap concat (zipWithM compileCaptureField [0 .. length captures - 1] captures)
  let ptrLocal = scratchPtrLocalIx fn
      sizeLocal = scratchAuxLocalIx fn
      alignLocal = scratchTmpLocalIx fn
      recordSize = runtimeClosureHeaderSize + length captures * 4
  pure
    ( rawI32Const recordSize
        <> [opcodeLocalSet]
        <> encodeU32 sizeLocal
        <> rawI32Const 4
        <> [opcodeLocalSet]
        <> encodeU32 alignLocal
        <> emitInlineAllocFromLocals sizeLocal alignLocal ptrLocal
        <> [opcodeLocalSet]
        <> encodeU32 ptrLocal
        <> storeI32ConstAt ptrLocal runtimeClosureMagic 0
        <> storeI32ConstAt ptrLocal calleeIx 4
        <> storeI32ConstAt ptrLocal totalArity 8
        <> storeI32ConstAt ptrLocal (length captures) 12
        <> fieldStores
        <> [opcodeLocalGet]
        <> encodeU32 ptrLocal
    )
  where
    compileCaptureField :: Int -> Atom -> Either String [Word8]
    compileCaptureField fieldIx atom = do
      valueInstrs <- compileAtom stringLayout fn atom
      pure
        ( [opcodeLocalGet]
            <> encodeU32 (scratchPtrLocalIx fn)
            <> valueInstrs
            <> [opcodeI32Store]
            <> encodeMemArg 2 (runtimeClosureHeaderSize + fieldIx * 4)
        )

compileCallClosureChain :: StringLayout -> [(Int, Int)] -> CollapsedFunction -> Atom -> [Atom] -> Either String [Word8]
compileCallClosureChain stringLayout closureCallTypeCases fn callee args = do
  calleeInstrs <- compileAtom stringLayout fn callee
  foldM applyOne calleeInstrs args
  where
    applyOne :: [Word8] -> Atom -> Either String [Word8]
    applyOne acc arg = do
      argInstrs <- compileAtom stringLayout fn arg
      pure (compileInlineApplyInstrs closureCallTypeCases fn acc argInstrs)

compileInlineApplyInstrs :: [(Int, Int)] -> CollapsedFunction -> [Word8] -> [Word8] -> [Word8]
compileInlineApplyInstrs closureCallTypeCases fn calleeInstrs argInstrs =
  let calleeLocal = applyCalleeLocalIx fn
      argLocal = applyArgLocalIx fn
      fnIndexLocal = applyFnIndexLocalIx fn
      totalArityLocal = applyTotalArityLocalIx fn
      captureCountLocal = applyCaptureCountLocalIx fn
      newCountLocal = applyNewCountLocalIx fn
      newPtrLocal = applyNewPtrLocalIx fn
      resultLocal = applyResultLocalIx fn
      sizeLocal = applySizeLocalIx fn
      alignLocal = applyAlignLocalIx fn
      partialPath =
        rawI32Const runtimeClosureHeaderSize
          <> [opcodeLocalGet]
          <> encodeU32 newCountLocal
          <> rawI32Const 2
          <> [opcodeI32Shl]
          <> [opcodeI32Add]
          <> [opcodeLocalSet]
          <> encodeU32 sizeLocal
          <> rawI32Const 4
          <> [opcodeLocalSet]
          <> encodeU32 alignLocal
          <> emitInlineAllocFromLocals sizeLocal alignLocal newPtrLocal
          <> [opcodeLocalSet]
          <> encodeU32 newPtrLocal
          <> storeI32ConstAt newPtrLocal runtimeClosureMagic 0
          <> storeI32LocalAt newPtrLocal fnIndexLocal 4
          <> storeI32LocalAt newPtrLocal totalArityLocal 8
          <> storeI32LocalAt newPtrLocal newCountLocal 12
          <> [opcodeLocalGet]
          <> encodeU32 captureCountLocal
          <> rawI32Const 0
          <> [opcodeI32GtS, opcodeIf, blockTypeEmpty]
          <> [opcodeLocalGet]
          <> encodeU32 newPtrLocal
          <> rawI32Const runtimeClosureHeaderSize
          <> [opcodeI32Add]
          <> [opcodeLocalGet]
          <> encodeU32 calleeLocal
          <> rawI32Const runtimeClosureHeaderSize
          <> [opcodeI32Add]
          <> [opcodeLocalGet]
          <> encodeU32 captureCountLocal
          <> rawI32Const 2
          <> [opcodeI32Shl]
          <> memoryCopyInstr
          <> [opcodeEnd]
          <> [opcodeLocalGet]
          <> encodeU32 newPtrLocal
          <> rawI32Const runtimeClosureHeaderSize
          <> [opcodeI32Add]
          <> [opcodeLocalGet]
          <> encodeU32 captureCountLocal
          <> rawI32Const 2
          <> [opcodeI32Shl]
          <> [opcodeI32Add]
          <> [opcodeLocalGet]
          <> encodeU32 argLocal
          <> [opcodeI32Store]
          <> encodeMemArg 2 0
          <> [opcodeLocalGet]
          <> encodeU32 newPtrLocal
          <> [opcodeLocalSet]
          <> encodeU32 resultLocal
          <> [opcodeBr]
          <> encodeU32 1
      dispatchCase :: (Int, Int) -> [Word8]
      dispatchCase (arity, typeIx) =
        [opcodeLocalGet]
          <> encodeU32 totalArityLocal
          <> rawI32Const arity
          <> [opcodeI32Eq, opcodeIf, blockTypeEmpty]
          <> concatMap (loadClosureCaptureFromLocal calleeLocal) [0 .. arity - 2]
          <> [opcodeLocalGet]
          <> encodeU32 argLocal
          <> [opcodeLocalGet]
          <> encodeU32 fnIndexLocal
          <> encodeCallIndirect typeIx
          <> [opcodeLocalSet]
          <> encodeU32 resultLocal
          <> [opcodeBr]
          <> encodeU32 1
          <> [opcodeEnd]
      fullPath =
        [opcodeLocalGet]
          <> encodeU32 newCountLocal
          <> [opcodeLocalGet]
          <> encodeU32 totalArityLocal
          <> [opcodeI32Eq, opcodeIf, blockTypeEmpty]
          <> [opcodeBlock, blockTypeEmpty]
          <> concatMap dispatchCase closureCallTypeCases
          <> [opcodeUnreachable]
          <> [opcodeEnd]
          <> [opcodeBr]
          <> encodeU32 1
          <> [opcodeEnd]
   in calleeInstrs
        <> [opcodeLocalSet]
        <> encodeU32 calleeLocal
        <> argInstrs
        <> [opcodeLocalSet]
        <> encodeU32 argLocal
        <> trapIfTrue
          ( [opcodeLocalGet] <> encodeU32 calleeLocal
              <> rawI32Const 1
              <> [opcodeI32And]
              <> rawI32Const 0
              <> [opcodeI32Ne]
          )
        <> trapIfTrue
          ( [opcodeLocalGet] <> encodeU32 calleeLocal
              <> [opcodeI32Load]
              <> encodeMemArg 2 0
              <> rawI32Const runtimeClosureMagic
              <> [opcodeI32Ne]
          )
        <> [opcodeLocalGet]
        <> encodeU32 calleeLocal
        <> [opcodeI32Load]
        <> encodeMemArg 2 4
        <> [opcodeLocalSet]
        <> encodeU32 fnIndexLocal
        <> [opcodeLocalGet]
        <> encodeU32 calleeLocal
        <> [opcodeI32Load]
        <> encodeMemArg 2 8
        <> [opcodeLocalSet]
        <> encodeU32 totalArityLocal
        <> [opcodeLocalGet]
        <> encodeU32 calleeLocal
        <> [opcodeI32Load]
        <> encodeMemArg 2 12
        <> [opcodeLocalSet]
        <> encodeU32 captureCountLocal
        <> trapIfTrue
          ( [opcodeLocalGet] <> encodeU32 captureCountLocal
              <> rawI32Const 0
              <> [opcodeI32LtS]
          )
        <> [opcodeLocalGet]
        <> encodeU32 captureCountLocal
        <> rawI32Const 1
        <> [opcodeI32Add]
        <> [opcodeLocalSet]
        <> encodeU32 newCountLocal
        <> [opcodeBlock, blockTypeEmpty]
        <> [opcodeLocalGet]
        <> encodeU32 newCountLocal
        <> [opcodeLocalGet]
        <> encodeU32 totalArityLocal
        <> [opcodeI32LtS, opcodeIf, blockTypeEmpty]
        <> partialPath
        <> [opcodeEnd]
        <> fullPath
        <> [opcodeUnreachable]
        <> [opcodeEnd]
        <> [opcodeLocalGet]
        <> encodeU32 resultLocal

compileAtom :: StringLayout -> CollapsedFunction -> Atom -> Either String [Word8]
compileAtom stringLayout fn atom =
  case atom of
    AConstI32 n ->
      Right (rawI32Const (tagI32Immediate n))
    AConstString s ->
      case lookup s stringLayout of
        Nothing ->
          Left ("wasm backend: missing string literal layout for: " <> show s)
        Just (offset, _literalLength) ->
          Right (rawI32Const offset)
    ALocal i ->
      Right ([opcodeLocalGet] <> encodeU32 i)
    ATemp t ->
      Right ([opcodeLocalGet] <> encodeU32 (tempLocalIx fn t))
    AGlobal g ->
      case globalIndexFor g of
        Nothing ->
          Left ("wasm backend: unknown global atom: " <> g)
        Just globalIx ->
          Right ([opcodeGlobalGet] <> encodeU32 globalIx)

typeIndexFor :: [(Int, Int)] -> Int -> Either String Int
typeIndexFor typeMap arity =
  lookupRequiredInt ("wasm backend: missing type for arity " <> show arity) arity typeMap

runtimeImports :: [(Int, Int)] -> Either String [[Word8]]
runtimeImports _typeMap = Right []

encodeFunctionExport :: [(Name, Int)] -> CollapsedFunction -> Either String [Word8]
encodeFunctionExport funcMap fn = do
  ix <- lookupRequired ("wasm backend: missing export function index for " <> name fn) (name fn) funcMap
  pure (encodeName (name fn) <> [exportKindFunction] <> encodeU32 ix)

encodeActiveElementSegment :: Int -> [Int] -> [Word8]
encodeActiveElementSegment offset fnIndices =
  [0x00]
    <> rawI32Const offset
    <> [opcodeEnd]
    <> encodeVecU32 fnIndices

encodeActiveDataSegment :: Int -> [Word8] -> [Word8]
encodeActiveDataSegment offset payload =
  [0x00]
    <> rawI32Const offset
    <> [opcodeEnd]
    <> encodeVecBytes payload

encodeMutableI32Global :: Int -> [Word8]
encodeMutableI32Global initialValue =
  [ valTypeI32
  , globalMutVar
  ]
    <> rawI32Const initialValue
    <> [opcodeEnd]

trapIfTrue :: [Word8] -> [Word8]
trapIfTrue conditionInstrs =
  conditionInstrs
    <> [opcodeIf, blockTypeEmpty, opcodeUnreachable, opcodeEnd]

encodeCallIndirect :: Int -> [Word8]
encodeCallIndirect typeIx =
  [opcodeCallIndirect]
    <> encodeU32 typeIx
    <> encodeU32 0

memoryCopyInstr :: [Word8]
memoryCopyInstr = [0xfc, 0x0a, 0x00, 0x00]

memoryFillInstr :: [Word8]
memoryFillInstr = [0xfc, 0x0b, 0x00]

storeI32LocalAt :: Int -> Int -> Int -> [Word8]
storeI32LocalAt ptrLocal valueLocal offset =
  [opcodeLocalGet]
    <> encodeU32 ptrLocal
    <> [opcodeLocalGet]
    <> encodeU32 valueLocal
    <> [opcodeI32Store]
    <> encodeMemArg 2 offset

storeI32ConstAt :: Int -> Int -> Int -> [Word8]
storeI32ConstAt ptrLocal constValue offset =
  [opcodeLocalGet]
    <> encodeU32 ptrLocal
    <> rawI32Const constValue
    <> [opcodeI32Store]
    <> encodeMemArg 2 offset

loadClosureCaptureFromLocal :: Int -> Int -> [Word8]
loadClosureCaptureFromLocal calleeLocal captureIx =
  [opcodeLocalGet]
    <> encodeU32 calleeLocal
    <> [opcodeI32Load]
    <> encodeMemArg 2 (runtimeClosureHeaderSize + captureIx * 4)

emitInlineAllocFromLocals :: Int -> Int -> Int -> [Word8]
emitInlineAllocFromLocals sizeLocal alignLocal outPtrLocal =
  trapIfTrue
    ( [opcodeLocalGet] <> encodeU32 sizeLocal
        <> rawI32Const 0
        <> [opcodeI32LtS]
    )
    <> trapIfTrue
      ( [opcodeLocalGet] <> encodeU32 alignLocal
          <> rawI32Const 0
          <> [opcodeI32LeS]
      )
    <> [opcodeGlobalGet]
    <> encodeU32 runtimeHeapGlobal
    <> [opcodeLocalGet]
    <> encodeU32 alignLocal
    <> rawI32Const 1
    <> [opcodeI32Sub]
    <> [opcodeI32Add]
    <> [opcodeLocalGet]
    <> encodeU32 alignLocal
    <> [opcodeI32DivU]
    <> [opcodeLocalGet]
    <> encodeU32 alignLocal
    <> [opcodeI32Mul]
    <> [opcodeLocalSet]
    <> encodeU32 outPtrLocal
    <> [opcodeLocalGet]
    <> encodeU32 outPtrLocal
    <> [opcodeLocalGet]
    <> encodeU32 sizeLocal
    <> [opcodeI32Add]
    <> [opcodeLocalSet]
    <> encodeU32 sizeLocal
    <> [opcodeMemorySize, 0x00]
    <> rawI32Const 16
    <> [opcodeI32Shl]
    <> [opcodeLocalSet]
    <> encodeU32 alignLocal
    <> [opcodeLocalGet]
    <> encodeU32 sizeLocal
    <> [opcodeLocalGet]
    <> encodeU32 alignLocal
    <> [opcodeI32GtU, opcodeIf, blockTypeEmpty]
    <> [opcodeLocalGet]
    <> encodeU32 sizeLocal
    <> [opcodeLocalGet]
    <> encodeU32 alignLocal
    <> [opcodeI32Sub]
    <> rawI32Const (wasmPageSize - 1)
    <> [opcodeI32Add]
    <> rawI32Const 16
    <> [opcodeI32ShrU]
    <> [opcodeLocalSet]
    <> encodeU32 alignLocal
    <> trapIfTrue
      ( [opcodeLocalGet] <> encodeU32 alignLocal
          <> [opcodeMemoryGrow, 0x00]
          <> rawI32Const (-1)
          <> [opcodeI32Eq]
      )
    <> [opcodeEnd]
    <> [opcodeLocalGet]
    <> encodeU32 sizeLocal
    <> [opcodeGlobalSet]
    <> encodeU32 runtimeHeapGlobal
    <> [opcodeLocalGet]
    <> encodeU32 outPtrLocal

localParamCount :: CollapsedFunction -> Int
localParamCount fn = arity fn + captureArity fn

runtimeScratchLocalCount :: Int
runtimeScratchLocalCount = 13

scratchPtrLocalIx :: CollapsedFunction -> Int
scratchPtrLocalIx fn = localParamCount fn

scratchAuxLocalIx :: CollapsedFunction -> Int
scratchAuxLocalIx fn = localParamCount fn + 1

scratchTmpLocalIx :: CollapsedFunction -> Int
scratchTmpLocalIx fn = localParamCount fn + 2

applyCalleeLocalIx :: CollapsedFunction -> Int
applyCalleeLocalIx fn = localParamCount fn + 3

applyArgLocalIx :: CollapsedFunction -> Int
applyArgLocalIx fn = localParamCount fn + 4

applyFnIndexLocalIx :: CollapsedFunction -> Int
applyFnIndexLocalIx fn = localParamCount fn + 5

applyTotalArityLocalIx :: CollapsedFunction -> Int
applyTotalArityLocalIx fn = localParamCount fn + 6

applyCaptureCountLocalIx :: CollapsedFunction -> Int
applyCaptureCountLocalIx fn = localParamCount fn + 7

applyNewCountLocalIx :: CollapsedFunction -> Int
applyNewCountLocalIx fn = localParamCount fn + 8

applyNewPtrLocalIx :: CollapsedFunction -> Int
applyNewPtrLocalIx fn = localParamCount fn + 9

applyResultLocalIx :: CollapsedFunction -> Int
applyResultLocalIx fn = localParamCount fn + 10

applySizeLocalIx :: CollapsedFunction -> Int
applySizeLocalIx fn = localParamCount fn + 11

applyAlignLocalIx :: CollapsedFunction -> Int
applyAlignLocalIx fn = localParamCount fn + 12

tempLocalIx :: CollapsedFunction -> Int -> Int
tempLocalIx fn t = localParamCount fn + runtimeScratchLocalCount + t

duplicates :: [Name] -> [Name]
duplicates = reverse . go [] []
  where
    go :: [Name] -> [Name] -> [Name] -> [Name]
    go _seen acc [] = acc
    go seen acc (x:xs)
      | x `elem` seen && x `notElem` acc = go seen (x : acc) xs
      | x `elem` seen = go seen acc xs
      | otherwise = go (x : seen) acc xs

unique :: [Int] -> [Int]
unique = go []
  where
    go :: [Int] -> [Int] -> [Int]
    go _seen [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs

lookupRequired :: String -> Name -> [(Name, Int)] -> Either String Int
lookupRequired err key pairs =
  case lookup key pairs of
    Just v -> Right v
    Nothing -> Left err

lookupTagId :: TagIdMap -> Name -> Either String Int
lookupTagId tagIdMap tag =
  lookupRequired ("wasm backend: missing struct tag id for: " <> tag) tag tagIdMap

lookupRequiredInt :: String -> Int -> [(Int, Int)] -> Either String Int
lookupRequiredInt err key pairs =
  case lookup key pairs of
    Just v -> Right v
    Nothing -> Left err

wasmMagicVersion :: [Word8]
wasmMagicVersion =
  [ 0x00
  , 0x61
  , 0x73
  , 0x6d
  , 0x01
  , 0x00
  , 0x00
  , 0x00
  ]

sectionType :: Word8
sectionType = 1

sectionImport :: Word8
sectionImport = 2

sectionFunction :: Word8
sectionFunction = 3

sectionTable :: Word8
sectionTable = 4

sectionMemory :: Word8
sectionMemory = 5

sectionGlobal :: Word8
sectionGlobal = 6

sectionExport :: Word8
sectionExport = 7

sectionElement :: Word8
sectionElement = 9

sectionCode :: Word8
sectionCode = 10

sectionData :: Word8
sectionData = 11

exportKindFunction :: Word8
exportKindFunction = 0

exportKindTable :: Word8
exportKindTable = 1

exportKindGlobal :: Word8
exportKindGlobal = 3

exportKindMemory :: Word8
exportKindMemory = 2

refTypeFuncref :: Word8
refTypeFuncref = 0x70

valTypeI32 :: Word8
valTypeI32 = 0x7f

globalMutVar :: Word8
globalMutVar = 0x01

blockTypeEmpty :: Word8
blockTypeEmpty = 0x40

opcodeUnreachable :: Word8
opcodeUnreachable = 0x00

opcodeCallIndirect :: Word8
opcodeCallIndirect = 0x11

opcodeBlock :: Word8
opcodeBlock = 0x02

opcodeBr :: Word8
opcodeBr = 0x0c

opcodeLocalGet :: Word8
opcodeLocalGet = 0x20

opcodeLocalSet :: Word8
opcodeLocalSet = 0x21

opcodeGlobalGet :: Word8
opcodeGlobalGet = 0x23

opcodeGlobalSet :: Word8
opcodeGlobalSet = 0x24

opcodeCall :: Word8
opcodeCall = 0x10

opcodeI32Eq :: Word8
opcodeI32Eq = 0x46

opcodeI32Eqz :: Word8
opcodeI32Eqz = 0x45

opcodeI32LtS :: Word8
opcodeI32LtS = 0x48

opcodeI32GtS :: Word8
opcodeI32GtS = 0x4a

opcodeI32GtU :: Word8
opcodeI32GtU = 0x4b

opcodeI32LeS :: Word8
opcodeI32LeS = 0x4c

opcodeI32GeS :: Word8
opcodeI32GeS = 0x4e

opcodeI32Ne :: Word8
opcodeI32Ne = 0x47

opcodeI32Const :: Word8
opcodeI32Const = 0x41

opcodeI32Add :: Word8
opcodeI32Add = 0x6a

opcodeI32Sub :: Word8
opcodeI32Sub = 0x6b

opcodeI32Mul :: Word8
opcodeI32Mul = 0x6c

opcodeI32DivS :: Word8
opcodeI32DivS = 0x6d

opcodeI32RemS :: Word8
opcodeI32RemS = 0x6f

opcodeI32DivU :: Word8
opcodeI32DivU = 0x6e

opcodeI32And :: Word8
opcodeI32And = 0x71

opcodeI32Or :: Word8
opcodeI32Or = 0x72

opcodeI32Load :: Word8
opcodeI32Load = 0x28

opcodeI32Load8U :: Word8
opcodeI32Load8U = 0x2d

opcodeI32Store :: Word8
opcodeI32Store = 0x36

opcodeI32Store8 :: Word8
opcodeI32Store8 = 0x3a

opcodeI32Shl :: Word8
opcodeI32Shl = 0x74

opcodeI32ShrS :: Word8
opcodeI32ShrS = 0x75

opcodeI32ShrU :: Word8
opcodeI32ShrU = 0x76

opcodeMemorySize :: Word8
opcodeMemorySize = 0x3f

opcodeMemoryGrow :: Word8
opcodeMemoryGrow = 0x40

opcodeIf :: Word8
opcodeIf = 0x04

opcodeElse :: Word8
opcodeElse = 0x05

opcodeEnd :: Word8
opcodeEnd = 0x0b

opcodeReturnCall :: Word8
opcodeReturnCall = 0x13

runtimeClosureMagic :: Int
runtimeClosureMagic = 0x434c4f53

runtimeClosureHeaderSize :: Int
runtimeClosureHeaderSize = 16

runtimeStructMagic :: Int
runtimeStructMagic = 0x53545255

runtimeStructHeaderSize :: Int
runtimeStructHeaderSize = 12

wasmPageSize :: Int
wasmPageSize = 65536

runtimeImportCount :: Int
runtimeImportCount = 0

runtimeHeapGlobal :: Int
runtimeHeapGlobal = 0

globalIndexFor :: Name -> Maybe Int
globalIndexFor globalName =
  case globalName of
    "__heap_ptr" -> Just runtimeHeapGlobal
    "__heap" -> Just runtimeHeapGlobal
    _ -> Nothing

builtinImportSig :: Name -> Maybe (Int, Int)
builtinImportSig _ = Nothing

encodeSection :: Word8 -> [Word8] -> [Word8]
encodeSection sid payload = [sid] <> encodeU32 (length payload) <> payload

encodeFunctionType :: Int -> [Word8]
encodeFunctionType paramCount =
  [0x60]
    <> encodeVecBytes (replicate paramCount valTypeI32)
    <> encodeVecBytes [valTypeI32]

encodeLimitsMin :: Int -> [Word8]
encodeLimitsMin minLimit = [0x00] <> encodeU32 minLimit

encodeVecRaw :: [[Word8]] -> [Word8]
encodeVecRaw entries = encodeU32 (length entries) <> concat entries

encodeVecBytes :: [Word8] -> [Word8]
encodeVecBytes bytes = encodeU32 (length bytes) <> bytes

encodeVecU32 :: [Int] -> [Word8]
encodeVecU32 values = encodeU32 (length values) <> concatMap encodeU32 values

encodeName :: String -> [Word8]
encodeName s =
  let bytes = BS.unpack (TE.encodeUtf8 (T.pack s))
   in encodeVecBytes bytes

rawI32Const :: Int -> [Word8]
rawI32Const n = [opcodeI32Const] <> encodeI32 n

encodeMemArg :: Int -> Int -> [Word8]
encodeMemArg align offset = encodeU32 align <> encodeU32 offset

tagI32Immediate :: Int -> Int
tagI32Immediate n =
  let n32 = fromIntegral n :: Int32
      tagged32 = (n32 `shiftL` 1) .|. 1
   in fromIntegral tagged32

encodeU32 :: Int -> [Word8]
encodeU32 n
  | n < 0 =
      error ("wasm backend: internal error, negative u32 immediate: " <> show n)
  | otherwise = go n
  where
    go :: Int -> [Word8]
    go x =
      let byte = x .&. 0x7f
          rest = x `shiftR` 7
       in if rest == 0
            then [fromIntegral byte]
            else fromIntegral (byte .|. 0x80) : go rest

encodeI32 :: Int -> [Word8]
encodeI32 n = go n
  where
    go :: Int -> [Word8]
    go x =
      let byte = x .&. 0x7f
          signBitSet = (byte .&. 0x40) /= 0
          rest = x `shiftR` 7
          done = (rest == 0 && not signBitSet) || (rest == (-1) && signBitSet)
          out = fromIntegral (if done then byte else byte .|. 0x80)
       in if done then [out] else out : go rest

parseMkBuiltin :: Name -> Maybe (Name, Int)
parseMkBuiltin n = do
  rest <- stripPrefix "__mk_" n
  case parseMkDetailed rest of
    Just parsed -> Just parsed
    Nothing -> parseMkLegacy rest
  where
    parseMkDetailed :: String -> Maybe (Name, Int)
    parseMkDetailed src = do
      (prefix0, tparAndMap0) <- splitOnToken "_tpar_" src
      (tparRaw, mapRaw) <- splitOnToken "_fmap_" tparAndMap0
      (tag0, fieldCountRaw) <- splitLast '_' prefix0
      fieldCount0 <- parseNat fieldCountRaw
      _typeParamCount0 <- parseNat tparRaw
      fieldMap0 <- parseFieldMap mapRaw
      if fieldCount0 == length fieldMap0
        then Just (tag0, fieldCount0)
        else Nothing

    parseMkLegacy :: String -> Maybe (Name, Int)
    parseMkLegacy src = do
      (tag0, fieldCountRaw) <- splitLast '_' src
      fieldCount0 <- parseNat fieldCountRaw
      Just (tag0, fieldCount0)

parseGetBuiltin :: Name -> Maybe (Name, Int)
parseGetBuiltin n = do
  rest <- stripPrefix "__get_" n
  case parseGetDetailed rest of
    Just parsed -> Just parsed
    Nothing -> parseGetLegacy rest
  where
    parseGetDetailed :: String -> Maybe (Name, Int)
    parseGetDetailed src = do
      (prefix0, tparAndMap0) <- splitOnToken "_tpar_" src
      (tparRaw, mapRaw) <- splitOnToken "_fmap_" tparAndMap0
      (tag0, idxRaw) <- splitLast '_' prefix0
      idx0 <- parseNat idxRaw
      _typeParamCount0 <- parseNat tparRaw
      fieldMap0 <- parseFieldMap mapRaw
      if idx0 < length fieldMap0
        then Just (tag0, idx0)
        else Nothing

    parseGetLegacy :: String -> Maybe (Name, Int)
    parseGetLegacy src = do
      (tag0, idxRaw) <- splitLast '_' src
      idx0 <- parseNat idxRaw
      Just (tag0, idx0)

parseIsBuiltin :: Name -> Maybe Name
parseIsBuiltin n = do
  rest <- stripPrefix "__is_" n
  case parseIsDetailed rest of
    Just out -> Just out
    Nothing -> parseIsLegacy rest
  where
    parseIsDetailed :: Name -> Maybe Name
    parseIsDetailed src = do
      (tag0, tparAndMap0) <- splitOnToken "_tpar_" src
      (_tparRaw, _mapRaw) <- splitOnToken "_fmap_" tparAndMap0
      Just tag0

    parseIsLegacy :: Name -> Maybe Name
    parseIsLegacy src =
      if null src
        then Nothing
        else Just src

parseFieldMap :: String -> Maybe [Maybe Int]
parseFieldMap raw
  | raw == "none" = Just []
  | otherwise = traverse parseMaybeNat (splitOn '_' raw)

parseMaybeNat :: String -> Maybe (Maybe Int)
parseMaybeNat "u" = Just Nothing
parseMaybeNat s = Just <$> parseNat s

splitOnToken :: String -> String -> Maybe (String, String)
splitOnToken token src
  | null token = Nothing
  | otherwise = go [] src
  where
    go :: String -> String -> Maybe (String, String)
    go acc rest
      | startsWithToken token rest = Just (reverse acc, drop (length token) rest)
      | otherwise =
          case rest of
            [] -> Nothing
            c:cs -> go (c : acc) cs

startsWithToken :: String -> String -> Bool
startsWithToken [] _ = True
startsWithToken _ [] = False
startsWithToken (x:xs) (y:ys)
  | x == y = startsWithToken xs ys
  | otherwise = False

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys)
  | x == y = stripPrefix xs ys
  | otherwise = Nothing

splitLast :: Char -> String -> Maybe (String, String)
splitLast delim s =
  case reverse (splitOn delim s) of
    [] -> Nothing
    [_one] -> Nothing
    suffix:restRev ->
      let prefix = joinWith delim (reverse restRev)
       in if null prefix || null suffix
            then Nothing
            else Just (prefix, suffix)

splitOn :: Char -> String -> [String]
splitOn delim = go [] []
  where
    go :: [String] -> String -> String -> [String]
    go acc cur [] = reverse (reverse cur : acc)
    go acc cur (c:cs)
      | c == delim = go (reverse cur : acc) [] cs
      | otherwise = go acc (c : cur) cs

joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith delim (x:xs) = x <> [delim] <> joinWith delim xs

parseNat :: String -> Maybe Int
parseNat s
  | null s = Nothing
  | all isDigit s =
      case reads s of
        [(k, "")] -> Just k
        _ -> Nothing
  | otherwise = Nothing
