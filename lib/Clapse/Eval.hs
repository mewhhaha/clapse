module Clapse.Eval
  ( evalSourceFunction
  , evalCollapsedFunction
  , differentialCheckSourceCollapsed
  ) where

import Control.Monad (foldM)
import Data.Char (isDigit)

import qualified Clapse.CollapseIR as C
import Clapse.Lowering (lowerModule)
import qualified Clapse.Syntax as S

data SourceValue
  = SourceInt Int
  | SourceString String
  | SourceClosure SourceEnv [S.Name] S.Expr
  | SourceBuiltin S.Name Int [SourceValue]
  | SourceStruct S.Name [SourceValue]

-- A tiny lexical environment for pure source evaluation.
type SourceEnv = [(S.Name, SourceValue)]

data RuntimeValue
  = RuntimeInt Int
  | RuntimeString String
  | RuntimeClosure S.Name Int [RuntimeValue]
  | RuntimeStruct S.Name [RuntimeValue]

type FunctionEnv = [(S.Name, C.CollapsedFunction)]
type TempEnv = [(Int, RuntimeValue)]

evalSourceFunction :: S.Module -> S.Name -> [Int] -> Either String Int
evalSourceFunction modu entry args = do
  _ <- validateNoDuplicateFunctions (S.functions modu)
  let fullEnv = sourceUserEnv modu <> sourceBuiltins
  entryVal <- lookupRequired ("source evaluator: unknown function: " <> entry) entry fullEnv
  applied <- foldM applySourceValue entryVal (map SourceInt args)
  toSourceInt ("source evaluator: function did not reduce to Int: " <> entry) applied

differentialCheckSourceCollapsed :: S.Module -> S.Name -> [Int] -> Either String ()
differentialCheckSourceCollapsed modu entry args = do
  srcOut <- evalSourceFunction modu entry args
  lowered <- lowerModule modu
  collapsed <- C.collapseAndVerifyModule lowered
  collapsedOut <- evalCollapsedFunction collapsed entry args
  if srcOut == collapsedOut
    then Right ()
    else
      Left
        ( "differential mismatch in "
            <> entry
            <> ": source="
            <> show srcOut
            <> " collapsed="
            <> show collapsedOut
        )

evalCollapsedFunction :: [C.CollapsedFunction] -> S.Name -> [Int] -> Either String Int
evalCollapsedFunction allFns entry args = do
  env <- buildFunctionEnv allFns
  fn <- lookupRequired ("collapsed evaluator: unknown function: " <> entry) entry env
  out <- runFunction env fn (map RuntimeInt args)
  toRuntimeInt ("collapsed evaluator: function did not reduce to Int: " <> entry) out

sourceUserEnv :: S.Module -> SourceEnv
sourceUserEnv S.Module {S.functions = funs} = userEnv
  where
    userEnv = map mkBinding funs
    fullEnv = userEnv <> sourceBuiltins

    mkBinding :: S.Function -> (S.Name, SourceValue)
    mkBinding (S.Function fnName params bodyExpr) =
      (fnName, SourceClosure fullEnv params bodyExpr)

sourceBuiltins :: SourceEnv
sourceBuiltins =
  [ ("add", SourceBuiltin "add" 2 [])
  , ("sub", SourceBuiltin "sub" 2 [])
  , ("mul", SourceBuiltin "mul" 2 [])
  , ("div", SourceBuiltin "div" 2 [])
  , ("eq", SourceBuiltin "eq" 2 [])
  , ("and", SourceBuiltin "and" 2 [])
  , ("if", SourceBuiltin "if" 3 [])
  , ("pure", SourceBuiltin "pure" 1 [])
  , ("bind", SourceBuiltin "bind" 2 [])
  , ("fmap", SourceBuiltin "fmap" 2 [])
  , ("ap", SourceBuiltin "ap" 2 [])
  , ("slice_len", SourceBuiltin "slice_len" 1 [])
  , ("slice_get_u8", SourceBuiltin "slice_get_u8" 2 [])
  , ("slice_set_u8", SourceBuiltin "slice_set_u8" 3 [])
  , ("slice_new_u8", SourceBuiltin "slice_new_u8" 1 [])
  , ("slice_data_ptr", SourceBuiltin "slice_data_ptr" 1 [])
  , ("slice_len_raw", SourceBuiltin "slice_len_raw" 1 [])
  , ("region_mark", SourceBuiltin "region_mark" 1 [])
  , ("region_alloc", SourceBuiltin "region_alloc" 2 [])
  , ("region_reset", SourceBuiltin "region_reset" 1 [])
  , ("memcpy_u8", SourceBuiltin "memcpy_u8" 3 [])
  , ("memset_u8", SourceBuiltin "memset_u8" 3 [])
  , ("struct_tag", SourceBuiltin "struct_tag" 1 [])
  , ("collection_empty", SourceBuiltin "collection_empty" 1 [])
  , ("collection_extend", SourceBuiltin "collection_extend" 2 [])
  ]

applySourceValue :: SourceValue -> SourceValue -> Either String SourceValue
applySourceValue fnVal argVal =
  case fnVal of
    SourceInt _ ->
      Left "source evaluator: attempted to call Int value"
    SourceString _ ->
      Left "source evaluator: attempted to call string value"
    SourceStruct _ _ ->
      Left "source evaluator: attempted to call struct value"
    SourceBuiltin builtinName builtinArity gotArgs -> do
      let nextArgs = gotArgs <> [argVal]
      if length nextArgs < builtinArity
        then Right (SourceBuiltin builtinName builtinArity nextArgs)
        else if length nextArgs == builtinArity
          then evalSourceBuiltin builtinName nextArgs
          else Left "source evaluator: builtin over-applied"
    SourceClosure env params bodyExpr ->
      case params of
        [] -> do
          forced <- evalSourceExpr env bodyExpr
          applySourceValue forced argVal
        p:ps -> do
          let env' = (p, argVal) : env
          if null ps
            then evalSourceExpr env' bodyExpr
            else Right (SourceClosure env' ps bodyExpr)

evalSourceExpr :: SourceEnv -> S.Expr -> Either String SourceValue
evalSourceExpr env expr =
  case expr of
    S.IntLit n ->
      Right (SourceInt n)
    S.StringLit s ->
      Right (SourceString s)
    S.CollectionLit elems ->
      evalSourceExpr env (desugarCollectionExpr elems)
    S.Case _ _ ->
      evalSourceExpr env (S.desugarCaseExpr expr)
    S.Var n ->
      case lookup n env of
        Just v ->
          Right v
        Nothing ->
          case sourceBuiltinArity n of
            Just arityN -> Right (SourceBuiltin n arityN [])
            Nothing -> Left ("source evaluator: unknown variable: " <> n)
    S.Lam n bodyExpr ->
      Right (SourceClosure env [n] bodyExpr)
    S.App f x -> do
      fnVal <- evalSourceExpr env f
      argVal <- evalSourceExpr env x
      applySourceValue fnVal argVal

evalSourceBuiltin :: S.Name -> [SourceValue] -> Either String SourceValue
evalSourceBuiltin name args =
  case (name, args) of
    ("add", [a, b]) ->
      SourceInt <$> ((+) <$> toSourceInt "source evaluator: add lhs" a <*> toSourceInt "source evaluator: add rhs" b)
    ("sub", [a, b]) ->
      SourceInt <$> ((-) <$> toSourceInt "source evaluator: sub lhs" a <*> toSourceInt "source evaluator: sub rhs" b)
    ("mul", [a, b]) ->
      SourceInt <$> ((*) <$> toSourceInt "source evaluator: mul lhs" a <*> toSourceInt "source evaluator: mul rhs" b)
    ("div", [a, b]) -> do
      lhs <- toSourceInt "source evaluator: div lhs" a
      rhs <- toSourceInt "source evaluator: div rhs" b
      if rhs == 0
        then Left "source evaluator: division by zero"
        else Right (SourceInt (lhs `div` rhs))
    ("eq", [a, b]) -> do
      lhs <- toSourceInt "source evaluator: eq lhs" a
      rhs <- toSourceInt "source evaluator: eq rhs" b
      Right (SourceInt (if lhs == rhs then 1 else 0))
    ("and", [a, b]) -> do
      lhs <- toSourceInt "source evaluator: and lhs" a
      rhs <- toSourceInt "source evaluator: and rhs" b
      Right (SourceInt (if lhs /= 0 && rhs /= 0 then 1 else 0))
    ("if", [cond, whenTrue, whenFalse]) -> do
      condI <- toSourceInt "source evaluator: if condition" cond
      let chosen = if condI /= 0 then whenTrue else whenFalse
      applySourceValue chosen (SourceInt 0)
    ("pure", [x]) ->
      Right x
    ("bind", [m, f]) ->
      applySourceValue f m
    ("fmap", [f, x]) ->
      applySourceValue f x
    ("ap", [f, x]) ->
      applySourceValue f x
    ("slice_len", [_slice]) ->
      Left "source evaluator: slice_len requires host-provided slice value"
    ("slice_get_u8", [_slice, _index]) ->
      Left "source evaluator: slice_get_u8 requires host-provided slice value"
    ("slice_set_u8", [_slice, _index, _value]) ->
      Left "source evaluator: slice_set_u8 requires host-provided slice value"
    ("slice_new_u8", [_len]) ->
      Left "source evaluator: slice_new_u8 requires wasm runtime memory"
    ("slice_data_ptr", [_slice]) ->
      Left "source evaluator: slice_data_ptr requires wasm runtime memory"
    ("slice_len_raw", [_slice]) ->
      Left "source evaluator: slice_len_raw requires wasm runtime memory"
    ("region_mark", [_ignored]) ->
      Left "source evaluator: region_mark requires wasm runtime memory"
    ("region_alloc", [_sizeBytes, _alignBytes]) ->
      Left "source evaluator: region_alloc requires wasm runtime memory"
    ("region_reset", [_mark]) ->
      Left "source evaluator: region_reset requires wasm runtime memory"
    ("memcpy_u8", [_destPtr, _srcPtr, _lenBytes]) ->
      Left "source evaluator: memcpy_u8 requires wasm runtime memory"
    ("memset_u8", [_destPtr, _value, _lenBytes]) ->
      Left "source evaluator: memset_u8 requires wasm runtime memory"
    ("struct_tag", [SourceStruct _tag _fields]) ->
      Left "source evaluator: struct_tag is only available after collapse/lowering"
    ("struct_tag", [_]) ->
      Left "source evaluator: struct_tag expects struct value"
    ("collection_empty", [_ignored]) ->
      Right (SourceStruct "collection_empty" [])
    ("collection_extend", [collectionVal, x]) ->
      Right (SourceStruct "collection_node" [collectionVal, x])
    _ ->
      case parseMkBuiltin name of
        Just (tag, arityN)
          | arityN == length args -> Right (SourceStruct tag args)
          | otherwise ->
              Left
                ( "source evaluator: constructor arity mismatch for "
                    <> name
                    <> ", expected "
                    <> show arityN
                    <> ", got "
                    <> show (length args)
                )
        Nothing ->
          case parseGetBuiltin name of
            Just (tag, idx) ->
              evalSourceGetter tag idx args
            Nothing ->
              case parseIsBuiltin name of
                Just expectedTag ->
                  evalSourceIsTag expectedTag args
                Nothing ->
                  Left ("source evaluator: unsupported builtin or arity: " <> name)

evalSourceGetter :: S.Name -> Int -> [SourceValue] -> Either String SourceValue
evalSourceGetter expectedTag idx args =
  case args of
    [SourceStruct tag fields]
      | tag /= expectedTag ->
          Left
            ( "source evaluator: struct tag mismatch, expected "
                <> expectedTag
                <> ", got "
                <> tag
            )
      | idx < 0 || idx >= length fields ->
          Left
            ( "source evaluator: struct field index out of range: "
                <> show idx
            )
      | otherwise ->
          Right (fields !! idx)
    [_] ->
      Left "source evaluator: getter expected struct value"
    _ ->
      Left "source evaluator: getter expected exactly one argument"

evalSourceIsTag :: S.Name -> [SourceValue] -> Either String SourceValue
evalSourceIsTag expectedTag args =
  case args of
    [SourceStruct tag _fields] ->
      Right (SourceInt (if tag == expectedTag then 1 else 0))
    [_] ->
      Right (SourceInt 0)
    _ ->
      Left "source evaluator: tag predicate expected exactly one argument"

toSourceInt :: String -> SourceValue -> Either String Int
toSourceInt err val =
  case val of
    SourceInt n -> Right n
    _ -> Left err

runFunction :: FunctionEnv -> C.CollapsedFunction -> [RuntimeValue] -> Either String RuntimeValue
runFunction env fn args = do
  let expected = localParamCount fn
  if length args == expected
    then evalBinds env fn args [] (C.binds fn)
    else
      Left
        ( "collapsed evaluator: function "
            <> C.name fn
            <> " expected "
            <> show expected
            <> " arg(s), got "
            <> show (length args)
        )

evalBinds :: FunctionEnv -> C.CollapsedFunction -> [RuntimeValue] -> TempEnv -> [C.Bind] -> Either String RuntimeValue
evalBinds env fn locals temps allBinds =
  case allBinds of
    [] ->
      evalAtom locals temps (C.result fn)
    b:bs -> do
      v <- evalValue env fn locals temps (C.value b)
      evalBinds env fn locals ((C.temp b, v) : temps) bs

evalValue :: FunctionEnv -> C.CollapsedFunction -> [RuntimeValue] -> TempEnv -> C.Value -> Either String RuntimeValue
evalValue env fn locals temps val =
  case val of
    C.VClosure callee captures -> do
      calleeFn <- lookupRequired ("collapsed evaluator: unknown closure callee: " <> callee) callee env
      captureVals <- traverse (evalAtom locals temps) captures
      let totalArity = localParamCount calleeFn
      Right (RuntimeClosure callee totalArity captureVals)
    C.VCallDirect callee args -> do
      argVals <- traverse (evalAtom locals temps) args
      callNamed env callee argVals
    C.VCurryDirect callee args -> do
      calleeFn <- lookupRequired ("collapsed evaluator: unknown curry callee: " <> callee) callee env
      argVals <- traverse (evalAtom locals temps) args
      let totalArity = localParamCount calleeFn
      if length argVals < totalArity
        then Right (RuntimeClosure callee totalArity argVals)
        else if length argVals == totalArity
          then runFunction env calleeFn argVals
          else Left "collapsed evaluator: curry over-application"
    C.VCallClosure callee args -> do
      calleeVal <- evalAtom locals temps callee
      argVals <- traverse (evalAtom locals temps) args
      foldM (applyRuntimeValue env) calleeVal argVals
    C.VApply callee arg -> do
      calleeVal <- evalAtom locals temps callee
      argVal <- evalAtom locals temps arg
      applyRuntimeValue env calleeVal argVal
    C.VSelfTailCall args -> do
      argVals <- traverse (evalAtom locals temps) args
      let captures = take (C.captureArity fn) locals
      runFunction env fn (captures <> argVals)

evalAtom :: [RuntimeValue] -> TempEnv -> C.Atom -> Either String RuntimeValue
evalAtom locals temps atom =
  case atom of
    C.AConstI32 n ->
      Right (RuntimeInt n)
    C.AConstString s ->
      Right (RuntimeString s)
    C.ALocal i ->
      lookupIndex ("collapsed evaluator: local index out of range: " <> show i) i locals
    C.ATemp t ->
      lookupRequired ("collapsed evaluator: unknown temp: t" <> show t) t temps
    C.AGlobal g ->
      Left ("collapsed evaluator: globals are not supported: " <> g)

applyRuntimeValue :: FunctionEnv -> RuntimeValue -> RuntimeValue -> Either String RuntimeValue
applyRuntimeValue env fnVal argVal =
  case fnVal of
    RuntimeInt _ ->
      Left "collapsed evaluator: attempted to call Int value"
    RuntimeString _ ->
      Left "collapsed evaluator: attempted to call string value"
    RuntimeStruct _ _ ->
      Left "collapsed evaluator: attempted to call struct value"
    RuntimeClosure callee totalArity boundArgs -> do
      calleeFn <- lookupRequired ("collapsed evaluator: closure target missing: " <> callee) callee env
      let nextArgs = boundArgs <> [argVal]
      if length nextArgs < totalArity
        then Right (RuntimeClosure callee totalArity nextArgs)
        else if length nextArgs == totalArity
          then runFunction env calleeFn nextArgs
          else Left "collapsed evaluator: closure over-application"

callNamed :: FunctionEnv -> S.Name -> [RuntimeValue] -> Either String RuntimeValue
callNamed env callee args =
  case lookup callee env of
    Just fn ->
      runFunction env fn args
    Nothing ->
      evalRuntimeBuiltin env callee args

evalRuntimeBuiltin :: FunctionEnv -> S.Name -> [RuntimeValue] -> Either String RuntimeValue
evalRuntimeBuiltin env name args =
  case (name, args) of
    ("add", [a, b]) ->
      RuntimeInt <$> ((+) <$> toRuntimeInt "collapsed evaluator: add lhs" a <*> toRuntimeInt "collapsed evaluator: add rhs" b)
    ("sub", [a, b]) ->
      RuntimeInt <$> ((-) <$> toRuntimeInt "collapsed evaluator: sub lhs" a <*> toRuntimeInt "collapsed evaluator: sub rhs" b)
    ("mul", [a, b]) ->
      RuntimeInt <$> ((*) <$> toRuntimeInt "collapsed evaluator: mul lhs" a <*> toRuntimeInt "collapsed evaluator: mul rhs" b)
    ("div", [a, b]) -> do
      lhs <- toRuntimeInt "collapsed evaluator: div lhs" a
      rhs <- toRuntimeInt "collapsed evaluator: div rhs" b
      if rhs == 0
        then Left "collapsed evaluator: division by zero"
        else Right (RuntimeInt (lhs `div` rhs))
    ("eq", [a, b]) -> do
      lhs <- toRuntimeInt "collapsed evaluator: eq lhs" a
      rhs <- toRuntimeInt "collapsed evaluator: eq rhs" b
      Right (RuntimeInt (if lhs == rhs then 1 else 0))
    ("and", [a, b]) -> do
      lhs <- toRuntimeInt "collapsed evaluator: and lhs" a
      rhs <- toRuntimeInt "collapsed evaluator: and rhs" b
      Right (RuntimeInt (if lhs /= 0 && rhs /= 0 then 1 else 0))
    ("if", [cond, whenTrue, whenFalse]) -> do
      condI <- toRuntimeInt "collapsed evaluator: if condition" cond
      let chosen = if condI /= 0 then whenTrue else whenFalse
      applyRuntimeValue env chosen (RuntimeInt 0)
    ("pure", [x]) ->
      Right x
    ("bind", [m, f]) ->
      applyRuntimeValue env f m
    ("fmap", [f, x]) ->
      applyRuntimeValue env f x
    ("ap", [f, x]) ->
      applyRuntimeValue env f x
    ("slice_len", [_slice]) ->
      Left "collapsed evaluator: slice_len requires host-provided slice value"
    ("slice_get_u8", [_slice, _index]) ->
      Left "collapsed evaluator: slice_get_u8 requires host-provided slice value"
    ("slice_set_u8", [_slice, _index, _value]) ->
      Left "collapsed evaluator: slice_set_u8 requires host-provided slice value"
    ("slice_new_u8", [_len]) ->
      Left "collapsed evaluator: slice_new_u8 requires wasm runtime memory"
    ("slice_data_ptr", [_slice]) ->
      Left "collapsed evaluator: slice_data_ptr requires wasm runtime memory"
    ("slice_len_raw", [_slice]) ->
      Left "collapsed evaluator: slice_len_raw requires wasm runtime memory"
    ("region_mark", [_ignored]) ->
      Left "collapsed evaluator: region_mark requires wasm runtime memory"
    ("region_alloc", [_sizeBytes, _alignBytes]) ->
      Left "collapsed evaluator: region_alloc requires wasm runtime memory"
    ("region_reset", [_mark]) ->
      Left "collapsed evaluator: region_reset requires wasm runtime memory"
    ("memcpy_u8", [_destPtr, _srcPtr, _lenBytes]) ->
      Left "collapsed evaluator: memcpy_u8 requires wasm runtime memory"
    ("memset_u8", [_destPtr, _value, _lenBytes]) ->
      Left "collapsed evaluator: memset_u8 requires wasm runtime memory"
    ("struct_tag", [_]) ->
      Left "collapsed evaluator: struct_tag requires wasm runtime tag table"
    ("collection_empty", [_ignored]) ->
      Right (RuntimeStruct "collection_empty" [])
    ("collection_extend", [collectionVal, x]) ->
      Right (RuntimeStruct "collection_node" [collectionVal, x])
    _ ->
      case parseMkBuiltin name of
        Just (tag, arityN)
          | arityN == length args -> Right (RuntimeStruct tag args)
          | otherwise ->
              Left
                ( "collapsed evaluator: constructor arity mismatch for "
                    <> name
                    <> ", expected "
                    <> show arityN
                    <> ", got "
                    <> show (length args)
                )
        Nothing ->
          case parseGetBuiltin name of
            Just (tag, idx) ->
              evalRuntimeGetter tag idx args
            Nothing ->
              case parseIsBuiltin name of
                Just expectedTag ->
                  evalRuntimeIsTag expectedTag args
                Nothing ->
                  Left ("collapsed evaluator: unknown callee or arity: " <> name)

evalRuntimeGetter :: S.Name -> Int -> [RuntimeValue] -> Either String RuntimeValue
evalRuntimeGetter expectedTag idx args =
  case args of
    [RuntimeStruct tag fields]
      | tag /= expectedTag ->
          Left
            ( "collapsed evaluator: struct tag mismatch, expected "
                <> expectedTag
                <> ", got "
                <> tag
            )
      | idx < 0 || idx >= length fields ->
          Left
            ( "collapsed evaluator: struct field index out of range: "
                <> show idx
            )
      | otherwise ->
          Right (fields !! idx)
    [_] ->
      Left "collapsed evaluator: getter expected struct value"
    _ ->
      Left "collapsed evaluator: getter expected exactly one argument"

evalRuntimeIsTag :: S.Name -> [RuntimeValue] -> Either String RuntimeValue
evalRuntimeIsTag expectedTag args =
  case args of
    [RuntimeStruct tag _fields] ->
      Right (RuntimeInt (if tag == expectedTag then 1 else 0))
    [_] ->
      Right (RuntimeInt 0)
    _ ->
      Left "collapsed evaluator: tag predicate expected exactly one argument"

toRuntimeInt :: String -> RuntimeValue -> Either String Int
toRuntimeInt err val =
  case val of
    RuntimeInt n -> Right n
    _ -> Left err

localParamCount :: C.CollapsedFunction -> Int
localParamCount fn = C.arity fn + C.captureArity fn

buildFunctionEnv :: [C.CollapsedFunction] -> Either String FunctionEnv
buildFunctionEnv allFns = go [] [] (flatten allFns)
  where
    flatten :: [C.CollapsedFunction] -> [C.CollapsedFunction]
    flatten = concatMap flattenOne

    flattenOne :: C.CollapsedFunction -> [C.CollapsedFunction]
    flattenOne fn = fn : flatten (C.lifted fn)

    go :: [S.Name] -> FunctionEnv -> [C.CollapsedFunction] -> Either String FunctionEnv
    go _seen acc [] = Right (reverse acc)
    go seen acc (fn:rest)
      | C.name fn `elem` seen =
          Left ("collapsed evaluator: duplicate function name: " <> C.name fn)
      | otherwise =
          go (C.name fn : seen) ((C.name fn, fn) : acc) rest

validateNoDuplicateFunctions :: [S.Function] -> Either String ()
validateNoDuplicateFunctions funs =
  case duplicates (map fnName funs) of
    [] -> Right ()
    dup:_ -> Left ("source evaluator: duplicate function name: " <> dup)
  where
    fnName :: S.Function -> S.Name
    fnName (S.Function n _ _) = n

lookupIndex :: String -> Int -> [a] -> Either String a
lookupIndex err idx xs
  | idx < 0 = Left err
  | otherwise =
      case drop idx xs of
        y:_ -> Right y
        [] -> Left err

lookupRequired :: Eq k => String -> k -> [(k, v)] -> Either String v
lookupRequired err key pairs =
  case lookup key pairs of
    Just v -> Right v
    Nothing -> Left err

duplicates :: [S.Name] -> [S.Name]
duplicates = reverse . go [] []
  where
    go :: [S.Name] -> [S.Name] -> [S.Name] -> [S.Name]
    go _seen dups [] = dups
    go seen dups (x:xs)
      | x `elem` seen && x `notElem` dups = go seen (x : dups) xs
      | x `elem` seen = go seen dups xs
      | otherwise = go (x : seen) dups xs

parseMkBuiltin :: S.Name -> Maybe (S.Name, Int)
parseMkBuiltin n = do
  rest <- stripPrefix "__mk_" n
  case parseMkDetailed rest of
    Just parsed -> Just parsed
    Nothing -> parseMkLegacy rest
  where
    parseMkDetailed :: String -> Maybe (S.Name, Int)
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

    parseMkLegacy :: String -> Maybe (S.Name, Int)
    parseMkLegacy src = do
      (tag0, fieldCountRaw) <- splitLast '_' src
      fieldCount0 <- parseNat fieldCountRaw
      Just (tag0, fieldCount0)

parseGetBuiltin :: S.Name -> Maybe (S.Name, Int)
parseGetBuiltin n = do
  rest <- stripPrefix "__get_" n
  case parseGetDetailed rest of
    Just parsed -> Just parsed
    Nothing -> parseGetLegacy rest
  where
    parseGetDetailed :: String -> Maybe (S.Name, Int)
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

    parseGetLegacy :: String -> Maybe (S.Name, Int)
    parseGetLegacy src = do
      (tag0, idxRaw) <- splitLast '_' src
      idx0 <- parseNat idxRaw
      Just (tag0, idx0)

parseFieldMap :: String -> Maybe [Maybe Int]
parseFieldMap raw
  | raw == "none" = Just []
  | otherwise = traverse parseMaybeNat (splitOn '_' raw)

parseMaybeNat :: String -> Maybe (Maybe Int)
parseMaybeNat "u" = Just Nothing
parseMaybeNat s = Just <$> parseNat s

parseIsBuiltin :: S.Name -> Maybe S.Name
parseIsBuiltin n = do
  rest <- stripPrefix "__is_" n
  case parseIsDetailed rest of
    Just out -> Just out
    Nothing -> parseIsLegacy rest
  where
    parseIsDetailed :: S.Name -> Maybe S.Name
    parseIsDetailed src = do
      (tag0, tparAndMap0) <- splitOnToken "_tpar_" src
      (_tparRaw, _mapRaw) <- splitOnToken "_fmap_" tparAndMap0
      Just tag0

    parseIsLegacy :: S.Name -> Maybe S.Name
    parseIsLegacy src =
      if null src
        then Nothing
        else Just src

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
        [(n, "")] -> Just n
        _ -> Nothing
  | otherwise = Nothing

sourceBuiltinArity :: S.Name -> Maybe Int
sourceBuiltinArity n =
  case n of
    "add" -> Just 2
    "sub" -> Just 2
    "mul" -> Just 2
    "div" -> Just 2
    "eq" -> Just 2
    "and" -> Just 2
    "if" -> Just 3
    "pure" -> Just 1
    "bind" -> Just 2
    "fmap" -> Just 2
    "ap" -> Just 2
    "slice_len" -> Just 1
    "slice_get_u8" -> Just 2
    "slice_set_u8" -> Just 3
    "slice_new_u8" -> Just 1
    "slice_data_ptr" -> Just 1
    "slice_len_raw" -> Just 1
    "region_mark" -> Just 1
    "region_alloc" -> Just 2
    "region_reset" -> Just 1
    "memcpy_u8" -> Just 3
    "memset_u8" -> Just 3
    "struct_tag" -> Just 1
    "collection_empty" -> Just 1
    "collection_extend" -> Just 2
    _ ->
      case parseMkBuiltin n of
        Just (_tag, arityN) -> Just arityN
        Nothing ->
          case parseGetBuiltin n of
            Just (_tag, _idx) -> Just 1
            Nothing ->
              case parseIsBuiltin n of
                Just _tag -> Just 1
                Nothing -> Nothing

desugarCollectionExpr :: [S.Expr] -> S.Expr
desugarCollectionExpr elems =
  let emptyExpr = S.App (S.Var "collection_empty") (S.IntLit 0)
   in foldl (\acc elemExpr -> S.App (S.App (S.Var "collection_extend") acc) elemExpr) emptyExpr elems
