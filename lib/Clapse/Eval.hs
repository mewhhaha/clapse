module Clapse.Eval
  ( evalSourceFunction
  , evalCollapsedFunction
  , differentialCheckSourceCollapsed
  ) where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Int (Int32)
import qualified Data.ByteString as BS

import qualified Clapse.CollapseIR as C
import Clapse.Lowering (lowerModule)
import qualified Clapse.Syntax as S

data SourceValue
  = SourceInt Int
  | SourceString String
  | SourceSlice String
  | SourceClosure SourceEnv [S.Name] S.Expr
  | SourceBuiltin S.Name Int [SourceValue]
  | SourceStruct S.Name [SourceValue]

-- A tiny lexical environment for pure source evaluation.
type SourceEnv = [(S.Name, SourceValue)]

data RuntimeValue
  = RuntimeInt Int
  | RuntimeString String
  | RuntimeSlice String
  | RuntimeClosure S.Name Int [RuntimeValue]
  | RuntimeStruct S.Name [RuntimeValue]

data EvalState = EvalState
  { evalRegionPtr :: !Int
  , evalRegionMarks :: ![Int]
  }

initialEvalState :: EvalState
initialEvalState =
  EvalState
    { evalRegionPtr = 0
    , evalRegionMarks = []
    }

newtype EvalM a = EvalM {runEvalM :: EvalState -> Either String (a, EvalState)}

evalPure :: a -> EvalM a
evalPure a = EvalM (\state -> Right (a, state))

evalFail :: String -> EvalM a
evalFail err = EvalM (\_ -> Left err)

evalGetState :: EvalM EvalState
evalGetState = EvalM (\state -> Right (state, state))

evalSetState :: EvalState -> EvalM ()
evalSetState state = EvalM (\_ -> Right ((), state))

instance Functor EvalM where
  fmap f ma = EvalM (\state -> do (a, state') <- runEvalM ma state; pure (f a, state'))

instance Applicative EvalM where
  pure = evalPure
  mf <*> mx =
    EvalM
      ( \state0 -> do
          (f, state1) <- runEvalM mf state0
          (x, state2) <- runEvalM mx state1
          pure (f x, state2)
      )

instance Monad EvalM where
  ma >>= f = EvalM (\state0 -> do
    (a, state1) <- runEvalM ma state0
    runEvalM (f a) state1
    )

type FunctionEnv = [(S.Name, C.CollapsedFunction)]
type TempEnv = [(Int, RuntimeValue)]

evalSourceFunction :: S.Module -> S.Name -> [Int] -> Either String Int
evalSourceFunction modu entry args = do
  _ <- validateNoDuplicateFunctions (S.functions modu)
  let fullEnv = sourceUserEnv modu <> sourceBuiltins
  entryVal <- lookupRequired ("source evaluator: unknown function: " <> entry) entry fullEnv
  (applied, _) <-
    runEvalM
      ( do
          applied0 <- foldM applySourceValue entryVal (map SourceInt args)
          applied1 <- applyUnusedArityDefaults applied0
          forceSourceValue applied1
      )
      initialEvalState
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
  (out, _) <- runEvalM (runFunction env fn (map RuntimeInt args)) initialEvalState
  toRuntimeInt ("collapsed evaluator: function did not reduce to Int: " <> entry) out

sourceUserEnv :: S.Module -> SourceEnv
sourceUserEnv S.Module {S.functions = funs} = userEnv
  where
    userEnv = map mkBinding funs
    fullEnv = userEnv <> sourceBuiltins

    mkBinding :: S.Function -> (S.Name, SourceValue)
    mkBinding (S.Function fnName params bodyExpr _) =
      (fnName, SourceClosure fullEnv params bodyExpr)

sourceBuiltins :: SourceEnv
sourceBuiltins =
  [ ("add", SourceBuiltin "add" 2 [])
  , ("sub", SourceBuiltin "sub" 2 [])
  , ("mul", SourceBuiltin "mul" 2 [])
  , ("div", SourceBuiltin "div" 2 [])
  , ("mod", SourceBuiltin "mod" 2 [])
  , ("eq", SourceBuiltin "eq" 2 [])
  , ("lt", SourceBuiltin "lt" 2 [])
  , ("le", SourceBuiltin "le" 2 [])
  , ("gt", SourceBuiltin "gt" 2 [])
  , ("ge", SourceBuiltin "ge" 2 [])
  , ("str_eq", SourceBuiltin "str_eq" 2 [])
  , ("str_to_slice", SourceBuiltin "str_to_slice" 1 [])
  , ("slice_to_string", SourceBuiltin "slice_to_string" 1 [])
  , ("slice_eq_u8", SourceBuiltin "slice_eq_u8" 2 [])
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

applySourceValue :: SourceValue -> SourceValue -> EvalM SourceValue
applySourceValue fnVal argVal =
  case fnVal of
    SourceInt _ ->
      evalFail "source evaluator: attempted to call Int value"
    SourceString _ ->
      evalFail "source evaluator: attempted to call string value"
    SourceSlice _ ->
      evalFail "source evaluator: attempted to call slice value"
    SourceStruct _ _ ->
      evalFail "source evaluator: attempted to call struct value"
    SourceBuiltin builtinName builtinArity gotArgs -> do
      let nextArgs = gotArgs <> [argVal]
      if length nextArgs < builtinArity
        then evalPure (SourceBuiltin builtinName builtinArity nextArgs)
        else if length nextArgs == builtinArity
          then evalSourceBuiltin builtinName nextArgs
          else evalFail "source evaluator: builtin over-applied"
    SourceClosure env params bodyExpr ->
      case params of
        [] -> do
          forced <- evalSourceExpr env bodyExpr
          applySourceValue forced argVal
        p:ps -> do
          let env' = (p, argVal) : env
          if null ps
            then evalSourceExpr env' bodyExpr
            else evalPure (SourceClosure env' ps bodyExpr)

evalSourceExpr :: SourceEnv -> S.Expr -> EvalM SourceValue
evalSourceExpr env expr =
  case expr of
    S.IntLit n ->
      evalPure (SourceInt n)
    S.StringLit s ->
      evalPure (SourceString s)
    S.CollectionLit elems ->
      evalSourceExpr env (desugarCollectionExpr elems)
    S.Case _ _ ->
      evalSourceExpr env (S.desugarCaseExpr expr)
    S.Var n ->
      case lookup n env of
        Just v ->
          forceSourceValue v
        Nothing ->
          case sourceBuiltinArity n of
            Just arityN -> forceSourceValue (SourceBuiltin n arityN [])
            Nothing -> evalFail ("source evaluator: unknown variable: " <> n)
    S.Lam n bodyExpr ->
      evalPure (SourceClosure env [n] bodyExpr)
    S.App f x -> do
      fnVal <- evalSourceExpr env f
      argVal <- evalSourceExpr env x
      applySourceValue fnVal argVal

forceSourceValue :: SourceValue -> EvalM SourceValue
forceSourceValue val =
  case val of
    SourceClosure env [] bodyExpr ->
      evalSourceExpr env bodyExpr >>= forceSourceValue
    SourceBuiltin builtinName 0 gotArgs
      | null gotArgs ->
          evalSourceBuiltin builtinName [] >>= forceSourceValue
    _ ->
      evalPure val

evalSourceBuiltin :: S.Name -> [SourceValue] -> EvalM SourceValue
evalSourceBuiltin name args =
  case (name, args) of
    ("add", [a, b]) ->
      do
        lhs <- toSourceIntM "source evaluator: add lhs" a
        rhs <- toSourceIntM "source evaluator: add rhs" b
        evalPure (SourceInt (taggedArith2 (+) lhs rhs))
    ("sub", [a, b]) ->
      do
        lhs <- toSourceIntM "source evaluator: sub lhs" a
        rhs <- toSourceIntM "source evaluator: sub rhs" b
        evalPure (SourceInt (taggedArith2 (-) lhs rhs))
    ("mul", [a, b]) ->
      do
        lhs <- toSourceIntM "source evaluator: mul lhs" a
        rhs <- toSourceIntM "source evaluator: mul rhs" b
        evalPure (SourceInt (taggedArith2 (*) lhs rhs))
    ("div", [a, b]) -> do
      lhs <- toSourceIntM "source evaluator: div lhs" a
      rhs <- toSourceIntM "source evaluator: div rhs" b
      if rhs == 0
        then evalFail "source evaluator: division by zero"
        else evalPure (SourceInt (taggedArith2 quot lhs rhs))
    ("mod", [a, b]) -> do
      lhs <- toSourceIntM "source evaluator: mod lhs" a
      rhs <- toSourceIntM "source evaluator: mod rhs" b
      if rhs == 0
        then evalFail "source evaluator: modulo by zero"
        else evalPure (SourceInt (taggedArith2 rem lhs rhs))
    ("eq", [a, b]) -> do
      lhs <- coerceSourceIntForEq "source evaluator: eq lhs" a
      rhs <- coerceSourceIntForEq "source evaluator: eq rhs" b
      evalPure (SourceInt (if normalizeTaggedIntPayload lhs == normalizeTaggedIntPayload rhs then 1 else 0))
    ("lt", [a, b]) -> do
      lhs <- toSourceIntM "source evaluator: lt lhs" a
      rhs <- toSourceIntM "source evaluator: lt rhs" b
      evalPure (SourceInt (if normalizeTaggedIntPayload lhs < normalizeTaggedIntPayload rhs then 1 else 0))
    ("le", [a, b]) -> do
      lhs <- toSourceIntM "source evaluator: le lhs" a
      rhs <- toSourceIntM "source evaluator: le rhs" b
      evalPure (SourceInt (if normalizeTaggedIntPayload lhs <= normalizeTaggedIntPayload rhs then 1 else 0))
    ("gt", [a, b]) -> do
      lhs <- toSourceIntM "source evaluator: gt lhs" a
      rhs <- toSourceIntM "source evaluator: gt rhs" b
      evalPure (SourceInt (if normalizeTaggedIntPayload lhs > normalizeTaggedIntPayload rhs then 1 else 0))
    ("ge", [a, b]) -> do
      lhs <- toSourceIntM "source evaluator: ge lhs" a
      rhs <- toSourceIntM "source evaluator: ge rhs" b
      evalPure (SourceInt (if normalizeTaggedIntPayload lhs >= normalizeTaggedIntPayload rhs then 1 else 0))
    ("str_eq", [a, b]) -> do
      lhs <- toSourceStringM "source evaluator: str_eq lhs" a
      rhs <- toSourceStringM "source evaluator: str_eq rhs" b
      evalPure (SourceInt (if lhs == rhs then 1 else 0))
    ("slice_eq_u8", [SourceString a, SourceString b]) ->
      evalPure (SourceInt (if a == b then 1 else 0))
    ("slice_eq_u8", [SourceSlice a, SourceSlice b]) ->
      evalPure (SourceInt (if a == b then 1 else 0))
    ("slice_eq_u8", [SourceSlice a, SourceString b]) ->
      evalPure (SourceInt (if a == b then 1 else 0))
    ("slice_eq_u8", [SourceString a, SourceSlice b]) ->
      evalPure (SourceInt (if a == b then 1 else 0))
    ("slice_eq_u8", [_, _]) ->
      evalFail "source evaluator: slice_eq_u8 requires host-provided slice value"
    ("str_to_slice", [x]) ->
      case x of
        SourceString s -> evalPure (SourceSlice s)
        SourceSlice _ -> evalPure x
        _ -> evalPure x
    ("slice_to_string", [x]) ->
      case x of
        SourceSlice s -> evalPure (SourceString s)
        _ -> evalPure x
    ("and", [a, b]) -> do
      lhs <- toSourceIntM "source evaluator: and lhs" a
      rhs <- toSourceIntM "source evaluator: and rhs" b
      evalPure
        ( SourceInt
            ( if normalizeTaggedIntPayload lhs /= 0 && normalizeTaggedIntPayload rhs /= 0
                then 1
                else 0
            )
        )
    ("if", [cond, whenTrue, whenFalse]) -> do
      condI <- toSourceIntM "source evaluator: if condition" cond
      let chosen = if condI /= 0 then whenTrue else whenFalse
      applySourceValue chosen (SourceInt 0)
    ("pure", [x]) ->
      evalPure x
    ("bind", [m, f]) ->
      applySourceValue f m
    ("fmap", [f, x]) ->
      applySourceValue f x
    ("ap", [f, x]) ->
      applySourceValue f x
    ("slice_len", [SourceSlice s]) ->
      do
        let bytes = encodeUtf8 (pack s)
        evalPure (SourceInt (BS.length bytes))
    ("slice_len", [_slice]) ->
      evalFail "source evaluator: slice_len requires host-provided slice value"
    ("slice_get_u8", [SourceSlice s, _index]) ->
      do
        let bytes = encodeUtf8 (pack s)
        index <- toSourceIntM "source evaluator: slice_get_u8 index" _index
        if index < 0 || index >= BS.length bytes
          then
            evalFail
              ( "source evaluator: slice_get_u8 index out of range (index="
                  <> show index
                  <> ", len="
                  <> show (BS.length bytes)
                  <> ")"
              )
          else evalPure (SourceInt (fromIntegral (bytes `BS.index` index)))
    ("slice_get_u8", [_slice, _index]) ->
      evalFail "source evaluator: slice_get_u8 requires host-provided slice value"
    ("slice_set_u8", [_slice, _index, _value]) ->
      evalFail "source evaluator: slice_set_u8 requires host-provided slice value"
    ("slice_new_u8", [_len]) ->
      evalFail "source evaluator: slice_new_u8 requires wasm runtime memory"
    ("slice_data_ptr", [_slice]) ->
      evalFail "source evaluator: slice_data_ptr requires wasm runtime memory"
    ("slice_len_raw", [_slice]) ->
      evalFail "source evaluator: slice_len_raw requires wasm runtime memory"
    ("region_mark", [_ignored]) ->
      do
        let ignored = _ignored
        _ <- toSourceIntM "source evaluator: region_mark expects region token" ignored
        st <- evalGetState
        let mark = evalRegionPtr st
        evalSetState st {evalRegionMarks = mark : evalRegionMarks st}
        evalPure (SourceInt mark)
    ("region_alloc", [_sizeBytes, _alignBytes]) ->
      do
        sizeBytes <- toSourceIntM "source evaluator: region_alloc size" _sizeBytes
        alignBytes <- toSourceIntM "source evaluator: region_alloc align" _alignBytes
        if sizeBytes < 0
          then evalFail "source evaluator: region_alloc size must be non-negative"
          else if alignBytes <= 0
            then evalFail "source evaluator: region_alloc align must be positive"
            else do
              st <- evalGetState
              let alignedSize =
                    let adjust = alignBytes - 1
                     in (sizeBytes + adjust) `div` alignBytes * alignBytes
                  ptr = evalRegionPtr st
                  nextPtr = ptr + alignedSize
              evalSetState st {evalRegionPtr = nextPtr}
              evalPure (SourceInt ptr)
    ("region_reset", [_mark]) ->
      do
        mark <- toSourceIntM "source evaluator: region_reset mark" _mark
        st <- evalGetState
        case evalRegionMarks st of
          [] ->
            evalFail "source evaluator: region_reset with no active region mark"
          markOnTop : rest
            | markOnTop /= mark ->
                evalFail
                  ( "source evaluator: region_reset expected top mark "
                      <> show markOnTop
                      <> ", got "
                      <> show mark
                  )
            | otherwise ->
                evalSetState st {evalRegionPtr = mark, evalRegionMarks = rest}
                  >> evalPure (SourceInt mark)
    ("memcpy_u8", [_destPtr, _srcPtr, _lenBytes]) ->
      evalFail "source evaluator: memcpy_u8 requires wasm runtime memory"
    ("memset_u8", [_destPtr, _value, _lenBytes]) ->
      evalFail "source evaluator: memset_u8 requires wasm runtime memory"
    ("struct_tag", [SourceStruct _tag _fields]) ->
      evalFail "source evaluator: struct_tag is only available after collapse/lowering"
    ("struct_tag", [_]) ->
      evalFail "source evaluator: struct_tag expects struct value"
    ("collection_empty", [_ignored]) ->
      evalPure (SourceStruct "collection_empty" [])
    ("collection_extend", [collectionVal, x]) ->
      evalPure (SourceStruct "collection_node" [collectionVal, x])
    _ ->
      case parseMkBuiltin name of
        Just (tag, arityN)
          | arityN == length args -> evalPure (SourceStruct tag args)
          | otherwise ->
              evalFail
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
                      evalFail ("source evaluator: unsupported builtin or arity: " <> name)

evalSourceGetter :: S.Name -> Int -> [SourceValue] -> EvalM SourceValue
evalSourceGetter expectedTag idx args =
  case args of
    [SourceStruct tag fields]
      | tag /= expectedTag ->
          evalFail
            ( "source evaluator: struct tag mismatch, expected "
                <> expectedTag
                <> ", got "
                <> tag
            )
      | idx < 0 || idx >= length fields ->
          evalFail
            ( "source evaluator: struct field index out of range: "
                <> show idx
            )
      | otherwise ->
          evalPure (fields !! idx)
    [_] ->
      evalFail "source evaluator: getter expected struct value"
    _ ->
      evalFail "source evaluator: getter expected exactly one argument"

evalSourceIsTag :: S.Name -> [SourceValue] -> EvalM SourceValue
evalSourceIsTag expectedTag args =
  case args of
    [SourceStruct tag _fields] ->
      evalPure (SourceInt (if tag == expectedTag then 1 else 0))
    [_] ->
      evalPure (SourceInt 0)
    _ ->
      evalFail "source evaluator: tag predicate expected exactly one argument"

toSourceInt :: String -> SourceValue -> Either String Int
toSourceInt err val =
  case val of
    SourceInt n -> Right n
    _ -> Left err

toSourceIntM :: String -> SourceValue -> EvalM Int
toSourceIntM err val = liftEither (toSourceInt err val)

coerceSourceIntForEq :: String -> SourceValue -> EvalM Int
coerceSourceIntForEq err val =
  case val of
    SourceInt n ->
      pure n
    SourceClosure _ [] _ ->
      forceSourceValue val >>= coerceSourceIntForEq err
    SourceBuiltin _ 0 _ ->
      forceSourceValue val >>= coerceSourceIntForEq err
    _ ->
      liftEither (Left err)

toSourceString :: String -> SourceValue -> Either String String
toSourceString err val =
  case val of
    SourceString s -> Right s
    _ -> Left err

toSourceStringM :: String -> SourceValue -> EvalM String
toSourceStringM err val = forceSourceValue val >>= liftEither . toSourceString err

liftEither :: Either String a -> EvalM a
liftEither ev = EvalM (\state -> case ev of
  Right x -> Right (x, state)
  Left err -> Left err)

applyUnusedArityDefaults :: SourceValue -> EvalM SourceValue
applyUnusedArityDefaults val =
  case val of
    SourceClosure env params bodyExpr
      | all (== "_") params ->
          foldM
            applySourceValue
            val
            (replicate (length params) (SourceInt 0))
    _ ->
      pure val

runFunction :: FunctionEnv -> C.CollapsedFunction -> [RuntimeValue] -> EvalM RuntimeValue
runFunction env fn args = do
  let expected = localParamCount fn
  if length args == expected
    then evalBinds env fn args [] (C.binds fn)
    else
      evalFail
        ( "collapsed evaluator: function "
            <> C.name fn
            <> " expected "
            <> show expected
            <> " arg(s), got "
            <> show (length args)
        )

evalBinds :: FunctionEnv -> C.CollapsedFunction -> [RuntimeValue] -> TempEnv -> [C.Bind] -> EvalM RuntimeValue
evalBinds env fn locals temps allBinds =
  case allBinds of
    [] ->
      evalAtom locals temps (C.result fn)
    b:bs -> do
      v <- evalValue env fn locals temps (C.value b)
      evalBinds env fn locals ((C.temp b, v) : temps) bs

evalValue :: FunctionEnv -> C.CollapsedFunction -> [RuntimeValue] -> TempEnv -> C.Value -> EvalM RuntimeValue
evalValue env fn locals temps val =
  case val of
    C.VClosure callee captures -> do
      calleeFn <- liftEither (lookupRequired ("collapsed evaluator: unknown closure callee: " <> callee) callee env)
      captureVals <- traverse (evalAtom locals temps) captures
      let totalArity = localParamCount calleeFn
      evalPure (RuntimeClosure callee totalArity captureVals)
    C.VCallDirect callee args -> do
      argVals <- traverse (evalAtom locals temps) args
      callNamed env callee argVals
    C.VCurryDirect callee args -> do
      calleeFn <- liftEither (lookupRequired ("collapsed evaluator: unknown curry callee: " <> callee) callee env)
      argVals <- traverse (evalAtom locals temps) args
      let totalArity = localParamCount calleeFn
      if length argVals < totalArity
        then evalPure (RuntimeClosure callee totalArity argVals)
        else if length argVals == totalArity
          then runFunction env calleeFn argVals
          else evalFail "collapsed evaluator: curry over-application"
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

evalAtom :: [RuntimeValue] -> TempEnv -> C.Atom -> EvalM RuntimeValue
evalAtom locals temps atom =
  case atom of
    C.AConstI32 n ->
      evalPure (RuntimeInt n)
    C.AConstString s ->
      evalPure (RuntimeString s)
    C.ALocal i ->
      liftEither (lookupIndex ("collapsed evaluator: local index out of range: " <> show i) i locals)
    C.ATemp t ->
      liftEither (lookupRequired ("collapsed evaluator: unknown temp: t" <> show t) t temps)
    C.AGlobal g ->
      evalFail ("collapsed evaluator: globals are not supported: " <> g)

applyRuntimeValue :: FunctionEnv -> RuntimeValue -> RuntimeValue -> EvalM RuntimeValue
applyRuntimeValue env fnVal argVal =
  case fnVal of
    RuntimeInt _ ->
      evalFail "collapsed evaluator: attempted to call Int value"
    RuntimeString _ ->
      evalFail "collapsed evaluator: attempted to call string value"
    RuntimeSlice _ ->
      evalFail "collapsed evaluator: attempted to call slice value"
    RuntimeStruct _ _ ->
      evalFail "collapsed evaluator: attempted to call struct value"
    RuntimeClosure callee totalArity boundArgs -> do
      calleeFn <- liftEither (lookupRequired ("collapsed evaluator: closure target missing: " <> callee) callee env)
      let nextArgs = boundArgs <> [argVal]
      if length nextArgs < totalArity
        then evalPure (RuntimeClosure callee totalArity nextArgs)
        else if length nextArgs == totalArity
          then runFunction env calleeFn nextArgs
          else evalFail "collapsed evaluator: closure over-application"

callNamed :: FunctionEnv -> S.Name -> [RuntimeValue] -> EvalM RuntimeValue
callNamed env callee args =
  case lookup callee env of
    Just fn ->
      runFunction env fn args
    Nothing ->
      evalRuntimeBuiltin env callee args

evalRuntimeBuiltin :: FunctionEnv -> S.Name -> [RuntimeValue] -> EvalM RuntimeValue
evalRuntimeBuiltin env name args =
  case (name, args) of
    ("add", [a, b]) ->
      do
        lhs <- toRuntimeIntM "collapsed evaluator: add lhs" a
        rhs <- toRuntimeIntM "collapsed evaluator: add rhs" b
        evalPure (RuntimeInt (taggedArith2 (+) lhs rhs))
    ("sub", [a, b]) ->
      do
        lhs <- toRuntimeIntM "collapsed evaluator: sub lhs" a
        rhs <- toRuntimeIntM "collapsed evaluator: sub rhs" b
        evalPure (RuntimeInt (taggedArith2 (-) lhs rhs))
    ("mul", [a, b]) ->
      do
        lhs <- toRuntimeIntM "collapsed evaluator: mul lhs" a
        rhs <- toRuntimeIntM "collapsed evaluator: mul rhs" b
        evalPure (RuntimeInt (taggedArith2 (*) lhs rhs))
    ("div", [a, b]) -> do
      lhs <- toRuntimeIntM "collapsed evaluator: div lhs" a
      rhs <- toRuntimeIntM "collapsed evaluator: div rhs" b
      if rhs == 0
        then evalFail "collapsed evaluator: division by zero"
        else evalPure (RuntimeInt (taggedArith2 quot lhs rhs))
    ("mod", [a, b]) -> do
      lhs <- toRuntimeIntM "collapsed evaluator: mod lhs" a
      rhs <- toRuntimeIntM "collapsed evaluator: mod rhs" b
      if rhs == 0
        then evalFail "collapsed evaluator: modulo by zero"
        else evalPure (RuntimeInt (taggedArith2 rem lhs rhs))
    ("eq", [a, b]) -> do
      lhs <- coerceRuntimeIntForEq env "collapsed evaluator: eq lhs" a
      rhs <- coerceRuntimeIntForEq env "collapsed evaluator: eq rhs" b
      evalPure (RuntimeInt (if normalizeTaggedIntPayload lhs == normalizeTaggedIntPayload rhs then 1 else 0))
    ("lt", [a, b]) -> do
      lhs <- toRuntimeIntM "collapsed evaluator: lt lhs" a
      rhs <- toRuntimeIntM "collapsed evaluator: lt rhs" b
      evalPure (RuntimeInt (if normalizeTaggedIntPayload lhs < normalizeTaggedIntPayload rhs then 1 else 0))
    ("le", [a, b]) -> do
      lhs <- toRuntimeIntM "collapsed evaluator: le lhs" a
      rhs <- toRuntimeIntM "collapsed evaluator: le rhs" b
      evalPure (RuntimeInt (if normalizeTaggedIntPayload lhs <= normalizeTaggedIntPayload rhs then 1 else 0))
    ("gt", [a, b]) -> do
      lhs <- toRuntimeIntM "collapsed evaluator: gt lhs" a
      rhs <- toRuntimeIntM "collapsed evaluator: gt rhs" b
      evalPure (RuntimeInt (if normalizeTaggedIntPayload lhs > normalizeTaggedIntPayload rhs then 1 else 0))
    ("ge", [a, b]) -> do
      lhs <- toRuntimeIntM "collapsed evaluator: ge lhs" a
      rhs <- toRuntimeIntM "collapsed evaluator: ge rhs" b
      evalPure (RuntimeInt (if normalizeTaggedIntPayload lhs >= normalizeTaggedIntPayload rhs then 1 else 0))
    ("str_eq", [RuntimeString a, RuntimeString b]) ->
      evalPure (RuntimeInt (if a == b then 1 else 0))
    ("str_eq", [_, _]) ->
      evalPure (RuntimeInt 0)
    ("slice_eq_u8", [RuntimeString a, RuntimeString b]) ->
      evalPure (RuntimeInt (if a == b then 1 else 0))
    ("slice_eq_u8", [RuntimeSlice a, RuntimeSlice b]) ->
      evalPure (RuntimeInt (if a == b then 1 else 0))
    ("slice_eq_u8", [RuntimeSlice a, RuntimeString b]) ->
      evalPure (RuntimeInt (if a == b then 1 else 0))
    ("slice_eq_u8", [RuntimeString a, RuntimeSlice b]) ->
      evalPure (RuntimeInt (if a == b then 1 else 0))
    ("slice_eq_u8", [_, _]) ->
      evalPure (RuntimeInt 0)
    ("str_to_slice", [x]) ->
      case x of
        RuntimeString s -> evalPure (RuntimeSlice s)
        RuntimeSlice _ -> evalPure x
        _ -> evalPure x
    ("slice_to_string", [x]) ->
      case x of
        RuntimeSlice s -> evalPure (RuntimeString s)
        _ -> evalPure x
    ("and", [a, b]) -> do
      lhs <- toRuntimeIntM "collapsed evaluator: and lhs" a
      rhs <- toRuntimeIntM "collapsed evaluator: and rhs" b
      evalPure
        ( RuntimeInt
            ( if normalizeTaggedIntPayload lhs /= 0 && normalizeTaggedIntPayload rhs /= 0
                then 1
                else 0
            )
        )
    ("if", [cond, whenTrue, whenFalse]) -> do
      condI <- toRuntimeIntM "collapsed evaluator: if condition" cond
      let chosen = if condI /= 0 then whenTrue else whenFalse
      applyRuntimeValue env chosen (RuntimeInt 0)
    ("pure", [x]) ->
      evalPure x
    ("bind", [m, f]) ->
      applyRuntimeValue env f m
    ("fmap", [f, x]) ->
      applyRuntimeValue env f x
    ("ap", [f, x]) ->
      applyRuntimeValue env f x
    ("slice_len", [RuntimeSlice s]) ->
      do
        let bytes = encodeUtf8 (pack s)
        evalPure (RuntimeInt (BS.length bytes))
    ("slice_len", [_slice]) ->
      evalFail "collapsed evaluator: slice_len requires host-provided slice value"
    ("slice_get_u8", [RuntimeSlice s, _index]) ->
      do
        let bytes = encodeUtf8 (pack s)
        index <- toRuntimeIntM "collapsed evaluator: slice_get_u8 index" _index
        if index < 0 || index >= BS.length bytes
          then
            evalFail
              ( "collapsed evaluator: slice_get_u8 index out of range (index="
                  <> show index
                  <> ", len="
                  <> show (BS.length bytes)
                  <> ")"
              )
          else evalPure (RuntimeInt (fromIntegral (bytes `BS.index` index)))
    ("slice_get_u8", [_slice, _index]) ->
      evalFail "collapsed evaluator: slice_get_u8 requires host-provided slice value"
    ("slice_set_u8", [_slice, _index, _value]) ->
      evalFail "collapsed evaluator: slice_set_u8 requires host-provided slice value"
    ("slice_new_u8", [_len]) ->
      evalFail "collapsed evaluator: slice_new_u8 requires wasm runtime memory"
    ("slice_data_ptr", [_slice]) ->
      evalFail "collapsed evaluator: slice_data_ptr requires wasm runtime memory"
    ("slice_len_raw", [_slice]) ->
      evalFail "collapsed evaluator: slice_len_raw requires wasm runtime memory"
    ("region_mark", [_ignored]) ->
      do
        _ <- toRuntimeIntM "collapsed evaluator: region_mark expects region token" _ignored
        st <- evalGetState
        let mark = evalRegionPtr st
        evalSetState st {evalRegionMarks = mark : evalRegionMarks st}
        evalPure (RuntimeInt mark)
    ("region_alloc", [_sizeBytes, _alignBytes]) ->
      do
        sizeBytes <- toRuntimeIntM "collapsed evaluator: region_alloc size" _sizeBytes
        alignBytes <- toRuntimeIntM "collapsed evaluator: region_alloc align" _alignBytes
        if sizeBytes < 0
          then evalFail "collapsed evaluator: region_alloc size must be non-negative"
          else if alignBytes <= 0
            then evalFail "collapsed evaluator: region_alloc align must be positive"
            else do
              st <- evalGetState
              let alignedSize =
                    let adjust = alignBytes - 1
                     in (sizeBytes + adjust) `div` alignBytes * alignBytes
                  ptr = evalRegionPtr st
                  nextPtr = ptr + alignedSize
              evalSetState st {evalRegionPtr = nextPtr}
              evalPure (RuntimeInt ptr)
    ("region_reset", [_mark]) ->
      do
        mark <- toRuntimeIntM "collapsed evaluator: region_reset mark" _mark
        st <- evalGetState
        case evalRegionMarks st of
          [] ->
            evalFail "collapsed evaluator: region_reset with no active region mark"
          markOnTop : rest
            | markOnTop /= mark ->
                evalFail
                  ( "collapsed evaluator: region_reset expected top mark "
                      <> show markOnTop
                      <> ", got "
                      <> show mark
                  )
            | otherwise ->
                evalSetState st {evalRegionPtr = mark, evalRegionMarks = rest}
                  >> evalPure (RuntimeInt mark)
    ("memcpy_u8", [_destPtr, _srcPtr, _lenBytes]) ->
      evalFail "collapsed evaluator: memcpy_u8 requires wasm runtime memory"
    ("memset_u8", [_destPtr, _value, _lenBytes]) ->
      evalFail "collapsed evaluator: memset_u8 requires wasm runtime memory"
    ("struct_tag", [_]) ->
      evalFail "collapsed evaluator: struct_tag requires wasm runtime tag table"
    ("collection_empty", [_ignored]) ->
      evalPure (RuntimeStruct "collection_empty" [])
    ("collection_extend", [collectionVal, x]) ->
      evalPure (RuntimeStruct "collection_node" [collectionVal, x])
    _ ->
      case parseMkBuiltin name of
        Just (tag, arityN)
          | arityN == length args -> evalPure (RuntimeStruct tag args)
          | otherwise ->
              evalFail
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
                  evalFail ("collapsed evaluator: unknown callee or arity: " <> name)

evalRuntimeGetter :: S.Name -> Int -> [RuntimeValue] -> EvalM RuntimeValue
evalRuntimeGetter expectedTag idx args =
  case args of
    [RuntimeStruct tag fields]
      | tag /= expectedTag ->
          evalFail
            ( "collapsed evaluator: struct tag mismatch, expected "
                <> expectedTag
                <> ", got "
                <> tag
            )
      | idx < 0 || idx >= length fields ->
          evalFail
            ( "collapsed evaluator: struct field index out of range: "
                <> show idx
            )
      | otherwise ->
          evalPure (fields !! idx)
    [_] ->
      evalFail "collapsed evaluator: getter expected struct value"
    _ ->
      evalFail "collapsed evaluator: getter expected exactly one argument"

evalRuntimeIsTag :: S.Name -> [RuntimeValue] -> EvalM RuntimeValue
evalRuntimeIsTag expectedTag args =
  case args of
    [RuntimeStruct tag _fields] ->
      evalPure (RuntimeInt (if tag == expectedTag then 1 else 0))
    [_] ->
      evalPure (RuntimeInt 0)
    _ ->
      evalFail "collapsed evaluator: tag predicate expected exactly one argument"

toRuntimeInt :: String -> RuntimeValue -> Either String Int
toRuntimeInt err val =
  case val of
    RuntimeInt n -> Right n
    _ -> Left err

toRuntimeIntM :: String -> RuntimeValue -> EvalM Int
toRuntimeIntM err val = liftEither (toRuntimeInt err val)

coerceRuntimeIntForEq :: FunctionEnv -> String -> RuntimeValue -> EvalM Int
coerceRuntimeIntForEq env err val =
  case val of
    RuntimeInt n ->
      pure n
    RuntimeClosure callee 0 _ ->
      do
        calleeFn <- liftEither (lookupRequired ("collapsed evaluator: closure target missing: " <> callee) callee env)
        coerceRuntimeIntForEq env err =<< runFunction env calleeFn []
    _ ->
      liftEither (Left err)

taggedArith2 :: (Int32 -> Int32 -> Int32) -> Int -> Int -> Int
taggedArith2 op lhs rhs =
  normalizeTaggedIntPayload
    ( fromIntegral
        ( op
            (fromIntegral lhs)
            (fromIntegral rhs)
        )
    )

normalizeTaggedIntPayload :: Int -> Int
normalizeTaggedIntPayload n =
  fromIntegral
    ( ( ((fromIntegral n :: Int32) `shiftL` 1)
          .|. 1
      )
        `shiftR` 1
    )

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
    fnName (S.Function n _ _ _) = n

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
    "mod" -> Just 2
    "eq" -> Just 2
    "lt" -> Just 2
    "le" -> Just 2
    "gt" -> Just 2
    "ge" -> Just 2
    "str_eq" -> Just 2
    "str_to_slice" -> Just 1
    "slice_to_string" -> Just 1
    "slice_eq_u8" -> Just 2
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
