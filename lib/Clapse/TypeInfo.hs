module Clapse.TypeInfo
  ( Type(..)
  , FunctionTypeInfo(..)
  , inferModuleTypes
  , inferSourceTypes
  , renderType
  ) where

import Data.Char (chr, isDigit)
import Control.Applicative ((<|>))

import Clapse.Syntax (CaseArm(..), CasePattern(..), ConstructorInfo(..), Expr(..), Function(..), Module(..), Name, parseModule)

data Type
  = TI64
  | TU64
  | TByte
  | TString
  | TData Name [Type]
  | TVar Int
  | TFun Type Type
  deriving (Eq, Show)

data FunctionTypeInfo = FunctionTypeInfo
  { fnName :: Name
  , fnArgs :: [Name]
  , fnArgTypes :: [Type]
  , fnResultType :: Type
  , fnType :: Type
  }
  deriving (Eq, Show)

data Seed = Seed
  { seedFn :: Function
  , seedArgTypes :: [Type]
  , seedRetType :: Type
  , seedFnType :: Type
  }

data InferState = InferState
  { nextVar :: Int
  , subst :: [(Int, Type)]
  }
  deriving (Eq, Show)

type Env = [(Name, Type)]

type InferResult a = Either String (a, InferState)

inferSourceTypes :: String -> Either String [FunctionTypeInfo]
inferSourceTypes src = do
  modu <- parseModule src
  inferModuleTypes modu

inferModuleTypes :: Module -> Either String [FunctionTypeInfo]
inferModuleTypes Module {functions = funs} = do
  _ <- validateUniqueNames funs
  (seeds, st0) <- buildSeeds funs (InferState {nextVar = 0, subst = []})
  let envFns = map (\seed -> (nameFromSeed seed, seedFnType seed)) seeds
  (_, stFinal) <- inferAll seeds envFns st0
  pure (map (toInfo (subst stFinal)) seeds)

validateUniqueNames :: [Function] -> Either String ()
validateUniqueNames funs =
  case duplicates (map fnName funs) of
    [] -> Right ()
    d:_ -> Left ("duplicate function name for type inference: " <> d)
  where
    fnName :: Function -> Name
    fnName (Function n _ _ _) = n

buildSeeds :: [Function] -> InferState -> Either String ([Seed], InferState)
buildSeeds funs st = go [] st funs
  where
    go :: [Seed] -> InferState -> [Function] -> Either String ([Seed], InferState)
    go acc cur [] = Right (reverse acc, cur)
    go acc cur (fn:fns) = do
      let argc = argCount fn
      (argTys, cur1) <- freshMany argc cur
      (retTy, cur2) <- freshType cur1
      let fullTy = foldr TFun retTy argTys
          seed =
            Seed
              { seedFn = fn
              , seedArgTypes = argTys
              , seedRetType = retTy
              , seedFnType = fullTy
              }
      go (seed : acc) cur2 fns

inferAll :: [Seed] -> Env -> InferState -> InferResult ()
inferAll [] _env st = Right ((), st)
inferAll (seed:rest) env st = do
  let fn = seedFn seed
      argBindings = zip (args fn) (seedArgTypes seed)
      envWithArgs = argBindings <> env
  (bodyTy, st1) <- inferExpr envWithArgs (body fn) st
  st2 <- unify bodyTy (seedRetType seed) st1
  inferAll rest env st2

toInfo :: [(Int, Type)] -> Seed -> FunctionTypeInfo
toInfo finalSubst seed =
  let argsApplied = map (applySubst finalSubst) (seedArgTypes seed)
      retApplied = applySubst finalSubst (seedRetType seed)
      fullApplied = applySubst finalSubst (seedFnType seed)
   in FunctionTypeInfo
        { fnName = nameFromSeed seed
        , fnArgs = args (seedFn seed)
        , fnArgTypes = argsApplied
        , fnResultType = retApplied
        , fnType = fullApplied
        }

inferExpr :: Env -> Expr -> InferState -> InferResult Type
inferExpr env expr st =
  case expr of
    IntLit _ ->
      Right (TI64, st)
    StringLit _ ->
      Right (TString, st)
    CollectionLit elems ->
      inferExpr env (desugarCollectionExpr elems) st
    Case scrutinees arms ->
      inferCaseExpr env scrutinees arms st
    Var n ->
      case lookup n env of
        Just ty -> Right (applySubst (subst st) ty, st)
        Nothing ->
          inferBuiltin n st
    Lam n bodyExpr -> do
      (argTy, st1) <- freshType st
      let env' = (n, argTy) : env
      (bodyTy, st2) <- inferExpr env' bodyExpr st1
      pure (TFun (applySubst (subst st2) argTy) bodyTy, st2)
    App f x -> do
      (fTy, st1) <- inferExpr env f st
      (xTy, st2) <- inferExpr env x st1
      (outTy, st3) <- freshType st2
      st4 <- unify fTy (TFun xTy outTy) st3
      pure (applySubst (subst st4) outTy, st4)

inferCaseExpr :: Env -> [Expr] -> [CaseArm] -> InferState -> InferResult Type
inferCaseExpr env scrutinees arms st0 = do
  (scrutineeTypes, st1) <- inferExprList env scrutinees st0
  case arms of
    [] ->
      Left "type inference: case expression has no arms"
    firstArm:restArms -> do
      (firstArmTy, st2) <- inferCaseArm env scrutineeTypes firstArm st1
      inferCaseArms env scrutineeTypes firstArmTy restArms st2

inferCaseArms :: Env -> [Type] -> Type -> [CaseArm] -> InferState -> InferResult Type
inferCaseArms _env _scrutineeTypes resultTy [] st =
  Right (applySubst (subst st) resultTy, st)
inferCaseArms env scrutineeTypes resultTy (arm:rest) st0 = do
  (armTy, st1) <- inferCaseArm env scrutineeTypes arm st0
  st2 <- unify resultTy armTy st1
  inferCaseArms env scrutineeTypes (applySubst (subst st2) resultTy) rest st2

inferCaseArm :: Env -> [Type] -> CaseArm -> InferState -> InferResult Type
inferCaseArm env scrutineeTypes arm st0 =
  if length (armPatterns arm) /= length scrutineeTypes
    then Left "type inference: case arm pattern count mismatch"
    else do
      (patternEnv, st1) <- inferPatternBindings scrutineeTypes (armPatterns arm) st0
      inferExpr (patternEnv <> env) (armBody arm) st1

inferPatternBindings :: [Type] -> [CasePattern] -> InferState -> Either String (Env, InferState)
inferPatternBindings [] [] st = Right ([], st)
inferPatternBindings (scrTy:scrRest) (pat:patRest) st0 = do
  (bindings0, st1) <- inferPatternBinding scrTy pat st0
  (bindingsRest, st2) <- inferPatternBindings scrRest patRest st1
  Right (bindings0 <> bindingsRest, st2)
inferPatternBindings _ _ _ =
  Left "type inference: case pattern/scrutinee mismatch"

inferPatternBinding :: Type -> CasePattern -> InferState -> Either String (Env, InferState)
inferPatternBinding scrutineeTy pat st0 =
  case pat of
    PatWildcard ->
      Right ([], st0)
    PatVar n ->
      Right ([(n, applySubst (subst st0) scrutineeTy)], st0)
    PatInt _ -> do
      st1 <- unify scrutineeTy TI64 st0
      Right ([], st1)
    PatConstructor _ ctorInfo fieldNames -> do
      (paramTypes, st1) <- freshMany (ctorTypeParamCount ctorInfo) st0
      st2 <- unify scrutineeTy (TData (ctorTypeName ctorInfo) paramTypes) st1
      (fieldTypes, st3) <-
        inferFieldTypes
          ("type inference: invalid constructor field map index in case pattern for " <> ctorName ctorInfo)
          paramTypes
          st2
          (ctorFieldParamMap ctorInfo)
      if length fieldTypes /= length fieldNames
        then Left "type inference: case constructor field arity mismatch"
        else Right (zipBound fieldNames fieldTypes, st3)
  where
    zipBound :: [Name] -> [Type] -> Env
    zipBound names tys =
      case (names, tys) of
        ([], []) -> []
        (name0:namesRest, ty0:tysRest)
          | name0 == "_" -> zipBound namesRest tysRest
          | otherwise -> (name0, ty0) : zipBound namesRest tysRest
        _ -> []

inferExprList :: Env -> [Expr] -> InferState -> InferResult [Type]
inferExprList _env [] st = Right ([], st)
inferExprList env (expr:rest) st0 = do
  (ty0, st1) <- inferExpr env expr st0
  (tysRest, st2) <- inferExprList env rest st1
  Right (ty0 : tysRest, st2)

inferBuiltin :: Name -> InferState -> InferResult Type
inferBuiltin n st =
  case n of
    "add" -> Right (binaryI64Type, st)
    "sub" -> Right (binaryI64Type, st)
    "mul" -> Right (binaryI64Type, st)
    "div" -> Right (binaryI64Type, st)
    "mod" -> Right (binaryI64Type, st)
    "eq" -> Right (binaryI64Type, st)
    "lt" -> Right (binaryI64Type, st)
    "gt" -> Right (binaryI64Type, st)
    "le" -> Right (binaryI64Type, st)
    "ge" -> Right (binaryI64Type, st)
    "and" -> Right (binaryI64Type, st)
    "if" -> do
      (a, st1) <- freshType st
      let thunkTy = TFun TI64 a
      Right (TFun TI64 (TFun thunkTy (TFun thunkTy a)), st1)
    "pure" -> do
      (a, st1) <- freshType st
      Right (TFun a a, st1)
    "bind" -> do
      (a, st1) <- freshType st
      (b, st2) <- freshType st1
      Right (TFun a (TFun (TFun a b) b), st2)
    "fmap" -> do
      (a, st1) <- freshType st
      (b, st2) <- freshType st1
      Right (TFun (TFun a b) (TFun a b), st2)
    "ap" -> do
      (a, st1) <- freshType st
      (b, st2) <- freshType st1
      Right (TFun (TFun a b) (TFun a b), st2)
    "slice_len" ->
      Right (TFun sliceByteType TI64, st)
    "slice_get_u8" ->
      Right (TFun sliceByteType (TFun TI64 TI64), st)
    "slice_set_u8" ->
      Right (TFun sliceByteType (TFun TI64 (TFun TI64 sliceByteType)), st)
    "slice_new_u8" ->
      Right (TFun TI64 sliceByteType, st)
    "slice_data_ptr" ->
      Right (TFun sliceByteType TI64, st)
    "slice_len_raw" ->
      Right (TFun sliceByteType TI64, st)
    "region_mark" ->
      Right (TFun TI64 TI64, st)
    "region_alloc" ->
      Right (TFun TI64 (TFun TI64 TI64), st)
    "region_reset" ->
      Right (TFun TI64 TI64, st)
    "memcpy_u8" ->
      Right (TFun TI64 (TFun TI64 (TFun TI64 TI64)), st)
    "memset_u8" ->
      Right (TFun TI64 (TFun TI64 (TFun TI64 TI64)), st)
    "struct_tag" -> do
      (a, st1) <- freshType st
      Right (TFun a TI64, st1)
    "collection_empty" -> do
      (c, st1) <- freshType st
      Right (TFun TI64 c, st1)
    "collection_extend" -> do
      (c, st1) <- freshType st
      (a, st2) <- freshType st1
      Right (TFun c (TFun a c), st2)
    _ ->
      case parseMkBuiltin n of
        Just sig -> do
          if sigFieldCount sig /= length (sigFieldParamMap sig)
            then Left ("type inference: invalid constructor field map for " <> n)
            else do
              paramsT <- freshMany (sigTypeParamCount sig) st
              case paramsT of
                (paramTypes, st1) -> do
                  (argTypes, st2) <- inferFieldTypes ("type inference: invalid constructor field map index in " <> n) paramTypes st1 (sigFieldParamMap sig)
                  let ctorTy = foldr TFun (TData (sigDataTypeTag sig) paramTypes) argTypes
                  Right (ctorTy, st2)
        Nothing ->
          case parseGetBuiltin n of
            Just (sig, idx) -> do
              paramsT <- freshMany (sigTypeParamCount sig) st
              case paramsT of
                (paramTypes, st1) -> do
                  fieldParam <- lookupIndexEither ("type inference: invalid getter index in " <> n) idx (sigFieldParamMap sig)
                  (fieldTy, st2) <- case fieldParam of
                    Just fieldParamIx ->
                      do
                        fieldTy0 <- lookupTypeParam ("type inference: invalid getter field map index in " <> n) paramTypes fieldParamIx
                        Right (fieldTy0, st1)
                    Nothing ->
                      do
                        freshType st1
                  Right (TFun (TData (sigDataTypeTag sig) paramTypes) fieldTy, st2)
            Nothing ->
              case parseIsBuiltin n of
                Just _tag -> do
                  (inputTy, st1) <- freshType st
                  Right (TFun inputTy TI64, st1)
                Nothing ->
                  Left ("type inference: unknown variable or function: " <> n)

binaryI64Type :: Type
binaryI64Type = TFun TI64 (TFun TI64 TI64)

sliceByteType :: Type
sliceByteType = TData "slice" [TByte]

freshType :: InferState -> InferResult Type
freshType st =
  Right
    ( TVar (nextVar st)
    , st {nextVar = nextVar st + 1}
    )

freshMany :: Int -> InferState -> InferResult [Type]
freshMany n st
  | n <= 0 = Right ([], st)
  | otherwise = do
      (t, st1) <- freshType st
      (rest, st2) <- freshMany (n - 1) st1
      pure (t : rest, st2)

inferFieldTypes :: String -> [Type] -> InferState -> [Maybe Int] -> Either String ([Type], InferState)
inferFieldTypes _ _ st [] = Right ([], st)
inferFieldTypes err paramTypes st (fieldIdx : rest) = do
  (fieldTy, st1) <- case fieldIdx of
    Just idx -> do
      ty <- lookupTypeParam err paramTypes idx
      Right (ty, st)
    Nothing -> freshType st
  (restTypes, st2) <- inferFieldTypes err paramTypes st1 rest
  Right (fieldTy : restTypes, st2)

unify :: Type -> Type -> InferState -> Either String InferState
unify a b st =
  unifyTypes (applySubst (subst st) a) (applySubst (subst st) b) st

unifyTypes :: Type -> Type -> InferState -> Either String InferState
unifyTypes a b st
  | a == b = Right st
  | otherwise =
      case (a, b) of
        (TVar v, t) -> bindVar v t st
        (t, TVar v) -> bindVar v t st
        (TFun a1 a2, TFun b1 b2) -> do
          st1 <- unify a1 b1 st
          unify a2 b2 st1
        (TData x xs, TData y ys)
          | x == y && length xs == length ys ->
              unifyMany xs ys st
        _ ->
          Left ("type mismatch: " <> renderType a <> " vs " <> renderType b)

bindVar :: Int -> Type -> InferState -> Either String InferState
bindVar v t st
  | t == TVar v = Right st
  | occurs v t = Left ("infinite type: t" <> show v <> " occurs in " <> renderType t)
  | otherwise =
      let t' = applySubst (subst st) t
          updated = map (\(k, ty) -> (k, applyOne v t' ty)) (subst st)
       in Right st {subst = (v, t') : updated}

occurs :: Int -> Type -> Bool
occurs v ty =
  case ty of
    TI64 -> False
    TU64 -> False
    TByte -> False
    TString -> False
    TData _ argsT -> any (occurs v) argsT
    TVar x -> x == v
    TFun a b -> occurs v a || occurs v b

applySubst :: [(Int, Type)] -> Type -> Type
applySubst s ty =
  case ty of
    TI64 -> TI64
    TU64 -> TU64
    TByte -> TByte
    TString -> TString
    TData n argsT -> TData n (map (applySubst s) argsT)
    TVar v ->
      case lookup v s of
        Nothing -> TVar v
        Just t -> applySubst s t
    TFun a b -> TFun (applySubst s a) (applySubst s b)

applyOne :: Int -> Type -> Type -> Type
applyOne v rep ty =
  case ty of
    TI64 -> TI64
    TU64 -> TU64
    TByte -> TByte
    TString -> TString
    TData n argsT -> TData n (map (applyOne v rep) argsT)
    TVar x
      | x == v -> rep
      | otherwise -> TVar x
    TFun a b -> TFun (applyOne v rep a) (applyOne v rep b)

renderType :: Type -> String
renderType ty =
  let normalized = normalizeVars ty
   in render normalized
  where
    render :: Type -> String
    render t =
      case t of
        TI64 -> "i64"
        TU64 -> "u64"
        TByte -> "byte"
        TString -> "string"
        TData n argsT ->
          if null argsT
            then n
            else n <> " " <> joinWith ' ' (map renderDataArg argsT)
        TVar v -> varName v
        TFun a b ->
          let lhs =
                case a of
                  TFun _ _ -> "(" <> render a <> ")"
                  _ -> render a
           in lhs <> " -> " <> render b

    renderDataArg :: Type -> String
    renderDataArg argTy =
      case argTy of
        TFun _ _ -> "(" <> render argTy <> ")"
        _ -> render argTy

normalizeVars :: Type -> Type
normalizeVars ty =
  let (out, _, _) = go [] 0 ty
   in out
  where
    go :: [(Int, Int)] -> Int -> Type -> (Type, Int, [(Int, Int)])
    go env next ty0 =
      case ty0 of
        TI64 -> (TI64, next, env)
        TU64 -> (TU64, next, env)
        TByte -> (TByte, next, env)
        TString -> (TString, next, env)
        TData n argsT ->
          let (argsOut, nextOut, envOut) = goList env next argsT
           in (TData n argsOut, nextOut, envOut)
        TVar v ->
          case lookup v env of
            Just mapped -> (TVar mapped, next, env)
            Nothing ->
              let mapped = next
               in (TVar mapped, next + 1, (v, mapped) : env)
        TFun a b ->
          let (a', next1, env1) = go env next a
              (b', next2, env2) = go env1 next1 b
           in (TFun a' b', next2, env2)

    goList :: [(Int, Int)] -> Int -> [Type] -> ([Type], Int, [(Int, Int)])
    goList env next tys =
      case tys of
        [] -> ([], next, env)
        t:ts ->
          let (tOut, next1, env1) = go env next t
              (tsOut, next2, env2) = goList env1 next1 ts
           in (tOut : tsOut, next2, env2)

varName :: Int -> String
varName i
  | i < 26 = "'" <> [chr (fromEnum 'a' + i)]
  | otherwise = "'t" <> show i

data DataBuiltinSig = DataBuiltinSig
  { sigDataTypeTag :: Name
  , sigCtorTag :: Name
  , sigFieldCount :: Int
  , sigTypeParamCount :: Int
  , sigFieldParamMap :: [Maybe Int]
  }

parseMkBuiltin :: Name -> Maybe DataBuiltinSig
parseMkBuiltin n = do
  rest <- stripPrefix "__mk_" n
  parseMkDetailed rest <|> parseMkLegacy rest
  where
    parseMkDetailed :: String -> Maybe DataBuiltinSig
    parseMkDetailed src = do
      (prefix0, tparAndMap0) <- splitOnToken "_tpar_" src
      (tparRaw, mapRaw) <- splitOnToken "_fmap_" tparAndMap0
      (tag0, fieldCountRaw) <- splitLast '_' prefix0
      fieldCount0 <- parseNat fieldCountRaw
      typeParamCount0 <- parseNat tparRaw
      fieldMap0 <- parseFieldMap mapRaw
      if fieldCount0 == length fieldMap0
        then Just
          DataBuiltinSig
            { sigDataTypeTag = fst (splitTag tag0)
            , sigCtorTag = snd (splitTag tag0)
            , sigFieldCount = fieldCount0
            , sigTypeParamCount = typeParamCount0
            , sigFieldParamMap = fieldMap0
            }
        else Nothing

    parseMkLegacy :: String -> Maybe DataBuiltinSig
    parseMkLegacy src = do
      (tag0, fieldCountRaw) <- splitLast '_' src
      fieldCount0 <- parseNat fieldCountRaw
      let fieldMap0 = [0 .. fieldCount0 - 1]
      Just
        DataBuiltinSig
          { sigDataTypeTag = tag0
          , sigCtorTag = tag0
          , sigFieldCount = fieldCount0
          , sigTypeParamCount = fieldCount0
          , sigFieldParamMap = map Just fieldMap0
          }

parseGetBuiltin :: Name -> Maybe (DataBuiltinSig, Int)
parseGetBuiltin n = do
  rest <- stripPrefix "__get_" n
  parseGetDetailed rest <|> parseGetLegacy rest
  where
    parseGetDetailed :: String -> Maybe (DataBuiltinSig, Int)
    parseGetDetailed src = do
      (prefix0, tparAndMap0) <- splitOnToken "_tpar_" src
      (tparRaw, mapRaw) <- splitOnToken "_fmap_" tparAndMap0
      (tag0, idxRaw) <- splitLast '_' prefix0
      idx0 <- parseNat idxRaw
      typeParamCount0 <- parseNat tparRaw
      fieldMap0 <- parseFieldMap mapRaw
      let fieldCount0 = length fieldMap0
      if idx0 < fieldCount0
        then
          Just
            ( DataBuiltinSig
                { sigDataTypeTag = fst (splitTag tag0)
                , sigCtorTag = snd (splitTag tag0)
                , sigFieldCount = fieldCount0
                , sigTypeParamCount = typeParamCount0
                , sigFieldParamMap = fieldMap0
                }
            , idx0
            )
        else Nothing

    parseGetLegacy :: String -> Maybe (DataBuiltinSig, Int)
    parseGetLegacy src = do
      (tag0, idxRaw) <- splitLast '_' src
      idx0 <- parseNat idxRaw
      let fieldCount0 = idx0 + 1
          fieldMap0 = [0 .. idx0]
      Just
        ( DataBuiltinSig
            { sigDataTypeTag = tag0
            , sigCtorTag = tag0
            , sigFieldCount = fieldCount0
            , sigTypeParamCount = fieldCount0
            , sigFieldParamMap = map Just fieldMap0
            }
        , idx0
        )

parseFieldMap :: String -> Maybe [Maybe Int]
parseFieldMap raw
  | raw == "none" = Just []
  | otherwise = traverse parseMaybeNat (splitOn '_' raw)

parseMaybeNat :: String -> Maybe (Maybe Int)
parseMaybeNat "u" = Just Nothing
parseMaybeNat s = Just <$> parseNat s

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
      Just (fst (splitTag tag0))

    parseIsLegacy :: Name -> Maybe Name
    parseIsLegacy src =
      if null src
        then Nothing
        else Just (fst (splitTag src))

splitTag :: Name -> (Name, Name)
splitTag tag0 =
  case splitOnLast '#' tag0 of
    Just (typeTag, ctorTag) -> (typeTag, ctorTag)
    Nothing -> (tag0, tag0)

splitOnLast :: Char -> String -> Maybe (String, String)
splitOnLast _ [] = Nothing
splitOnLast ch s =
  case reverse (splitOn ch s) of
    [] -> Nothing
    [_one] -> Nothing
    suffix : restRev ->
      let prefix = joinWith ch (reverse restRev)
       in if null prefix || null suffix
            then Nothing
            else Just (prefix, suffix)

lookupTypeParam :: String -> [Type] -> Int -> Either String Type
lookupTypeParam err allTypes idx
  | idx < 0 = Left err
  | otherwise =
      case drop idx allTypes of
        t:_ -> Right t
        [] -> Left err

lookupIndexEither :: String -> Int -> [a] -> Either String a
lookupIndexEither err idx xs
  | idx < 0 = Left err
  | otherwise =
      case drop idx xs of
        x:_ -> Right x
        [] -> Left err

unifyMany :: [Type] -> [Type] -> InferState -> Either String InferState
unifyMany as bs st0 =
  case (as, bs) of
    ([], []) -> Right st0
    (a:asRest, b:bsRest) -> do
      st1 <- unify a b st0
      unifyMany asRest bsRest st1
    _ -> Left "type mismatch: data type parameter arity differs"

parseNat :: String -> Maybe Int
parseNat s
  | null s = Nothing
  | all isDigit s =
      case reads s of
        [(k, "")] -> Just k
        _ -> Nothing
  | otherwise = Nothing

splitOn :: Char -> String -> [String]
splitOn delim = go [] []
  where
    go :: [String] -> String -> String -> [String]
    go acc cur [] = reverse (reverse cur : acc)
    go acc cur (c:cs)
      | c == delim = go (reverse cur : acc) [] cs
      | otherwise = go acc (c : cur) cs

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
    [_only] -> Nothing
    suffixRev:restRev ->
      let suffix = suffixRev
          prefix = joinWith delim (reverse restRev)
       in if null prefix || null suffix
            then Nothing
            else Just (prefix, suffix)

joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith delim (x:xs) = x <> [delim] <> joinWith delim xs

nameFromSeed :: Seed -> Name
nameFromSeed seed =
    case seedFn seed of
      Function n _ _ _ -> n

desugarCollectionExpr :: [Expr] -> Expr
desugarCollectionExpr elems =
  let emptyExpr = App (Var "collection_empty") (IntLit 0)
   in foldl (\acc elemExpr -> App (App (Var "collection_extend") acc) elemExpr) emptyExpr elems

argCount :: Function -> Int
argCount fn =
  case fn of
    Function _ as _ _ -> length as

duplicates :: [Name] -> [Name]
duplicates = reverse . go [] []
  where
    go :: [Name] -> [Name] -> [Name] -> [Name]
    go _seen dups [] = dups
    go seen dups (x:xs)
      | x `elem` seen && x `notElem` dups = go seen (x : dups) xs
      | x `elem` seen = go seen dups xs
      | otherwise = go (x : seen) dups xs
