module Clapse.Lowering
  ( Op(..)
  , FlatFunction(..)
  , lowerFunction
  , lowerModule
  ) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)

import Clapse.Syntax
  ( CaseArm(..)
  , CasePattern(..)
  , Expr(..)
  , Function(..)
  , Module(..)
  , Name
  , desugarCaseExpr
  )

data Op
  = PushI32 Int
  | PushString String
  | LocalGet Int
  | Call Name Int
  | MakeClosure Name [Int]
  | CallClosure Int
  deriving (Eq, Show)

data FlatFunction = FlatFunction
  { name :: Name
  , arity :: Int
  , captureArity :: Int
  , ops :: [Op]
  , lifted :: [FlatFunction]
  }
  deriving (Eq, Show)

type ArgMap = [(Name, Int)]

lowerModule :: Module -> Either String [FlatFunction]
lowerModule Module {functions = funs} = traverse lowerFunction funs

lowerFunction :: Function -> Either String FlatFunction
lowerFunction (Function fn params expr) = do
  argMap <- buildArgMap params
  (flatOps, liftedFns, _) <- lowerExpr fn argMap 0 expr
  pure
    FlatFunction
      { name = fn
      , arity = length params
      , captureArity = 0
      , ops = flatOps
      , lifted = liftedFns
      }

buildArgMap :: [Name] -> Either String ArgMap
buildArgMap names =
  case duplicates names of
    [] ->
      Right (zip names [0 ..])
    d:_ ->
      Left ("duplicate argument name: " <> d)

duplicates :: [Name] -> [Name]
duplicates = reverse . go [] []
  where
    go :: [Name] -> [Name] -> [Name] -> [Name]
    go _seen dups [] = dups
    go seen dups (x:xs)
      | x `elem` seen && x `notElem` dups = go seen (x : dups) xs
      | x `elem` seen = go seen dups xs
      | otherwise = go (x : seen) dups xs

lowerExpr :: Name -> ArgMap -> Int -> Expr -> Either String ([Op], [FlatFunction], Int)
lowerExpr owner args nextId expr =
  case expr of
    IntLit n ->
      Right ([PushI32 n], [], nextId)
    StringLit s ->
      Right ([PushString s], [], nextId)
    CollectionLit elems ->
      lowerExpr owner args nextId (desugarCollectionExpr elems)
    Case _ _ ->
      lowerExpr owner args nextId (desugarCaseExpr expr)
    Var v ->
      case lookup v args of
        Just localIx -> Right ([LocalGet localIx], [], nextId)
        Nothing ->
          if isZeroArityCtorBuiltin v
            then Right ([Call v 0], [], nextId)
            else Left ("unknown variable in value position: " <> v)
    Lam param bodyExpr ->
      lowerLambda owner args nextId param bodyExpr
    App _ _ ->
      lowerCall owner args nextId expr

lowerLambda :: Name -> ArgMap -> Int -> Name -> Expr -> Either String ([Op], [FlatFunction], Int)
lowerLambda owner args nextId param bodyExpr = do
  let lamName = owner <> "$lam" <> show nextId
      nextIdAfterName = nextId + 1
      captures = captureNames args (Lam param bodyExpr)
  captureArgMap <- buildArgMap captures
  let shiftedCaptureArgMap = captureArgMap
      lambdaParamArgMap = [(param, length captures)]
      lambdaArgMap = shiftedCaptureArgMap <> lambdaParamArgMap
  (bodyOps, nestedLifted, nextIdFinal) <- lowerExpr lamName lambdaArgMap nextIdAfterName bodyExpr
  captureIndices <- traverse (lookupArg args) captures
  let closureOp = MakeClosure lamName captureIndices
      lambdaFn =
        FlatFunction
          { name = lamName
          , arity = 1
          , captureArity = length captures
          , ops = bodyOps
          , lifted = nestedLifted
          }
  Right ([closureOp], [lambdaFn], nextIdFinal)

lowerCall :: Name -> ArgMap -> Int -> Expr -> Either String ([Op], [FlatFunction], Int)
lowerCall owner args nextId expr =
  case unwindApp expr of
    (Var callee, callArgs)
      | callee `elem` map fst args ->
          lowerDynamicCall owner args nextId (Var callee) callArgs
      | otherwise ->
          lowerDirectCall owner args nextId callee callArgs
    (calleeExpr, callArgs) ->
      lowerDynamicCall owner args nextId calleeExpr callArgs

lowerDirectCall :: Name -> ArgMap -> Int -> Name -> [Expr] -> Either String ([Op], [FlatFunction], Int)
lowerDirectCall owner args nextId callee callArgs = do
  (argOps, liftedFns, nextIdFinal) <- lowerExprList owner args nextId callArgs
  Right (argOps <> [Call callee (length callArgs)], liftedFns, nextIdFinal)

lowerDynamicCall :: Name -> ArgMap -> Int -> Expr -> [Expr] -> Either String ([Op], [FlatFunction], Int)
lowerDynamicCall owner args nextId calleeExpr callArgs = do
  (calleeOps, calleeLifted, nextAfterCallee) <- lowerExpr owner args nextId calleeExpr
  (argOps, argLifted, nextIdFinal) <- lowerExprList owner args nextAfterCallee callArgs
  Right (calleeOps <> argOps <> [CallClosure (length callArgs)], calleeLifted <> argLifted, nextIdFinal)

lowerExprList :: Name -> ArgMap -> Int -> [Expr] -> Either String ([Op], [FlatFunction], Int)
lowerExprList _owner _args nextId [] = Right ([], [], nextId)
lowerExprList owner args nextId (e:es) = do
  (ops0, lifted0, next0) <- lowerExpr owner args nextId e
  (ops1, lifted1, next1) <- lowerExprList owner args next0 es
  Right (ops0 <> ops1, lifted0 <> lifted1, next1)

lookupArg :: ArgMap -> Name -> Either String Int
lookupArg args n =
  case lookup n args of
    Just ix -> Right ix
    Nothing -> Left ("capture lookup failed for: " <> n)

captureNames :: ArgMap -> Expr -> [Name]
captureNames args expr =
  let inScope = map fst args
   in filter (`elem` inScope) (freeVars expr)

freeVars :: Expr -> [Name]
freeVars = unique . go []
  where
    go :: [Name] -> Expr -> [Name]
    go bound expr =
      case expr of
        IntLit _ ->
          []
        StringLit _ ->
          []
        CollectionLit elems ->
          foldMap (go bound) elems
        Case scrutinees arms ->
          concatMap (go bound) scrutinees
            <> concatMap (goArm bound) arms
        Var n
          | n `elem` bound -> []
          | otherwise -> [n]
        App f x ->
          go bound f <> go bound x
        Lam n bodyExpr ->
          go (n : bound) bodyExpr

    goArm :: [Name] -> CaseArm -> [Name]
    goArm bound arm =
      go (patternBound (armPatterns arm) <> bound) (armBody arm)

    patternBound :: [CasePattern] -> [Name]
    patternBound = foldr (\pat acc -> fromPat pat <> acc) []

    fromPat :: CasePattern -> [Name]
    fromPat pat =
      case pat of
        PatWildcard -> []
        PatVar n -> [n]
        PatInt _ -> []
        PatConstructor _ _ fieldNames -> [n | n <- fieldNames, n /= "_"]

    unique :: [Name] -> [Name]
    unique = goUnique []

    goUnique :: [Name] -> [Name] -> [Name]
    goUnique _seen [] = []
    goUnique seen (n:ns)
      | n `elem` seen = goUnique seen ns
      | otherwise = n : goUnique (n : seen) ns

unwindApp :: Expr -> (Expr, [Expr])
unwindApp expr = go expr []
  where
    go :: Expr -> [Expr] -> (Expr, [Expr])
    go (App fn arg) acc = go fn (arg : acc)
    go headExpr acc = (headExpr, acc)

desugarCollectionExpr :: [Expr] -> Expr
desugarCollectionExpr elems =
  let emptyExpr = App (Var "collection_empty") (IntLit 0)
   in foldl (\acc elemExpr -> App (App (Var "collection_extend") acc) elemExpr) emptyExpr elems

isZeroArityCtorBuiltin :: Name -> Bool
isZeroArityCtorBuiltin n =
  case parseMkBuiltinArity n of
    Just arityN -> arityN == 0
    Nothing -> False

parseMkBuiltinArity :: Name -> Maybe Int
parseMkBuiltinArity n = do
  rest <- stripPrefix "__mk_" n
  parseMkDetailed rest <|> parseMkLegacy rest
  where
    parseMkDetailed :: String -> Maybe Int
    parseMkDetailed src = do
      (prefix0, tparAndMap0) <- splitOnToken "_tpar_" src
      (_tparRaw, mapRaw) <- splitOnToken "_fmap_" tparAndMap0
      (_tag0, fieldCountRaw) <- splitLast '_' prefix0
      fieldCount0 <- parseNat fieldCountRaw
      _fieldMap0 <- parseFieldMap mapRaw
      Just fieldCount0

    parseMkLegacy :: String -> Maybe Int
    parseMkLegacy src = do
      (_tag0, fieldCountRaw) <- splitLast '_' src
      parseNat fieldCountRaw

parseFieldMap :: String -> Maybe [Maybe Int]
parseFieldMap raw
  | raw == "none" = Just []
  | otherwise = traverse parseFieldEntry (splitOn '_' raw)
  where
    parseFieldEntry :: String -> Maybe (Maybe Int)
    parseFieldEntry entry
      | entry == "u" = Just Nothing
      | otherwise = Just <$> parseNat entry

parseNat :: String -> Maybe Int
parseNat s
  | null s = Nothing
  | all isDigit s =
      case reads s of
        [(n, "")] -> Just n
        _ -> Nothing
  | otherwise = Nothing

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
