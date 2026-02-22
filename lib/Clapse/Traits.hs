module Clapse.Traits
  ( TraitCategory(..)
  , RewriteRule(..)
  , Pattern(..)
  , Trait(..)
  , basicTraits
  , traitsByCategory
  , rewriteOnceWithTraits
  , rewriteOnceWithBasicTraits
  , normalizeWithTraits
  , normalizeWithBasicTraits
  ) where

import Control.Applicative ((<|>))

import Clapse.Syntax (CaseArm(..), Expr(..), Name)

data TraitCategory
  = ArithmeticCategory
  | MonoidCategory
  | FunctorCategory
  | ApplicativeCategory
  | MonadCategory
  deriving (Eq, Show)

data Pattern
  = PHole Name
  | PVar Name
  | PInt Int
  | PLam Name Pattern
  | PApp Pattern Pattern
  deriving (Eq, Show)

data RewriteRule = RewriteRule
  { ruleName :: Name
  , lhs :: Pattern
  , rhs :: Pattern
  }
  deriving (Eq, Show)

data Trait = Trait
  { traitName :: Name
  , category :: TraitCategory
  , operations :: [Name]
  , rules :: [RewriteRule]
  }
  deriving (Eq, Show)

basicTraits :: [Trait]
basicTraits =
  [ addTrait
  , subTrait
  , mulTrait
  , divTrait
  , monoidTrait
  , functorTrait
  , applicativeTrait
  , monadTrait
  ]

traitsByCategory :: TraitCategory -> [Trait] -> [Trait]
traitsByCategory cat = filter (\t -> category t == cat)

rewriteOnceWithBasicTraits :: Expr -> Maybe Expr
rewriteOnceWithBasicTraits = rewriteOnceWithTraits basicTraits

normalizeWithBasicTraits :: Int -> Expr -> Expr
normalizeWithBasicTraits maxSteps = normalizeWithTraits maxSteps basicTraits

normalizeWithTraits :: Int -> [Trait] -> Expr -> Expr
normalizeWithTraits maxSteps allTraits = go maxSteps
  where
    go :: Int -> Expr -> Expr
    go steps expr
      | steps <= 0 = expr
      | otherwise =
          case rewriteOnceWithTraits allTraits expr of
            Nothing -> expr
            Just expr' -> go (steps - 1) expr'

rewriteOnceWithTraits :: [Trait] -> Expr -> Maybe Expr
rewriteOnceWithTraits allTraits expr =
  rewriteAtRoot allTraits expr
    <|> rewriteInside allTraits expr

rewriteAtRoot :: [Trait] -> Expr -> Maybe Expr
rewriteAtRoot allTraits expr =
  foldFirst (map (`applyTrait` expr) allTraits)
    <|> numericFold expr

rewriteInside :: [Trait] -> Expr -> Maybe Expr
rewriteInside allTraits expr =
  case expr of
    App f x ->
      ((`App` x) <$> rewriteOnceWithTraits allTraits f)
        <|> ((App f) <$> rewriteOnceWithTraits allTraits x)
    Lam n bodyExpr ->
      Lam n <$> rewriteOnceWithTraits allTraits bodyExpr
    CollectionLit elems ->
      CollectionLit <$> rewriteCollectionElems elems
    Case scrutinees arms ->
      ((\scrutinees' -> Case scrutinees' arms) <$> rewriteCaseScrutinees scrutinees)
        <|> ((\arms' -> Case scrutinees arms') <$> rewriteCaseArms arms)
    _ ->
      Nothing
  where
    rewriteCollectionElems :: [Expr] -> Maybe [Expr]
    rewriteCollectionElems elems =
      case elems of
        [] -> Nothing
        e:es ->
          case rewriteOnceWithTraits allTraits e of
            Just e' -> Just (e' : es)
            Nothing -> (e :) <$> rewriteCollectionElems es

    rewriteCaseScrutinees :: [Expr] -> Maybe [Expr]
    rewriteCaseScrutinees scrs =
      case scrs of
        [] -> Nothing
        e:es ->
          case rewriteOnceWithTraits allTraits e of
            Just e' -> Just (e' : es)
            Nothing -> (e :) <$> rewriteCaseScrutinees es

    rewriteCaseArms :: [CaseArm] -> Maybe [CaseArm]
    rewriteCaseArms arms0 =
      case arms0 of
        [] -> Nothing
        arm:rest ->
          case rewriteOnceWithTraits allTraits (armBody arm) of
            Just body' -> Just (arm {armBody = body'} : rest)
            Nothing -> (arm :) <$> rewriteCaseArms rest

applyTrait :: Trait -> Expr -> Maybe Expr
applyTrait trait expr = foldFirst (map (`applyRule` expr) (rules trait))

applyRule :: RewriteRule -> Expr -> Maybe Expr
applyRule rule expr = do
  subs <- matchPattern [] (lhs rule) expr
  instantiatePattern subs (rhs rule)

type Substitution = [(Name, Expr)]

matchPattern :: Substitution -> Pattern -> Expr -> Maybe Substitution
matchPattern subs pat expr =
  case pat of
    PHole holeName ->
      case lookup holeName subs of
        Nothing ->
          Just ((holeName, expr) : subs)
        Just previous
          | previous == expr -> Just subs
          | otherwise -> Nothing
    PVar v ->
      case expr of
        Var x
          | x == v -> Just subs
          | otherwise -> Nothing
        _ -> Nothing
    PInt n ->
      case expr of
        IntLit m
          | m == n -> Just subs
          | otherwise -> Nothing
        _ -> Nothing
    PLam n pBody ->
      case expr of
        Lam m eBody
          | m == n -> matchPattern subs pBody eBody
          | otherwise -> Nothing
        _ -> Nothing
    PApp pF pX ->
      case expr of
        App eF eX -> do
          subs1 <- matchPattern subs pF eF
          matchPattern subs1 pX eX
        _ ->
          Nothing

instantiatePattern :: Substitution -> Pattern -> Maybe Expr
instantiatePattern subs pat =
  case pat of
    PHole holeName ->
      lookup holeName subs
    PVar n ->
      Just (Var n)
    PInt n ->
      Just (IntLit n)
    PLam n bodyPat ->
      Lam n <$> instantiatePattern subs bodyPat
    PApp fPat xPat ->
      App <$> instantiatePattern subs fPat <*> instantiatePattern subs xPat

numericFold :: Expr -> Maybe Expr
numericFold expr =
  case unwindApp expr of
    (Var "add", [IntLit a, IntLit b]) ->
      Just (IntLit (a + b))
    (Var "sub", [IntLit a, IntLit b]) ->
      Just (IntLit (a - b))
    (Var "mul", [IntLit a, IntLit b]) ->
      Just (IntLit (a * b))
    (Var "div", [IntLit _a, IntLit 0]) ->
      Nothing
    (Var "div", [IntLit a, IntLit b]) ->
      Just (IntLit (a `div` b))
    _ ->
      Nothing

foldFirst :: [Maybe a] -> Maybe a
foldFirst [] = Nothing
foldFirst (x:xs) =
  case x of
    Just _ -> x
    Nothing -> foldFirst xs

unwindApp :: Expr -> (Expr, [Expr])
unwindApp = go []
  where
    go :: [Expr] -> Expr -> (Expr, [Expr])
    go acc (App f x) = go (x : acc) f
    go acc headExpr = (headExpr, acc)

pCall :: Name -> [Pattern] -> Pattern
pCall fnName args = foldl PApp (PVar fnName) args

addTrait :: Trait
addTrait =
  Trait
    { traitName = "add"
    , category = ArithmeticCategory
    , operations = ["add"]
    , rules =
        [ RewriteRule "add-right-zero" (pCall "add" [PHole "x", PInt 0]) (PHole "x")
        , RewriteRule "add-left-zero" (pCall "add" [PInt 0, PHole "x"]) (PHole "x")
        ]
    }

subTrait :: Trait
subTrait =
  Trait
    { traitName = "sub"
    , category = ArithmeticCategory
    , operations = ["sub"]
    , rules =
        [ RewriteRule "sub-right-zero" (pCall "sub" [PHole "x", PInt 0]) (PHole "x")
        , RewriteRule "sub-self" (pCall "sub" [PHole "x", PHole "x"]) (PInt 0)
        ]
    }

mulTrait :: Trait
mulTrait =
  Trait
    { traitName = "mul"
    , category = ArithmeticCategory
    , operations = ["mul"]
    , rules =
        [ RewriteRule "mul-right-one" (pCall "mul" [PHole "x", PInt 1]) (PHole "x")
        , RewriteRule "mul-left-one" (pCall "mul" [PInt 1, PHole "x"]) (PHole "x")
        , RewriteRule "mul-right-zero" (pCall "mul" [PHole "x", PInt 0]) (PInt 0)
        , RewriteRule "mul-left-zero" (pCall "mul" [PInt 0, PHole "x"]) (PInt 0)
        ]
    }

divTrait :: Trait
divTrait =
  Trait
    { traitName = "div"
    , category = ArithmeticCategory
    , operations = ["div"]
    , rules =
        [ RewriteRule "div-right-one" (pCall "div" [PHole "x", PInt 1]) (PHole "x")
        ]
    }

monoidTrait :: Trait
monoidTrait =
  Trait
    { traitName = "monoid"
    , category = MonoidCategory
    , operations = ["empty", "append"]
    , rules =
        [ RewriteRule "append-left-empty" (pCall "append" [PVar "empty", PHole "x"]) (PHole "x")
        , RewriteRule "append-right-empty" (pCall "append" [PHole "x", PVar "empty"]) (PHole "x")
        , RewriteRule
            "append-assoc-right"
            (pCall "append" [pCall "append" [PHole "x", PHole "y"], PHole "z"])
            (pCall "append" [PHole "x", pCall "append" [PHole "y", PHole "z"]])
        ]
    }

functorTrait :: Trait
functorTrait =
  Trait
    { traitName = "functor"
    , category = FunctorCategory
    , operations = ["fmap", "id", "compose"]
    , rules =
        [ RewriteRule "fmap-id" (pCall "fmap" [PVar "id", PHole "x"]) (PHole "x")
        , RewriteRule
            "fmap-compose"
            (pCall "fmap" [PHole "f", pCall "fmap" [PHole "g", PHole "x"]])
            (pCall "fmap" [pCall "compose" [PHole "f", PHole "g"], PHole "x"])
        ]
    }

applicativeTrait :: Trait
applicativeTrait =
  Trait
    { traitName = "applicative"
    , category = ApplicativeCategory
    , operations = ["pure", "ap", "fmap"]
    , rules =
        [ RewriteRule
            "ap-pure"
            (pCall "ap" [pCall "pure" [PHole "f"], PHole "x"])
            (pCall "fmap" [PHole "f", PHole "x"])
        ]
    }

monadTrait :: Trait
monadTrait =
  Trait
    { traitName = "monad"
    , category = MonadCategory
    , operations = ["pure", "bind"]
    , rules =
        [ RewriteRule
            "bind-left-identity"
            (pCall "bind" [pCall "pure" [PHole "x"], PHole "f"])
            (PApp (PHole "f") (PHole "x"))
        , RewriteRule
            "bind-right-identity"
            (pCall "bind" [PHole "m", PVar "pure"])
            (PHole "m")
        , RewriteRule
            "bind-assoc"
            (pCall "bind" [pCall "bind" [PHole "m", PHole "f"], PHole "g"])
            ( pCall
                "bind"
                [ PHole "m"
                , PLam "x" (pCall "bind" [PApp (PHole "f") (PVar "x"), PHole "g"])
                ]
            )
        ]
    }
