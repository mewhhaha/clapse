module Clapse.Syntax
  ( Name
  , Module(..)
  , TypeSignature(..)
  , SignatureConstraint(..)
  , FunctionAttributePlugin(..)
  , FunctionAttribute(..)
  , FunctionAttributeValue(..)
  , Function(..)
  , Expr(..)
  , CaseArm(..)
  , CasePattern(..)
  , ConstructorInfo(..)
  , parseModule
  , parseModuleWithPlugins
  , parseModuleWithConstructorImportsInfo
  , parseFunctionLine
  , parseExpr
  , desugarCaseExpr
  , defaultFunctionAttributePlugins
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace, toLower)
import Data.Foldable (traverse_)
import Data.List (dropWhileEnd, intercalate, isSuffixOf)
import Data.Maybe (isNothing, mapMaybe)

import Clapse.Laws
  ( ClassDef(..)
  , ClassKind
  , Law(..)
  , mkClassDef
  , parseClassKind
  , requiredMethodNames
  )

type Name = String

data Module = Module
  { signatures :: [TypeSignature]
  , functions :: [Function]
  }
  deriving (Eq, Show)

data TypeSignature = TypeSignature
  { sigName :: Name
  , sigConstraints :: [SignatureConstraint]
  , sigTypeExpr :: String
  }
  deriving (Eq, Show)

data SignatureConstraint = SignatureConstraint
  { constraintWitness :: Maybe Name
  , constraintClass :: Name
  , constraintTypeArgs :: [Name]
  }
  deriving (Eq, Show)

data Function = Function
  { name :: Name
  , args :: [Name]
  , body :: Expr
  , attributes :: [FunctionAttribute]
  }
  deriving (Eq, Show)

data FunctionAttributeValue
  = AttributeString String
  | AttributeInt Int
  | AttributeName Name
  | AttributeRaw String
  deriving (Eq, Show)

data FunctionAttribute = FunctionAttribute
  { attributeName :: Name
  , attributeValue :: Maybe FunctionAttributeValue
  }
  deriving (Eq, Show)

data FunctionAttributePlugin = FunctionAttributePlugin
  { pluginName :: Name
  , pluginApply :: Function -> FunctionAttribute -> Either String Function
  }

data Expr
  = Var Name
  | IntLit Int
  | StringLit String
  | CollectionLit [Expr]
  | Case [Expr] [CaseArm]
  | Lam Name Expr
  | App Expr Expr
  deriving (Eq, Show)

data CaseArm = CaseArm
  { armPatterns :: [CasePattern]
  , armBody :: Expr
  }
  deriving (Eq, Show)

data CasePattern
  = PatWildcard
  | PatVar Name
  | PatInt Int
  | PatString String
  | PatConstructor Name ConstructorInfo [Name]
  deriving (Eq, Show)

data ParsedLine
  = ParsedFunctions [FunctionClause]
  | ParsedSignature TypeSignature
  | ParsedClass ClassDecl
  | ParsedLaw LawDecl
  | ParsedInstance InstanceDecl
  | ParsedOperator OperatorDecl

data FunctionClause = FunctionClause
  { fnClauseName :: Name
  , fnClausePatterns :: [CasePattern]
  , fnClauseBody :: Expr
  , fnClauseAttributes :: [FunctionAttribute]
  } deriving (Eq, Show)

data RawLine
  = RawEmpty
  | RawAttribute Int String
  | RawSignature Int String
  | RawFunction Int String
  | RawFunctionContinuation Int String
  | RawData Int String
  | RawType Int String
  | RawClass Int String
  | RawLaw Int String
  | RawInstance Int String
  | RawOperator Int String

data ContinuationKind
  = ContinueNone
  | ContinueExpr
  | ContinueCase Bool
  | ContinueLet Bool
  | ContinueGuard

data ClassDecl = ClassDecl
  { declClassLine :: Int
  , declClassName :: Name
  , declClassParams :: [Name]
  , declClassKind :: ClassKind
  }

data LawDecl = LawDecl
  { declLawLine :: Int
  , declLawClass :: Name
  , declLaw :: Law Expr
  }

data InstanceDecl = InstanceDecl
  { declInstanceLine :: Int
  , declInstanceName :: Name
  , declInstanceClass :: Name
  , declInstanceTypeArgs :: [Name]
  , declInstanceBindings :: [(Name, Name)]
  }

data CheckedClass = CheckedClass
  { checkedClassName :: Name
  , checkedClassParams :: [Name]
  , checkedClassDef :: ClassDef Expr
  }

data CheckedInstance = CheckedInstance
  { checkedInstanceClass :: CheckedClass
  , checkedInstanceBindings :: [(Name, Name)]
  }

data DerivedRule = DerivedRule
  { derivedLiteralNames :: [Name]
  , derivedLhs :: Expr
  , derivedRhs :: Expr
  }

data Assoc
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Eq, Show)

data OperatorDecl = OperatorDecl
  { declOpLine :: Int
  , declOpAssoc :: Assoc
  , declOpPrecedence :: Int
  , declOpToken :: String
  , declOpTarget :: Name
  }

data OperatorInfo = OperatorInfo
  { opAssoc :: Assoc
  , opPrecedence :: Int
  , opTarget :: Name
  }

data ExprTok
  = TokIdentifier Name
  | TokInteger Int
  | TokString String
  | TokOperator String
  | TokLParen
  | TokRParen
  | TokLBracket
  | TokRBracket
  | TokComma
  | TokLambda
  | TokArrow
  | TokLet
  | TokIn
  | TokCase
  | TokOf
  | TokSemicolon
  deriving (Eq, Show)

data LetBinding
  = LetValueBinding Name Expr
  | LetPatternBinding ConstructorInfo [Name] Expr
  deriving (Eq, Show)

data ConstructorInfo = ConstructorInfo
  { ctorTypeName :: Name
  , ctorName :: Name
  , ctorFieldCount :: Int
  , ctorTypeParamCount :: Int
  , ctorFieldParamMap :: [Maybe Int]
  , ctorLiteralBacking :: Maybe TypeUnionLiteral
  }
  deriving (Eq, Show)

data DataDecl = DataDecl
  { dataTypeName :: Name
  , dataTypeParams :: [Name]
  , dataCtorName :: Name
  , dataCtorFields :: [Name]
  , dataCtorInfo :: ConstructorInfo
  }
  deriving (Eq, Show)

data TypeUnionLiteral
  = TypeUnionInt Int
  | TypeUnionString String
  deriving (Eq, Show)

type Substitution = [(Name, Expr)]
type OperatorTable = [(String, OperatorInfo)]
type ConstructorTable = [(Name, ConstructorInfo)]

parseModule :: String -> Either String Module
parseModule src = parseModuleWithPlugins defaultFunctionAttributePlugins src

parseModuleWithPlugins :: [FunctionAttributePlugin] -> String -> Either String Module
parseModuleWithPlugins plugins src =
  fst <$> parseModuleWithPluginsAndConstructors plugins [] src

parseModuleWithConstructorImportsInfo :: ConstructorTable -> String -> Either String (Module, ConstructorTable)
parseModuleWithConstructorImportsInfo importedConstructors src =
  parseModuleWithPluginsAndConstructors defaultFunctionAttributePlugins importedConstructors src

parseModuleWithPluginsAndConstructors :: [FunctionAttributePlugin] -> ConstructorTable -> String -> Either String (Module, ConstructorTable)
parseModuleWithPluginsAndConstructors plugins importedConstructors src = do
  rawLines <- traverse classifyLine (zip [1 :: Int ..] (lines src))
  logicalLines <- coalesceRawLines rawLines
  operatorDecls <- collectOperatorDecls rawLines
  localConstructorTable <- collectConstructorDecls rawLines
  let constructorTable = localConstructorTable <> importedConstructors
  let operatorTable = buildOperatorTable operatorDecls
  parsed <- parseLinesWithAttributes operatorTable constructorTable logicalLines
  signatureDecls <- collectSignatureDecls parsed
  validateSignatureTargets signatureDecls parsed
  derivedRules <- deriveRules parsed
  let runtimeFns = concatMap lineFunctions parsed
  mergedFns <- mergeFunctionClauses runtimeFns
  let optimizedFns = optimizeFunctions derivedRules mergedFns
  processedFns <- applyFunctionAttributePlugins plugins optimizedFns
  pure (Module {signatures = signatureDecls, functions = processedFns}, localConstructorTable)
  where
    parseLinesWithAttributes :: OperatorTable -> ConstructorTable -> [RawLine] -> Either String [ParsedLine]
    parseLinesWithAttributes operators constructors linesToParse =
      go linesToParse [] Nothing []
      where
        go
          :: [RawLine]
          -> [(Int, FunctionAttribute)]
          -> Maybe (Name, [FunctionAttribute])
          -> [ParsedLine]
          -> Either String [ParsedLine]
        go [] pending _active parsedOut =
          case pending of
            [] -> Right (reverse parsedOut)
            (pendingLineNo, _) : _ ->
              Left (withLine pendingLineNo "orphaned attribute; expected a following function declaration")
        go (currentLine : rest) pending active parsedOut =
          case currentLine of
            RawAttribute lineNo cleaned -> do
              attr <- parseFunctionAttribute lineNo cleaned
              go rest ((lineNo, attr) : pending) active parsedOut
            RawFunction _ _ -> do
              parsedLine <- parseRawLine operators constructors currentLine
              case parsedLine of
                ParsedFunctions [] ->
                  Left "internal parser error: function declaration produced no clauses"
                ParsedFunctions fnClauses@(firstClause : _) -> do
                  let fnName = fnClauseName firstClause
                      attrsFromPending = map snd (reverse pending)
                      currentAttrs =
                        if null pending
                          then
                            case active of
                              Just (activeName, activeAttrs)
                                | activeName == fnName -> activeAttrs
                                | otherwise -> []
                              Nothing -> []
                          else attrsFromPending
                      parsedWithAttrs = ParsedFunctions (map (attachClauseAttributes currentAttrs) fnClauses)
                      nextActive =
                        case (pending, currentAttrs) of
                          ([], []) ->
                            case active of
                              Just (activeName, activeAttrs)
                                | activeName == fnName -> Just (activeName, activeAttrs)
                                | otherwise -> Nothing
                              _ -> Nothing
                          _ -> Just (fnName, currentAttrs)
                  go rest [] nextActive (parsedWithAttrs : parsedOut)
                _ -> Left "internal parser error: expected function clauses"
            _ ->
              if null pending
                then do
                  parsedLine <- parseRawLine operators constructors currentLine
                  go rest [] Nothing (parsedLine : parsedOut)
                else
                  Left
                    ( withLine
                        (fst (last pending))
                        "attribute must annotate a function declaration"
                    )

        parseFunctionAttribute :: Int -> String -> Either String FunctionAttribute
        parseFunctionAttribute lineNo cleaned = do
          let source = trim cleaned
          if null source
            then Left (withLine lineNo "attribute name is missing")
            else do
              let (nameText, valueTextRaw) = span isIdentifierChar source
                  valueText = trim valueTextRaw
              if null nameText
                then Left (withLine lineNo "attribute name is missing")
                else do
                  validateIdentifier "attribute name" nameText
                  attrValue <-
                    if null valueText
                      then Right Nothing
                      else Just <$> parseAttributeValue lineNo valueText
                  Right
                    FunctionAttribute
                      { attributeName = nameText
                      , attributeValue = attrValue
                      }

        parseAttributeValue :: Int -> String -> Either String FunctionAttributeValue
        parseAttributeValue lineNo raw =
          case tokenizeExpr raw of
            Right [TokString value] ->
              Right (AttributeString value)
            Right [TokInteger value] ->
              Right (AttributeInt value)
            Right [TokIdentifier value] ->
              Right (AttributeName value)
            Right _ ->
              Left
                ( withLine
                    lineNo
                    "attribute value must be one token (string, integer, or identifier)"
                )
            Left _ ->
              Left (withLine lineNo "invalid attribute value")

        attachClauseAttributes :: [FunctionAttribute] -> FunctionClause -> FunctionClause
        attachClauseAttributes attrs clause =
          clause {fnClauseAttributes = attrs}
    coalesceRawLines :: [RawLine] -> Either String [RawLine]
    coalesceRawLines rawLines = do
      merged <- go [] rawLines
      Right (map fst (reverse merged))
      where
        step :: [(RawLine, ContinuationKind)] -> RawLine -> Either String [(RawLine, ContinuationKind)]
        step acc nextLine =
          case nextLine of
            RawFunctionContinuation lineNo arm ->
              case acc of
                [] ->
                  Left (withLine lineNo "unexpected indented declaration line")
                (current, continuation) : rest ->
                  case continuation of
                    ContinueNone ->
                      case current of
                        RawFunction declLineNo currentText ->
                          let currentTrimmed = trim currentText
                           in if endsWithEquals currentTrimmed
                                then
                                  let mergedText = currentText <> " " <> arm
                                      detectedContinuation = continuationKindForFunction mergedText
                                      nextContinuation =
                                        case detectedContinuation of
                                          ContinueNone -> ContinueExpr
                                          _ -> detectedContinuation
                                   in Right ((RawFunction declLineNo mergedText, nextContinuation) : rest)
                                else
                                  Left
                                    ( withLine
                                        lineNo
                                        "indented line is not allowed here (expected case/let/guard continuation)"
                                    )
                        _ ->
                          Left
                            ( withLine
                                lineNo
                                "indented line is not allowed here (expected case/let/guard continuation)"
                            )
                    ContinueExpr ->
                      case current of
                        RawFunction declLineNo currentText ->
                          let mergedText = currentText <> " " <> arm
                              detectedContinuation = continuationKindForFunction mergedText
                              nextContinuation =
                                case detectedContinuation of
                                  ContinueNone -> ContinueExpr
                                  _ -> detectedContinuation
                           in Right ((RawFunction declLineNo mergedText, nextContinuation) : rest)
                        _ ->
                          Left (withLine lineNo "indented line is not allowed here")
                    ContinueCase hasArm ->
                      case current of
                        RawFunction declLineNo currentText ->
                          let currentTrimmed = trim currentText
                              armBodyContinues = endsWithArrow currentTrimmed
                              currentEndsWithOf = endsWithKeyword "of" currentTrimmed
                              separator =
                                if hasArm && not armBodyContinues && not currentEndsWithOf
                                  then " ; "
                                  else " "
                              mergedText = currentText <> separator <> arm
                              nextContinuation =
                                if hasUnclosedLet mergedText
                                  then ContinueLet True
                                  else ContinueCase True
                           in Right ((RawFunction declLineNo mergedText, nextContinuation) : rest)
                        _ ->
                          Left (withLine lineNo "indented line is not allowed here")
                    ContinueLet continueCaseAfterLet ->
                      case current of
                        RawFunction declLineNo currentText ->
                          let armTrimmed = trim arm
                              currentTrimmed = trim currentText
                              currentEndsWithLet =
                                case words currentTrimmed of
                                  [] -> False
                                  ws -> last ws == "let"
                              currentEndsWithEquals = endsWithEquals currentTrimmed
                              separator =
                                if startsWith "in " armTrimmed || armTrimmed == "in" || currentEndsWithLet || currentEndsWithEquals || ";" `isSuffixOf` currentTrimmed || startsWithOperatorContinuation armTrimmed
                                  then " "
                                  else " ; "
                              mergedText = currentText <> separator <> arm
                              detectedContinuation = continuationKindForFunction mergedText
                              nextContinuation =
                                case detectedContinuation of
                                  ContinueNone
                                    | continueCaseAfterLet ->
                                        ContinueCase True
                                  _ ->
                                    detectedContinuation
                           in Right ((RawFunction declLineNo mergedText, nextContinuation) : rest)
                        _ ->
                          Left (withLine lineNo "indented line is not allowed here")
                    ContinueGuard ->
                      if startsWith "|" arm
                        then
                          case current of
                            RawFunction declLineNo currentText ->
                              let mergedText = currentText <> " ; " <> arm
                               in Right ((RawFunction declLineNo mergedText, ContinueGuard) : rest)
                            _ ->
                              Left (withLine lineNo "indented line is not allowed here")
                        else
                          Left
                            ( withLine
                                lineNo
                                "indented line is not allowed here (expected guard continuation line starting with |)"
                            )
            _ ->
              Right ((nextLine, continuationKindForLine nextLine) : acc)

        go :: [(RawLine, ContinuationKind)] -> [RawLine] -> Either String [(RawLine, ContinuationKind)]
        go acc [] = Right acc
        go acc (next : rest) = do
          nextAcc <- step acc next
          go nextAcc rest

        continuationKindForLine :: RawLine -> ContinuationKind
        continuationKindForLine rawLine =
          case rawLine of
            RawFunction _ rhs -> continuationKindForFunction rhs
            _ -> ContinueNone

        continuationKindForFunction :: String -> ContinuationKind
        continuationKindForFunction source =
          if isCaseHeader source
            then ContinueCase False
            else
              if hasUnclosedLet source
                then ContinueLet (hasCaseContext source)
                else if hasGuardHeader source then ContinueGuard else ContinueNone

        isCaseHeader :: String -> Bool
        isCaseHeader source =
          case splitOnFirst '=' source of
            Nothing -> False
            Just (_, rhsRaw) ->
              case words (trim rhsRaw) of
                [] -> False
                rhsWords -> last rhsWords == "of"

        hasCaseContext :: String -> Bool
        hasCaseContext source =
          case splitOnFirst '=' source of
            Nothing -> False
            Just (_, rhsRaw) ->
              let rhsWords = words (trim rhsRaw)
               in case rhsWords of
                    [] -> False
                    firstWord : _ ->
                      firstWord == "case" && "of" `elem` rhsWords

        hasUnclosedLet :: String -> Bool
        hasUnclosedLet source =
          case splitOnFirst '=' source of
            Nothing -> False
            Just (_, rhsRaw) ->
              let kws = keywordsOutsideString rhsRaw
                  openCount = keywordCount "let" kws
                  closeCount = keywordCount "in" kws
               in openCount > closeCount

        hasGuardHeader :: String -> Bool
        hasGuardHeader source =
          case splitOnFirst '=' source of
            Nothing -> False
            Just (lhsRaw, _rhsRaw) -> '|' `elem` lhsRaw

        endsWithArrow :: String -> Bool
        endsWithArrow source =
          let trimmed = trimRight source
              n = length trimmed
           in n >= 2 && drop (n - 2) trimmed == "->"

        endsWithEquals :: String -> Bool
        endsWithEquals source =
          let trimmed = trimRight source
              n = length trimmed
           in n >= 1 && drop (n - 1) trimmed == "="

        endsWithKeyword :: String -> String -> Bool
        endsWithKeyword keyword source =
          case words source of
            [] -> False
            ws -> last ws == keyword

        startsWithOperatorContinuation :: String -> Bool
        startsWithOperatorContinuation txt = startsWithAny operatorContinuationPrefixes txt

        startsWithAny :: [String] -> String -> Bool
        startsWithAny [] _ = False
        startsWithAny (prefix : rest) txt =
          if startsWith prefix txt
            then True
            else startsWithAny rest txt

        operatorContinuationPrefixes :: [String]
        operatorContinuationPrefixes =
          [ "<|>"
          , "*>"
          , "<*"
          , "<$>"
          , "<*>"
          , ">>="
          , ">>"
          , "&&"
          , "||"
          , "=="
          , "/="
          , "<="
          , ">="
          , "+"
          , "-"
          , "*"
          , "/"
          , "<"
          , ">"
          , ":"
          , "."
          , "|"
          , "&"
          , "="
          ]

        trimRight :: String -> String
        trimRight = reverse . trimLeft . reverse

        keywordCount :: String -> [String] -> Int
        keywordCount _ [] = 0
        keywordCount target (x:xs)
          | x == target = 1 + keywordCount target xs
          | otherwise = keywordCount target xs

        keywordsOutsideString :: String -> [String]
        keywordsOutsideString = goKeywords False False [] []
          where
            goKeywords :: Bool -> Bool -> String -> [String] -> String -> [String]
            goKeywords _inString _escaped tokenRev acc [] =
              finalizeToken tokenRev acc
            goKeywords inString escaped tokenRev acc (c:cs)
              | inString =
                  if escaped
                    then goKeywords True False tokenRev acc cs
                    else
                      if c == '\\'
                        then goKeywords True True tokenRev acc cs
                        else
                          if c == '"'
                            then goKeywords False False tokenRev acc cs
                            else goKeywords True False tokenRev acc cs
              | c == '"' =
                  goKeywords True False [] (finalizeToken tokenRev acc) cs
              | isKeywordChar c =
                  goKeywords False False (toLower c : tokenRev) acc cs
              | otherwise =
                  goKeywords False False [] (finalizeToken tokenRev acc) cs

            finalizeToken :: String -> [String] -> [String]
            finalizeToken [] acc = acc
            finalizeToken tokenRev acc = reverse tokenRev : acc

        isKeywordChar :: Char -> Bool
        isKeywordChar c =
          isAsciiLower c
            || isAsciiUpper c
            || isDigit c
            || c == '_'
            || c == '\''

    classifyLine :: (Int, String) -> Either String RawLine
    classifyLine (lineNo, rawLine) =
      let cleaned = trim (stripComment rawLine)
          noComment = stripComment rawLine
          isIndented =
            case noComment of
              [] -> False
              firstChar:_ -> isSpace firstChar
       in if null cleaned
            then Right RawEmpty
            else
            if isIndented
                then Right (RawFunctionContinuation lineNo cleaned)
                else
                  if isAttributeLine cleaned
                    then Right (RawAttribute lineNo (trim (init (drop 2 cleaned))))
                    else
                  if startsWith "module " cleaned || startsWith "import " cleaned || startsWith "export " cleaned
                    then Right RawEmpty
                    else
                      if startsWith "data " cleaned || startsWith "newtype " cleaned
                        then Right (RawData lineNo cleaned)
                        else
                          if startsWith "type " cleaned
                            then Right (RawType lineNo cleaned)
                            else
                              if startsWith "class " cleaned
                                then Right (RawClass lineNo cleaned)
                                else
                                  if startsWith "law " cleaned
                                    then Right (RawLaw lineNo cleaned)
                                    else
                                      if startsWith "instance " cleaned
                                        then Right (RawInstance lineNo cleaned)
                                        else
                                          if startsWith "infixl " cleaned || startsWith "infixr " cleaned || startsWith "infix " cleaned
                                            then Right (RawOperator lineNo cleaned)
                                            else
                                              if looksLikeTypeSignature cleaned
                                                then Right (RawSignature lineNo cleaned)
                                                else Right (RawFunction lineNo cleaned)
    isAttributeLine :: String -> Bool
    isAttributeLine lineText =
      length lineText >= 3 && startsWith "#[" lineText && last lineText == ']'

    collectOperatorDecls :: [RawLine] -> Either String [OperatorDecl]
    collectOperatorDecls raws = do
      decls <- traverse toDecl [entry | entry@(RawOperator _ _) <- raws]
      case duplicates (map declOpToken decls) of
        [] ->
          Right decls
        d:_ ->
          Left ("duplicate operator declaration: " <> d)
      where
        toDecl :: RawLine -> Either String OperatorDecl
        toDecl raw =
          case raw of
            RawOperator lineNo cleaned ->
              parseOperatorDeclAt lineNo cleaned
            _ ->
              Left "internal parser error while collecting operators"

    collectConstructorDecls :: [RawLine] -> Either String ConstructorTable
    collectConstructorDecls raws = do
      ctorEntries <-
        traverse toCtor
          [entry | entry <- raws, isDataOrTypeDecl entry]
      let ctorPairs = concat ctorEntries
      case duplicates (map fst ctorPairs) of
        [] ->
          Right ctorPairs
        d:_ ->
          Left ("duplicate data constructor declaration: " <> d)
      where
        isDataOrTypeDecl :: RawLine -> Bool
        isDataOrTypeDecl raw =
          case raw of
            RawData{} -> True
            RawType{} -> True
            _ -> False

        toCtor :: RawLine -> Either String [ (Name, ConstructorInfo) ]
        toCtor raw =
          case raw of
            RawData lineNo cleaned -> do
              decls <- parseDataDeclAt lineNo cleaned
              pure (map (\decl -> (dataCtorName decl, dataCtorInfo decl)) decls)
            RawType lineNo cleaned -> do
              decls <- parseTypeDeclAt lineNo cleaned
              pure (map (\decl -> (dataCtorName decl, dataCtorInfo decl)) decls)
            _ ->
              Left "internal parser error while collecting constructors"

    collectSignatureDecls :: [ParsedLine] -> Either String [TypeSignature]
    collectSignatureDecls parsedLines = do
      let sigs = [sig | ParsedSignature sig <- parsedLines]
      case duplicates (map sigName sigs) of
        [] -> Right sigs
        d:_ -> Left ("duplicate type signature declaration: " <> d)

    validateSignatureTargets :: [TypeSignature] -> [ParsedLine] -> Either String ()
    validateSignatureTargets sigs parsedLines =
      let functionNames = uniqueNames (map fnClauseName (concatMap lineFunctions parsedLines))
          missing = [sigName sig | sig <- sigs, sigName sig `notElem` functionNames]
       in case missing of
            [] -> Right ()
            fn:_ -> Left ("type signature references unknown function: " <> fn)

    parseRawLine :: OperatorTable -> ConstructorTable -> RawLine -> Either String ParsedLine
    parseRawLine _ _ RawEmpty = Right (ParsedFunctions [])
    parseRawLine _ _ (RawAttribute _ _) =
      Left "internal parser error: stray attribute outside function declaration"
    parseRawLine _ _ (RawSignature lineNo cleaned) = ParsedSignature <$> parseTypeSignatureAt lineNo cleaned
    parseRawLine _ _ (RawData lineNo cleaned) = ParsedFunctions <$> parseDataLineAt lineNo cleaned
    parseRawLine _ _ (RawType lineNo cleaned) = ParsedFunctions <$> parseTypeLineAt lineNo cleaned
    parseRawLine _ _ (RawClass lineNo cleaned) = ParsedClass <$> parseClassDecl lineNo cleaned
    parseRawLine operators constructors (RawLaw lineNo cleaned) =
      ParsedLaw <$> parseLawDeclWithOperators operators constructors lineNo cleaned
    parseRawLine _ _ (RawInstance lineNo cleaned) = ParsedInstance <$> parseInstanceDecl lineNo cleaned
    parseRawLine _ _ (RawOperator lineNo cleaned) = ParsedOperator <$> parseOperatorDeclAt lineNo cleaned
    parseRawLine _ _ (RawFunctionContinuation lineNo _) =
      Left (withLine lineNo "unexpected indented declaration line")
    parseRawLine operators constructors (RawFunction lineNo cleaned) =
      ParsedFunctions <$> parseFunctionClausesAt operators constructors lineNo cleaned

defaultFunctionAttributePlugins :: [FunctionAttributePlugin]
defaultFunctionAttributePlugins =
  [ memoAttributePlugin
  , testAttributePlugin
  , benchAttributePlugin
  ]

applyFunctionAttributePlugins :: [FunctionAttributePlugin] -> [Function] -> Either String [Function]
applyFunctionAttributePlugins plugins = traverse (applyFunctionPlugins plugins)

applyFunctionPlugins ::
  [FunctionAttributePlugin]
  -> Function
  -> Either String Function
applyFunctionPlugins plugins fn =
  foldM applySingle fn (attributes fn)
  where
    applySingle :: Function -> FunctionAttribute -> Either String Function
    applySingle currentFn attr =
      let matching = filter ((== attributeName attr) . pluginName) plugins
       in foldM (\acc plugin -> pluginApply plugin acc attr) currentFn matching

memoAttributePlugin :: FunctionAttributePlugin
memoAttributePlugin =
  FunctionAttributePlugin
    { pluginName = "memo"
    , pluginApply = validateMemoAttribute
    }

testAttributePlugin :: FunctionAttributePlugin
testAttributePlugin =
  FunctionAttributePlugin
    { pluginName = "test"
    , pluginApply = validateTestAttribute
    }

benchAttributePlugin :: FunctionAttributePlugin
benchAttributePlugin =
  FunctionAttributePlugin
    { pluginName = "bench"
    , pluginApply = validateBenchAttribute
    }

validateMemoAttribute :: Function -> FunctionAttribute -> Either String Function
validateMemoAttribute _fn attr =
  case attributeValue attr of
    Just (AttributeInt size) | size >= 0 -> Right _fn
    Just _ ->
      Left
        ( "memo attribute requires a non-negative integer size, e.g. #[memo 100],"
            <> " got "
            <> show (attributeValue attr)
        )
    Nothing ->
      Left "memo attribute requires a size argument, e.g. #[memo 100]"

validateTestAttribute :: Function -> FunctionAttribute -> Either String Function
validateTestAttribute _fn attr =
  case attributeValue attr of
    Just (AttributeString _) -> Right _fn
    Just _ ->
      Left
        ( "test attribute requires a string label, e.g. #[test \"name\"],"
            <> " got "
            <> show (attributeValue attr)
        )
    Nothing -> Left "test attribute requires a string label, e.g. #[test \"name\"]"

validateBenchAttribute :: Function -> FunctionAttribute -> Either String Function
validateBenchAttribute _fn attr =
  case attributeValue attr of
    Just (AttributeString _) -> Right _fn
    Just _ ->
      Left
        ( "bench attribute requires a string label, e.g. #[bench \"name\"],"
            <> " got "
            <> show (attributeValue attr)
        )
    Nothing -> Left "bench attribute requires a string label, e.g. #[bench \"name\"]"

parseFunctionLine :: String -> Either String Function
parseFunctionLine src = do
  clauses <- parseFunctionClausesWithOperators (buildOperatorTable []) [] src
  case mergeFunctionClauses clauses of
    Right [fn] -> Right fn
    Right _ -> Left "could not parse function declaration; expected single function"
    Left err -> Left err

parseExpr :: String -> Either String Expr
parseExpr = parseExprWithOperators (buildOperatorTable []) []

lineFunctions :: ParsedLine -> [FunctionClause]
lineFunctions parsed =
  case parsed of
    ParsedFunctions fns -> fns
    ParsedSignature _ -> []
    ParsedClass _ -> []
    ParsedLaw _ -> []
    ParsedInstance _ -> []
    ParsedOperator _ -> []

mergeFunctionClauses :: [FunctionClause] -> Either String [Function]
mergeFunctionClauses clauses = do
  groups <- groupFunctionClauses clauses
  traverse mergeFunctionClauseGroup groups

groupFunctionClauses :: [FunctionClause] -> Either String [[FunctionClause]]
groupFunctionClauses = Right . foldl addClauseToGroups []
  where
    addClauseToGroups :: [[FunctionClause]] -> FunctionClause -> [[FunctionClause]]
    addClauseToGroups [] clause = [[clause]]
    addClauseToGroups (group@(headClause : _):rest) clause
      | fnClauseName headClause == fnClauseName clause = (group ++ [clause]) : rest
      | otherwise = group : addClauseToGroups rest clause
    addClauseToGroups groups _ = groups

mergeFunctionClauseGroup :: [FunctionClause] -> Either String Function
mergeFunctionClauseGroup clauses =
  case clauses of
    [] -> Left "function group is empty"
    _ -> do
      let clauseAttrs = fnClauseAttributes (head clauses)
      if any (\clause -> fnClauseAttributes clause /= clauseAttrs) clauses
        then Left "function clauses do not agree on attributes"
        else do
          case clauses of
            firstClause : [] ->
              case firstClause of
                FunctionClause _ _ body _
                  | all isVarPattern (fnClausePatterns firstClause) ->
                    let args = mapMaybe varPatternName (fnClausePatterns firstClause)
                     in Right Function {name = fnClauseName firstClause, args = args, body = body, attributes = clauseAttrs}
                  | otherwise ->
                    buildPatternMatchFn clauses
            _ : _ -> do
              buildPatternMatchFn clauses
  where
    buildPatternMatchFn :: [FunctionClause] -> Either String Function
    buildPatternMatchFn clausesToBuild =
      case clausesToBuild of
        [] ->
          Left "function group is empty"
        firstClause0 : _ -> do
          let fnName = fnClauseName firstClause0
              arity = length (fnClausePatterns firstClause0)
          if any (\fnClause -> length (fnClausePatterns fnClause) /= arity) clausesToBuild
            then
              Left
                ("all function clauses for " <> fnName <> " must have the same number of arguments")
            else Right ()
          argNames <- pickArgNames clausesToBuild arity
          let arms = map (mkFunctionClauseArm argNames) clausesToBuild
              body = Case (map Var argNames) arms
          pure Function {name = fnName, args = argNames, body = body, attributes = fnClauseAttributes firstClause0}

    mkFunctionClauseArm :: [Name] -> FunctionClause -> CaseArm
    mkFunctionClauseArm _ clause =
      CaseArm {armPatterns = fnClausePatterns clause, armBody = fnClauseBody clause}

    pickArgNames :: [FunctionClause] -> Int -> Either String [Name]
    pickArgNames fnClauses expectedArity = do
      let names = map (pickArgName fnClauses) [0 .. expectedArity - 1]
      case duplicates names of
        d:_ -> Left ("duplicate function argument name: " <> d)
        [] -> Right names

    pickArgName :: [FunctionClause] -> Int -> Name
    pickArgName fnClauses argIx =
      case firstPatternVarName fnClauses argIx of
        Just out -> out
        Nothing -> "__arg" <> show argIx

    firstPatternVarName :: [FunctionClause] -> Int -> Maybe Name
    firstPatternVarName fnClauses argIx =
      go fnClauses
      where
        go [] = Nothing
        go (clause:rest) =
          case fnClausePatterns clause !! argIx of
            PatVar n -> Just n
            _ -> go rest

    isVarPattern :: CasePattern -> Bool
    isVarPattern pat =
      case pat of
        PatVar _ -> True
        _ -> False

    varPatternName :: CasePattern -> Maybe Name
    varPatternName pat =
      case pat of
        PatVar n -> Just n
        _ -> Nothing

deriveRules :: [ParsedLine] -> Either String [DerivedRule]
deriveRules parsed = do
  checkedClasses <- buildCheckedClasses classDecls lawDecls
  checkedInstances <- buildCheckedInstances checkedClasses instanceDecls
  pure (concatMap deriveInstanceRules checkedInstances)
  where
    classDecls = [decl | ParsedClass decl <- parsed]
    lawDecls = [decl | ParsedLaw decl <- parsed]
    instanceDecls = [decl | ParsedInstance decl <- parsed]

buildCheckedClasses :: [ClassDecl] -> [LawDecl] -> Either String [CheckedClass]
buildCheckedClasses classDecls lawDecls = do
  case duplicates (map declClassName classDecls) of
    [] -> Right ()
    d:_ -> Left ("duplicate class declaration: " <> d)

  traverse_ (verifyLawClassExists classNames) lawDecls
  traverse buildClass classDecls
  where
    classNames = map declClassName classDecls

    verifyLawClassExists :: [Name] -> LawDecl -> Either String ()
    verifyLawClassExists known lawDecl =
      if declLawClass lawDecl `elem` known
        then Right ()
        else
          Left
            ( withLine
                (declLawLine lawDecl)
                ("law references unknown class: " <> declLawClass lawDecl)
            )

    buildClass :: ClassDecl -> Either String CheckedClass
    buildClass classDecl = do
      let className0 = declClassName classDecl
          classKind0 = declClassKind classDecl
          classMethods = requiredMethodNames classKind0
          classLaws = [declLaw lawDecl | lawDecl <- lawDecls, declLawClass lawDecl == className0]
      classDef <-
        case mkClassDef className0 classKind0 classMethods classLaws of
          Left err -> Left (withLine (declClassLine classDecl) err)
          Right out -> Right out
      pure
        CheckedClass
          { checkedClassName = className0
          , checkedClassParams = declClassParams classDecl
          , checkedClassDef = classDef
          }

buildCheckedInstances :: [CheckedClass] -> [InstanceDecl] -> Either String [CheckedInstance]
buildCheckedInstances checkedClasses instanceDecls = do
  case duplicates (map declInstanceName instanceDecls) of
    [] -> Right ()
    d:_ -> Left ("duplicate instance declaration: " <> d)

  traverse checkInstance instanceDecls
  where
    checkInstance :: InstanceDecl -> Either String CheckedInstance
    checkInstance decl = do
      checkedClass <-
        case findCheckedClass (declInstanceClass decl) checkedClasses of
          Nothing ->
            Left
              ( withLine
                  (declInstanceLine decl)
                  ("instance references unknown class: " <> declInstanceClass decl)
              )
          Just out -> Right out

      let bindingPairs = declInstanceBindings decl
          classParamCount = length (checkedClassParams checkedClass)
          instanceTypeArgCount = length (declInstanceTypeArgs decl)
          bindingMethods = map fst bindingPairs
          classMethods = methods (checkedClassDef checkedClass)
          missingMethods = filter (`notElem` bindingMethods) classMethods
          unknownMethods = filter (`notElem` classMethods) bindingMethods

      case duplicates bindingMethods of
        [] -> Right ()
        d:_ ->
          Left
            ( withLine
                (declInstanceLine decl)
                ("duplicate instance method binding: " <> d)
            )

      case unknownMethods of
        [] -> Right ()
        m:_ ->
          Left
            ( withLine
                (declInstanceLine decl)
                ("instance binds unknown method: " <> m)
            )

      case missingMethods of
        [] -> Right ()
        m:_ ->
          Left
            ( withLine
                (declInstanceLine decl)
                ("instance missing method binding: " <> m)
            )
      case classParamCount == instanceTypeArgCount of
        True -> Right ()
        False ->
          Left
            ( withLine
                (declInstanceLine decl)
                ( "instance type argument arity mismatch for class "
                    <> declInstanceClass decl
                    <> ": expected "
                    <> show classParamCount
                    <> ", got "
                    <> show instanceTypeArgCount
                )
            )

      pure
        CheckedInstance
          { checkedInstanceClass = checkedClass
          , checkedInstanceBindings = bindingPairs
          }

findCheckedClass :: Name -> [CheckedClass] -> Maybe CheckedClass
findCheckedClass _ [] = Nothing
findCheckedClass target (x:xs)
  | checkedClassName x == target = Just x
  | otherwise = findCheckedClass target xs

deriveInstanceRules :: CheckedInstance -> [DerivedRule]
deriveInstanceRules checkedInstance =
  let classDef = checkedClassDef (checkedInstanceClass checkedInstance)
      bindings = checkedInstanceBindings checkedInstance
      literals = uniqueNames (map snd bindings)
   in map (toRule bindings literals) (laws classDef)
  where
    toRule :: [(Name, Name)] -> [Name] -> Law Expr -> DerivedRule
    toRule bindings literals lawDef =
      DerivedRule
        { derivedLiteralNames = literals
        , derivedLhs = substituteMethods bindings (lhs lawDef)
        , derivedRhs = substituteMethods bindings (rhs lawDef)
        }

substituteMethods :: [(Name, Name)] -> Expr -> Expr
substituteMethods methodMap = go []
  where
    go :: [Name] -> Expr -> Expr
    go bound expr =
      case expr of
        Var n
          | n `elem` bound -> Var n
          | otherwise ->
              case lookup n methodMap of
                Nothing -> Var n
                Just target -> Var target
        IntLit n -> IntLit n
        StringLit s -> StringLit s
        CollectionLit elems -> CollectionLit (map (go bound) elems)
        Case scrutinees arms ->
          Case
            (map (go bound) scrutinees)
            (map (rewriteArm bound) arms)
        Lam n bodyExpr -> Lam n (go (n : bound) bodyExpr)
        App f x -> App (go bound f) (go bound x)

    rewriteArm :: [Name] -> CaseArm -> CaseArm
    rewriteArm bound arm =
      arm
        { armBody =
            go (patternBoundNames (armPatterns arm) <> bound) (armBody arm)
        }

optimizeFunctions :: [DerivedRule] -> [Function] -> [Function]
optimizeFunctions derivedRules = map optimizeFunction
  where
    optimizeFunction :: Function -> Function
    optimizeFunction fn =
      fn {body = normalizeWithDerivedRules 64 derivedRules (body fn)}

normalizeWithDerivedRules :: Int -> [DerivedRule] -> Expr -> Expr
normalizeWithDerivedRules maxSteps allRules = go maxSteps
  where
    go :: Int -> Expr -> Expr
    go steps expr
      | steps <= 0 = expr
      | otherwise =
          case rewriteOnceWithDerivedRules allRules expr of
            Nothing -> expr
            Just expr' -> go (steps - 1) expr'

rewriteOnceWithDerivedRules :: [DerivedRule] -> Expr -> Maybe Expr
rewriteOnceWithDerivedRules allRules expr =
  rewriteAtRoot allRules expr
    <|> rewriteInside allRules expr

rewriteAtRoot :: [DerivedRule] -> Expr -> Maybe Expr
rewriteAtRoot allRules expr = foldFirst (map (`applyDerivedRule` expr) allRules)

rewriteInside :: [DerivedRule] -> Expr -> Maybe Expr
rewriteInside allRules expr =
  case expr of
    App f x ->
      ((`App` x) <$> rewriteOnceWithDerivedRules allRules f)
        <|> ((App f) <$> rewriteOnceWithDerivedRules allRules x)
    Lam n bodyExpr ->
      Lam n <$> rewriteOnceWithDerivedRules allRules bodyExpr
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
          case rewriteOnceWithDerivedRules allRules e of
            Just e' -> Just (e' : es)
            Nothing -> (e :) <$> rewriteCollectionElems es

    rewriteCaseScrutinees :: [Expr] -> Maybe [Expr]
    rewriteCaseScrutinees scrs =
      case scrs of
        [] -> Nothing
        e:es ->
          case rewriteOnceWithDerivedRules allRules e of
            Just e' -> Just (e' : es)
            Nothing -> (e :) <$> rewriteCaseScrutinees es

    rewriteCaseArms :: [CaseArm] -> Maybe [CaseArm]
    rewriteCaseArms arms0 =
      case arms0 of
        [] -> Nothing
        a:as ->
          case rewriteOnceWithDerivedRules allRules (armBody a) of
            Just body' -> Just (a {armBody = body'} : as)
            Nothing -> (a :) <$> rewriteCaseArms as

applyDerivedRule :: DerivedRule -> Expr -> Maybe Expr
applyDerivedRule rule expr = do
  subs <- matchRuleExpr (derivedLiteralNames rule) [] [] (derivedLhs rule) expr
  instantiateRuleExpr (derivedLiteralNames rule) [] subs (derivedRhs rule)

matchRuleExpr :: [Name] -> [Name] -> Substitution -> Expr -> Expr -> Maybe Substitution
matchRuleExpr literalNames boundNames subs patternExpr actualExpr =
  case patternExpr of
    Var n
      | n `elem` boundNames ->
          case actualExpr of
            Var m
              | m == n -> Just subs
              | otherwise -> Nothing
            _ -> Nothing
      | n `elem` literalNames ->
          case actualExpr of
            Var m
              | m == n -> Just subs
              | otherwise -> Nothing
            _ -> Nothing
      | otherwise ->
          case lookup n subs of
            Nothing -> Just ((n, actualExpr) : subs)
            Just previous
              | previous == actualExpr -> Just subs
              | otherwise -> Nothing
    IntLit n ->
      case actualExpr of
        IntLit m
          | m == n -> Just subs
          | otherwise -> Nothing
        _ -> Nothing
    StringLit s ->
      case actualExpr of
        StringLit t
          | t == s -> Just subs
          | otherwise -> Nothing
        _ -> Nothing
    CollectionLit patternElems ->
      case actualExpr of
        CollectionLit actualElems
          | length patternElems == length actualElems ->
              matchMany patternElems actualElems subs
          | otherwise -> Nothing
        _ -> Nothing
    Case patternScrutinees patternArms ->
      case actualExpr of
        Case actualScrutinees actualArms
          | length patternScrutinees == length actualScrutinees
              && length patternArms == length actualArms ->
              matchCase patternScrutinees actualScrutinees patternArms actualArms subs
          | otherwise -> Nothing
        _ -> Nothing
    Lam n patternBody ->
      case actualExpr of
        Lam m actualBody
          | m == n -> matchRuleExpr literalNames (n : boundNames) subs patternBody actualBody
          | otherwise -> Nothing
        _ -> Nothing
    App patternF patternX ->
      case actualExpr of
        App actualF actualX -> do
          subs1 <- matchRuleExpr literalNames boundNames subs patternF actualF
          matchRuleExpr literalNames boundNames subs1 patternX actualX
        _ -> Nothing
  where
    matchMany :: [Expr] -> [Expr] -> Substitution -> Maybe Substitution
    matchMany pats vals subs0 =
      case (pats, vals) of
        ([], []) -> Just subs0
        (p:ps, v:vs) -> do
          subs1 <- matchRuleExpr literalNames boundNames subs0 p v
          matchMany ps vs subs1
        _ -> Nothing

    matchCase :: [Expr] -> [Expr] -> [CaseArm] -> [CaseArm] -> Substitution -> Maybe Substitution
    matchCase patScrs valScrs patArms valArms subs0 = do
      subs1 <- matchMany patScrs valScrs subs0
      matchArms patArms valArms subs1

    matchArms :: [CaseArm] -> [CaseArm] -> Substitution -> Maybe Substitution
    matchArms pats vals subs0 =
      case (pats, vals) of
        ([], []) -> Just subs0
        (patArm:patRest, valArm:valRest)
          | armPatterns patArm == armPatterns valArm -> do
              let boundArm = patternBoundNames (armPatterns patArm) <> boundNames
              subs1 <- matchRuleExpr literalNames boundArm subs0 (armBody patArm) (armBody valArm)
              matchArms patRest valRest subs1
          | otherwise -> Nothing
        _ -> Nothing

instantiateRuleExpr :: [Name] -> [Name] -> Substitution -> Expr -> Maybe Expr
instantiateRuleExpr literalNames boundNames subs expr =
  case expr of
    Var n
      | n `elem` boundNames -> Just (Var n)
      | n `elem` literalNames -> Just (Var n)
      | otherwise -> lookup n subs
    IntLit n -> Just (IntLit n)
    StringLit s -> Just (StringLit s)
    CollectionLit elems -> CollectionLit <$> traverse (instantiateRuleExpr literalNames boundNames subs) elems
    Case scrutinees arms ->
      Case
        <$> traverse (instantiateRuleExpr literalNames boundNames subs) scrutinees
        <*> traverse instantiateArm arms
    Lam n bodyExpr -> Lam n <$> instantiateRuleExpr literalNames (n : boundNames) subs bodyExpr
    App f x -> App <$> instantiateRuleExpr literalNames boundNames subs f <*> instantiateRuleExpr literalNames boundNames subs x
  where
    instantiateArm :: CaseArm -> Maybe CaseArm
    instantiateArm arm = do
      bodyOut <-
        instantiateRuleExpr
          literalNames
          (patternBoundNames (armPatterns arm) <> boundNames)
          subs
          (armBody arm)
      pure arm {armBody = bodyOut}

foldFirst :: [Maybe a] -> Maybe a
foldFirst [] = Nothing
foldFirst (x:xs) =
  case x of
    Just _ -> x
    Nothing -> foldFirst xs

parseFunctionClausesAt :: OperatorTable -> ConstructorTable -> Int -> String -> Either String [FunctionClause]
parseFunctionClausesAt operators constructors lineNo src =
  case parseFunctionClausesWithOperators operators constructors src of
    Left err ->
      Left (withLine lineNo err)
    Right clauses ->
      Right clauses

parseFunctionClausesWithOperators :: OperatorTable -> ConstructorTable -> String -> Either String [FunctionClause]
parseFunctionClausesWithOperators operators constructors src =
  case splitOnFirst '=' src of
    Nothing ->
      Left "could not parse function declaration"
    Just (lhsRaw, rhsRaw) -> do
      if '|' `elem` lhsRaw
        then parseGuardedFunctionClause operators constructors src
        else parsePlainFunctionClause operators constructors rhsRaw lhsRaw

parsePlainFunctionClause :: OperatorTable -> ConstructorTable -> String -> Name -> Either String [FunctionClause]
parsePlainFunctionClause operators constructors rhsRaw lhsRaw = do
  toks <- tokenizeExpr (trim lhsRaw)
  case toks of
    [] ->
      Left "could not parse function declaration"
    TokIdentifier fnName:rest -> do
      validateIdentifier "function name" fnName
      (patterns, remaining) <- parseFunctionPatterns constructors rest
      case remaining of
        [] ->
          case trim rhsRaw of
            "" -> Left "could not parse function declaration"
            rhs -> do
              bodyExpr <- parseExprWithOperators operators constructors rhs
              pure
                [ FunctionClause
                  { fnClauseName = fnName
                  , fnClausePatterns = patterns
                  , fnClauseBody = bodyExpr
                  , fnClauseAttributes = []
                  }
                ]
        _ ->
          Left "could not parse function declaration; invalid lhs argument"
    _ ->
      Left "could not parse function declaration"

parsePlainFunctionPatterns :: ConstructorTable -> [ExprTok] -> Either String ([CasePattern], [ExprTok])
parsePlainFunctionPatterns constructors tokens = go [] tokens
  where
    go :: [CasePattern] -> [ExprTok] -> Either String ([CasePattern], [ExprTok])
    go revAcc toks0 =
      case toks0 of
        [] -> Right (reverse revAcc, [])
        _ -> do
          (pat, rest) <- parseCasePattern constructors "function argument pattern" toks0
          case rest of
            [] -> Right (reverse (pat : revAcc), rest)
            _ -> go (pat : revAcc) rest

parseFunctionPatterns :: ConstructorTable -> [ExprTok] -> Either String ([CasePattern], [ExprTok])
parseFunctionPatterns constructors = parsePlainFunctionPatterns constructors

parseGuardedFunctionClause :: OperatorTable -> ConstructorTable -> String -> Either String [FunctionClause]
parseGuardedFunctionClause operators constructors src = do
  let segments = splitGuardSegments src
  firstSeg <- case segments of
    [] -> Left "could not parse guarded function declaration"
    s:_ -> Right s
  restSegs <- case segments of
    [] -> Left "could not parse guarded function declaration"
    _:ss -> Right ss
  (fnName, patterns, firstClause) <- parseFirstGuardSegment firstSeg
  moreClauses <- traverse parseContinuationGuardSegment restSegs
  bodyExpr <- buildGuardedBody (firstClause : moreClauses)
  pure
    [ FunctionClause
        { fnClauseName = fnName
        , fnClausePatterns = patterns
        , fnClauseBody = bodyExpr
        , fnClauseAttributes = []
        }
    ]
  where
    splitGuardSegments :: String -> [String]
    splitGuardSegments = reverse . go False False [] []
      where
        go :: Bool -> Bool -> String -> [String] -> String -> [String]
        go _inString _escaped curRev acc [] =
          reverse curRev : acc
        go inString escaped curRev acc (c:cs)
          | inString =
              if escaped
                then go True False (c : curRev) acc cs
                else
                  if c == '\\'
                    then go True True (c : curRev) acc cs
                    else
                      if c == '"'
                        then go False False (c : curRev) acc cs
                        else go True False (c : curRev) acc cs
          | c == '"' =
              go True False (c : curRev) acc cs
          | c == ';' =
              let restTrimmed = trimLeft cs
               in if startsWith "|" restTrimmed
                    then go False False [] (reverse curRev : acc) restTrimmed
                    else go False False (c : curRev) acc cs
          | otherwise =
              go False False (c : curRev) acc cs

    parseFirstGuardSegment :: String -> Either String (Name, [CasePattern], (Maybe Expr, Expr))
    parseFirstGuardSegment seg = do
      (lhsRaw, guardEqRaw) <-
        case splitOnFirst '|' seg of
          Nothing -> Left "could not parse guarded function declaration"
          Just out -> Right out
      toks <- tokenizeExpr (trim lhsRaw)
      (fnName, params) <- parseGuardLhsHead toks
      clause <- parseGuardClause guardEqRaw
      pure (fnName, params, clause)

    parseGuardLhsHead :: [ExprTok] -> Either String (Name, [CasePattern])
    parseGuardLhsHead toks =
      case toks of
        TokIdentifier fnName:rest -> do
          validateIdentifier "function name" fnName
          (patterns, remaining) <- parseFunctionPatterns constructors rest
          case remaining of
            [] -> Right (fnName, patterns)
            _ -> Left "could not parse guarded function declaration"
        _ -> Left "could not parse guarded function declaration"

    parseContinuationGuardSegment :: String -> Either String (Maybe Expr, Expr)
    parseContinuationGuardSegment seg =
      let trimmed = trim seg
       in if not (startsWith "|" trimmed)
            then Left "guard continuation must start with |"
            else parseGuardClause (trimLeft (drop 1 trimmed))

    parseGuardClause :: String -> Either String (Maybe Expr, Expr)
    parseGuardClause raw = do
      toks <- tokenizeExpr raw
      let candidates = splitCandidates toks
      if null candidates
        then Left "guard clause is missing ="
        else tryCandidates candidates Nothing
      where
        splitCandidates :: [ExprTok] -> [([ExprTok], [ExprTok])]
        splitCandidates = go []
          where
            go :: [ExprTok] -> [ExprTok] -> [([ExprTok], [ExprTok])]
            go _revPrefix [] = []
            go revPrefix (TokOperator "=" : rest) =
              (reverse revPrefix, rest) : go (TokOperator "=" : revPrefix) rest
            go revPrefix (tok:rest) =
              go (tok : revPrefix) rest

        tryCandidates :: [([ExprTok], [ExprTok])] -> Maybe String -> Either String (Maybe Expr, Expr)
        tryCandidates [] firstErr =
          case firstErr of
            Just err -> Left err
            Nothing -> Left "could not parse guard clause"
        tryCandidates ((guardToks, bodyToks):rest) firstErr =
          case parseCandidate guardToks bodyToks of
            Right out -> Right out
            Left err ->
              case firstErr of
                Nothing -> tryCandidates rest (Just err)
                Just _ -> tryCandidates rest firstErr

        parseCandidate :: [ExprTok] -> [ExprTok] -> Either String (Maybe Expr, Expr)
        parseCandidate guardToks bodyToks = do
          guardExpr <- parseWholeExpr "guard condition" False guardToks
          bodyExpr <- parseWholeExpr "guard clause body" True bodyToks
          if guardExpr == Var "otherwise"
            then Right (Nothing, bodyExpr)
            else Right (Just guardExpr, bodyExpr)

        parseWholeExpr :: String -> Bool -> [ExprTok] -> Either String Expr
        parseWholeExpr label isBody toksToParse =
          if null toksToParse
            then
              if isBody
                then Left "guard clause body is empty"
                else Left "guard clause condition is empty"
            else do
              (exprOut, restOut) <- parseExprTokens operators constructors 0 toksToParse
              case restOut of
                [] -> Right exprOut
                tok:_ ->
                  Left (label <> " has unexpected trailing token: " <> renderExprTok tok)

    buildGuardedBody :: [(Maybe Expr, Expr)] -> Either String Expr
    buildGuardedBody clauses =
      case clauses of
        [] ->
          Left "guarded function requires at least one clause"
        [(Nothing, bodyExpr)] ->
          Right bodyExpr
        [(Just _guardExpr, _bodyExpr)] ->
          Left "guarded function must end with otherwise clause"
        (Nothing, _):_ ->
          Left "otherwise guard must be the final guard clause"
        (Just guardExpr, bodyExpr):rest -> do
          fallbackExpr <- buildGuardedBody rest
          pure
            ( App
                ( App
                    ( App
                        (Var "if")
                        guardExpr
                    )
                    (Lam "__guard_dummy" bodyExpr)
                )
                (Lam "__guard_dummy" fallbackExpr)
            )

parseCasePattern :: ConstructorTable -> String -> [ExprTok] -> Either String (CasePattern, [ExprTok])
parseCasePattern constructors label toks0 =
  case toks0 of
    TokInteger n:rest ->
      Right (PatInt n, rest)
    TokString s:rest ->
      Right (PatString s, rest)
    TokIdentifier "_":rest ->
      Right (PatWildcard, rest)
    TokIdentifier headName:rest ->
      case lookup headName constructors of
        Just ctorInfo -> do
          (fieldNames, restOut) <- parsePatternConstructorFields (label <> " field name") headName ctorInfo rest
          pure (PatConstructor headName ctorInfo fieldNames, restOut)
        Nothing -> do
          let variableLabel = label <> " variable name"
          validateIdentifier variableLabel headName
          pure (PatVar headName, rest)
    TokArrow:_ ->
      Left "could not parse expression; invalid pattern: expected pattern before ->"
    TokSemicolon:_ ->
      Left "could not parse expression; invalid pattern before ;"
    TokOf:_ ->
      Left "could not parse expression; invalid pattern before of"
    tok:_ ->
      Left ("could not parse expression; invalid pattern token: " <> renderExprTok tok)
    [] ->
      Left "could not parse expression; unexpected end of pattern"

parsePatternConstructorFields ::
  String -> Name -> ConstructorInfo -> [ExprTok] -> Either String ([Name], [ExprTok])
parsePatternConstructorFields fieldLabel ctorName ctorInfo toks0 =
  go [] (ctorFieldCount ctorInfo) toks0
  where
    go :: [Name] -> Int -> [ExprTok] -> Either String ([Name], [ExprTok])
    go revFields remaining toks1
      | remaining <= 0 = do
          let outFields = reverse revFields
          case duplicates (filter (/= "_") outFields) of
            [] -> Right (outFields, toks1)
            d:_ -> Left ("duplicate " <> fieldLabel <> ": " <> d)
      | otherwise =
          case toks1 of
            TokIdentifier n:rest -> do
              if n == "_"
                then go (n : revFields) (remaining - 1) rest
                else do
                  validateIdentifier fieldLabel n
                  go (n : revFields) (remaining - 1) rest
            _ ->
              Left
                ( "could not parse expression; constructor pattern "
                    <> ctorName
                    <> " expects "
                    <> show (ctorFieldCount ctorInfo)
                    <> " field name(s)"
                )

parseExprWithOperators :: OperatorTable -> ConstructorTable -> String -> Either String Expr
parseExprWithOperators operators constructors src = do
  toks <- tokenizeExpr src
  (expr, rest) <- parseExprTokens operators constructors 0 toks
  case rest of
    [] ->
      Right expr
    tok:_ ->
      Left ("could not parse expression; unexpected token: " <> renderExprTok tok)

parseExprTokens :: OperatorTable -> ConstructorTable -> Int -> [ExprTok] -> Either String (Expr, [ExprTok])
parseExprTokens operators constructors minPrec toks =
  case toks of
    TokLambda:_
      | minPrec <= 0 ->
          parseLambdaTokens operators constructors toks
    TokLet:_
      | minPrec <= 0 ->
          parseLetTokens operators constructors toks
    TokCase:_
      | minPrec <= 0 ->
          parseCaseTokens operators constructors toks
    _ ->
      parseInfixTokens operators constructors minPrec toks

parseCaseTokens :: OperatorTable -> ConstructorTable -> [ExprTok] -> Either String (Expr, [ExprTok])
parseCaseTokens operators constructors toks =
  parseCaseTokensWithContext operators constructors Nothing toks

parseCaseTokensWithContext ::
  OperatorTable ->
  ConstructorTable ->
  Maybe Int ->
  [ExprTok] ->
  Either String (Expr, [ExprTok])
parseCaseTokensWithContext operators constructors outerArmCount toks =
  case toks of
    TokCase:rest -> do
      (scrutinees, afterOf) <- parseCaseScrutinees rest
      (arms, restFinal) <- parseCaseArms (length scrutinees) outerArmCount afterOf
      validateCaseArms scrutinees arms
      pure (Case scrutinees arms, restFinal)
    _ ->
      Left "could not parse expression; expected case"
  where
    parseCaseScrutinees :: [ExprTok] -> Either String ([Expr], [ExprTok])
    parseCaseScrutinees toks0 = go [] toks0
      where
        go :: [Expr] -> [ExprTok] -> Either String ([Expr], [ExprTok])
        go acc toks1 =
          case toks1 of
            TokOf:afterOf ->
              case acc of
                [] -> Left "could not parse expression; case has no scrutinee values"
                _ -> Right (reverse acc, afterOf)
            [] ->
              Left "could not parse expression; case is missing of"
            _ -> do
              (scrutineeExpr, rest1) <- parseTermTokens operators constructors toks1
              go (scrutineeExpr : acc) rest1

    parseCaseArms :: Int -> Maybe Int -> [ExprTok] -> Either String ([CaseArm], [ExprTok])
    parseCaseArms scrutineeCount outerArmCount toks0 = do
      (firstArm, rest1) <- parseCaseArm scrutineeCount toks0
      gather [firstArm] rest1
      where
        gather :: [CaseArm] -> [ExprTok] -> Either String ([CaseArm], [ExprTok])
        gather acc toks1 =
          case toks1 of
            TokSemicolon:rest2 -> do
              if startsCaseArm scrutineeCount rest2
                then do
                  (nextArm, rest3) <- parseCaseArm scrutineeCount rest2
                  gather (acc <> [nextArm]) rest3
                else if looksLikeCaseArmPrefix scrutineeCount rest2
                  then case outerArmCount of
                    Just outerCount
                      | outerCount /= scrutineeCount &&
                        startsCaseArm outerCount rest2 ->
                          Right (acc, toks1)
                    _ ->
                      Left "could not parse expression; case arm list is malformed (missing ->)"
                  else
                    Right (acc, toks1)
            _
              | startsCaseArm scrutineeCount toks1 -> do
                  (nextArm, rest2) <- parseCaseArm scrutineeCount toks1
                  gather (acc <> [nextArm]) rest2
              | looksLikeCaseArmPrefix scrutineeCount toks1 ->
                  Left
                    "could not parse expression; case arm list is malformed (missing ';' between arms)"
              | otherwise ->
                  Right (acc, toks1)

    parseCaseArm :: Int -> [ExprTok] -> Either String (CaseArm, [ExprTok])
    parseCaseArm scrutineeCount toks0 = do
      (patterns0, afterPatterns) <- parsePatterns scrutineeCount toks0
      afterArrow <-
        case afterPatterns of
          TokArrow:rest -> Right rest
          _ -> Left "could not parse expression; case arm is missing ->"
      (bodyExpr0, restOut) <-
        case afterArrow of
          TokCase : _ ->
            parseCaseTokensWithContext operators constructors (Just scrutineeCount) afterArrow
          _ ->
            parseExprTokens operators constructors 0 afterArrow
      pure (CaseArm {armPatterns = patterns0, armBody = bodyExpr0}, restOut)

    parsePatterns :: Int -> [ExprTok] -> Either String ([CasePattern], [ExprTok])
    parsePatterns expectedCount toks0 = go [] expectedCount toks0
      where
        go :: [CasePattern] -> Int -> [ExprTok] -> Either String ([CasePattern], [ExprTok])
        go revAcc remaining toks1
          | remaining <= 0 = Right (reverse revAcc, toks1)
          | otherwise = do
              (pat, rest1) <- parseCasePattern constructors "case pattern" toks1
              go (pat : revAcc) (remaining - 1) rest1

    startsCaseArm :: Int -> [ExprTok] -> Bool
    startsCaseArm expectedCount toks1 =
      case parsePatterns expectedCount toks1 of
        Right (_, TokArrow : _) -> True
        _ -> False

    looksLikeCaseArmPrefix :: Int -> [ExprTok] -> Bool
    looksLikeCaseArmPrefix expectedCount toks1 =
      case parsePatterns expectedCount toks1 of
        Right _ -> True
        _ -> False

    validateCaseArms :: [Expr] -> [CaseArm] -> Either String ()
    validateCaseArms scrutinees arms =
      let allScrutinees = length scrutinees
       in case arms of
            [] ->
              Left "could not parse expression; case has no arms"
            _ -> do
              traverse_ (validateArm allScrutinees) arms
              let lastArm = last arms
              if all isIrrefutablePattern (armPatterns lastArm) || isConstructorExhaustiveCase scrutinees arms
                then Right ()
                else Left "could not parse expression; final case arm must be a catch-all pattern"

    isConstructorExhaustiveCase :: [Expr] -> [CaseArm] -> Bool
    isConstructorExhaustiveCase scrutinees arms0 =
      case scrutinees of
        [_oneScrutinee] ->
          case traverse armConstructor arms0 of
            Nothing ->
              False
            Just ctorInfos ->
              case ctorInfos of
                [] ->
                  False
                (_, firstInfo) : _ ->
                  let targetType = ctorTypeName firstInfo
                      ctorNames = map fst ctorInfos
                      sameType = all (\(_, info) -> ctorTypeName info == targetType) ctorInfos
                      declaredForType = [ctorN | (ctorN, info) <- constructors, ctorTypeName info == targetType]
                   in sameType
                        && not (null declaredForType)
                        && sameSet ctorNames declaredForType
        _ ->
          False

    armConstructor :: CaseArm -> Maybe (Name, ConstructorInfo)
    armConstructor arm =
      case armPatterns arm of
        [PatConstructor ctorName info _fieldNames] ->
          Just (ctorName, info)
        _ ->
          Nothing

    sameSet :: [Name] -> [Name] -> Bool
    sameSet xs ys =
      let ux = uniqueNames xs
          uy = uniqueNames ys
       in length ux == length uy
            && all (`elem` uy) ux
            && all (`elem` ux) uy

    validateArm :: Int -> CaseArm -> Either String ()
    validateArm expectedCount arm =
      if length (armPatterns arm) /= expectedCount
        then
          Left
            ( "could not parse expression; case arm expects "
                <> show expectedCount
                <> " pattern(s)"
            )
        else
          case duplicates (patternBoundNames (armPatterns arm)) of
            [] -> Right ()
            d:_ -> Left ("duplicate case pattern binding: " <> d)

patternBoundNames :: [CasePattern] -> [Name]
patternBoundNames = foldr (\pat acc -> boundInPattern pat <> acc) []
  where
    boundInPattern :: CasePattern -> [Name]
    boundInPattern pat =
      case pat of
        PatWildcard -> []
        PatVar n -> [n]
        PatInt _ -> []
        PatString _ -> []
        PatConstructor _ _ fieldNames ->
          [n | n <- fieldNames, n /= "_"]

isIrrefutablePattern :: CasePattern -> Bool
isIrrefutablePattern pat =
  case pat of
    PatWildcard -> True
    PatVar _ -> True
    PatInt _ -> False
    PatString _ -> False
    PatConstructor _ _ _ -> False

desugarCaseExpr :: Expr -> Expr
desugarCaseExpr expr =
  case expr of
    Var _ -> expr
    IntLit _ -> expr
    StringLit _ -> expr
    CollectionLit elems ->
      CollectionLit (map desugarCaseExpr elems)
    Lam n bodyExpr ->
      Lam n (desugarCaseExpr bodyExpr)
    App f x ->
      App (desugarCaseExpr f) (desugarCaseExpr x)
    Case scrutinees arms ->
      desugarCaseExpression scrutinees arms
  where
    desugarCaseExpression :: [Expr] -> [CaseArm] -> Expr
    desugarCaseExpression scrutinees0 arms0 =
      let scrutinees = map desugarCaseExpr scrutinees0
          arms = map desugarArm arms0
          usedNames = uniqueNames (concatMap freeVarsExpr scrutinees <> concatMap armUsedNames arms)
          (scrutineeNames, nextAfterScrutinees) = freshMany "__case_scrutinee_" 0 (length scrutinees) usedNames
          (dummyNames, _nextAfterDummies) = freshMany "__case_dummy_" nextAfterScrutinees (max 0 (length arms - 1)) (scrutineeNames <> usedNames)
          branchExpr = buildBranches scrutineeNames dummyNames arms
       in foldr (\(tmpName, valExpr) acc -> App (Lam tmpName acc) valExpr) branchExpr (zip scrutineeNames scrutinees)

    desugarArm :: CaseArm -> CaseArm
    desugarArm arm =
      arm {armBody = desugarCaseExpr (armBody arm)}

    armUsedNames :: CaseArm -> [Name]
    armUsedNames arm = patternBoundNames (armPatterns arm) <> freeVarsExpr (armBody arm)

    buildBranches :: [Name] -> [Name] -> [CaseArm] -> Expr
    buildBranches scrutineeNames dummyNames arms0 =
      case reverse arms0 of
        [] ->
          IntLit 0
        lastArm:restReversed ->
          let startExpr = applyPatternBindings scrutineeNames (armPatterns lastArm) (armBody lastArm)
           in foldl
                (\acc (arm, dummyName) ->
                    let condExpr = buildArmCondition scrutineeNames (armPatterns arm)
                        thenExpr = applyPatternBindings scrutineeNames (armPatterns arm) (armBody arm)
                     in mkIfExpr dummyName condExpr thenExpr acc
                )
                startExpr
                (zip restReversed dummyNames)

    buildArmCondition :: [Name] -> [CasePattern] -> Expr
    buildArmCondition scrutineeNames patterns0 =
      let checks = zipWith buildPatternCondition scrutineeNames patterns0
       in case checks of
            [] -> trueExpr
            c:cs -> foldl (\acc x -> App (App (Var "and") acc) x) c cs

    buildPatternCondition :: Name -> CasePattern -> Expr
    buildPatternCondition scrutineeName pat =
      case pat of
        PatWildcard -> trueExpr
        PatVar _ -> trueExpr
        PatInt n ->
          App (App (Var "eq") (Var scrutineeName)) (IntLit n)
        PatString s ->
          App (App (Var "str_eq") (Var scrutineeName)) (StringLit s)
        PatConstructor _ ctorInfo _ ->
          case ctorLiteralBacking ctorInfo of
            Just (TypeUnionInt n) ->
              App (App (Var "eq") (Var scrutineeName)) (IntLit n)
            Just (TypeUnionString s) ->
              App (App (Var "str_eq") (Var scrutineeName)) (StringLit s)
            Nothing ->
              App (Var (mkIsBuiltinName ctorInfo)) (Var scrutineeName)

    applyPatternBindings :: [Name] -> [CasePattern] -> Expr -> Expr
    applyPatternBindings scrutineeNames patterns0 bodyExpr0 =
      foldr bindOne bodyExpr0 (zip scrutineeNames patterns0)
      where
        bindOne :: (Name, CasePattern) -> Expr -> Expr
        bindOne (scrutineeName, pat) bodyExpr =
          case pat of
            PatWildcard -> bodyExpr
            PatInt _ -> bodyExpr
            PatString _ -> bodyExpr
            PatVar n ->
              App (Lam n bodyExpr) (Var scrutineeName)
            PatConstructor _ ctorInfo fieldNames ->
              case ctorLiteralBacking ctorInfo of
                Just _ ->
                  bodyExpr
                Nothing ->
                  foldr
                    (\(idx, fieldName) acc ->
                        if fieldName == "_"
                          then acc
                          else
                            App
                              (Lam fieldName acc)
                              (App (Var (mkGetterBuiltinName ctorInfo idx)) (Var scrutineeName))
                    )
                    bodyExpr
                    (zip [0 :: Int ..] fieldNames)

    mkIfExpr :: Name -> Expr -> Expr -> Expr -> Expr
    mkIfExpr dummyName condExpr thenExpr elseExpr =
      App
        ( App
            ( App
                (Var "if")
                condExpr
            )
            (Lam dummyName thenExpr)
        )
        (Lam dummyName elseExpr)

    trueExpr :: Expr
    trueExpr =
      App (App (Var "eq") (IntLit 0)) (IntLit 0)

    freshMany :: Name -> Int -> Int -> [Name] -> ([Name], Int)
    freshMany _prefix seed count _used
      | count <= 0 = ([], seed)
    freshMany prefix seed count used =
      let (n, seed1) = freshName prefix seed used
          (rest, seed2) = freshMany prefix seed1 (count - 1) (n : used)
       in (n : rest, seed2)

    freshName :: Name -> Int -> [Name] -> (Name, Int)
    freshName prefix seed used =
      let candidate = prefix <> show seed
       in if candidate `elem` used
            then freshName prefix (seed + 1) used
            else (candidate, seed + 1)

    freeVarsExpr :: Expr -> [Name]
    freeVarsExpr = uniqueNames . go []
      where
        go :: [Name] -> Expr -> [Name]
        go bound expr0 =
          case expr0 of
            Var n
              | n `elem` bound -> []
              | otherwise -> [n]
            IntLit _ -> []
            StringLit _ -> []
            CollectionLit elems -> concatMap (go bound) elems
            Case scrutinees arms ->
              concatMap (go bound) scrutinees
                <> concatMap (goArm bound) arms
            Lam n bodyExpr -> go (n : bound) bodyExpr
            App f x -> go bound f <> go bound x

        goArm :: [Name] -> CaseArm -> [Name]
        goArm bound arm =
          go (patternBoundNames (armPatterns arm) <> bound) (armBody arm)

parseLetTokens :: OperatorTable -> ConstructorTable -> [ExprTok] -> Either String (Expr, [ExprTok])
parseLetTokens operators constructors toks =
  case toks of
    TokLet:rest -> do
      (bindings, afterBindings) <- parseLetBindings operators constructors rest
      afterIn <-
        case afterBindings of
          TokIn:after -> Right after
          _ -> Left "could not parse expression; let is missing in"
      (bodyExpr, restFinal) <- parseExprTokens operators constructors 0 afterIn
      pure (desugarLetBindings bindings bodyExpr, restFinal)
    _ ->
      Left "could not parse expression; expected let"
  where
    parseLetBindings :: OperatorTable -> ConstructorTable -> [ExprTok] -> Either String ([LetBinding], [ExprTok])
    parseLetBindings operators0 ctors0 toks0 = do
      (firstBinding, rest1) <- parseLetBinding operators0 ctors0 toks0
      go [firstBinding] rest1
      where
        go :: [LetBinding] -> [ExprTok] -> Either String ([LetBinding], [ExprTok])
        go acc toks1 =
          case toks1 of
            TokSemicolon:rest2 -> do
              (nextBinding, rest3) <- parseLetBinding operators0 ctors0 rest2
              go (acc <> [nextBinding]) rest3
            TokIn:_ ->
              Right (acc, toks1)
            _ ->
              Left "could not parse expression; let bindings must be separated by ; and followed by in"

    parseLetBinding :: OperatorTable -> ConstructorTable -> [ExprTok] -> Either String (LetBinding, [ExprTok])
    parseLetBinding operators0 ctors0 toks0 = do
      (allNames, afterEq) <- consumeBindingHead toks0
      (bindingHead, trailingNames) <-
        case allNames of
          [] -> Left "could not parse expression; expected let binding name"
          name0:restNames -> Right (name0, restNames)
      (valueExprRaw, restOut) <- parseExprTokens operators0 ctors0 0 afterEq
      case lookup bindingHead ctors0 of
        Just ctorInfo
          | length trailingNames == ctorFieldCount ctorInfo -> do
              traverse_ (validateIdentifier "let pattern field name") trailingNames
              case duplicates trailingNames of
                [] ->
                  pure (LetPatternBinding ctorInfo trailingNames valueExprRaw, restOut)
                d:_ ->
                  Left ("duplicate let pattern field name: " <> d)
          | otherwise ->
              Left
                ( "could not parse expression; constructor pattern "
                    <> bindingHead
                    <> " expects "
                    <> show (ctorFieldCount ctorInfo)
                    <> " field(s), got "
                    <> show (length trailingNames)
                )
        Nothing -> do
          validateIdentifier "let binding name" bindingHead
          traverse_ (validateIdentifier "let binding parameter") trailingNames
          let valueExpr = foldr Lam valueExprRaw trailingNames
          pure (LetValueBinding bindingHead valueExpr, restOut)
      where
        consumeBindingHead :: [ExprTok] -> Either String ([Name], [ExprTok])
        consumeBindingHead toks1 =
          case toks1 of
            TokIdentifier bindingName:rest1 ->
              gather [bindingName] rest1
            TokIn:_ ->
              Left "could not parse expression; let has no bindings"
            _ ->
              Left "could not parse expression; expected let binding name"

        gather :: [Name] -> [ExprTok] -> Either String ([Name], [ExprTok])
        gather acc toks1 =
          case toks1 of
            TokIdentifier param:rest1 ->
              gather (acc <> [param]) rest1
            TokOperator "=":rest1 ->
              Right (acc, rest1)
            _ ->
              Left "could not parse expression; expected = in let binding"

    desugarLetBindings :: [LetBinding] -> Expr -> Expr
    desugarLetBindings bindings0 bodyExpr = fst (go 0 bindings0 bodyExpr)
      where
        go :: Int -> [LetBinding] -> Expr -> (Expr, Int)
        go nextTmp allBindings body0 =
          case allBindings of
            [] ->
              (body0, nextTmp)
            binding0:restBindings ->
              let (restExpr, nextAfterRest) = go nextTmp restBindings body0
               in case binding0 of
                    LetValueBinding bindingName valueExpr ->
                      ( App (Lam bindingName restExpr) valueExpr
                      , nextAfterRest
                      )
                    LetPatternBinding ctorInfo fieldNames valueExpr ->
                      let usedNames = freeVarsExpr valueExpr <> freeVarsExpr restExpr <> fieldNames
                          tmpName = freshLetTempName nextAfterRest usedNames
                          getterBase getterName =
                            App
                              (Var getterName)
                              (Var tmpName)
                          destructured =
                            foldr
                              (\(idx, fieldName) acc -> App (Lam fieldName acc) (getterBase (mkGetterBuiltinName ctorInfo idx)))
                              restExpr
                              (zip [0 :: Int ..] fieldNames)
                       in ( App (Lam tmpName destructured) valueExpr
                          , nextAfterRest + 1
                          )

    freeVarsExpr :: Expr -> [Name]
    freeVarsExpr = uniqueNames . go []
      where
        go :: [Name] -> Expr -> [Name]
        go bound expr =
          case expr of
            Var n
              | n `elem` bound -> []
              | otherwise -> [n]
            IntLit _ -> []
            StringLit _ -> []
            CollectionLit elems -> concatMap (go bound) elems
            Case scrutinees arms ->
              concatMap (go bound) scrutinees
                <> concatMap (goArm bound) arms
            Lam n bodyExpr -> go (n : bound) bodyExpr
            App f x -> go bound f <> go bound x

        goArm :: [Name] -> CaseArm -> [Name]
        goArm bound arm =
          go (patternBoundNames (armPatterns arm) <> bound) (armBody arm)

    freshLetTempName :: Int -> [Name] -> Name
    freshLetTempName seed usedNames =
      let candidate = "__let_tmp_" <> show seed
       in if candidate `elem` usedNames
            then freshLetTempName (seed + 1) usedNames
            else candidate

parseLambdaTokens :: OperatorTable -> ConstructorTable -> [ExprTok] -> Either String (Expr, [ExprTok])
parseLambdaTokens operators constructors toks =
  case toks of
    TokLambda:rest ->
      do
        (params, afterParams) <- consumeLambdaParams rest
        afterArrow <-
          case afterParams of
            TokArrow:after -> Right after
            _ -> Left "could not parse expression; lambda is missing ->"
        (bodyExpr, restFinal) <- parseExprTokens operators constructors 0 afterArrow
        pure (foldr Lam bodyExpr params, restFinal)
    _ ->
      Left "could not parse expression; expected lambda"
  where
    consumeLambdaParams :: [ExprTok] -> Either String ([Name], [ExprTok])
    consumeLambdaParams toks0 =
      let (params, rest0) = consume [] toks0
       in case params of
            [] -> Left "could not parse expression; lambda has no parameter"
            _ -> Right (reverse params, rest0)

    consume :: [Name] -> [ExprTok] -> ([Name], [ExprTok])
    consume acc toks0 =
      case toks0 of
        TokIdentifier n:rest ->
          consume (n : acc) rest
        _ ->
          (acc, toks0)

parseInfixTokens :: OperatorTable -> ConstructorTable -> Int -> [ExprTok] -> Either String (Expr, [ExprTok])
parseInfixTokens operators constructors minPrec toks = do
  (lhs0, rest0) <- parseApplicationTokens operators constructors toks
  rewrite lhs0 rest0
  where
    -- Haskell-style backtick operators can use any identifier function name
    -- without an explicit infix declaration. We treat them as infixl 9.
    lookupOperatorInfo :: String -> Maybe OperatorInfo
    lookupOperatorInfo opToken =
      case lookup opToken operators of
        Just info ->
          Just info
        Nothing
          | validIdentifier opToken ->
              Just
                OperatorInfo
                  { opAssoc = AssocLeft
                  , opPrecedence = 9
                  , opTarget = opToken
                  }
          | otherwise ->
              Nothing

    rewrite :: Expr -> [ExprTok] -> Either String (Expr, [ExprTok])
    rewrite lhs toks0 =
      case toks0 of
        TokOperator opToken:afterOp ->
          case lookupOperatorInfo opToken of
            Nothing ->
              Left ("unknown infix operator: " <> opToken)
            Just info
              | opPrecedence info < minPrec ->
                  Right (lhs, toks0)
              | otherwise -> do
                  let rhsMin =
                        case opAssoc info of
                          AssocLeft -> opPrecedence info + 1
                          AssocRight -> opPrecedence info
                          AssocNone -> opPrecedence info + 1
                  (rhsExpr, restAfterRhs) <- parseExprTokens operators constructors rhsMin afterOp
                  let nextExpr =
                        App
                          (App (Var (opTarget info)) lhs)
                          rhsExpr
                  case opAssoc info of
                    AssocNone ->
                      case restAfterRhs of
                        TokOperator nextOp:_ ->
                          case lookupOperatorInfo nextOp of
                            Just nextInfo
                              | opPrecedence nextInfo == opPrecedence info ->
                                  Left ("non-associative operator cannot be chained: " <> opToken)
                            _ ->
                              rewrite nextExpr restAfterRhs
                        _ ->
                          rewrite nextExpr restAfterRhs
                    _ ->
                      rewrite nextExpr restAfterRhs
        _ ->
          Right (lhs, toks0)

parseApplicationTokens :: OperatorTable -> ConstructorTable -> [ExprTok] -> Either String (Expr, [ExprTok])
parseApplicationTokens operators constructors toks = do
  (headExpr, rest0) <- parseTermTokens operators constructors toks
  applyMany headExpr rest0
  where
    applyMany :: Expr -> [ExprTok] -> Either String (Expr, [ExprTok])
    applyMany cur toks0
      | startsTerm toks0 = do
          (argExpr, rest1) <- parseTermTokens operators constructors toks0
          applyMany (App cur argExpr) rest1
      | otherwise =
          Right (cur, toks0)

startsTerm :: [ExprTok] -> Bool
startsTerm toks =
  case toks of
    TokIdentifier _:_ -> True
    TokInteger _:_ -> True
    TokString _:_ -> True
    TokLParen:_ -> True
    TokLBracket:_ -> True
    _ -> False

parseTermTokens :: OperatorTable -> ConstructorTable -> [ExprTok] -> Either String (Expr, [ExprTok])
parseTermTokens operators constructors toks =
  case toks of
    TokOperator "-":TokInteger n:rest ->
      Right (IntLit (negate n), rest)
    TokIdentifier n:rest ->
      Right (Var n, rest)
    TokInteger n:rest ->
      Right (IntLit n, rest)
    TokString s:rest ->
      Right (StringLit s, rest)
    TokLParen:rest -> do
      (inner, afterInner) <- parseExprTokens operators constructors 0 rest
      case afterInner of
        TokRParen:afterParen ->
          Right (inner, afterParen)
        _ ->
          Left "could not parse expression; missing closing parenthesis"
    TokLBracket:rest ->
      parseCollectionLiteral rest
    TokOperator op:_ ->
      Left ("could not parse expression; unexpected operator token: " <> op)
    TokRParen:_ ->
      Left "could not parse expression; unexpected )"
    TokRBracket:_ ->
      Left "could not parse expression; unexpected ]"
    TokComma:_ ->
      Left "could not parse expression; unexpected ,"
    TokLambda:_ ->
      Left "could not parse expression; lambda must be top-level or parenthesized"
    TokArrow:_ ->
      Left "could not parse expression; unexpected ->"
    TokLet:_ ->
      Left "could not parse expression; let must be top-level or parenthesized"
    TokIn:_ ->
      Left "could not parse expression; unexpected in"
    TokCase:_ ->
      Left "could not parse expression; case must be top-level or parenthesized"
    TokOf:_ ->
      Left "could not parse expression; unexpected of"
    TokSemicolon:_ ->
      Left "could not parse expression; unexpected ;"
    [] ->
      Left "could not parse expression; unexpected end of input"
  where
    parseCollectionLiteral :: [ExprTok] -> Either String (Expr, [ExprTok])
    parseCollectionLiteral toks0 =
      case toks0 of
        TokRBracket:after ->
          Right (CollectionLit [], after)
        _ -> do
          (firstElem, rest1) <- parseExprTokens operators constructors 0 toks0
          gather [firstElem] rest1

    gather :: [Expr] -> [ExprTok] -> Either String (Expr, [ExprTok])
    gather revElems toks0 =
      case toks0 of
        TokComma:rest -> do
          (nextElem, restAfterElem) <- parseExprTokens operators constructors 0 rest
          gather (nextElem : revElems) restAfterElem
        TokRBracket:after ->
          Right (CollectionLit (reverse revElems), after)
        _ ->
          Left "could not parse expression; collection literal is missing closing ]"

tokenizeExpr :: String -> Either String [ExprTok]
tokenizeExpr = go []
  where
    go :: [ExprTok] -> String -> Either String [ExprTok]
    go acc [] = Right (reverse acc)
    go acc (c:cs)
      | isSpace c =
          go acc cs
      | c == '(' =
          go (TokLParen : acc) cs
      | c == ')' =
          go (TokRParen : acc) cs
      | c == '[' =
          go (TokLBracket : acc) cs
      | c == ']' =
          go (TokRBracket : acc) cs
      | c == ',' =
          go (TokComma : acc) cs
      | c == '\\' =
          go (TokLambda : acc) cs
      | c == ';' =
          go (TokSemicolon : acc) cs
      | c == '"' = do
          (literalText, restSrc) <- consumeStringLiteral [] cs
          go (TokString literalText : acc) restSrc
      | c == '\'' = do
          (literalChar, restSrc) <- consumeCharLiteral cs
          go (TokInteger (fromEnum literalChar) : acc) restSrc
      | c == '`' =
          case span (/= '`') cs of
            (_inner, []) ->
              Left "could not parse expression; unterminated backtick operator"
            (inner, _tick:rest) -> do
              let opName = inner
              if validIdentifier opName
                then go (TokOperator opName : acc) rest
                else Left ("could not parse expression; invalid backtick operator: " <> opName)
      | isIdentifierStart c =
          let (restIdent, restSrc) = span isIdentifierChar cs
              ident = c : restIdent
          in case ident of
                "let" -> go (TokLet : acc) restSrc
                "in" -> go (TokIn : acc) restSrc
                "case" -> go (TokCase : acc) restSrc
                "of" -> go (TokOf : acc) restSrc
                "then" -> Left "could not parse expression; if/then/else syntax is not supported; use case"
                "else" -> Left "could not parse expression; if/then/else syntax is not supported; use case"
                _ -> go (TokIdentifier ident : acc) restSrc
      | isDigit c =
          let (restDigits, restSrc) = span isDigit cs
              intText = c : restDigits
           in case reads intText of
                [(n, "")] -> go (TokInteger n : acc) restSrc
                _ -> Left ("could not parse expression integer literal: " <> intText)
      | isOperatorChar c =
          let (restOps, restSrc) = span isOperatorChar cs
              opText = c : restOps
           in case opText of
                "->" -> go (TokArrow : acc) restSrc
                _ -> go (TokOperator opText : acc) restSrc
      | otherwise =
          Left ("could not parse expression; unexpected character: " <> [c])

    consumeStringLiteral :: String -> String -> Either String (String, String)
    consumeStringLiteral revAcc src =
      case src of
        [] ->
          Left "could not parse expression; unterminated string literal"
        '"':rest ->
          Right (reverse revAcc, rest)
        '\n':_ ->
          Left "could not parse expression; string literal cannot contain newline"
        '\\':rest ->
          case rest of
            [] ->
              Left "could not parse expression; unterminated string escape"
            esc:tailChars ->
              case decodeEscape esc of
                Nothing ->
                  Left ("could not parse expression; invalid string escape: \\" <> [esc])
                Just decoded ->
                  consumeStringLiteral (decoded : revAcc) tailChars
        ch:rest ->
          consumeStringLiteral (ch : revAcc) rest

    consumeCharLiteral :: String -> Either String (Char, String)
    consumeCharLiteral src =
      case src of
        [] ->
          Left "could not parse expression; unterminated char literal"
        '\n':_ ->
          Left "could not parse expression; char literal cannot contain newline"
        '\\':rest ->
          case rest of
            [] ->
              Left "could not parse expression; unterminated char escape"
            esc:tailChars ->
              case decodeEscape esc of
                Nothing ->
                  Left ("could not parse expression; invalid char escape: \\" <> [esc])
                Just decoded ->
                  case tailChars of
                    '\'':after ->
                      Right (decoded, after)
                    _ ->
                      Left "could not parse expression; char literal must contain exactly one character"
        ch:rest ->
          case rest of
            '\'':after ->
              Right (ch, after)
            _ ->
              Left "could not parse expression; char literal must contain exactly one character"

    decodeEscape :: Char -> Maybe Char
    decodeEscape esc =
      case esc of
        '"' -> Just '"'
        '\'' -> Just '\''
        '\\' -> Just '\\'
        'n' -> Just '\n'
        't' -> Just '\t'
        'r' -> Just '\r'
        _ -> Nothing

renderExprTok :: ExprTok -> String
renderExprTok tok =
  case tok of
    TokIdentifier n -> n
    TokInteger n -> show n
    TokString s -> "\"" <> concatMap escapeChar s <> "\""
    TokOperator op -> op
    TokLParen -> "("
    TokRParen -> ")"
    TokLBracket -> "["
    TokRBracket -> "]"
    TokComma -> ","
    TokLambda -> "\\"
    TokArrow -> "->"
    TokLet -> "let"
    TokIn -> "in"
    TokCase -> "case"
    TokOf -> "of"
    TokSemicolon -> ";"
  where
    escapeChar :: Char -> String
    escapeChar c =
      case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\t' -> "\\t"
        '\r' -> "\\r"
        _ -> [c]

buildOperatorTable :: [OperatorDecl] -> OperatorTable
buildOperatorTable userDecls = map toEntry activeDecls
  where
    userTokens = map declOpToken userDecls
    activeDecls = userDecls <> filter (\decl -> declOpToken decl `notElem` userTokens) defaultOperatorDecls

    toEntry :: OperatorDecl -> (String, OperatorInfo)
    toEntry decl =
      ( declOpToken decl
      , OperatorInfo
          { opAssoc = declOpAssoc decl
          , opPrecedence = declOpPrecedence decl
          , opTarget = declOpTarget decl
          }
      )

defaultOperatorDecls :: [OperatorDecl]
defaultOperatorDecls =
  [ OperatorDecl 0 AssocLeft 6 "+" "add"
  , OperatorDecl 0 AssocLeft 6 "-" "sub"
  , OperatorDecl 0 AssocLeft 7 "*" "mul"
  , OperatorDecl 0 AssocLeft 7 "/" "div"
  , OperatorDecl 0 AssocNone 4 "<" "lt"
  , OperatorDecl 0 AssocNone 4 "<=" "le"
  , OperatorDecl 0 AssocNone 4 ">" "gt"
  , OperatorDecl 0 AssocNone 4 ">=" "ge"
  , OperatorDecl 0 AssocNone 4 "==" "eq"
  , OperatorDecl 0 AssocRight 3 "&&" "and"
  ]

parseOperatorDeclAt :: Int -> String -> Either String OperatorDecl
parseOperatorDeclAt lineNo src =
  case words src of
    [assocRaw, precRaw, opToken, "=", targetName] -> do
      assoc <-
        case parseAssoc assocRaw of
          Nothing ->
            Left
              ( withLine
                  lineNo
                  ("unknown fixity declaration: " <> assocRaw)
              )
          Just out ->
            Right out
      precedence <-
        case parsePrecedence precRaw of
          Nothing ->
            Left
              ( withLine
                  lineNo
                  ("invalid operator precedence: " <> precRaw <> " (expected 0..9)")
              )
          Just out ->
            Right out
      if validOperatorToken opToken
        then Right ()
        else
          Left
            ( withLine
                lineNo
                ("invalid operator token: " <> opToken)
            )
      validateIdentifier "operator target name" targetName
      pure
        OperatorDecl
          { declOpLine = lineNo
          , declOpAssoc = assoc
          , declOpPrecedence = precedence
          , declOpToken = opToken
          , declOpTarget = targetName
          }
    _ ->
      Left
        ( withLine
            lineNo
            "could not parse operator declaration; expected: infixl|infixr|infix <precedence> <operator> = <target>"
        )

parseAssoc :: String -> Maybe Assoc
parseAssoc assocRaw =
  case assocRaw of
    "infixl" -> Just AssocLeft
    "infixr" -> Just AssocRight
    "infix" -> Just AssocNone
    _ -> Nothing

parsePrecedence :: String -> Maybe Int
parsePrecedence raw =
  if all isDigit raw && not (null raw)
    then
      case reads raw of
        [(n, "")]
          | n >= 0 && n <= 9 ->
              Just n
        _ ->
          Nothing
    else
      Nothing

validOperatorToken :: String -> Bool
validOperatorToken token =
  validIdentifier token
    || validSymbolicOperator token
  where
    validSymbolicOperator :: String -> Bool
    validSymbolicOperator opToken =
      not (null opToken)
        && all isOperatorChar opToken
        && opToken /= "->"
        && opToken /= "=>"
        && opToken /= "="

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` "!#$%&*+./<=>?@\\^|-~:"

isIdentifierStart :: Char -> Bool
isIdentifierStart c = isLowerIdentifierStart c || isAsciiUpper c

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isLowerIdentifierChar c || isAsciiUpper c

isLowerIdentifierStart :: Char -> Bool
isLowerIdentifierStart c = isAsciiLower c || c == '_'

isLowerIdentifierChar :: Char -> Bool
isLowerIdentifierChar c = isAsciiLower c || isDigit c || c == '_' || c == '\''

stripPrefixExact :: String -> String -> Maybe String
stripPrefixExact prefix src =
  if startsWith prefix src
    then Just (trimLeft (drop (length prefix) src))
    else Nothing

consumeIdentifier :: String -> Int -> String -> Either String (Name, String)
consumeIdentifier label lineNo src =
  let trimmed = trimLeft src
      (prefix, rest) = span isIdentifierChar trimmed
   in if validIdentifier prefix
        then Right (prefix, trimLeft rest)
        else Left (withLine lineNo (label <> " is invalid: " <> prefix))

splitOnFirst :: Char -> String -> Maybe (String, String)
splitOnFirst needle src =
  case break (== needle) src of
    (lhs, _sep:rhs) -> Just (lhs, rhs)
    _ -> Nothing

looksLikeTypeSignature :: String -> Bool
looksLikeTypeSignature src =
  case splitOnFirst ':' src of
    Nothing -> False
    Just (lhsRaw, rhsRaw) ->
      let lhs = trim lhsRaw
          rhs = trim rhsRaw
       in validIdentifier lhs
            && not (null rhs)
            && length (words lhs) == 1

parseTypeSignatureAt :: Int -> String -> Either String TypeSignature
parseTypeSignatureAt lineNo src =
  case parseTypeSignature src of
    Left err -> Left (withLine lineNo err)
    Right out -> Right out

parseTypeSignature :: String -> Either String TypeSignature
parseTypeSignature src = do
  (lhsRaw, rhsRaw) <-
    case splitOnFirst ':' src of
      Nothing -> Left "could not parse type signature; expected: <name> : <type_expr>"
      Just out -> Right out
  let fnName0 = trim lhsRaw
      rhs0 = trim rhsRaw
  validateIdentifier "type signature name" fnName0
  if null rhs0
    then Left "could not parse type signature; expected type expression after :"
    else do
      let (constraintsRaw0, typeRaw0) =
            case breakOn "=>" rhs0 of
              Nothing -> ("", rhs0)
              Just (lhs, rhs) -> (lhs, rhs)
          constraintsRaw = trim constraintsRaw0
          typeRaw = trim typeRaw0
      if null typeRaw
        then Left "could not parse type signature; expected type expression after =>"
        else do
          constraintsOut <- parseSignatureConstraints constraintsRaw
          pure
            TypeSignature
              { sigName = fnName0
              , sigConstraints = constraintsOut
              , sigTypeExpr = typeRaw
              }

parseSignatureConstraints :: String -> Either String [SignatureConstraint]
parseSignatureConstraints raw
  | null raw = Right []
  | otherwise = do
      parts <- splitTopLevelCommas raw
      constraintsOut <- traverse parseConstraint parts
      let witnessNames = [w | SignatureConstraint {constraintWitness = Just w} <- constraintsOut]
      case duplicates witnessNames of
        [] -> Right constraintsOut
        d:_ -> Left ("duplicate signature witness name: " <> d)
  where
    parseConstraint :: String -> Either String SignatureConstraint
    parseConstraint piece = do
      let trimmed = trim piece
      if null trimmed
        then Left "could not parse signature constraint; empty constraint"
        else parseConstraintTrimmed trimmed

    parseConstraintTrimmed :: String -> Either String SignatureConstraint
    parseConstraintTrimmed trimmed =
      if startsWith "(" trimmed && last trimmed == ')'
        then do
          let inner = trim (init (drop 1 trimmed))
          if null inner
            then Left "could not parse signature constraint; empty () constraint"
            else
              case splitOnFirst ':' inner of
                Just (wRaw, restRaw) -> do
                  let witness0 = trim wRaw
                  validateIdentifier "signature witness name" witness0
                  parseConstraintCore (Just witness0) (trim restRaw)
                Nothing ->
                  parseConstraintCore Nothing inner
        else parseConstraintCore Nothing trimmed

    parseConstraintCore :: Maybe Name -> String -> Either String SignatureConstraint
    parseConstraintCore witness0 text =
      case words text of
        [] ->
          Left "could not parse signature constraint; expected: <class_name> <type_arg>..."
        className0:typeArgs0 -> do
          validateIdentifier "signature constraint class name" className0
          traverse_ (validateTypeArg "signature constraint type argument") typeArgs0
          pure
            SignatureConstraint
              { constraintWitness = witness0
              , constraintClass = className0
              , constraintTypeArgs = typeArgs0
              }

splitTopLevelCommas :: String -> Either String [String]
splitTopLevelCommas src = go 0 [] [] src
  where
    go :: Int -> String -> [String] -> String -> Either String [String]
    go depth revCur revAcc rest =
      case rest of
        [] ->
          if depth /= 0
            then Left "could not parse signature constraints; unbalanced parentheses"
            else Right (reverse (reverse revCur : revAcc))
        c:cs
          | c == '(' ->
              go (depth + 1) (c : revCur) revAcc cs
          | c == ')' ->
              if depth <= 0
                then Left "could not parse signature constraints; unexpected )"
                else go (depth - 1) (c : revCur) revAcc cs
          | c == ',' && depth == 0 ->
              go depth [] (reverse revCur : revAcc) cs
          | otherwise ->
              go depth (c : revCur) revAcc cs

splitOnArrow :: String -> (String, String)
splitOnArrow src =
  case breakOn "=>" src of
    Nothing -> (src, "")
    Just (lhs, rhs) -> (lhs, rhs)

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack = go [] haystack
  where
    go :: String -> String -> Maybe (String, String)
    go _ [] = Nothing
    go acc rest =
      if startsWith needle rest
        then Just (reverse acc, drop (length needle) rest)
        else
          case rest of
            c:cs -> go (c : acc) cs

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseClassDecl :: Int -> String -> Either String ClassDecl
parseClassDecl lineNo src =
  case words src of
    "class":rest ->
      case break (== ":") rest of
        (classNameAndParams, ":" : classKindTokens) -> do
          case classNameAndParams of
            [] ->
              Left
                ( withLine
                    lineNo
                    "could not parse class declaration; expected: class <class_name> [type_param ...] : <kind_name>"
                )
            className0 : classParams0 -> do
              validateIdentifier "class name" className0
              traverse_ (validateIdentifier "class parameter") classParams0
              case duplicates classParams0 of
                [] -> Right ()
                d:_ -> Left ("duplicate class parameter: " <> d)
              case classKindTokens of
                [classKindName] -> do
                  classKind <-
                    case parseClassKind classKindName of
                      Nothing -> Left (withLine lineNo ("unknown class kind: " <> classKindName))
                      Just out -> Right out
                  pure
                    ClassDecl
                      { declClassLine = lineNo
                      , declClassName = className0
                      , declClassParams = classParams0
                      , declClassKind = classKind
                      }
                _ ->
                  Left
                    ( withLine
                        lineNo
                        "could not parse class declaration; expected: class <class_name> [type_param ...] : <kind_name>"
                    )
        _ ->
          Left
            ( withLine
                lineNo
                "could not parse class declaration; expected: class <class_name> [type_param ...] : <kind_name>"
            )
    _ ->
      Left
        ( withLine
            lineNo
            "could not parse class declaration; expected: class <class_name> [type_param ...] : <kind_name>"
        )

parseLawDeclWithOperators :: OperatorTable -> ConstructorTable -> Int -> String -> Either String LawDecl
parseLawDeclWithOperators operators constructors lineNo src = do
  restLaw <-
    case stripPrefixExact "law" src of
      Nothing ->
        Left
          ( withLine
              lineNo
              "could not parse law declaration; expected: law <class_name> <law_name> = <lhs_expr> => <rhs_expr>"
          )
      Just out ->
        Right out
  (className0, restClass) <- consumeIdentifier "law class name" lineNo restLaw
  (lawName0, restName) <- consumeIdentifier "law name" lineNo restClass
  afterEq <-
    case stripPrefixExact "=" restName of
      Nothing ->
        Left
          ( withLine
              lineNo
              "could not parse law declaration; expected: law <class_name> <law_name> = <lhs_expr> => <rhs_expr>"
          )
      Just out ->
        Right out
  let (lhsRaw, rhsRaw) = splitOnArrow afterEq
      lhsExprText = trim lhsRaw
      rhsExprText = trim rhsRaw
  if null lhsExprText
    then Left (withLine lineNo "law declaration has empty left-hand side")
    else
      if null rhsExprText
        then Left (withLine lineNo "law declaration has empty right-hand side")
        else do
          lhsExpr <-
            case parseExprWithOperators operators constructors lhsExprText of
              Left err -> Left (withLine lineNo ("could not parse law left-hand side: " <> err))
              Right out -> Right out
          rhsExpr <-
            case parseExprWithOperators operators constructors rhsExprText of
              Left err -> Left (withLine lineNo ("could not parse law right-hand side: " <> err))
              Right out -> Right out
          pure
            LawDecl
              { declLawLine = lineNo
              , declLawClass = className0
              , declLaw = Law {lawName = lawName0, lhs = lhsExpr, rhs = rhsExpr}
              }

parseInstanceDecl :: Int -> String -> Either String InstanceDecl
parseInstanceDecl lineNo src =
    case words src of
    "instance":instanceName0:":":className0:restTokens -> do
      validateIdentifier "instance name" instanceName0
      validateIdentifier "instance class name" className0
      let (typeArgTokens, bindingTokens) = span (isNothing . parseBindingParts) restTokens
      if null bindingTokens
        then Left "could not parse instance declaration; expected at least one <method>=<target> pair"
        else do
          traverse_ (validateTypeArg "instance type argument") typeArgTokens
          bindings <- traverse parseBinding bindingTokens
          let typeArgs = typeArgTokens
          pure
            InstanceDecl
              { declInstanceLine = lineNo
              , declInstanceName = instanceName0
              , declInstanceClass = className0
              , declInstanceTypeArgs = typeArgs
              , declInstanceBindings = bindings
              }
    _ ->
      Left
        ( withLine
            lineNo
            "could not parse instance declaration; expected: instance <instance_name> : <class_name> <method>=<target>..."
        )
  where
    parseBindingParts :: String -> Maybe (Name, Name)
    parseBindingParts token =
      case splitBinding token of
        Just out -> Just out
        Nothing -> Nothing

    parseBinding :: String -> Either String (Name, Name)
    parseBinding token =
      case splitBinding token of
        Nothing ->
          Left
            ( withLine
                lineNo
                ("could not parse instance binding: " <> token <> " (expected <method>=<target>)")
            )
        Just (methodName, targetName) -> do
          validateIdentifier "instance method name" methodName
          validateIdentifier "instance target name" targetName
          pure (methodName, targetName)

splitBinding :: String -> Maybe (String, String)
splitBinding token =
  case break (== '=') token of
    (lhs, '=':rhs)
      | not (null lhs) && not (null rhs) && '=' `notElem` rhs -> Just (lhs, rhs)
      | otherwise -> Nothing
    _ -> Nothing

stripComment :: String -> String
stripComment [] = []
stripComment ('-':'-':_) = []
stripComment (c:cs) = c : stripComment cs

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseDataLineAt :: Int -> String -> Either String [FunctionClause]
parseDataLineAt lineNo src =
  case parseDataDeclAt lineNo src of
    Left err ->
      Left err
    Right decls ->
      Right (map mkConstructorClause decls)

parseDataDeclAt :: Int -> String -> Either String [DataDecl]
parseDataDeclAt lineNo src =
  case parseDataDecl src of
    Left err ->
      Left (withLine lineNo err)
    Right out ->
      Right out

parseDataDecl :: String -> Either String [DataDecl]
parseDataDecl src =
  do
    (isNewtypeDecl, typeName, restTokens) <-
      case words src of
        ("data":typeName0:rest0) ->
          Right (False, typeName0, rest0)
        ("newtype":typeName0:rest0) ->
          Right (True, typeName0, rest0)
        _ ->
          Left "could not parse data declaration; expected: data|newtype <type_name> [type_param ...] = <constructor_name> <field>..."
    validateTypeArg "data type name" typeName
    (typeParams, ctorAndFields) <-
      case break (== "=") restTokens of
        (params0, "=":rhs0) -> Right (params0, rhs0)
        _ -> Left "could not parse data declaration; expected: data|newtype <type_name> [type_param ...] = <constructor_name> <field>..."
    traverse_ (validateIdentifier "data type parameter") typeParams
    case duplicates typeParams of
      [] -> Right ()
      d:_ -> Left ("duplicate data type parameter: " <> d)
    decls <- traverse (parseDataConstructor typeName typeParams) (splitTopLevelChar '|' (unwords ctorAndFields))
    validateLiteralBackedConstructors decls
    if isNewtypeDecl
      then validateNewtypeConstructors decls
      else Right decls
  where
    validateLiteralBackedConstructors :: [DataDecl] -> Either String ()
    validateLiteralBackedConstructors decls =
      let backed = [literal | decl <- decls, Just literal <- [ctorLiteralBacking (dataCtorInfo decl)]]
       in case duplicates (map renderTypeUnionLiteral backed) of
            [] -> Right ()
            d:_ ->
              Left ("could not parse data declaration; duplicate literal-backed constructor value: " <> d)

    renderTypeUnionLiteral :: TypeUnionLiteral -> String
    renderTypeUnionLiteral literal =
      case literal of
        TypeUnionInt n -> show n
        TypeUnionString s -> "\"" <> s <> "\""

    validateNewtypeConstructors :: [DataDecl] -> Either String [DataDecl]
    validateNewtypeConstructors decls =
      case decls of
        [decl]
          | ctorFieldCount (dataCtorInfo decl) == 1 ->
              Right decls
          | otherwise ->
              Left "could not parse newtype declaration; expected exactly one constructor field"
        _ ->
          Left "could not parse newtype declaration; expected exactly one constructor"

    parseDataConstructor :: Name -> [Name] -> String -> Either String DataDecl
    parseDataConstructor typeName0 typeParams0 rawCtor0 =
      let rawCtor = trim rawCtor0
       in if null rawCtor
        then Left "could not parse data declaration; expected constructor declaration"
        else
          case parseLiteralBackedZeroFieldCtor typeName0 typeParams0 rawCtor of
            Just (Right decl) ->
              Right decl
            Just (Left err) ->
              Left err
            Nothing ->
              case parsePrimitiveBackedSingleFieldCtor typeName0 typeParams0 rawCtor of
                Just (Right decl) ->
                  Right decl
                Just (Left err) ->
                  Left err
                Nothing ->
                  case splitOnFirst ':' rawCtor of
                    Nothing ->
                      parseOldStyleCtor typeName0 typeParams0 rawCtor
                    Just (ctorNameRaw, rhsRaw) ->
                      parseGadtStyleCtor typeName0 typeParams0 (trim ctorNameRaw) (trim rhsRaw)

    parseLiteralBackedZeroFieldCtor ::
      Name ->
      [Name] ->
      String ->
      Maybe (Either String DataDecl)
    parseLiteralBackedZeroFieldCtor typeName0 typeParams0 rawCtor =
      case splitAngleCtor rawCtor of
        Nothing ->
          Nothing
        Just (ctorName0, payloadRaw) ->
          case parseLiteralPayload payloadRaw of
            Nothing ->
              Nothing
            Just literal ->
              if not (validLowerIdentifier typeName0)
                then
                  Just
                    ( Left
                        "could not parse data declaration; literal-backed type name must be lowercase"
                    )
                else
                  case
                    ( if validLowerIdentifier ctorName0
                        then Right ()
                        else Left ("data constructor name is invalid: " <> ctorName0)
                    ) of
                    Left err ->
                      Just (Left err)
                    Right () ->
                      let ctorInfo0 =
                            ConstructorInfo
                              { ctorTypeName = typeName0
                              , ctorName = ctorName0
                              , ctorFieldCount = 0
                              , ctorTypeParamCount = length typeParams0
                              , ctorFieldParamMap = []
                              , ctorLiteralBacking = Just literal
                              }
                       in Just
                            ( Right
                                DataDecl
                                  { dataTypeName = typeName0
                                  , dataTypeParams = typeParams0
                                  , dataCtorName = ctorName0
                                  , dataCtorFields = []
                                  , dataCtorInfo = ctorInfo0
                                  }
                            )
      where
        parseLiteralPayload :: String -> Maybe TypeUnionLiteral
        parseLiteralPayload raw =
          case tokenizeExpr raw of
            Right [TokInteger n] ->
              Just (TypeUnionInt n)
            Right [TokOperator "-", TokInteger n] ->
              Just (TypeUnionInt (-n))
            Right [TokString s] ->
              Just (TypeUnionString s)
            _ ->
              Nothing

        splitAngleCtor :: String -> Maybe (Name, String)
        splitAngleCtor raw =
          let trimmed = trim raw
           in case break (== '<') trimmed of
                (ctorNameRaw, '<' : rest) ->
                  let ctorName0 = trim ctorNameRaw
                   in case consumeAngleBody 1 "" rest of
                        Just (payload, trailing)
                          | not (null ctorName0)
                              && null (trim trailing) ->
                              Just (ctorName0, trim payload)
                        _ ->
                          Nothing
                _ ->
                  Nothing

        consumeAngleBody :: Int -> String -> String -> Maybe (String, String)
        consumeAngleBody _ _ [] = Nothing
        consumeAngleBody depth cur (c:rest)
          | c == '<' =
              consumeAngleBody (depth + 1) (cur <> [c]) rest
          | c == '>' && depth == 1 =
              Just (cur, rest)
          | c == '>' =
              consumeAngleBody (depth - 1) (cur <> [c]) rest
          | otherwise =
              consumeAngleBody depth (cur <> [c]) rest

    parsePrimitiveBackedSingleFieldCtor ::
      Name ->
      [Name] ->
      String ->
      Maybe (Either String DataDecl)
    parsePrimitiveBackedSingleFieldCtor typeName0 typeParams0 rawCtor =
      case splitPrimitiveCtor rawCtor of
        Nothing ->
          Nothing
        Just (ctorName0, fieldTypeExpr) ->
          if not (validLowerIdentifier typeName0)
            then
              Just
                ( Left
                    "could not parse data declaration; primitive-backed type name must be lowercase"
                )
            else
              if ctorName0 /= typeName0
                then
                  Just
                    ( Left
                        "could not parse data declaration; primitive-backed constructor name must match data type name"
                    )
                else
                  case
                    ( if validLowerIdentifier ctorName0
                        then Right ()
                        else Left ("data constructor name is invalid: " <> ctorName0)
                    ) of
                    Left err ->
                      Just (Left err)
                    Right () ->
                      case validateFieldTypeExpr fieldTypeExpr of
                        Left err ->
                          Just (Left err)
                        Right () ->
                          case buildFieldParamMap typeParams0 [fieldTypeExpr] of
                            Left err ->
                              Just (Left err)
                            Right (typeParamCount0, fieldParamMap0) ->
                              Just
                                ( Right
                                    DataDecl
                                      { dataTypeName = typeName0
                                      , dataTypeParams = typeParams0
                                      , dataCtorName = ctorName0
                                      , dataCtorFields = inferFieldArgNames [fieldTypeExpr]
                                      , dataCtorInfo =
                                          ConstructorInfo
                                            { ctorTypeName = typeName0
                                            , ctorName = ctorName0
                                            , ctorFieldCount = 1
                                            , ctorTypeParamCount = typeParamCount0
                                            , ctorFieldParamMap = fieldParamMap0
                                            , ctorLiteralBacking = Nothing
                                            }
                                      }
                                )
      where
        splitPrimitiveCtor :: String -> Maybe (Name, String)
        splitPrimitiveCtor raw =
          let trimmed = trim raw
           in case break (== '<') trimmed of
                (ctorNameRaw, '<' : rest) ->
                  let ctorName0 = trim ctorNameRaw
                   in case consumeAngleBody 1 "" rest of
                        Just (fieldExpr, trailing)
                          | not (null ctorName0)
                              && null (trim trailing) ->
                              Just (ctorName0, trim fieldExpr)
                        _ ->
                          Nothing
                _ ->
                  Nothing

        consumeAngleBody :: Int -> String -> String -> Maybe (String, String)
        consumeAngleBody _ _ [] = Nothing
        consumeAngleBody depth cur (c:rest)
          | c == '<' =
              consumeAngleBody (depth + 1) (cur <> [c]) rest
          | c == '>' && depth == 1 =
              Just (cur, rest)
          | c == '>' =
              consumeAngleBody (depth - 1) (cur <> [c]) rest
          | otherwise =
              consumeAngleBody depth (cur <> [c]) rest

    parseOldStyleCtor :: Name -> [Name] -> String -> Either String DataDecl
    parseOldStyleCtor _typeName _typeParams0 rawCtor =
      case splitTopLevelWords rawCtor of
        [] -> Left "could not parse data declaration; expected constructor name"
        ctorName0 : fieldTypeExprs -> do
          validateUpperIdentifier "data constructor name" ctorName0
          traverse_ validateFieldTypeExpr fieldTypeExprs
          (typeParamCount0, fieldParamMap0) <- buildFieldParamMap _typeParams0 fieldTypeExprs
          let fieldNames0 = inferFieldArgNames fieldTypeExprs
              ctorInfo0 =
                ConstructorInfo
                  { ctorTypeName = _typeName
                  , ctorName = ctorName0
                  , ctorFieldCount = length fieldTypeExprs
                  , ctorTypeParamCount = typeParamCount0
                  , ctorFieldParamMap = fieldParamMap0
                  , ctorLiteralBacking = Nothing
                  }
          Right
            DataDecl
              { dataTypeName = _typeName
              , dataTypeParams = _typeParams0
              , dataCtorName = ctorName0
              , dataCtorFields = fieldNames0
              , dataCtorInfo = ctorInfo0
              }

    parseGadtStyleCtor :: Name -> [Name] -> Name -> String -> Either String DataDecl
    parseGadtStyleCtor _typeName _typeParams0 ctorName0 rawRhs =
      validateUpperIdentifier "data constructor name" ctorName0 >>
      if null rawRhs
        then Left "could not parse data declaration; constructor type annotation is missing after :"
        else do
          let typeParts = splitTopLevelToken "->" rawRhs
          if null typeParts
            then Left "could not parse data declaration; invalid constructor type annotation"
            else do
              let fieldTypeExprs = init typeParts
                  resultTypeExpr = last typeParts
              (resultTypeName0, resultTypeArgs) <- parseTypeExpr resultTypeExpr
              if resultTypeName0 /= _typeName
                then
                  Left
                    ( "could not parse data declaration; constructor result type must be "
                        <> _typeName
                        <> ", got "
                        <> resultTypeName0
                        )
                else
                  if length resultTypeArgs /= length _typeParams0
                    then
                      Left
                        ( "could not parse data declaration; constructor result arity for "
                            <> _typeName
                            <> " must have "
                            <> show (length _typeParams0)
                            <> " argument(s)"
                        )
                    else
                      Right
                        DataDecl
                          { dataTypeName = _typeName
                          , dataTypeParams = _typeParams0
                          , dataCtorName = ctorName0
                          , dataCtorFields = syntheticFieldNames (length fieldTypeExprs)
                          , dataCtorInfo =
                              ConstructorInfo
                                { ctorTypeName = _typeName
                                , ctorName = ctorName0
                                , ctorFieldCount = length fieldTypeExprs
                                , ctorTypeParamCount = length _typeParams0
                                , ctorFieldParamMap = map (parseTypeParamField _typeParams0) fieldTypeExprs
                                , ctorLiteralBacking = Nothing
                                }
                          }

    parseTypeExpr :: String -> Either String (Name, [Name])
    parseTypeExpr input =
      case words (trim input) of
        [] ->
          Left "could not parse data declaration; constructor result type is empty"
        typeName0:typeArgs -> do
          validateUpperIdentifier "data constructor result type" typeName0
          traverse_ (validateTypeArg "data constructor result type argument") typeArgs
          Right (typeName0, typeArgs)

    parseTypeParamField :: [Name] -> String -> Maybe Int
    parseTypeParamField allParams fieldExpr =
      case words (stripOuterParens (trim fieldExpr)) of
        [typeArg]
          | typeArg `elem` allParams -> indexOf 0 typeArg allParams
        _ ->
          Nothing

    syntheticFieldNames :: Int -> [Name]
    syntheticFieldNames n = [ "__ctor_field_" <> show idx | idx <- [0 .. n - 1] ]

    buildFieldParamMap :: [Name] -> [String] -> Either String (Int, [Maybe Int])
    buildFieldParamMap typeParams fieldTypeExprs =
      if null typeParams
        then Right (0, replicate (length fieldTypeExprs) Nothing)
        else do
          Right (length typeParams, map (parseTypeParamField typeParams) fieldTypeExprs)

    inferFieldArgNames :: [String] -> [Name]
    inferFieldArgNames fieldTypeExprs = go 0 [] fieldTypeExprs
      where
        go :: Int -> [Name] -> [String] -> [Name]
        go _ _ [] = []
        go idx used (expr : rest) =
          let candidate = inferOne expr
              picked =
                case candidate of
                  Just n | n `notElem` used -> n
                  _ -> "__ctor_field_" <> show idx
           in picked : go (idx + 1) (picked : used) rest

        inferOne :: String -> Maybe Name
        inferOne rawExpr =
          case words (stripOuterParens (trim rawExpr)) of
            [tok]
              | validIdentifier tok -> Just tok
            _ -> Nothing

    validateFieldTypeExpr :: String -> Either String ()
    validateFieldTypeExpr rawExpr =
      let expr = stripOuterParens (trim rawExpr)
       in case words expr of
            [] ->
              Left "data field type expression is empty"
            toks ->
              traverse_ (validateTypeArg "data field type token") toks

    stripOuterParens :: String -> String
    stripOuterParens raw =
      let trimmed = trim raw
       in if hasSingleOuterParens trimmed
            then
              case trimmed of
                '(' : innerWithParen ->
                  case reverse innerWithParen of
                    ')' : innerRev ->
                      stripOuterParens (trim (reverse innerRev))
                    _ ->
                      trimmed
                _ ->
                  trimmed
            else trimmed

    hasSingleOuterParens :: String -> Bool
    hasSingleOuterParens text =
      case text of
        '(' : rest ->
          not (null rest)
            && last rest == ')'
            && enclosesWholeExpr 1 (init rest)
        _ ->
          False
      where
        enclosesWholeExpr :: Int -> String -> Bool
        enclosesWholeExpr depth0 chars =
          go depth0 chars

        go :: Int -> String -> Bool
        go depth chars =
          case chars of
            [] -> depth == 1
            c : cs
              | c == '(' ->
                  go (depth + 1) cs
              | c == ')' ->
                  if depth <= 1
                    then False
                    else go (depth - 1) cs
              | otherwise ->
                  go depth cs

    splitTopLevelWords :: String -> [String]
    splitTopLevelWords input =
      let goWords :: Int -> String -> String -> [String] -> [String]
          goWords depth cur xs revOut =
            case xs of
              [] ->
                let token = trim cur
                 in reverse (if null token then revOut else token : revOut)
              c : rest
                | c == '(' ->
                    goWords (depth + 1) (cur <> [c]) rest revOut
                | c == ')' ->
                    goWords (max 0 (depth - 1)) (cur <> [c]) rest revOut
                | isSpace c && depth == 0 ->
                    let token = trim cur
                     in if null token
                          then goWords depth "" rest revOut
                          else goWords depth "" rest (token : revOut)
                | otherwise ->
                    goWords depth (cur <> [c]) rest revOut
       in goWords 0 "" input []

    indexOf :: Int -> Name -> [Name] -> Maybe Int
    indexOf _ _ [] = Nothing
    indexOf idx target (x:xs)
      | x == target = Just idx
      | otherwise = indexOf (idx + 1) target xs

    splitTopLevelToken :: String -> String -> [String]
    splitTopLevelToken token input =
      let tokenLen = length token
          split' :: Int -> String -> String -> [String]
          split' depth cur xs =
            case xs of
              [] ->
                if null (trim cur)
                  then []
                  else [trim cur]
              c:rest
                | c == '(' ->
                    split' (depth + 1) (cur <> [c]) rest
                | c == ')' ->
                    split' (max 0 (depth - 1)) (cur <> [c]) rest
                | depth == 0 && startsWith token (c : rest) ->
                    let chunk = trim cur
                        next = drop tokenLen (c : rest)
                     in trim chunk : split' 0 "" next
                | otherwise ->
                    split' depth (cur <> [c]) rest
       in split' 0 "" input

    splitTopLevelChar :: Char -> String -> [String]
    splitTopLevelChar sep input =
      let split' :: Int -> String -> String -> [String]
          split' depth cur xs =
            case xs of
              [] ->
                if null (trim cur)
                  then []
                  else [trim cur]
              c:rest
                | c == '(' ->
                    split' (depth + 1) (cur <> [c]) rest
                | c == ')' ->
                    split' (max 0 (depth - 1)) (cur <> [c]) rest
                | c == sep && depth == 0 ->
                    let chunk = trim cur
                        next = rest
                    in trim chunk : split' 0 "" next
                | otherwise ->
                    split' depth (cur <> [c]) rest
       in split' 0 "" input

parseTypeLineAt :: Int -> String -> Either String [FunctionClause]
parseTypeLineAt lineNo src =
  case parseTypeDeclAt lineNo src of
    Left err ->
      Left err
    Right decls ->
      Right (map mkConstructorClause decls)

parseTypeDeclAt :: Int -> String -> Either String [DataDecl]
parseTypeDeclAt lineNo src =
  case parseTypeDecl src of
    Left err ->
      Left (withLine lineNo err)
    Right out ->
      Right out

parseTypeDecl :: String -> Either String [DataDecl]
parseTypeDecl src =
  do
    (typeName, typeParams, rhsRaw) <-
      case splitOnFirst '=' (trim src) of
        Nothing ->
          Left "could not parse type declaration; expected: type <type_name> [type_param ...] = <member> | ..."
        Just (lhsRaw, rhsRaw0) -> do
          let rhsRaw1 = trim rhsRaw0
          case words (trim lhsRaw) of
            "type":typeName0:typeParams0 -> Right (typeName0, typeParams0, rhsRaw1)
            _ ->
              Left "could not parse type declaration; expected: type <type_name> [type_param ...] = <member> | ..."
    validateTypeArg "type name" typeName
    traverse_ (validateTypeArg "type parameter") typeParams
    case duplicates typeParams of
      [] -> Right ()
      d:_ -> Left ("duplicate type parameter: " <> d)
    if null rhsRaw
      then Left "could not parse type declaration; type body is missing"
      else
        if isUnionSyntax rhsRaw
          then do
            _ <- parseTypeUnionDecl typeName typeParams rhsRaw
            Right []
          else Right []
  where
    isUnionSyntax :: String -> Bool
    isUnionSyntax raw =
      let trimmed = trim raw
       in length trimmed >= 2 && head trimmed == '<' && last trimmed == '>'

    parseTypeUnionDecl :: Name -> [Name] -> String -> Either String [TypeUnionLiteral]
    parseTypeUnionDecl _typeName _typeParams rawUnion =
      do
        let inner = trim (drop 1 (init (trim rawUnion)))
        if null inner
          then Left "could not parse type declaration; empty type union"
          else do
            literals <- traverse parseTypeUnionMember (splitTopLevelUnionMembers inner)
            case duplicates (map renderUnionLiteral literals) of
              [] -> Right literals
              d:_ -> Left ("could not parse type declaration; duplicate literal in type union: " <> d)

    parseTypeUnionMember ::
      String ->
      Either String TypeUnionLiteral
    parseTypeUnionMember rawMember =
      let member = trim rawMember
       in if null member
            then Left "could not parse type declaration; expected union member"
            else parseTypeUnionLiteral member

    parseTypeUnionLiteral :: String -> Either String TypeUnionLiteral
    parseTypeUnionLiteral raw =
      case tokenizeExpr raw of
        Right [TokInteger n] ->
          Right (TypeUnionInt n)
        Right [TokOperator "-", TokInteger n] ->
          Right (TypeUnionInt (-n))
        Right [TokString s] ->
          Right (TypeUnionString s)
        Right _ ->
          Left ("could not parse type declaration; expected literal union member: " <> raw)
        Left _ ->
          Left ("could not parse type declaration; expected literal union member: " <> raw)

    renderUnionLiteral :: TypeUnionLiteral -> String
    renderUnionLiteral literal =
      case literal of
        TypeUnionInt n -> show n
        TypeUnionString s -> "\"" <> s <> "\""

    splitTopLevelUnionMembers :: String -> [String]
    splitTopLevelUnionMembers input =
      let split' ::
            Int ->
            Int ->
            Bool ->
            Bool ->
            Bool ->
            String ->
            String ->
            [String] ->
            [String]
          split' parenDepth angleDepth inString inChar escaped cur xs out =
            case xs of
              [] ->
                let token = trim cur
                    outWithToken = if null token then out else token : out
                 in reverse outWithToken
              c:rest
                | escaped ->
                    split' parenDepth angleDepth inString inChar False (cur <> [c]) rest out
                | inString ->
                    case c of
                      '\\' ->
                        split' parenDepth angleDepth True inChar True (cur <> [c]) rest out
                      '\"' ->
                        split' parenDepth angleDepth False inChar False (cur <> [c]) rest out
                      _ ->
                        split' parenDepth angleDepth True inChar False (cur <> [c]) rest out
                | inChar ->
                    case c of
                      '\\' ->
                        split' parenDepth angleDepth inString True True (cur <> [c]) rest out
                      '\'' ->
                        split' parenDepth angleDepth inString False False (cur <> [c]) rest out
                      _ ->
                        split' parenDepth angleDepth inString False False (cur <> [c]) rest out
                | c == '\"' ->
                    split' parenDepth angleDepth True False False (cur <> [c]) rest out
                | c == '\'' ->
                    split' parenDepth angleDepth False True False (cur <> [c]) rest out
                | c == '|' && parenDepth == 0 && angleDepth == 0 ->
                    let token = trim cur
                        out' = if null token then out else token : out
                     in split' parenDepth angleDepth False False False "" rest out'
                | c == '(' ->
                    split' (parenDepth + 1) angleDepth False False False (cur <> [c]) rest out
                | c == ')' ->
                    split' (max 0 (parenDepth - 1)) angleDepth False False False (cur <> [c]) rest out
                | c == '<' ->
                    split' parenDepth (angleDepth + 1) False False False (cur <> [c]) rest out
                | c == '>' ->
                    split' parenDepth (max 0 (angleDepth - 1)) False False False (cur <> [c]) rest out
                | otherwise ->
                    split' parenDepth angleDepth False False False (cur <> [c]) rest out
       in split' 0 0 False False False "" input []

mkConstructorClause :: DataDecl -> FunctionClause
mkConstructorClause decl =
  let ctorInfo = dataCtorInfo decl
      ctorFields = dataCtorFields decl
      ctorBody =
        case ctorLiteralBacking ctorInfo of
          Just (TypeUnionInt n) -> IntLit n
          Just (TypeUnionString s) -> StringLit s
          Nothing -> mkCallExpr ctorInfo ctorFields
   in
  FunctionClause
    { fnClauseName = dataCtorName decl
    , fnClausePatterns = map PatVar ctorFields
    , fnClauseBody = ctorBody
    , fnClauseAttributes = []
    }

mkCallExpr :: ConstructorInfo -> [Name] -> Expr
mkCallExpr ctorInfo fields =
  let builtin = mkCtorBuiltinName ctorInfo
   in foldl App (Var builtin) (map Var fields)

mkCtorBuiltinName :: ConstructorInfo -> Name
mkCtorBuiltinName ctorInfo =
  "__mk_"
    <> mkCtorTag ctorInfo
    <> "_"
    <> show (ctorFieldCount ctorInfo)
    <> "_tpar_"
    <> show (ctorTypeParamCount ctorInfo)
    <> "_fmap_"
    <> encodeFieldMap (ctorFieldParamMap ctorInfo)

mkGetterBuiltinName :: ConstructorInfo -> Int -> Name
mkGetterBuiltinName ctorInfo idx =
  "__get_"
    <> mkCtorTag ctorInfo
    <> "_"
    <> show idx
    <> "_tpar_"
    <> show (ctorTypeParamCount ctorInfo)
    <> "_fmap_"
    <> encodeFieldMap (ctorFieldParamMap ctorInfo)

mkIsBuiltinName :: ConstructorInfo -> Name
mkIsBuiltinName ctorInfo =
  "__is_"
    <> mkCtorTag ctorInfo
    <> "_tpar_"
    <> show (ctorTypeParamCount ctorInfo)
    <> "_fmap_"
    <> encodeFieldMap (ctorFieldParamMap ctorInfo)

mkCtorTag :: ConstructorInfo -> Name
mkCtorTag ctorInfo =
  ctorTypeName ctorInfo <> "#" <> ctorName ctorInfo

encodeFieldMap :: [Maybe Int] -> String
encodeFieldMap [] = "none"
encodeFieldMap xs = intercalate "_" (map encodeField xs)
  where
    encodeField :: Maybe Int -> String
    encodeField = maybe "u" show

validateTypeArg :: String -> Name -> Either String ()
validateTypeArg label ident
  | validLowerIdentifier ident = Right ()
  | validUpperIdentifier ident = Right ()
  | otherwise = Left (label <> " is invalid: " <> ident)

validateIdentifier :: String -> Name -> Either String ()
validateIdentifier label ident
  | validLowerIdentifier ident = Right ()
  | otherwise = Left (label <> " is invalid: " <> ident)

validateUpperIdentifier :: String -> Name -> Either String ()
validateUpperIdentifier label ident
  | validUpperIdentifier ident = Right ()
  | otherwise = Left (label <> " is invalid: " <> ident)

validIdentifier :: Name -> Bool
validIdentifier = validLowerIdentifier

validLowerIdentifier :: Name -> Bool
validLowerIdentifier [] = False
validLowerIdentifier (first:rest) =
  isLowerIdentifierStart first
    && all isLowerIdentifierChar rest
    && not ((first:rest) `elem` reservedKeywords)

validUpperIdentifier :: Name -> Bool
validUpperIdentifier [] = False
validUpperIdentifier (first:rest) =
  isAsciiUpper first
    && all (\c -> isLowerIdentifierChar c || isAsciiUpper c) rest
    && not ((first:rest) `elem` reservedKeywords)

reservedKeywords :: [Name]
reservedKeywords =
  [ "class"
  , "law"
  , "instance"
  , "data"
  , "newtype"
  , "infix"
  , "infixl"
  , "infixr"
  , "let"
  , "in"
  , "case"
  , "of"
  , "then"
  , "else"
  ]

duplicates :: [Name] -> [Name]
duplicates = reverse . go [] []
  where
    go :: [Name] -> [Name] -> [Name] -> [Name]
    go _seen dups [] = dups
    go seen dups (x:xs)
      | x `elem` seen && x `notElem` dups = go seen (x : dups) xs
      | x `elem` seen = go seen dups xs
      | otherwise = go (x : seen) dups xs

uniqueNames :: [Name] -> [Name]
uniqueNames = go []
  where
    go :: [Name] -> [Name] -> [Name]
    go _seen [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

withLine :: Int -> String -> String
withLine lineNo err = "line " <> show lineNo <> ": " <> err
