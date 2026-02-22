module Main (main) where

import qualified Data.ByteString as BS
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char (chr)
import Data.List (isInfixOf, isPrefixOf)
import Data.Word (Word32, Word8)
import Clapse.Modules
  ( CompileArtifact(..)
  , ExportApi(..)
  , compileEntryModule
  , compileEntryModuleToWasm
  , renderTypeScriptBindings
  )
import Control.Exception (finally)
import Control.Monad (forM_, foldM, when)
import MyLib
  ( ClassKind(..)
  , Atom(..)
  , Bind(..)
  , CaseArm(..)
  , CasePattern(..)
  , CollapsedFunction(..)
  , Expr(..)
  , FlatFunction(..)
  , FunctionTypeInfo(..)
  , Function(..)
  , Law(..)
  , Module(..)
  , Op(..)
  , SignatureConstraint(..)
  , TypeSignature(..)
  , Trait(..)
  , TraitCategory(..)
  , Value(..)
  , basicTraits
  , collapseAndVerifyFunction
  , collapseAndVerifyModule
  , collapseAndVerifyModuleFromRoots
  , differentialCheckSourceCollapsed
  , evalCollapsedFunction
  , evalSourceFunction
  , normalizeWithBasicTraits
  , normalizeWithTraits
  , rewriteOnceWithBasicTraits
  , rewriteOnceWithTraits
  , pruneDeadFunctionsFromRoots
  , verifyCollapsedModule
  , traitsByCategory
  , verifyCollapsedFunction
  , lowerFunction
  , lowerModule
  , compileModuleToWasm
  , compileSourceToWasm
  , mkClassDef
  , inferSourceTypes
  , parseModule
  , formatSource
  , renderType
  )
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Exit (exitFailure)

main :: IO ()
main = do
  ok <- and <$> sequence tests
  if ok
    then putStrLn "All tests passed."
    else exitFailure

tests :: [IO Bool]
tests =
  [ testParseModule
  , testParseModuleWithDirectives
  , testParseLambda
  , testParseStringLiteral
  , testParseCaseMultipleScrutinees
  , testParseCaseConstructorPattern
  , testParseCaseConstructorPatternMultiline
  , testParseCaseConstructorPatternExhaustiveSingleScrutinee
  , testParseCaseRequiresCatchAll
  , testParseLetExpression
  , testParseLetMultipleBindings
  , testParseLetFunctionBinding
  , testParseLetMultilineContinuation
  , testParseLetMultilineNoSemicolon
  , testParseFunctionGuards
  , testParseFunctionGuardsWithBuiltinOperatorConditions
  , testParseFunctionGuardsWithOperatorConditions
  , testParseDataDeclarationGeneratesFunctions
  , testParseDataDeclarationSupportsPascalCase
  , testParseDataDeclarationSupportsMultiConstructors
  , testParseDataDeclarationGadtStyle
  , testParseTypeSignatureWithOptionalWitness
  , testParseTypeSignatureRequiresFunction
  , testParseCollectionLiteral
  , testParseRejectsDoNotation
  , testParseRejectsAdoNotation
  , testParseClassInstanceDeclarationsRewriteFunctions
  , testParseClassDeclarationRequiresLaws
  , testParseHKTInstanceArityMismatchFails
  , testParseBuiltinInfixPrecedence
  , testParseBuiltinEqRejectsChain
  , testParseCustomInfixPrecedence
  , testParseCustomInfixRightAssociative
  , testParseCustomInfixOverrideBuiltin
  , testParseBacktickOperator
  , testParseBacktickFunctionOperatorWithoutDeclaration
  , testParseUnknownInfixOperatorFails
  , testParseNonAssociativeInfixRejectsChain
  , testParseRejectsCamelCaseIdentifiers
  , testParseErrorLine
  , testFormatPreservesClassLawInstanceDeclarations
  , testFormatPreservesMultilineCaseLayout
  , testFormatNormalizesMultilineCaseArmIndentation
  , testFormatExpandsInlineCase
  , testFormatExpandsInlineCaseWithContinuation
  , testFormatCollapsesInnerWhitespace
  , testFormatExpandsLongInlineLet
  , testFormatExpandsNestedInlineLet
  , testFormatRejectsInvalidSource
  , testLowerFunction
  , testLowerStringLiteral
  , testLowerClosureFunction
  , testLowerDynamicCall
  , testLowerFunctionRejectsDuplicateArgs
  , testCollapseDirectCall
  , testCollapseClosure
  , testCollapseDynamicCall
  , testAutoCurryPartialKnownCall
  , testAutoCurryOverApplication
  , testTailOptimizeSelfRecursion
  , testTailOptimizeDoesNotMarkNonTailRecursion
  , testCollapsedVerifierRejectsDeadTemp
  , testCollapsedVerifierRejectsUnsaturatedKnownDirectCall
  , testExampleSourceCurryingNormalization
  , testExampleSourceOverApplicationNormalization
  , testExampleSourceClosureCallsNormalizedToApply
  , testImmediateClosureApplyCollapsesToDirect
  , testImmediateCurryApplyCollapsesToDirect
  , testEscapeStructFlattenRemovesConstructorHelpers
  , testEscapeStructFlattenPreservesTagMismatchChecks
  , testSliceSetSharedTargetCopiesBeforeWrite
  , testSliceSetLinearTargetReusesBuffer
  , testCollapseInsertsFunctionRegionScopeForLocalAllocation
  , testCollapseSkipsFunctionRegionScopeForExplicitRegionOps
  , testCollapseSkipsFunctionRegionScopeForEscapingAllocation
  , testHotUncurryWrapperRemovesDirectWrapperCalls
  , testPruneDeadFunctionsFromRootsKeepsReachableOnly
  , testPruneKeepsReachableLiftedDescendant
  , testCollapseFromRootsPrunesUnusedHelpers
  , testTraitCatalogHasCoreTraits
  , testTraitCategoriesAvailable
  , testRewriteWithArithmeticSubset
  , testRewriteAddRightZero
  , testRewriteFunctorIdentity
  , testRewriteMonadLeftIdentity
  , testNormalizeArithmeticExpression
  , testClassDefRequiresLaws
  , testClassDefAcceptsCompleteLawSet
  , testInferSourceTypesForDataAndNoDo
  , testInferSourceTypesForOldStyleDataNoTypeParams
  , testInferSourceTypesStringLiteral
  , testInferSourceTypesSliceBuiltins
  , testInferSourceTypesLinearMemoryBuiltins
  , testInferSourceTypesCasePatternConstrainsScrutinee
  , testEvalSourceFunctionWithClosure
  , testEvalSourceFunctionWithCaseExpression
  , testEvalSourceFunctionWithCaseExpressionMultiline
  , testEvalSourceFunctionWithMaybeEitherMonads
  , testDifferentialDataAndNoDoSemantics
  , testDifferentialCaseExpressionSemantics
  , testEvalCollapsedFunctionWithCurrying
  , testDifferentialMaybeEitherMonadsSemantics
  , testDifferentialSourceCollapsedSemantics
  , testCompileWasmInlinesNumericBuiltins
  , testCompileWasmModule
  , testCompileWasmRejectsReservedRuntimeExportNames
  , testCompileWasmSupportsCaseExpressions
  , testCompileWasmSupportsCaseExpressionsMultiline
  , testCompileWasmSupportsCaseExpressionBranchThunkDirectCalls
  , testCompileWasmSupportsClosures
  , testCompileWasmSupportsClosuresWithManyCaptures
  , testCompileWasmSupportsMaybeEitherMonads
  , testCompileWasmSupportsCurrying
  , testCompileWasmSupportsDataAndNoDo
  , testCompileWasmSupportsStructWithManyFields
  , testCompileWasmSupportsCollectionLiterals
  , testCompileWasmSupportsStringLiterals
  , testCompileWasmSupportsUtf8StringLiterals
  , testCompileWasmSupportsSliceInteropImports
  , testCompileWasmSupportsSliceSetImport
  , testCompileWasmSupportsLinearMemoryHelpers
  , testCompileWasmStructHelpersInlined
  , testCompileWasmExportsHeapPtr
  , testCompileWasmSupportsHeapGlobalAtom
  , testCompileWasmRejectsHeapPtrAsUserFunction
  , testCompileWasmRejectsUnknownGlobalAtom
  , testCompileEntryModuleLoadsDottedImport
  , testCompileEntryModuleMissingImport
  , testCompileEntryModuleImportCycle
  , testCompileEntryModuleDefaultExports
  , testCompileEntryModuleExplicitExports
  , testCompileEntryModuleRejectsUnknownExport
  , testCompileEntryModuleArtifactExportsIncludeArity
  , testRenderTypeScriptBindingsUsesExportArityFromIr
  , testCollapsePipelineUsesDerivedRules
  ]

testParseModule :: IO Bool
testParseModule = do
  let src =
        unlines
          [ "id x = x"
          , "add2 x = add x 2"
          , ""
          , "-- comments are ignored"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function {name = "id", args = ["x"], body = Var "x"}
              , Function
                  { name = "add2"
                  , args = ["x"]
                  , body = App (App (Var "add") (Var "x")) (IntLit 2)
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse module succeeds" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse module succeeds" expected parsed

testParseModuleWithDirectives :: IO Bool
testParseModuleWithDirectives = do
  let src =
        unlines
          [ "module entry"
          , "import util.math"
          , "export main, helper"
          , "helper x = add x 1"
          , "main y = helper y"
          ]
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "helper"
                  , args = ["x"]
                  , body = App (App (Var "add") (Var "x")) (IntLit 1)
                  }
              , Function
                  { name = "main"
                  , args = ["y"]
                  , body = App (Var "helper") (Var "y")
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse module with directives" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse module with directives" expected parsed

testParseErrorLine :: IO Bool
testParseErrorLine = do
  let src =
        unlines
          [ "id x = x"
          , "broken = (x"
          ]
  case parseModule src of
    Left err ->
      assertTrue "parse errors report source line number" ("line 2" `isInfixOf` err)
    Right parsed ->
      failTest "parse errors report source line number" ("unexpected parse success: " <> show parsed)

testFormatPreservesClassLawInstanceDeclarations :: IO Bool
testFormatPreservesClassLawInstanceDeclarations = do
  let src =
        unlines
          [ "class monoid_rules : monoid   "
          , "law monoid_rules left_identity = append empty x => x "
          , "law monoid_rules right_identity = append x empty => x "
          , "law monoid_rules associativity = append (append x y) z => append x (append y z) "
          , "instance monoid_on_i64 : monoid_rules empty=empty append=append"
          , "main x = append empty x   "
          ]
      expected =
        unlines
          [ "class monoid_rules : monoid"
          , "law monoid_rules left_identity = append empty x => x"
          , "law monoid_rules right_identity = append x empty => x"
          , "law monoid_rules associativity = append (append x y) z => append x (append y z)"
          , "instance monoid_on_i64 : monoid_rules empty=empty append=append"
          , "main x = append empty x"
          ]
  case formatSource src of
    Left err ->
      failTest "format preserves class/law/instance declarations" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format preserves class/law/instance declarations" expected out

testFormatPreservesMultilineCaseLayout :: IO Bool
testFormatPreservesMultilineCaseLayout = do
  let src =
        unlines
          [ "data Pair a b = Pair a b"
          , ""
          , "first_or_zero p = case p of   "
          , "  Pair left right -> left"
          , "  _ -> 0 "
          ]
      expected =
        unlines
          [ "data Pair a b = Pair a b"
          , ""
          , "first_or_zero p = case p of"
          , "  Pair left right -> left"
          , "  _ -> 0"
          ]
  case formatSource src of
    Left err ->
      failTest "format preserves multiline case layout" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format preserves multiline case layout" expected out

testFormatNormalizesMultilineCaseArmIndentation :: IO Bool
testFormatNormalizesMultilineCaseArmIndentation = do
  let src =
        unlines
          [ "is_alive_next alive neighbors = case (neighbors == 3) (alive && neighbors == 2) of"
          , "  1 _ -> 1"
          , "       _ 1 -> 1"
          , "  _ _ -> 0"
          ]
      expected =
        unlines
          [ "is_alive_next alive neighbors = case (neighbors == 3) (alive && neighbors == 2) of"
          , "  1 _ -> 1"
          , "  _ 1 -> 1"
          , "  _ _ -> 0"
          ]
  case formatSource src of
    Left err ->
      failTest "format normalizes multiline case arm indentation" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format normalizes multiline case arm indentation" expected out

testFormatExpandsInlineCase :: IO Bool
testFormatExpandsInlineCase = do
  let src =
        "add_or_sum a b = case a b of 0 0 -> 0; x y -> add x y\n"
      expected =
        unlines
          [ "add_or_sum a b = case a b of"
          , "  0 0 -> 0"
          , "  x y -> add x y"
          ]
  case formatSource src of
    Left err ->
      failTest "format expands inline case" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format expands inline case" expected out

testFormatExpandsInlineCaseWithContinuation :: IO Bool
testFormatExpandsInlineCaseWithContinuation = do
  let src =
        unlines
          [ "step_range in_cells out_cells w h start span = case (eq span 0) (eq span 1) of 1 _ -> out_cells; _ 1 -> slice_set_u8 out_cells start (life_next_from_slice in_cells w h start); _ _ -> let half = div span 2;"
          , "  left_out = step_range in_cells out_cells w h start half;"
          , "  right_start = add start half;"
          , "  right_span = sub span half"
          , "  in step_range in_cells left_out w h right_start right_span"
          ]
      expected =
        unlines
          [ "step_range in_cells out_cells w h start span = case (eq span 0) (eq span 1) of"
          , "  1 _ -> out_cells"
          , "  _ 1 -> slice_set_u8 out_cells start (life_next_from_slice in_cells w h start)"
          , "  _ _ ->"
          , "    let half = div span 2"
          , "        left_out = step_range in_cells out_cells w h start half"
          , "        right_start = add start half"
          , "        right_span = sub span half"
          , "    in step_range in_cells left_out w h right_start right_span"
          ]
  case formatSource src of
    Left err ->
      failTest "format expands inline case with continuation" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format expands inline case with continuation" expected out

testFormatCollapsesInnerWhitespace :: IO Bool
testFormatCollapsesInnerWhitespace = do
  let src =
        unlines
          [ "add_or_sum a b = case a b of"
          , "  0 0 -> 0"
          , "  x y ->   add x y"
          ]
      expected =
        unlines
          [ "add_or_sum a b = case a b of"
          , "  0 0 -> 0"
          , "  x y -> add x y"
          ]
  case formatSource src of
    Left err ->
      failTest "format collapses inner whitespace" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format collapses inner whitespace" expected out

testFormatExpandsLongInlineLet :: IO Bool
testFormatExpandsLongInlineLet = do
  let src =
        "step_range in_cells out_cells w h start span = let half = div span 2; left_out = step_range in_cells out_cells w h start half; right_start = add start half; right_span = sub span half in step_range in_cells left_out w h right_start right_span\n"
      expected =
        unlines
          [ "step_range in_cells out_cells w h start span ="
          , "  let half = div span 2"
          , "      left_out = step_range in_cells out_cells w h start half"
          , "      right_start = add start half"
          , "      right_span = sub span half"
          , "  in step_range in_cells left_out w h right_start right_span"
          ]
  case formatSource src of
    Left err ->
      failTest "format expands long inline let" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format expands long inline let" expected out

testFormatExpandsNestedInlineLet :: IO Bool
testFormatExpandsNestedInlineLet = do
  let src =
        unlines
          [ "data LifeState = LifeState current next generation"
          , "step_state w h state = let LifeState current next generation = state in let filled = step_board current next w h; next_generation = add generation 1 in LifeState filled current next_generation"
          ]
      expected =
        unlines
          [ "data LifeState = LifeState current next generation"
          , "step_state w h state ="
          , "  let LifeState current next generation = state"
          , "  in let filled = step_board current next w h"
          , "         next_generation = add generation 1"
          , "     in LifeState filled current next_generation"
          ]
  case formatSource src of
    Left err ->
      failTest "format expands nested inline let" ("unexpected format error: " <> err)
    Right out ->
      assertEqual "format expands nested inline let" expected out

testFormatRejectsInvalidSource :: IO Bool
testFormatRejectsInvalidSource = do
  let src = "main = (x"
  case formatSource src of
    Left _ -> passTest "format rejects invalid source"
    Right out ->
      failTest "format rejects invalid source" ("unexpected format success: " <> show out)

testParseLambda :: IO Bool
testParseLambda = do
  let src = "make_adder x = \\y -> add x y"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "make_adder"
                  , args = ["x"]
                  , body =
                      Lam
                        "y"
                        (App (App (Var "add") (Var "x")) (Var "y"))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse lambda expressions" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse lambda expressions" expected parsed

testParseStringLiteral :: IO Bool
testParseStringLiteral = do
  let src = "main = \"hello\\nworld\""
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = []
                  , body = StringLit "hello\nworld"
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse string literals" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse string literals" expected parsed

testParseCaseMultipleScrutinees :: IO Bool
testParseCaseMultipleScrutinees = do
  let src = "main a b = case a b of 0 0 -> 0; x y -> add x y"
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "main"
                  , args = ["a", "b"]
                  , body =
                      Case
                        [Var "a", Var "b"]
                        [ CaseArm
                            { armPatterns = [PatInt 0, PatInt 0]
                            , armBody = IntLit 0
                            }
                        , CaseArm
                            { armPatterns = [PatVar "x", PatVar "y"]
                            , armBody = App (App (Var "add") (Var "x")) (Var "y")
                            }
                        ]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse case expression with multiple scrutinees" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse case expression with multiple scrutinees" expected parsed

testParseCaseConstructorPattern :: IO Bool
testParseCaseConstructorPattern = do
  let src =
        unlines
          [ "data Pair a b = Pair a b"
          , "main x = case (Pair x 9) of Pair left right -> left; _ -> 0"
          ]
  case parseModule src of
    Left err ->
      failTest "parse case constructor pattern" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "main" (functions parsed) of
        Just Function {body = Case [App (App (Var "Pair") (Var "x")) (IntLit 9)] arms} ->
          case arms of
            [ CaseArm {armPatterns = [PatConstructor ctorName _ ["left", "right"]], armBody = Var "left"}
              , CaseArm {armPatterns = [PatWildcard], armBody = IntLit 0}
              ] ->
                  assertEqual "parse case constructor pattern" "Pair" ctorName
            _ ->
              failTest "parse case constructor pattern" ("unexpected case arms: " <> show arms)
        Just fn ->
          failTest "parse case constructor pattern" ("unexpected main function body: " <> show (body fn))
        Nothing ->
          failTest "parse case constructor pattern" "missing function: main"

testParseCaseConstructorPatternMultiline :: IO Bool
testParseCaseConstructorPatternMultiline = do
  let src =
        unlines
          [ "data Pair a b = Pair a b"
          , "main x = case (Pair x 9) of"
          , "  Pair left right -> left"
          , "  _ -> 0"
          ]
  case parseModule src of
    Left err ->
      failTest "parse case constructor pattern multiline" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "main" (functions parsed) of
        Just Function {body = Case [App (App (Var "Pair") (Var "x")) (IntLit 9)] arms} ->
          case arms of
            [ CaseArm {armPatterns = [PatConstructor ctorName _ ["left", "right"]], armBody = Var "left"}
              , CaseArm {armPatterns = [PatWildcard], armBody = IntLit 0}
              ] ->
                  assertEqual "parse case constructor pattern multiline" "Pair" ctorName
            _ ->
              failTest "parse case constructor pattern multiline" ("unexpected case arms: " <> show arms)
        Just fn ->
          failTest "parse case constructor pattern multiline" ("unexpected main function body: " <> show (body fn))
        Nothing ->
          failTest "parse case constructor pattern multiline" "missing function: main"

testParseCaseRequiresCatchAll :: IO Bool
testParseCaseRequiresCatchAll = do
  let src = "main x = case x of 0 -> 0"
  case parseModule src of
    Left err ->
      assertTrue "parse case requires catch-all final arm" ("final case arm" `isInfixOf` err)
    Right parsed ->
      failTest "parse case requires catch-all final arm" ("unexpected parse success: " <> show parsed)

testParseCaseConstructorPatternExhaustiveSingleScrutinee :: IO Bool
testParseCaseConstructorPatternExhaustiveSingleScrutinee = do
  let src =
        unlines
          [ "data Maybe a = Nothing : Maybe a | Just : a -> Maybe a"
          , "main m = case m of Nothing -> 0; Just n -> add n 1"
          ]
  case parseModule src of
    Left err ->
      failTest
        "parse constructor-pattern exhaustive single-scrutinee case without final catch-all"
        ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "main" (functions parsed) of
        Just Function {body = Case [_] arms} ->
          case arms of
            [ CaseArm {armPatterns = [PatConstructor "Nothing" _ []], armBody = IntLit 0}
              , CaseArm {armPatterns = [PatConstructor "Just" _ ["n"]], armBody = App (App (Var "add") (Var "n")) (IntLit 1)}
              ] ->
                passTest "parse constructor-pattern exhaustive single-scrutinee case without final catch-all"
            _ ->
              failTest
                "parse constructor-pattern exhaustive single-scrutinee case without final catch-all"
                ("unexpected case arms: " <> show arms)
        Just fn ->
          failTest
            "parse constructor-pattern exhaustive single-scrutinee case without final catch-all"
            ("unexpected main function body: " <> show (body fn))
        Nothing ->
          failTest
            "parse constructor-pattern exhaustive single-scrutinee case without final catch-all"
            "missing function: main"

testParseLetExpression :: IO Bool
testParseLetExpression = do
  let src = "main x = let y = add x 1 in y"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body =
                      App
                        ( Lam
                            "y"
                            (Var "y")
                        )
                        (App (App (Var "add") (Var "x")) (IntLit 1))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse let expressions" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse let expressions" expected parsed

testParseLetMultipleBindings :: IO Bool
testParseLetMultipleBindings = do
  let src = "main x = let y = add x 1; z = mul y 2 in z"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body =
                      App
                        ( Lam
                            "y"
                            ( App
                                (Lam "z" (Var "z"))
                                (App (App (Var "mul") (Var "y")) (IntLit 2))
                            )
                        )
                        (App (App (Var "add") (Var "x")) (IntLit 1))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse let expressions with multiple bindings" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse let expressions with multiple bindings" expected parsed

testParseLetFunctionBinding :: IO Bool
testParseLetFunctionBinding = do
  let src = "main x = let inc y = add y 1 in inc x"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body =
                      App
                        (Lam "inc" (App (Var "inc") (Var "x")))
                        (Lam "y" (App (App (Var "add") (Var "y")) (IntLit 1)))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse let local function bindings" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse let local function bindings" expected parsed

testParseLetMultilineContinuation :: IO Bool
testParseLetMultilineContinuation = do
  let src =
        unlines
          [ "main x = let y = add x 1;"
          , "  z = mul y 2"
          , "  in z"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body =
                      App
                        ( Lam
                            "y"
                            ( App
                                (Lam "z" (Var "z"))
                                (App (App (Var "mul") (Var "y")) (IntLit 2))
                            )
                        )
                        (App (App (Var "add") (Var "x")) (IntLit 1))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse let multiline continuation" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse let multiline continuation" expected parsed

testParseLetMultilineNoSemicolon :: IO Bool
testParseLetMultilineNoSemicolon = do
  let src =
        unlines
          [ "main x = let y = add x 1"
          , "  z = mul y 2"
          , "  in z"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body =
                      App
                        ( Lam
                            "y"
                            ( App
                                (Lam "z" (Var "z"))
                                (App (App (Var "mul") (Var "y")) (IntLit 2))
                            )
                        )
                        (App (App (Var "add") (Var "x")) (IntLit 1))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse let multiline continuation without semicolons" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse let multiline continuation without semicolons" expected parsed

testParseFunctionGuards :: IO Bool
testParseFunctionGuards = do
  let src =
        unlines
          [ "add_or_zero x y | eq x 0 = 0"
          , "  | otherwise = add x y"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "add_or_zero"
                  , args = ["x", "y"]
                  , body =
                      Case
                        [App (App (Var "eq") (Var "x")) (IntLit 0)]
                        [ CaseArm
                            { armPatterns = [PatInt 1]
                            , armBody = IntLit 0
                            }
                        , CaseArm
                            { armPatterns = [PatWildcard]
                            , armBody = App (App (Var "add") (Var "x")) (Var "y")
                            }
                        ]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse guarded function declarations" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse guarded function declarations" expected parsed

testParseFunctionGuardsWithBuiltinOperatorConditions :: IO Bool
testParseFunctionGuardsWithBuiltinOperatorConditions = do
  let src =
        unlines
          [ "add x y | x == 0 && y == 0 = 0"
          , "        | otherwise = x + y"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "add"
                  , args = ["x", "y"]
                  , body =
                      Case
                        [ App
                            (App (Var "and") (App (App (Var "eq") (Var "x")) (IntLit 0)))
                            (App (App (Var "eq") (Var "y")) (IntLit 0))
                        ]
                        [ CaseArm
                            { armPatterns = [PatInt 1]
                            , armBody = IntLit 0
                            }
                        , CaseArm
                            { armPatterns = [PatWildcard]
                            , armBody = App (App (Var "add") (Var "x")) (Var "y")
                            }
                        ]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse guarded function declarations with builtin operators" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse guarded function declarations with builtin operators" expected parsed

testParseFunctionGuardsWithOperatorConditions :: IO Bool
testParseFunctionGuardsWithOperatorConditions = do
  let src =
        unlines
          [ "infixl 6 + = add"
          , "infixl 3 && = and"
          , "infix 4 == = eq"
          , "add x y | x == 0 && y == 0 = 0"
          , "        | otherwise = x + y"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "add"
                  , args = ["x", "y"]
                  , body =
                      Case
                        [ App
                            (App (Var "and") (App (App (Var "eq") (Var "x")) (IntLit 0)))
                            (App (App (Var "eq") (Var "y")) (IntLit 0))
                        ]
                        [ CaseArm
                            { armPatterns = [PatInt 1]
                            , armBody = IntLit 0
                            }
                        , CaseArm
                            { armPatterns = [PatWildcard]
                            , armBody = App (App (Var "add") (Var "x")) (Var "y")
                            }
                        ]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse guarded function declarations with operators" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse guarded function declarations with operators" expected parsed

testParseDataDeclarationGeneratesFunctions :: IO Bool
testParseDataDeclarationGeneratesFunctions = do
  let src = "data Pair a b = Pair a b"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "Pair"
                  , args = ["a", "b"]
                  , body =
                      App
                        (App (Var "__mk_Pair#Pair_2_tpar_2_fmap_0_1") (Var "a"))
                        (Var "b")
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse data declaration generates functions" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse data declaration generates functions" expected parsed

testParseDataDeclarationSupportsPascalCase :: IO Bool
testParseDataDeclarationSupportsPascalCase = do
  let src = "data HttpRequest h p v = HttpRequest h p v"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "HttpRequest"
                  , args = ["h", "p", "v"]
                  , body =
                      App
                        ( App
                            (App (Var "__mk_HttpRequest#HttpRequest_3_tpar_3_fmap_0_1_2") (Var "h"))
                            (Var "p")
                        )
                        (Var "v")
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse data declaration supports PascalCase names" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse data declaration supports PascalCase names" expected parsed

testParseDataDeclarationSupportsMultiConstructors :: IO Bool
testParseDataDeclarationSupportsMultiConstructors = do
  let src =
        unlines
          [ "data Maybe a = Just : a -> Maybe a | Nothing : Maybe a"
          , "data Either e a = Left : e -> Either e a | Right : a -> Either e a"
          , "main x = case (Right x) of Left _ -> 0; Right y -> add y 1; _ -> 0"
          , "other = Left 0"
          ]
  case parseModule src of
    Left err ->
      failTest "parse multi-constructor data declarations and constructor-specific case matching" ("unexpected parse error: " <> err)
    Right parsed ->
      case
        ( findFunctionByName "Just" (functions parsed)
        , findFunctionByName "Nothing" (functions parsed)
        , findFunctionByName "Left" (functions parsed)
        , findFunctionByName "Right" (functions parsed)
        , findFunctionByName "main" (functions parsed)
        ) of
        ( Just Function {args = justArgs}
          , Just Function {args = nothingArgs}
          , Just Function {args = leftArgs}
          , Just Function {args = rightArgs}
          , Just Function {body = Case [_] arms}
          ) ->
            let argsAreValid =
                  length justArgs == 1
                    && null nothingArgs
                    && length leftArgs == 1
                    && length rightArgs == 1
             in assertTrue "parse multi-constructor data declarations and constructor-specific case matching" $
                  argsAreValid
                    && case arms of
                      [ CaseArm {armPatterns = [PatConstructor ctorLeft _ ["_"]], armBody = IntLit 0}
                        , CaseArm
                            {armPatterns = [PatConstructor ctorRight _ ["y"]], armBody = App (App (Var "add") (Var "y")) (IntLit 1)}
                        , CaseArm {armPatterns = [PatWildcard], armBody = IntLit 0}
                        ] ->
                          ctorLeft == "Left" && ctorRight == "Right"
                      _ -> False
        _ ->
          failTest
            "parse multi-constructor data declarations and constructor-specific case matching"
            "missing expected constructor functions or case expression"

testParseDataDeclarationGadtStyle :: IO Bool
testParseDataDeclarationGadtStyle = do
  let src =
        unlines
          [ "data Pair a b = MkPair : a -> b -> Pair a b"
          , "main x y = MkPair x y"
          ]
  case parseModule src of
    Left err ->
      failTest "parse gadt-style data declaration" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "MkPair" (functions parsed) of
        Just Function {args = pairArgs} ->
          assertTrue "parse gadt-style data declaration" (length pairArgs == 2)
        Nothing ->
          failTest "parse gadt-style data declaration" "missing constructor function for MkPair"

testParseTypeSignatureWithOptionalWitness :: IO Bool
testParseTypeSignatureWithOptionalWitness = do
  let src =
        unlines
          [ "combine : (m : monoid_rules t) => t -> t -> t"
          , "combine x y = append x y"
          ]
      expected =
        Module
          { signatures =
              [ TypeSignature
                  { sigName = "combine"
                  , sigConstraints =
                      [ SignatureConstraint
                          { constraintWitness = Just "m"
                          , constraintClass = "monoid_rules"
                          , constraintTypeArgs = ["t"]
                          }
                      ]
                  , sigTypeExpr = "t -> t -> t"
                  }
              ]
          , functions =
              [ Function
                  { name = "combine"
                  , args = ["x", "y"]
                  , body = App (App (Var "append") (Var "x")) (Var "y")
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse type signature with optional witness" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse type signature with optional witness" expected parsed

testParseTypeSignatureRequiresFunction :: IO Bool
testParseTypeSignatureRequiresFunction = do
  let src = "id : a -> a"
  case parseModule src of
    Left err ->
      assertTrue "parse type signature must target existing function" ("unknown function" `isInfixOf` err)
    Right parsed ->
      failTest "parse type signature must target existing function" ("unexpected parse success: " <> show parsed)

testParseCollectionLiteral :: IO Bool
testParseCollectionLiteral = do
  let src = "main = [1, 2, 3]"
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "main"
                  , args = []
                  , body = CollectionLit [IntLit 1, IntLit 2, IntLit 3]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse collection literal" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse collection literal" expected parsed

testParseRejectsDoNotation :: IO Bool
testParseRejectsDoNotation = do
  let src = "main x = do { y <- add x 1; y }"
  case parseModule src of
    Left err ->
      assertTrue "parse rejects do notation" ("line 1" `isInfixOf` err)
    Right parsed ->
      failTest "parse rejects do notation" ("unexpected parse success: " <> show parsed)

testParseRejectsAdoNotation :: IO Bool
testParseRejectsAdoNotation = do
  let src = "main x = ado { y <- add x 1; z <- add x 2; add y z }"
  case parseModule src of
    Left err ->
      assertTrue "parse rejects ado notation" ("line 1" `isInfixOf` err)
    Right parsed ->
      failTest "parse rejects ado notation" ("unexpected parse success: " <> show parsed)

testParseClassInstanceDeclarationsRewriteFunctions :: IO Bool
testParseClassInstanceDeclarationsRewriteFunctions = do
  let src =
        unlines
          [ "class plus_rules i : add"
          , "law plus_rules left_identity = add 0 x => x"
          , "law plus_rules right_identity = add x 0 => x"
          , "law plus_rules associativity = add (add x y) z => add x (add y z)"
          , "instance plus_on_int : plus_rules i add=plus"
          , "plus a b = add a b"
          , "main x = plus x 0"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "plus"
                  , args = ["a", "b"]
                  , body = App (App (Var "add") (Var "a")) (Var "b")
                  }
              , Function
                  { name = "main"
                  , args = ["x"]
                  , body = Var "x"
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse class/law/instance declarations rewrite functions" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse class/law/instance declarations rewrite functions" expected parsed

testParseClassDeclarationRequiresLaws :: IO Bool
testParseClassDeclarationRequiresLaws = do
  let src =
        unlines
          [ "class bad_add i : add"
          , "law bad_add right_identity = add x 0 => x"
          , "instance bad_add_int : bad_add i add=add"
          , "main x = add x 0"
          ]
  case parseModule src of
    Left err ->
      assertTrue
        "parse class declarations require complete law set"
        ("missing required laws" `isInfixOf` err)
    Right parsed ->
      failTest "parse class declarations require complete law set" ("unexpected parse success: " <> show parsed)

testParseHKTInstanceArityMismatchFails :: IO Bool
testParseHKTInstanceArityMismatchFails = do
  let src =
        unlines
          [ "class monad_rules m : monad"
          , "law monad_rules left_identity = bind (pure x) f => f x"
          , "law monad_rules right_identity = bind m pure => m"
          , "law monad_rules associativity = bind (bind m f) g => bind m (\\x -> bind (f x) g)"
          , "data Maybe a = Nothing : Maybe a | Just : a -> Maybe a"
          , "instance bad_maybe_instance : monad_rules m a pure=maybe_pure bind=maybe_bind"
          , "maybe_just x = Just x"
          , "maybe_pure x = maybe_just x"
          , "maybe_bind m f = case m of Nothing -> m; Just x -> f x; _ -> m"
          ]
  case parseModule src of
    Left err ->
      assertTrue "parse HKT class instance arity mismatch is rejected" ("arity" `isInfixOf` err || "mismatch" `isInfixOf` err)
    Right parsed ->
      failTest "parse HKT class instance arity mismatch is rejected" ("unexpected parse success: " <> show parsed)

testParseBuiltinInfixPrecedence :: IO Bool
testParseBuiltinInfixPrecedence = do
  let src = "main x = x + 1 * 2"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body =
                      App
                        (App (Var "add") (Var "x"))
                        (App (App (Var "mul") (IntLit 1)) (IntLit 2))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse builtin infix precedence" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse builtin infix precedence" expected parsed

testParseBuiltinEqRejectsChain :: IO Bool
testParseBuiltinEqRejectsChain = do
  let src = "main x y z = x == y == z"
  case parseModule src of
    Left err ->
      assertTrue
        "parse builtin non-associative eq rejects chains"
        ("non-associative operator cannot be chained" `isInfixOf` err)
    Right parsed ->
      failTest "parse builtin non-associative eq rejects chains" ("unexpected parse success: " <> show parsed)

testParseCustomInfixPrecedence :: IO Bool
testParseCustomInfixPrecedence = do
  let src =
        unlines
          [ "infixl 6 +. = add"
          , "infixl 7 *. = mul"
          , "main x = x +. 1 *. 2"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body =
                      App
                        (App (Var "add") (Var "x"))
                        (App (App (Var "mul") (IntLit 1)) (IntLit 2))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse custom infix precedence" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse custom infix precedence" expected parsed

testParseCustomInfixRightAssociative :: IO Bool
testParseCustomInfixRightAssociative = do
  let src =
        unlines
          [ "infixr 5 <> = append"
          , "main a b c = a <> b <> c"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["a", "b", "c"]
                  , body =
                      App
                        (App (Var "append") (Var "a"))
                        (App (App (Var "append") (Var "b")) (Var "c"))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse custom right-associative infix" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse custom right-associative infix" expected parsed

testParseCustomInfixOverrideBuiltin :: IO Bool
testParseCustomInfixOverrideBuiltin = do
  let src =
        unlines
          [ "infixr 6 - = sub"
          , "main a b c = a - b - c"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["a", "b", "c"]
                  , body =
                      App
                        (App (Var "sub") (Var "a"))
                        (App (App (Var "sub") (Var "b")) (Var "c"))
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse custom infix declaration overrides builtin token" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse custom infix declaration overrides builtin token" expected parsed

testParseBacktickOperator :: IO Bool
testParseBacktickOperator = do
  let src =
        unlines
          [ "infixl 6 plus_op = add"
          , "main x = x `plus_op` 1"
          ]
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body = App (App (Var "add") (Var "x")) (IntLit 1)
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse backtick operator" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse backtick operator" expected parsed

testParseBacktickFunctionOperatorWithoutDeclaration :: IO Bool
testParseBacktickFunctionOperatorWithoutDeclaration = do
  let src = "main a b c = a `mod` b + c"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["a", "b", "c"]
                  , body =
                      App
                        (App (Var "add") (App (App (Var "mod") (Var "a")) (Var "b")))
                        (Var "c")
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse backtick function operator without declaration" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse backtick function operator without declaration" expected parsed

testParseUnknownInfixOperatorFails :: IO Bool
testParseUnknownInfixOperatorFails = do
  let src = "main x = x +. 1"
  case parseModule src of
    Left err ->
      assertTrue "parse unknown infix operator fails" ("unknown infix operator" `isInfixOf` err)
    Right parsed ->
      failTest "parse unknown infix operator fails" ("unexpected parse success: " <> show parsed)

testParseNonAssociativeInfixRejectsChain :: IO Bool
testParseNonAssociativeInfixRejectsChain = do
  let src =
        unlines
          [ "infix 4 ==. = eq"
          , "main x y z = x ==. y ==. z"
          ]
  case parseModule src of
    Left err ->
      assertTrue
        "parse non-associative infix rejects chains"
        ("non-associative operator cannot be chained" `isInfixOf` err)
    Right parsed ->
      failTest "parse non-associative infix rejects chains" ("unexpected parse success: " <> show parsed)

testParseRejectsCamelCaseIdentifiers :: IO Bool
testParseRejectsCamelCaseIdentifiers = do
  let src = "makeAdder x = x"
  case parseModule src of
    Left err ->
      assertTrue "parse rejects camelCase identifiers" ("line 1" `isInfixOf` err)
    Right parsed ->
      failTest "parse rejects camelCase identifiers" ("unexpected parse success: " <> show parsed)

testLowerFunction :: IO Bool
testLowerFunction = do
  let fn =
        Function
          { name = "add2"
          , args = ["x"]
          , body = App (App (Var "add") (Var "x")) (IntLit 2)
          }
      expected =
        FlatFunction
          { name = "add2"
          , arity = 1
          , captureArity = 0
          , ops = [LocalGet 0, PushI32 2, Call "add" 2]
          , lifted = []
          }
  case lowerFunction fn of
    Left err ->
      failTest "lower function to stack ops" ("unexpected lowering error: " <> err)
    Right lowered ->
      assertEqual "lower function to stack ops" expected lowered

testLowerStringLiteral :: IO Bool
testLowerStringLiteral = do
  let fn =
        Function
          { name = "greeting"
          , args = []
          , body = StringLit "hello"
          }
      expected =
        FlatFunction
          { name = "greeting"
          , arity = 0
          , captureArity = 0
          , ops = [PushString "hello"]
          , lifted = []
          }
  case lowerFunction fn of
    Left err ->
      failTest "lower string literal to stack ops" ("unexpected lowering error: " <> err)
    Right lowered ->
      assertEqual "lower string literal to stack ops" expected lowered

testLowerFunctionRejectsDuplicateArgs :: IO Bool
testLowerFunctionRejectsDuplicateArgs = do
  let fn =
        Function
          { name = "dup"
          , args = ["x", "x"]
          , body = Var "x"
          }
  case lowerFunction fn of
    Left err ->
      assertTrue "lowering rejects duplicate argument names" ("duplicate argument" `isInfixOf` err)
    Right lowered ->
      failTest "lowering rejects duplicate argument names" ("unexpected lowering success: " <> show lowered)

testLowerClosureFunction :: IO Bool
testLowerClosureFunction = do
  let fn =
        Function
          { name = "make_adder"
          , args = ["x"]
          , body = Lam "y" (App (App (Var "add") (Var "x")) (Var "y"))
          }
      expected =
        FlatFunction
          { name = "make_adder"
          , arity = 1
          , captureArity = 0
          , ops = [MakeClosure "make_adder$lam0" [0]]
          , lifted =
              [ FlatFunction
                  { name = "make_adder$lam0"
                  , arity = 1
                  , captureArity = 1
                  , ops = [LocalGet 0, LocalGet 1, Call "add" 2]
                  , lifted = []
                  }
              ]
          }
  case lowerFunction fn of
    Left err ->
      failTest "lower closure function" ("unexpected lowering error: " <> err)
    Right lowered ->
      assertEqual "lower closure function" expected lowered

testLowerDynamicCall :: IO Bool
testLowerDynamicCall = do
  let fn =
        Function
          { name = "apply"
          , args = ["f", "x"]
          , body = App (Var "f") (Var "x")
          }
      expected =
        FlatFunction
          { name = "apply"
          , arity = 2
          , captureArity = 0
          , ops = [LocalGet 0, LocalGet 1, CallClosure 1]
          , lifted = []
          }
  case lowerFunction fn of
    Left err ->
      failTest "lower dynamic closure calls" ("unexpected lowering error: " <> err)
    Right lowered ->
      assertEqual "lower dynamic closure calls" expected lowered

testCollapseDirectCall :: IO Bool
testCollapseDirectCall = do
  let fn =
        Function
          { name = "add2"
          , args = ["x"]
          , body = App (App (Var "add") (Var "x")) (IntLit 2)
          }
      expected =
        CollapsedFunction
          { name = "add2"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCallDirect "add" [ALocal 0, AConstI32 2]
                  }
              ]
          , result = ATemp 0
          , lifted = []
          }
  case lowerFunction fn >>= collapseAndVerifyFunction of
    Left err ->
      failTest "collapse direct call into normalized IR" ("unexpected collapse error: " <> err)
    Right collapsed ->
      assertEqual "collapse direct call into normalized IR" expected collapsed

testCollapseClosure :: IO Bool
testCollapseClosure = do
  let fn =
        Function
          { name = "make_adder"
          , args = ["x"]
          , body = Lam "y" (App (App (Var "add") (Var "x")) (Var "y"))
          }
      expected =
        CollapsedFunction
          { name = "make_adder"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VClosure "make_adder$lam0" [ALocal 0]
                  }
              ]
          , result = ATemp 0
          , lifted =
              [ CollapsedFunction
                  { name = "make_adder$lam0"
                  , arity = 1
                  , captureArity = 1
                  , binds =
                      [ Bind
                          { temp = 0
                          , value = VCallDirect "add" [ALocal 0, ALocal 1]
                          }
                      ]
                  , result = ATemp 0
                  , lifted = []
                  }
              ]
          }
  case lowerFunction fn >>= collapseAndVerifyFunction of
    Left err ->
      failTest "collapse closure into normalized IR" ("unexpected collapse error: " <> err)
    Right collapsed ->
      assertEqual "collapse closure into normalized IR" expected collapsed

testCollapseDynamicCall :: IO Bool
testCollapseDynamicCall = do
  let fn =
        Function
          { name = "apply"
          , args = ["f", "x"]
          , body = App (Var "f") (Var "x")
          }
      expected =
        CollapsedFunction
          { name = "apply"
          , arity = 2
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VApply (ALocal 0) (ALocal 1)
                  }
              ]
          , result = ATemp 0
          , lifted = []
          }
  case lowerFunction fn >>= collapseAndVerifyFunction of
    Left err ->
      failTest "collapse dynamic call into normalized IR" ("unexpected collapse error: " <> err)
    Right collapsed ->
      assertEqual "collapse dynamic call into normalized IR" expected collapsed

testAutoCurryPartialKnownCall :: IO Bool
testAutoCurryPartialKnownCall = do
  let m =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "add"
                  , args = ["a", "b"]
                  , body = Var "a"
                  }
              , Function
                  { name = "inc"
                  , args = ["x"]
                  , body = App (Var "add") (Var "x")
                  }
              ]
          }
      expectedInc =
        CollapsedFunction
          { name = "inc"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCurryDirect "add" [ALocal 0]
                  }
              ]
          , result = ATemp 0
          , lifted = []
          }
  case lowerModule m >>= collapseAndVerifyModule of
    Left err ->
      failTest "auto curry partial known call" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "inc" collapsed of
        Nothing ->
          failTest "auto curry partial known call" "missing collapsed function: inc"
        Just gotInc ->
          assertEqual "auto curry partial known call" expectedInc gotInc

testAutoCurryOverApplication :: IO Bool
testAutoCurryOverApplication = do
  let m =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "add"
                  , args = ["a", "b"]
                  , body = Var "a"
                  }
              , Function
                  { name = "boom"
                  , args = ["x"]
                  , body =
                      App
                        (App (App (Var "add") (Var "x")) (IntLit 1))
                        (IntLit 2)
                  }
              ]
          }
      expectedBoom =
        CollapsedFunction
          { name = "boom"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCallDirect "add" [ALocal 0, AConstI32 1]
                  }
              , Bind
                  { temp = 1
                  , value = VApply (ATemp 0) (AConstI32 2)
                  }
              ]
          , result = ATemp 1
          , lifted = []
          }
  case lowerModule m >>= collapseAndVerifyModule of
    Left err ->
      failTest "auto curry over-application" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "boom" collapsed of
        Nothing ->
          failTest "auto curry over-application" "missing collapsed function: boom"
        Just gotBoom ->
          assertEqual "auto curry over-application" expectedBoom gotBoom

testTailOptimizeSelfRecursion :: IO Bool
testTailOptimizeSelfRecursion = do
  let src = "loop n = loop n"
      expectedLoop =
        CollapsedFunction
          { name = "loop"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VSelfTailCall [ALocal 0]
                  }
              ]
          , result = ATemp 0
          , lifted = []
          }
  case compileSourceToCollapsed src of
    Left err ->
      failTest "tail optimize self recursion" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "loop" collapsed of
        Nothing ->
          failTest "tail optimize self recursion" "expected function loop in collapsed output"
        Just gotLoop ->
          assertEqual "tail optimize self recursion" expectedLoop gotLoop

testTailOptimizeDoesNotMarkNonTailRecursion :: IO Bool
testTailOptimizeDoesNotMarkNonTailRecursion = do
  let src =
        unlines
          [ "non_tail n = add 1 (non_tail n)"
          , "add a b = a"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "tail optimize does not mark non-tail recursion" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "non_tail" collapsed of
        Nothing ->
          failTest "tail optimize does not mark non-tail recursion" "expected function non_tail in collapsed output"
        Just fn ->
          assertTrue "tail optimize does not mark non-tail recursion" (not (functionHasSelfTail fn))

testCollapsedVerifierRejectsDeadTemp :: IO Bool
testCollapsedVerifierRejectsDeadTemp = do
  let dead =
        CollapsedFunction
          { name = "bad"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCallDirect "id" [ALocal 0]
                  }
              ]
          , result = ALocal 0
          , lifted = []
          }
  case verifyCollapsedFunction dead of
    Left err ->
      assertTrue "collapsed verifier rejects dead temps" ("dead temp" `isInfixOf` err)
    Right _ ->
      failTest "collapsed verifier rejects dead temps" "unexpected verifier success"

testCollapsedVerifierRejectsUnsaturatedKnownDirectCall :: IO Bool
testCollapsedVerifierRejectsUnsaturatedKnownDirectCall = do
  let addFn =
        CollapsedFunction
          { name = "add"
          , arity = 2
          , captureArity = 0
          , binds = []
          , result = ALocal 0
          , lifted = []
          }
      badFn =
        CollapsedFunction
          { name = "bad"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCallDirect "add" [ALocal 0]
                  }
              ]
          , result = ATemp 0
          , lifted = []
          }
  case verifyCollapsedModule [addFn, badFn] of
    Left err ->
      assertTrue "collapsed verifier rejects unsaturated known direct call" ("not optimal" `isInfixOf` err)
    Right _ ->
      failTest "collapsed verifier rejects unsaturated known direct call" "unexpected verifier success"

testExampleSourceCurryingNormalization :: IO Bool
testExampleSourceCurryingNormalization = do
  let src =
        unlines
          [ "add a b = a"
          , "inc x = add x"
          , "use x = add x 1"
          ]
      expectedInc =
        CollapsedFunction
          { name = "inc"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCurryDirect "add" [ALocal 0]
                  }
              ]
          , result = ATemp 0
          , lifted = []
          }
      expectedUse =
        CollapsedFunction
          { name = "use"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCallDirect "add" [ALocal 0, AConstI32 1]
                  }
              ]
          , result = ATemp 0
          , lifted = []
          }
  case compileSourceToCollapsed src of
    Left err ->
      failTest "example source currying normalization" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case (findCollapsed "inc" collapsed, findCollapsed "use" collapsed) of
        (Just gotInc, Just gotUse) ->
          assertTrue
            "example source currying normalization"
            (gotInc == expectedInc && gotUse == expectedUse)
        _ ->
          failTest "example source currying normalization" "expected functions inc/use in collapsed output"

testExampleSourceOverApplicationNormalization :: IO Bool
testExampleSourceOverApplicationNormalization = do
  let src =
        unlines
          [ "add a b = a"
          , "over x = add x 1 2"
          ]
      expectedOver =
        CollapsedFunction
          { name = "over"
          , arity = 1
          , captureArity = 0
          , binds =
              [ Bind
                  { temp = 0
                  , value = VCallDirect "add" [ALocal 0, AConstI32 1]
                  }
              , Bind
                  { temp = 1
                  , value = VApply (ATemp 0) (AConstI32 2)
                  }
              ]
          , result = ATemp 1
          , lifted = []
          }
  case compileSourceToCollapsed src of
    Left err ->
      failTest "example source over-application normalization" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "over" collapsed of
        Nothing ->
          failTest "example source over-application normalization" "expected function over in collapsed output"
        Just gotOver ->
          assertEqual "example source over-application normalization" expectedOver gotOver

testExampleSourceClosureCallsNormalizedToApply :: IO Bool
testExampleSourceClosureCallsNormalizedToApply = do
  let src =
        unlines
          [ "apply f x = f x"
          , "twice f x = f (f x)"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "example source closure calls normalize to apply" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      assertTrue "example source closure calls normalize to apply" (not (hasRawCallClosure collapsed))

testImmediateClosureApplyCollapsesToDirect :: IO Bool
testImmediateClosureApplyCollapsesToDirect = do
  let src =
        unlines
          [ "main x = (\\y -> add y 1) x"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "immediate closure apply collapses to direct call" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "immediate closure apply collapses to direct call" "expected function main in collapsed output"
        Just fn ->
          let vals = map value (getBinds fn)
              hasHigherOrderArtifacts =
                any
                  ( \v ->
                      case v of
                        VClosure _ _ -> True
                        VCurryDirect _ _ -> True
                        VCallClosure _ _ -> True
                        VApply _ _ -> True
                        _ -> False
                  )
                  vals
              hasDirectAdd =
                any
                  ( \v ->
                      case v of
                        VCallDirect "add" [ALocal 0, AConstI32 1] -> True
                        _ -> False
                  )
                  vals
           in assertTrue
                "immediate closure apply collapses to direct call"
                (hasDirectAdd && not hasHigherOrderArtifacts)
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

testImmediateCurryApplyCollapsesToDirect :: IO Bool
testImmediateCurryApplyCollapsesToDirect = do
  let src =
        unlines
          [ "plus a b = add a b"
          , "main x = (plus 1) x"
          ]
      expected =
        [ VCallDirect "add" [AConstI32 1, ALocal 0]
        ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "immediate curry apply collapses to direct call" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "immediate curry apply collapses to direct call" "expected function main in collapsed output"
        Just fn ->
          assertEqual "immediate curry apply collapses to direct call" expected (map value (getBinds fn))
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

testEscapeStructFlattenRemovesConstructorHelpers :: IO Bool
testEscapeStructFlattenRemovesConstructorHelpers = do
  let src =
        unlines
          [ "main x = add (__get_pair_0 (__mk_pair_2 (add x 1) (mul x 2))) (__get_pair_1 (__mk_pair_2 (add x 1) (mul x 2)))"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "escape/lifetime struct flatten removes constructor helper calls" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "escape/lifetime struct flatten removes constructor helper calls" "expected function main in collapsed output"
        Just fn ->
          assertTrue
            "escape/lifetime struct flatten removes constructor helper calls"
            (not (any valueUsesStructHelpers (map value (getBinds fn))))
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

    valueUsesStructHelpers :: Value -> Bool
    valueUsesStructHelpers val =
      case val of
        VCallDirect callee _ ->
          "__mk_" `isPrefixOf` callee
            || "__get_" `isPrefixOf` callee
            || "__is_" `isPrefixOf` callee
        _ ->
          False

testEscapeStructFlattenPreservesTagMismatchChecks :: IO Bool
testEscapeStructFlattenPreservesTagMismatchChecks = do
  let src =
        unlines
          [ "main x = __get_other_0 (__mk_pair_2 (add x 1) (mul x 2))"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "escape/lifetime struct flatten preserves getter tag checks" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case evalCollapsedFunction collapsed "main" [10] of
        Right out ->
          failTest
            "escape/lifetime struct flatten preserves getter tag checks"
            ("expected tag mismatch error, got output: " <> show out)
        Left err ->
          assertTrue
            "escape/lifetime struct flatten preserves getter tag checks"
            ("struct tag mismatch" `isInfixOf` err)

testSliceSetSharedTargetCopiesBeforeWrite :: IO Bool
testSliceSetSharedTargetCopiesBeforeWrite = do
  let src =
        unlines
          [ "main bytes idx val = let alias = bytes; out = slice_set_u8 bytes idx val in add (slice_get_u8 alias idx) (slice_get_u8 out idx)"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "slice_set shared target copies before write" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "slice_set shared target copies before write" "expected function main in collapsed output"
        Just fn ->
          let vals = map value (getBinds fn)
              hasMemcpy = any isMemcpy vals
              hasDirectWriteToInput = any writesInputSlice vals
           in assertTrue
                "slice_set shared target copies before write"
                (hasMemcpy && not hasDirectWriteToInput)
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

    isMemcpy :: Value -> Bool
    isMemcpy val =
      case val of
        VCallDirect "memcpy_u8" _ -> True
        _ -> False

    writesInputSlice :: Value -> Bool
    writesInputSlice val =
      case val of
        VCallDirect "slice_set_u8" (ALocal 0 : _rest) -> True
        _ -> False

testSliceSetLinearTargetReusesBuffer :: IO Bool
testSliceSetLinearTargetReusesBuffer = do
  let src =
        unlines
          [ "main idx val = let s0 = slice_new_u8 16; s1 = slice_set_u8 s0 idx val; s2 = slice_set_u8 s1 idx val in slice_get_u8 s2 idx"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "slice_set linear target reuses buffer" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "slice_set linear target reuses buffer" "expected function main in collapsed output"
        Just fn ->
          let vals = map value (getBinds fn)
              memcpyCount = length (filter isMemcpy vals)
              sliceNewCount = length (filter isSliceNew vals)
              sliceSetCount = length (filter isSliceSet vals)
           in assertTrue
                "slice_set linear target reuses buffer"
                (memcpyCount == 0 && sliceNewCount == 1 && sliceSetCount == 2)
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

    isMemcpy :: Value -> Bool
    isMemcpy val =
      case val of
        VCallDirect "memcpy_u8" _ -> True
        _ -> False

    isSliceNew :: Value -> Bool
    isSliceNew val =
      case val of
        VCallDirect "slice_new_u8" _ -> True
        _ -> False

    isSliceSet :: Value -> Bool
    isSliceSet val =
      case val of
        VCallDirect "slice_set_u8" _ -> True
        _ -> False

testCollapseInsertsFunctionRegionScopeForLocalAllocation :: IO Bool
testCollapseInsertsFunctionRegionScopeForLocalAllocation = do
  let src =
        unlines
          [ "main bytes idx = let s = slice_new_u8 (slice_len bytes)"
          , "  v = slice_set_u8 s idx 1"
          , "  in 0"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "collapse inserts function region scope for local allocation" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "collapse inserts function region scope for local allocation" "expected function main in collapsed output"
        Just fn ->
          let (markCount, resetCount, markUsages) = regionOpCountsAndUsages fn
           in assertTrue
                "collapse inserts function region scope for local allocation"
                (markCount == 1 && resetCount == 1 && markUsages == 1)

testCollapseSkipsFunctionRegionScopeForExplicitRegionOps :: IO Bool
testCollapseSkipsFunctionRegionScopeForExplicitRegionOps = do
  let src =
        unlines
          [ "main bytes = let mark = region_mark 0"
          , "  out = slice_new_u8 (slice_len bytes)"
          , "  reset = region_reset mark"
          , "  in out"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "collapse skips function region scope for explicit region ops" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "collapse skips function region scope for explicit region ops" "expected function main in collapsed output"
        Just fn ->
          let (markCount, resetCount, markUsages) = regionOpCountsAndUsages fn
           in assertTrue
                "collapse skips function region scope for explicit region ops"
                (markCount == 1 && resetCount == 1 && markUsages == 1)

testCollapseSkipsFunctionRegionScopeForEscapingAllocation :: IO Bool
testCollapseSkipsFunctionRegionScopeForEscapingAllocation = do
  let src =
        unlines
          [ "main bytes = slice_new_u8 (slice_len bytes)"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "collapse skips function region scope for escaping allocation" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "collapse skips function region scope for escaping allocation" "expected function main in collapsed output"
        Just fn ->
          let (markCount, resetCount, _markUsages) = regionOpCountsAndUsages fn
           in assertTrue
                "collapse skips function region scope for escaping allocation"
                (markCount == 0 && resetCount == 0)

testHotUncurryWrapperRemovesDirectWrapperCalls :: IO Bool
testHotUncurryWrapperRemovesDirectWrapperCalls = do
  let src =
        unlines
          [ "apply4 f a b c d = f a b c d"
          , "branch_a x = let f = \\a -> \\b -> \\c -> \\d -> add (add a b) (add c d) in apply4 f x 1 2 3"
          , "branch_b x = let f = \\a -> \\b -> \\c -> \\d -> add (add a b) (add c d) in apply4 f x 4 5 6"
          , "branch_c x = let f = \\a -> \\b -> \\c -> \\d -> add (add a b) (add c d) in apply4 f x 7 8 9"
          , "main x = add (branch_a x) (add (branch_b x) (branch_c x))"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "hot uncurrying removes direct wrapper calls" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case evalCollapsedFunction collapsed "main" [10] of
        Left err ->
          failTest "hot uncurrying removes direct wrapper calls" ("unexpected eval error: " <> err)
        Right out ->
          assertTrue
            "hot uncurrying removes direct wrapper calls"
            (out == 75 && not (moduleHasDirectCall "apply4" collapsed))

testPruneDeadFunctionsFromRootsKeepsReachableOnly :: IO Bool
testPruneDeadFunctionsFromRootsKeepsReachableOnly = do
  let keepFn =
        CollapsedFunction
          { name = "keep"
          , arity = 0
          , captureArity = 0
          , binds = [Bind {temp = 0, value = VCallDirect "used" []}]
          , result = ATemp 0
          , lifted = []
          }
      usedFn =
        CollapsedFunction
          { name = "used"
          , arity = 0
          , captureArity = 0
          , binds = []
          , result = AConstI32 1
          , lifted = []
          }
      deadFn =
        CollapsedFunction
          { name = "dead"
          , arity = 0
          , captureArity = 0
          , binds = []
          , result = AConstI32 2
          , lifted = []
          }
  case pruneDeadFunctionsFromRoots ["keep"] [keepFn, usedFn, deadFn] of
    Left err ->
      failTest "prune dead functions from roots keeps reachable only" ("unexpected prune error: " <> err)
    Right pruned ->
      assertEqual "prune dead functions from roots keeps reachable only" ["keep", "used"] (map collapsedName pruned)
  where
    collapsedName :: CollapsedFunction -> String
    collapsedName (CollapsedFunction fnName _ _ _ _ _) = fnName

testPruneKeepsReachableLiftedDescendant :: IO Bool
testPruneKeepsReachableLiftedDescendant = do
  let rootFn =
        CollapsedFunction
          { name = "root"
          , arity = 0
          , captureArity = 0
          , binds = [Bind {temp = 0, value = VCallDirect "leaf" []}]
          , result = ATemp 0
          , lifted = []
          }
      leafFn =
        CollapsedFunction
          { name = "leaf"
          , arity = 0
          , captureArity = 0
          , binds = []
          , result = AConstI32 9
          , lifted = []
          }
      containerFn =
        CollapsedFunction
          { name = "container"
          , arity = 0
          , captureArity = 0
          , binds = [Bind {temp = 0, value = VCallDirect "dead_dep" []}]
          , result = ATemp 0
          , lifted = [leafFn]
          }
      deadDepFn =
        CollapsedFunction
          { name = "dead_dep"
          , arity = 0
          , captureArity = 0
          , binds = []
          , result = AConstI32 13
          , lifted = []
          }
      collapsedFnName :: CollapsedFunction -> String
      collapsedFnName (CollapsedFunction fnName _ _ _ _ _) = fnName
      collapsedLifted :: CollapsedFunction -> [CollapsedFunction]
      collapsedLifted (CollapsedFunction _ _ _ _ _ ls) = ls
      hasName :: String -> [CollapsedFunction] -> Bool
      hasName target = any (fnHasName target)
      fnHasName :: String -> CollapsedFunction -> Bool
      fnHasName target fn =
        collapsedFnName fn == target || any (fnHasName target) (collapsedLifted fn)
      findInTree :: String -> [CollapsedFunction] -> Maybe CollapsedFunction
      findInTree _ [] = Nothing
      findInTree target (fn:rest)
        | collapsedFnName fn == target = Just fn
        | otherwise =
            case findInTree target (collapsedLifted fn) of
              Just out -> Just out
              Nothing -> findInTree target rest
  case pruneDeadFunctionsFromRoots ["root"] [rootFn, containerFn, deadDepFn] of
    Left err ->
      failTest "prune keeps reachable lifted descendant" ("unexpected prune error: " <> err)
    Right pruned ->
      case findInTree "container" pruned of
        Nothing ->
          failTest "prune keeps reachable lifted descendant" "expected retained container for reachable lifted child"
        Just keptContainer ->
          assertTrue
            "prune keeps reachable lifted descendant"
            ( hasName "root" pruned
                && hasName "leaf" pruned
                && not (hasName "dead_dep" pruned)
                && null (binds keptContainer)
                && result keptContainer == AConstI32 0
            )

testCollapseFromRootsPrunesUnusedHelpers :: IO Bool
testCollapseFromRootsPrunesUnusedHelpers = do
  let src =
        unlines
          [ "plus a b = add a b"
          , "square x = mul x x"
          , "apply f x = f x"
          , "abstraction x = apply (plus 1) (square x)"
          ]
      expectedValues =
        [ VCallDirect "mul" [ALocal 0, ALocal 0]
        , VCallDirect "add" [AConstI32 1, ATemp 0]
        ]
  case parseModule src >>= lowerModule >>= collapseAndVerifyModuleFromRoots ["abstraction"] of
    Left err ->
      failTest "collapse from roots prunes unused helpers" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "abstraction" collapsed of
        Nothing ->
          failTest "collapse from roots prunes unused helpers" "missing abstraction function"
        Just fn ->
          assertTrue
            "collapse from roots prunes unused helpers"
            ( map value (getBinds fn) == expectedValues
                && findCollapsed "plus" collapsed == Nothing
                && findCollapsed "square" collapsed == Nothing
                && findCollapsed "apply" collapsed == Nothing
            )
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

testTraitCatalogHasCoreTraits :: IO Bool
testTraitCatalogHasCoreTraits = do
  let names = map traitName basicTraits
  assertTrue "trait catalog has core traits" (all (`elem` names) ["add", "sub", "mul", "div", "monoid", "functor", "applicative", "monad"])

testTraitCategoriesAvailable :: IO Bool
testTraitCategoriesAvailable = do
  let arithmeticCount = length (traitsByCategory ArithmeticCategory basicTraits)
      monadCount = length (traitsByCategory MonadCategory basicTraits)
  assertTrue "trait categories filter traits" (arithmeticCount >= 4 && monadCount >= 1)

testRewriteWithArithmeticSubset :: IO Bool
testRewriteWithArithmeticSubset = do
  let arithmeticOnly = traitsByCategory ArithmeticCategory basicTraits
      expr = App (App (Var "mul") (IntLit 1)) (Var "x")
  case rewriteOnceWithTraits arithmeticOnly expr of
    Nothing ->
      failTest "rewrite with arithmetic trait subset" "expected a rewrite"
    Just out ->
      assertEqual "rewrite with arithmetic trait subset" (Var "x") out

testRewriteAddRightZero :: IO Bool
testRewriteAddRightZero = do
  let expr = App (App (Var "add") (Var "x")) (IntLit 0)
  case rewriteOnceWithBasicTraits expr of
    Nothing ->
      failTest "rewrite add right zero" "expected a rewrite"
    Just out ->
      assertEqual "rewrite add right zero" (Var "x") out

testRewriteFunctorIdentity :: IO Bool
testRewriteFunctorIdentity = do
  let expr = App (App (Var "fmap") (Var "id")) (Var "xs")
      out = normalizeWithBasicTraits 8 expr
  assertEqual "rewrite functor identity" (Var "xs") out

testRewriteMonadLeftIdentity :: IO Bool
testRewriteMonadLeftIdentity = do
  let expr = App (App (Var "bind") (App (Var "pure") (IntLit 7))) (Var "k")
      out = normalizeWithBasicTraits 8 expr
      expected = App (Var "k") (IntLit 7)
  assertEqual "rewrite monad left identity" expected out

testNormalizeArithmeticExpression :: IO Bool
testNormalizeArithmeticExpression = do
  let expr =
        App
          (App (Var "mul") (App (App (Var "add") (IntLit 2)) (IntLit 0)))
          (IntLit 1)
      out = normalizeWithTraits 16 basicTraits expr
  assertEqual "normalize arithmetic expression" (IntLit 2) out

testClassDefRequiresLaws :: IO Bool
testClassDefRequiresLaws = do
  let result =
        mkClassDef
          "bad_functor"
          FunctorClass
          ["fmap", "id", "compose"]
          [ Law
              { lawName = "identity"
              , lhs = Var "left"
              , rhs = Var "right"
              }
          ]
  case result of
    Left err ->
      assertTrue "class definitions require complete law set" ("missing required laws" `isInfixOf` err)
    Right classDef ->
      failTest "class definitions require complete law set" ("unexpected class acceptance: " <> show classDef)

testClassDefAcceptsCompleteLawSet :: IO Bool
testClassDefAcceptsCompleteLawSet = do
  let result =
        mkClassDef
          "good_monoid"
          MonoidClass
          ["empty", "append"]
          [ Law {lawName = "left_identity", lhs = Var "a", rhs = Var "b"}
          , Law {lawName = "right_identity", lhs = Var "a", rhs = Var "b"}
          , Law {lawName = "associativity", lhs = Var "a", rhs = Var "b"}
          ]
  case result of
    Left err ->
      failTest "class definitions accept complete law sets" ("unexpected validation error: " <> err)
    Right _ ->
      passTest "class definitions accept complete law sets"

testInferSourceTypesForDataAndNoDo :: IO Bool
testInferSourceTypesForDataAndNoDo = do
  let src =
        unlines
          [ "data Pair a b = Pair a b"
          , "mk x y = Pair x y"
          , "sum_direct x = add (add x 1) 2"
          , "first p = let Pair left right = p in left"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for data and no-do" ("unexpected inference error: " <> err)
    Right infos ->
      case (findTypeInfo "sum_direct" infos, findTypeInfo "mk" infos, findTypeInfo "first" infos) of
        (Just sumInfo, Just mkInfo, Just firstInfo) ->
          let sumType = renderType (fnType sumInfo)
              mkType = renderType (fnType mkInfo)
              firstType = renderType (fnType firstInfo)
           in assertTrue
                "infer source types for data and no-do"
                ( sumType == "i64 -> i64"
                    && "Pair" `isInfixOf` mkType
                    && "Pair" `isInfixOf` firstType
                )
        _ ->
          failTest "infer source types for data and no-do" "missing inferred type for expected function(s)"

testInferSourceTypesForOldStyleDataNoTypeParams :: IO Bool
testInferSourceTypesForOldStyleDataNoTypeParams = do
  let src =
        unlines
          [ "data LifeEvent = Tick steps | ToggleCell x y | ClearBoard token | LoadBoard cells"
          , "main event = case event of"
          , "  Tick steps -> steps"
          , "  ToggleCell x y -> add x y"
          , "  ClearBoard token -> token"
          , "  LoadBoard _ -> 0"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for old-style data without type params" ("unexpected inference error: " <> err)
    Right infos ->
      case findTypeInfo "main" infos of
        Just mainInfo ->
          let inferred = renderType (fnType mainInfo)
           in assertTrue
                "infer source types for old-style data without type params"
                (inferred == "LifeEvent -> i64")
        Nothing ->
          failTest "infer source types for old-style data without type params" "missing inferred type for main"

testInferSourceTypesStringLiteral :: IO Bool
testInferSourceTypesStringLiteral = do
  let src =
        unlines
          [ "greeting = \"hello\""
          , "main = greeting"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for string literals" ("unexpected inference error: " <> err)
    Right infos ->
      case (findTypeInfo "greeting" infos, findTypeInfo "main" infos) of
        (Just greetingInfo, Just mainInfo) ->
          let greetingType = renderType (fnType greetingInfo)
              mainType = renderType (fnType mainInfo)
           in assertTrue
                "infer source types for string literals"
                (greetingType == "string" && mainType == "string")
        _ ->
          failTest "infer source types for string literals" "missing inferred type for expected function(s)"

testInferSourceTypesSliceBuiltins :: IO Bool
testInferSourceTypesSliceBuiltins = do
  let src =
        unlines
          [ "len bytes = slice_len bytes"
          , "first_byte bytes = slice_get_u8 bytes 0"
          , "write_zero bytes = slice_set_u8 bytes 0 0"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for slice builtins" ("unexpected inference error: " <> err)
    Right infos ->
      case (findTypeInfo "len" infos, findTypeInfo "first_byte" infos, findTypeInfo "write_zero" infos) of
        (Just lenInfo, Just firstInfo, Just writeInfo) ->
          let lenType = renderType (fnType lenInfo)
              firstType = renderType (fnType firstInfo)
              writeType = renderType (fnType writeInfo)
           in assertTrue
                "infer source types for slice builtins"
                (lenType == "slice byte -> i64" && firstType == "slice byte -> i64" && writeType == "slice byte -> slice byte")
        _ ->
          failTest "infer source types for slice builtins" "missing inferred type for expected function(s)"

testInferSourceTypesLinearMemoryBuiltins :: IO Bool
testInferSourceTypesLinearMemoryBuiltins = do
  let src =
        unlines
          [ "mk bytes = slice_new_u8 (slice_len bytes)"
          , "ptr bytes = slice_data_ptr bytes"
          , "raw_len bytes = slice_len_raw bytes"
          , "mark _ = region_mark 0"
          , "alloc bytes = region_alloc (slice_len_raw bytes) 1"
          , "reset mark = region_reset mark"
          , "copy dst src len = memcpy_u8 dst src len"
          , "fill dst value len = memset_u8 dst value len"
          , "tag x = struct_tag x"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for linear memory builtins" ("unexpected inference error: " <> err)
    Right infos ->
      case (findTypeInfo "mk" infos, findTypeInfo "ptr" infos, findTypeInfo "copy" infos, findTypeInfo "tag" infos) of
        (Just mkInfo, Just ptrInfo, Just copyInfo, Just tagInfo) ->
          let mkType = renderType (fnType mkInfo)
              ptrType = renderType (fnType ptrInfo)
              copyType = renderType (fnType copyInfo)
              tagType = renderType (fnType tagInfo)
           in assertTrue
                "infer source types for linear memory builtins"
                ( mkType == "slice byte -> slice byte"
                    && ptrType == "slice byte -> i64"
                    && copyType == "i64 -> i64 -> i64 -> i64"
                    && "-> i64" `isInfixOf` tagType
                )
        _ ->
          failTest "infer source types for linear memory builtins" "missing inferred type for expected function(s)"

testInferSourceTypesCasePatternConstrainsScrutinee :: IO Bool
testInferSourceTypesCasePatternConstrainsScrutinee = do
  let src =
        unlines
          [ "data Maybe a = Nothing : Maybe a | Just : a -> Maybe a"
          , "main m = case m of Nothing -> 0; Just n -> add n 1"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for case-pattern bound variable constrains scrutinee" ("unexpected inference error: " <> err)
    Right infos ->
      case findTypeInfo "main" infos of
        Just mainInfo ->
          let inferred = renderType (fnType mainInfo)
           in assertTrue
                "infer source types for case-pattern bound variable constrains scrutinee"
                (inferred == "Maybe i64 -> i64")
        Nothing ->
          failTest "infer source types for case-pattern bound variable constrains scrutinee" "missing inferred type for main"

testEvalSourceFunctionWithClosure :: IO Bool
testEvalSourceFunctionWithClosure = do
  let src =
        unlines
          [ "make_adder x = \\y -> add x y"
          , "main x = (make_adder 3) x"
          ]
  case parseModule src of
    Left err ->
      failTest "eval source function with closure" ("unexpected parse error: " <> err)
    Right modu ->
      case evalSourceFunction modu "main" [7] of
        Left err ->
          failTest "eval source function with closure" ("unexpected eval error: " <> err)
        Right out ->
          assertEqual "eval source function with closure" 10 out

testEvalSourceFunctionWithCaseExpression :: IO Bool
testEvalSourceFunctionWithCaseExpression = do
  let src = "main a b = case a b of 0 0 -> 0; x y -> add x y"
  case parseModule src of
    Left err ->
      failTest "eval source function with case expression" ("unexpected parse error: " <> err)
    Right modu -> do
      case evalSourceFunction modu "main" [0, 0] of
        Left err ->
          failTest "eval source function with case expression" ("unexpected eval error at [0,0]: " <> err)
        Right out0 ->
          if out0 /= 0
            then failTest "eval source function with case expression" ("expected 0 at [0,0], got " <> show out0)
            else
              case evalSourceFunction modu "main" [3, 4] of
                Left err ->
                  failTest "eval source function with case expression" ("unexpected eval error at [3,4]: " <> err)
                Right out1 ->
                  assertEqual "eval source function with case expression" 7 out1

testEvalSourceFunctionWithCaseExpressionMultiline :: IO Bool
testEvalSourceFunctionWithCaseExpressionMultiline = do
  let src =
        unlines
          [ "main a b = case a b of"
          , "  0 0 -> 0"
          , "  x y -> add x y"
          ]
  case parseModule src of
    Left err ->
      failTest "eval source function with case expression multiline" ("unexpected parse error: " <> err)
    Right modu -> do
      case evalSourceFunction modu "main" [0, 0] of
        Left err ->
          failTest
            "eval source function with case expression multiline"
            ("unexpected eval error at [0,0]: " <> err)
        Right out0 ->
          if out0 /= 0
            then
              failTest
                "eval source function with case expression multiline"
                ("expected 0 at [0,0], got " <> show out0)
            else
              case evalSourceFunction modu "main" [3, 4] of
                Left err ->
                  failTest
                    "eval source function with case expression multiline"
                    ("unexpected eval error at [3,4]: " <> err)
                Right out1 ->
                  assertEqual "eval source function with case expression multiline" 7 out1

testEvalSourceFunctionWithMaybeEitherMonads :: IO Bool
testEvalSourceFunctionWithMaybeEitherMonads = do
  case parseModule monadsMaybeEitherSource of
    Left err ->
      failTest "eval source function with maybe/either monads" ("unexpected parse error: " <> err)
    Right modu ->
      case evalSourceFunction modu "main" [7] of
        Left err ->
          failTest "eval source function with maybe/either monads" ("unexpected eval error: " <> err)
        Right out ->
          assertEqual "eval source function with maybe/either monads" 22 out

testEvalCollapsedFunctionWithCurrying :: IO Bool
testEvalCollapsedFunctionWithCurrying = do
  let src =
        unlines
          [ "plus a b = add a b"
          , "make_inc = plus 1"
          , "main x = make_inc x"
          ]
  case parseModule src >>= lowerModule >>= collapseAndVerifyModule of
    Left err ->
      failTest "eval collapsed function with currying" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case evalCollapsedFunction collapsed "main" [11] of
        Left err ->
          failTest "eval collapsed function with currying" ("unexpected eval error: " <> err)
        Right out ->
          assertEqual "eval collapsed function with currying" 12 out

testDifferentialDataAndNoDoSemantics :: IO Bool
testDifferentialDataAndNoDoSemantics = do
  let src =
        unlines
          [ "data Pair a b = Pair a b"
          , "main x = let Pair left right = Pair (add x 1) 9 in left"
          ]
  case parseModule src of
    Left err ->
      failTest "differential data and no-do semantics" ("unexpected parse error: " <> err)
    Right modu ->
      case go modu [0 .. 20] of
        Left err ->
          failTest "differential data and no-do semantics" err
        Right () ->
          passTest "differential data and no-do semantics"
  where
    go :: Module -> [Int] -> Either String ()
    go _ [] = Right ()
    go modu (x:xs) = do
      differentialCheckSourceCollapsed modu "main" [x]
      go modu xs

testDifferentialMaybeEitherMonadsSemantics :: IO Bool
testDifferentialMaybeEitherMonadsSemantics = do
  case parseModule monadsMaybeEitherSource of
    Left err ->
      failTest "differential maybe/either monad semantics" ("unexpected parse error: " <> err)
    Right modu ->
      case go modu [0 .. 20] of
        Left err ->
          failTest "differential maybe/either monad semantics" err
        Right () ->
          passTest "differential maybe/either monad semantics"
  where
    go :: Module -> [Int] -> Either String ()
    go _ [] = Right ()
    go modu (x:xs) = do
      differentialCheckSourceCollapsed modu "main" [x]
      go modu xs

testDifferentialCaseExpressionSemantics :: IO Bool
testDifferentialCaseExpressionSemantics = do
  let src = "main x = case x of 0 -> 1; y -> add y 2"
  case parseModule src of
    Left err ->
      failTest "differential case expression semantics" ("unexpected parse error: " <> err)
    Right modu ->
      case go modu [0 .. 20] of
        Left err ->
          failTest "differential case expression semantics" err
        Right () ->
          passTest "differential case expression semantics"
  where
    go :: Module -> [Int] -> Either String ()
    go _ [] = Right ()
    go modu (x:xs) = do
      differentialCheckSourceCollapsed modu "main" [x]
      go modu xs

testDifferentialSourceCollapsedSemantics :: IO Bool
testDifferentialSourceCollapsedSemantics = do
  let src =
        unlines
          [ "plus a b = add a b"
          , "apply f x = f x"
          , "main x = apply (plus 1) (plus x 2)"
          ]
  case parseModule src of
    Left err ->
      failTest "differential source/collapsed semantics" ("unexpected parse error: " <> err)
    Right modu ->
      case go modu [0 .. 32] of
        Left err ->
          failTest "differential source/collapsed semantics" err
        Right () ->
          passTest "differential source/collapsed semantics"
  where
    go :: Module -> [Int] -> Either String ()
    go _ [] = Right ()
    go modu (x:xs) = do
      differentialCheckSourceCollapsed modu "main" [x]
      go modu xs

testCompileWasmInlinesNumericBuiltins :: IO Bool
testCompileWasmInlinesNumericBuiltins = do
  let src =
        unlines
          [ "main x = and (eq (add (mul x 2) (sub x 3)) (div x 1)) 1"
          ]
      removedRuntimeFns =
        [ "rt_i32_const"
        , "rt_add"
        , "rt_sub"
        , "rt_mul"
        , "rt_div"
        , "rt_eq"
        , "rt_and"
        ]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm inlines numeric builtins" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm inlines numeric builtins"
        (all (not . wasmContainsName wasmBytes) removedRuntimeFns)

testCompileWasmModule :: IO Bool
testCompileWasmModule = do
  let src =
        unlines
          [ "square x = mul x x"
          , "main x = add (square x) 1"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm module from source" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue "compile wasm module from source" (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmRejectsReservedRuntimeExportNames :: IO Bool
testCompileWasmRejectsReservedRuntimeExportNames = do
  let src =
        unlines
          [ "__memory x = x"
          , "main x = __memory x"
          ]
  case compileSourceToWasm src of
    Left err ->
      assertTrue
        "compile wasm rejects reserved runtime export names"
        ("reserved for runtime export" `isInfixOf` err)
    Right _ ->
      failTest
        "compile wasm rejects reserved runtime export names"
        "expected compile failure for reserved export name, but compilation succeeded"

testCompileWasmSupportsCaseExpressions :: IO Bool
testCompileWasmSupportsCaseExpressions = do
  let src = "main x = case x of 0 -> 1; y -> add y 2"
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports case expressions" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue "compile wasm supports case expressions" (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmSupportsCaseExpressionsMultiline :: IO Bool
testCompileWasmSupportsCaseExpressionsMultiline = do
  let src =
        unlines
          [ "main x = case x of"
          , "  0 -> 1"
          , "  y -> add y 2"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports multiline case expressions" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue "compile wasm supports multiline case expressions" (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmSupportsCaseExpressionBranchThunkDirectCalls :: IO Bool
testCompileWasmSupportsCaseExpressionBranchThunkDirectCalls = do
  let src =
        unlines
          [ "main x = case x of"
          , "  0 -> add x 1"
          , "  _ -> x"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest
        "compile wasm direct-if branch thunks"
        ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm direct-if branch thunks"
        ( BS.take 4 wasmBytes == wasmMagic
            && not (wasmContainsName wasmBytes "rt_make_closure")
            && not (wasmContainsName wasmBytes "rt_apply")
        )

testCompileWasmSupportsClosures :: IO Bool
testCompileWasmSupportsClosures = do
  let src =
        unlines
          [ "make_adder x = \\y -> add x y"
          , "main x = (make_adder 3) x"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports closures" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports closures"
        ( BS.take 4 wasmBytes == wasmMagic
            && not (wasmContainsName wasmBytes "rt_alloc")
            && not (wasmContainsName wasmBytes "rt_make_closure")
            && not (wasmContainsName wasmBytes "rt_apply")
            && not (wasmContainsName wasmBytes "rt_apply2")
            && not (wasmContainsName wasmBytes "rt_apply3")
            && not (wasmContainsName wasmBytes "rt_apply4")
        )

testCompileWasmSupportsClosuresWithManyCaptures :: IO Bool
testCompileWasmSupportsClosuresWithManyCaptures = do
  let src =
        unlines
          [ "mk a b c d e f g h i = \\x -> add x (add a (add b (add c (add d (add e (add f (add g (add h i))))))))"
          , "main x = (mk 1 2 3 4 5 6 7 8 9) x"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports closures with many captures" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports closures with many captures"
        (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmSupportsMaybeEitherMonads :: IO Bool
testCompileWasmSupportsMaybeEitherMonads = do
  let wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm monadsMaybeEitherSource of
    Left err ->
      failTest "compile wasm supports maybe/either monads" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue "compile wasm supports maybe/either monads" (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmSupportsCurrying :: IO Bool
testCompileWasmSupportsCurrying = do
  let src =
        unlines
          [ "plus a b = add a b"
          , "make_inc = plus 1"
          , "main x = make_inc x"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports currying" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports currying"
        ( BS.take 4 wasmBytes == wasmMagic
            && not (wasmContainsName wasmBytes "rt_make_closure")
            && not (wasmContainsName wasmBytes "rt_apply")
        )

testCompileWasmSupportsDataAndNoDo :: IO Bool
testCompileWasmSupportsDataAndNoDo = do
  let src =
        unlines
          [ "data Pair a b = Pair a b"
          , "main x = let Pair left right = Pair (add x 1) 9 in left"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports data and no-do" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports data and no-do"
        ( BS.take 4 wasmBytes == wasmMagic
            && not (wasmContainsName wasmBytes "rt_make_struct")
        )

testCompileWasmSupportsStructWithManyFields :: IO Bool
testCompileWasmSupportsStructWithManyFields = do
  let src =
        unlines
          [ "data Nine a b c d e f g h i = Nine a b c d e f g h i"
          , "main x = let Nine a b c d e f g h i = Nine x x x x x x x x x in add a i"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports struct with many fields" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports struct with many fields"
        (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmSupportsCollectionLiterals :: IO Bool
testCompileWasmSupportsCollectionLiterals = do
  let src =
        unlines
          [ "main = [1, 2, 3]"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports collection literals" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue "compile wasm supports collection literals" (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmSupportsStringLiterals :: IO Bool
testCompileWasmSupportsStringLiterals = do
  let src =
        unlines
          [ "main = \"hello\""
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
      literalBytes = BS.pack (map (fromIntegral . fromEnum) ("hello" :: String))
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports string literals" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports string literals"
        (BS.take 4 wasmBytes == wasmMagic && literalBytes `BS.isInfixOf` wasmBytes)

testCompileWasmSupportsUtf8StringLiterals :: IO Bool
testCompileWasmSupportsUtf8StringLiterals = do
  let src = "main = \"" <> [chr 233] <> "\""
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
      utf8Bytes = BS.pack [0xc3, 0xa9]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports utf8 string literals" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports utf8 string literals"
        (BS.take 4 wasmBytes == wasmMagic && utf8Bytes `BS.isInfixOf` wasmBytes)

testCompileWasmSupportsSliceInteropImports :: IO Bool
testCompileWasmSupportsSliceInteropImports = do
  let src =
        unlines
          [ "main bytes idx = add (slice_len bytes) (slice_get_u8 bytes idx)"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm inlines slice len/get memory ops" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm inlines slice len/get memory ops"
        ( BS.take 4 wasmBytes == wasmMagic
            && not (wasmContainsName wasmBytes "rt_slice_len")
            && not (wasmContainsName wasmBytes "rt_slice_get_u8")
            && not (wasmContainsName wasmBytes "rt_slice_set_u8")
        )

testCompileWasmSupportsSliceSetImport :: IO Bool
testCompileWasmSupportsSliceSetImport = do
  let src =
        unlines
          [ "main in_bytes out_bytes i = slice_set_u8 out_bytes i (slice_get_u8 in_bytes i)"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm inlines slice set memory op" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
        assertTrue
          "compile wasm inlines slice set memory op"
          ( BS.take 4 wasmBytes == wasmMagic
              && not (wasmContainsName wasmBytes "rt_slice_set_u8")
              && not (wasmContainsName wasmBytes "rt_slice_get_u8")
              && not (wasmContainsName wasmBytes "rt_slice_len")
          )

testCompileWasmSupportsLinearMemoryHelpers :: IO Bool
testCompileWasmSupportsLinearMemoryHelpers = do
  let src =
        unlines
          [ "main in_bytes idx = let out = slice_new_u8 (slice_len in_bytes);"
          , "                    src = slice_data_ptr in_bytes;"
          , "                    dst = slice_data_ptr out;"
          , "                    len = slice_len_raw in_bytes;"
          , "                    copied = memcpy_u8 dst src len;"
          , "                    filled = memset_u8 dst 0 len;"
          , "                    mark = region_mark 0;"
          , "                    tmp = region_alloc len 1;"
          , "                    reset = region_reset mark"
          , "                in slice_get_u8 out idx"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
      removedRuntimeFns =
        [ "rt_slice_new_u8"
        , "rt_alloc"
        , "rt_memcpy"
        , "rt_memset"
        , "rt_slice_data_ptr"
        , "rt_slice_len_raw"
        , "rt_region_mark"
        , "rt_region_alloc"
        , "rt_region_reset"
        ]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports linear memory helpers" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports linear memory helpers"
        ( BS.take 4 wasmBytes == wasmMagic
            && all (not . wasmContainsName wasmBytes) removedRuntimeFns
        )

testCompileWasmStructHelpersInlined :: IO Bool
testCompileWasmStructHelpersInlined = do
  let src =
        unlines
          [ "data Pair a = Pair a"
          , "main x = let Pair y = Pair x in y"
          ]
      removedRuntimeFns =
        [ "rt_make_struct"
        , "rt_get_field"
        , "rt_has_tag"
        , "rt_get_tag"
        ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm inlines struct helper checks" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm inlines struct helper checks"
        ( BS.take 4 wasmBytes == wasmMagic
            && all (not . wasmContainsName wasmBytes) removedRuntimeFns
        )

testCompileWasmExportsHeapPtr :: IO Bool
testCompileWasmExportsHeapPtr = do
  let src =
        unlines
          [ "main x = x"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
      expectedExports =
        [ ("__memory", 2)
        , ("__table", 1)
        , ("__heap_ptr", 3)
        ]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm runtime exports have expected kinds" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      case checkExportKinds wasmBytes expectedExports of
        Left err ->
          failTest "compile wasm runtime exports have expected kinds" err
        Right () ->
          assertTrue
            "compile wasm runtime exports have expected kinds"
            (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmSupportsHeapGlobalAtom :: IO Bool
testCompileWasmSupportsHeapGlobalAtom = do
  let fn =
        CollapsedFunction
          { name = "main"
          , arity = 0
          , captureArity = 0
          , binds = []
          , result = AGlobal "__heap_ptr"
          , lifted = []
          }
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileModuleToWasm [fn] of
    Left err ->
      failTest "compile wasm supports heap global atom" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm supports heap global atom"
        (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmRejectsHeapPtrAsUserFunction :: IO Bool
testCompileWasmRejectsHeapPtrAsUserFunction = do
  let src =
        unlines
          [ "__heap_ptr x = x"
          , "main x = __heap_ptr x"
          ]
  case compileSourceToWasm src of
    Left err ->
      assertTrue
        "compile wasm rejects __heap_ptr as user function name"
        ("reserved for runtime export" `isInfixOf` err)
    Right _ ->
      failTest
        "compile wasm rejects __heap_ptr as user function name"
        "expected compile failure for reserved export name, but compilation succeeded"

testCompileWasmRejectsUnknownGlobalAtom :: IO Bool
testCompileWasmRejectsUnknownGlobalAtom = do
  let fn =
        CollapsedFunction
          { name = "main"
          , arity = 0
          , captureArity = 0
          , binds = []
          , result = AGlobal "__unknown_global"
          , lifted = []
          }
  case compileModuleToWasm [fn] of
    Left err ->
      assertTrue
        "compile wasm rejects unknown global atom"
        ("unknown global atom" `isInfixOf` err)
    Right _ ->
      failTest
        "compile wasm rejects unknown global atom"
        "expected compile failure for unknown global atom"

testCompileEntryModuleLoadsDottedImport :: IO Bool
testCompileEntryModuleLoadsDottedImport = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import util.math"
              , "main x = add (square x) 1"
              ]
          )
        , ( "util/math.clapse"
          , unlines
              [ "module util.math"
              , "export square"
              , "square x = mul x x"
              ]
          )
        ]
      expectedMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module with dotted import" ("unexpected compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile entry module with dotted import"
        (BS.take 4 wasmBytes == expectedMagic)

testCompileEntryModuleMissingImport :: IO Bool
testCompileEntryModuleMissingImport = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import util.missing"
              , "main x = add x 1"
              ]
          )
        ]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      assertTrue
        "compile entry module reports missing module"
        ("missing module file" `isInfixOf` err)
    Right _ ->
      failTest "compile entry module reports missing module" "expected missing module failure"

testCompileEntryModuleImportCycle :: IO Bool
testCompileEntryModuleImportCycle = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import a"
              , "main x = add x 1"
              ]
          )
        , ( "a.clapse"
          , unlines
              [ "module a"
              , "import b"
              , "a_fn x = add x 1"
              ]
          )
        , ( "b.clapse"
          , unlines
              [ "module b"
              , "import a"
              , "b_fn x = add x 2"
              ]
          )
        ]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      assertTrue
        "compile entry module detects import cycle"
        ("cyclic module import detected" `isInfixOf` err)
    Right _ ->
      failTest "compile entry module detects import cycle" "expected cycle detection failure"

testCompileEntryModuleDefaultExports :: IO Bool
testCompileEntryModuleDefaultExports = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "exported x = add x 1"
              , "hidden x = add x 2"
              , "main x = exported x"
              ]
          )
        ]
      expectedMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module default exports" ("unexpected compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile entry module default exports"
        ( BS.take 4 wasmBytes == expectedMagic
            && wasmContainsName wasmBytes "main"
            && wasmContainsName wasmBytes "exported"
            && wasmContainsName wasmBytes "hidden"
        )

testCompileEntryModuleExplicitExports :: IO Bool
testCompileEntryModuleExplicitExports = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "export main"
              , "hidden x = add x 2"
              , "main x = add x 1"
              ]
          )
        ]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module explicit exports" ("unexpected compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile entry module explicit exports"
        ( BS.take 4 wasmBytes == BS.pack [0x00, 0x61, 0x73, 0x6d]
            && wasmContainsName wasmBytes "main"
            && not (wasmContainsName wasmBytes "hidden")
        )

testCompileEntryModuleRejectsUnknownExport :: IO Bool
testCompileEntryModuleRejectsUnknownExport = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "export nope"
              , "main x = add x 1"
              ]
          )
        ]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      assertTrue
        "compile entry module rejects unknown export"
        ("unknown export in entry module" `isInfixOf` err)
    Right _ ->
      failTest "compile entry module rejects unknown export" "expected unknown-export failure"

testCompileEntryModuleArtifactExportsIncludeArity :: IO Bool
testCompileEntryModuleArtifactExportsIncludeArity = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "export main"
              , "main x y = add x y"
              ]
          )
        ]
  result <- compileFixtureArtifact files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module artifact exports include arity" ("unexpected compile error: " <> err)
    Right artifact ->
      assertEqual
        "compile entry module artifact exports include arity"
        [ExportApi {exportName = "main", exportArity = 2}]
        (artifactExports artifact)

testRenderTypeScriptBindingsUsesExportArityFromIr :: IO Bool
testRenderTypeScriptBindingsUsesExportArityFromIr = do
  let bindings =
        renderTypeScriptBindings
          [ ExportApi {exportName = "main", exportArity = 2}
          , ExportApi {exportName = "odd'name", exportArity = 1}
          ]
      hasMainSig =
        "readonly \"main\": (arg0: ClapseValue, arg1: ClapseValue) => ClapseValue;" `isInfixOf` bindings
      hasQuotedSig =
        "readonly \"odd'name\": (arg0: ClapseValue) => ClapseValue;" `isInfixOf` bindings
      hasArityMap =
        "readonly \"main\": 2;" `isInfixOf` bindings
          && "readonly \"odd'name\": 1;" `isInfixOf` bindings
  assertTrue
    "render typescript bindings uses export arity from ir"
    (hasMainSig && hasQuotedSig && hasArityMap)

testCollapsePipelineUsesDerivedRules :: IO Bool
testCollapsePipelineUsesDerivedRules = do
  let src =
        unlines
          [ "class plus_rules i : add"
          , "law plus_rules left_identity = add 0 x => x"
          , "law plus_rules right_identity = add x 0 => x"
          , "law plus_rules associativity = add (add x y) z => add x (add y z)"
          , "instance plus_on_int : plus_rules i add=plus"
          , "plus a b = add a b"
          , "main x = plus x 0"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "collapse pipeline uses derived rules" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "collapse pipeline uses derived rules" "missing collapsed function: main"
        Just mainFn ->
          assertTrue
            "collapse pipeline uses derived rules"
            (collapsedMainOptimized mainFn)
  where
    collapsedMainOptimized :: CollapsedFunction -> Bool
    collapsedMainOptimized fn =
      case fn of
        CollapsedFunction _ _ _ bs out _ ->
          null bs && out == ALocal 0

monadsMaybeEitherSource :: String
monadsMaybeEitherSource =
  unlines
    [ "class monad_rules m : monad"
    , "law monad_rules left_identity = bind (pure x) f => f x"
    , "law monad_rules right_identity = bind m pure => m"
    , "law monad_rules associativity = bind (bind m f) g => bind m (\\x -> bind (f x) g)"
    , "data Maybe a = Nothing : Maybe a | Just : a -> Maybe a"
    , "data Either e a = Left : e -> Either e a | Right : a -> Either e a"
    , "maybe_just x = Just x"
    , "maybe_pure x = maybe_just x"
    , "maybe_bind m f = case m of Nothing -> m; Just x -> f x; _ -> m"
    , "maybe_with_default default_value m = case m of Nothing -> default_value; Just x -> x; _ -> default_value"
    , "instance monad_on_maybe : monad_rules Maybe pure=maybe_pure bind=maybe_bind"
    , "either_left err = Left err"
    , "either_right x = Right x"
    , "either_pure x = either_right x"
    , "either_bind m f = case m of Left err -> m; Right x -> f x; _ -> m"
    , "either_with_default default_value m = case m of Left _ -> default_value; Right x -> x; _ -> default_value"
    , "instance monad_on_either : monad_rules Either pure=either_pure bind=either_bind"
    , "inc x = add x 1"
    , "double x = mul x 2"
    , "maybe_demo x = maybe_with_default 0 (maybe_bind (maybe_pure x) (\\n -> maybe_pure (inc n)))"
    , "either_demo x = either_with_default 0 (either_bind (either_pure x) (\\n -> either_pure (double n)))"
    , "main x = add (maybe_demo x) (either_demo x)"
    ]

checkExportKinds :: BS.ByteString -> [(String, Word8)] -> Either String ()
checkExportKinds wasmBytes expected = do
  exports <- parseWasmExports wasmBytes
  let findKind name = lookup name exports
  let checkOne (name, expectedKind) =
        case findKind name of
          Nothing -> Left ("missing export: " <> name)
          Just actualKind
            | actualKind == expectedKind -> Right ()
            | otherwise ->
                Left
                  ( "runtime export kind mismatch for "
                      <> name
                      <> ": expected "
                      <> show expectedKind
                      <> ", got "
                      <> show actualKind
                  )
  forM_ expected checkOne

wasmContainsName :: BS.ByteString -> String -> Bool
wasmContainsName wasmBytes nameText =
  BS.pack (map (fromIntegral . fromEnum) nameText) `BS.isInfixOf` wasmBytes

parseWasmExports :: BS.ByteString -> Either String [(String, Word8)]
parseWasmExports wasmBytes = do
  let wasmOffset = 8
  exportSection <- findSection wasmBytes wasmOffset sectionExport
  parseExportSection exportSection
    `orElse` []
  where
    -- Prefer a specific section if present; ignore missing section as empty.
    orElse :: Either String a -> a -> Either String a
    orElse action fallback =
      case action of
        Left _ -> Right fallback
        Right value -> Right value

sectionExport :: Word8
sectionExport = 7

findSection :: BS.ByteString -> Int -> Word8 -> Either String BS.ByteString
findSection bytes initialOffset targetSectionId = do
  if BS.length bytes < 8
    then Left "malformed wasm: missing magic/version"
    else go initialOffset
  where
    go :: Int -> Either String BS.ByteString
    go offset
      | offset >= BS.length bytes = Left ("missing section " <> show targetSectionId)
      | otherwise = do
          sid <- readByte bytes offset
          let payloadStart = offset + 1
          (payloadLen, nextOffset) <- readU32 bytes payloadStart
          let payloadOffset = nextOffset
              sectionEnd = payloadOffset + fromIntegral payloadLen
          if sectionEnd > BS.length bytes
            then Left ("section payload exceeds wasm size for section " <> show sid)
            else if sid == targetSectionId
              then
                Right (BS.take (fromIntegral payloadLen) (BS.drop payloadOffset bytes))
              else go sectionEnd

parseExportSection :: BS.ByteString -> Either String [(String, Word8)]
parseExportSection payload = do
  (count, offsetAfterCount) <- readU32 payload 0
  let exportCount = fromIntegral count
  foldM parseExport ([], fromIntegral offsetAfterCount) [1 .. exportCount]
    >>= (pure . reverse . fst)
  where
    parseExport ::
      ([(String, Word8)], Int) ->
      Word32 ->
      Either String ([(String, Word8)], Int)
    parseExport (acc, offset) _ = do
      (nameLen, off1) <- readU32 payload offset
      let nameBytes = BS.take (fromIntegral nameLen) (BS.drop off1 payload)
          off2 = off1 + fromIntegral nameLen
      when (off2 >= BS.length payload)
        (Left "malformed export name")
      kind <- readByte payload off2
      (_, nextOffset) <- readU32 payload (off2 + 1)
      let name = map (chr . fromIntegral) (BS.unpack nameBytes)
      pure ((name, kind) : acc, nextOffset)

readByte :: BS.ByteString -> Int -> Either String Word8
readByte bytes offset =
  if offset < 0 || offset >= BS.length bytes
    then Left ("offset out of bounds: " <> show offset)
    else Right (BS.index bytes offset)

readU32 :: BS.ByteString -> Int -> Either String (Word32, Int)
readU32 bytes offset =
  if offset < 0 || offset >= BS.length bytes
    then Left ("uleb offset out of bounds: " <> show offset)
    else decode 0 0 offset
  where
    decode :: Word32 -> Int -> Int -> Either String (Word32, Int)
    decode !acc !shift !idx = do
      byte <- readByte bytes idx
      let value = acc .|. (fromIntegral (byte .&. 0x7f) `shiftL` shift)
          nextShift = shift + 7
      if byte .&. 0x80 == 0
        then Right (value, idx + 1)
        else if nextShift >= 32
          then Left "uleb integer too large"
          else decode value nextShift (idx + 1)


assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual
  | expected == actual = passTest label
  | otherwise =
      failTest
        label
        ("expected " <> show expected <> ", got " <> show actual)

assertTrue :: String -> Bool -> IO Bool
assertTrue label condition
  | condition = passTest label
  | otherwise = failTest label "condition was False"

passTest :: String -> IO Bool
passTest label = do
  putStrLn ("[PASS] " <> label)
  pure True

failTest :: String -> String -> IO Bool
failTest label details = do
  putStrLn ("[FAIL] " <> label)
  putStrLn ("       " <> details)
  pure False

findCollapsed :: String -> [CollapsedFunction] -> Maybe CollapsedFunction
findCollapsed _name [] = Nothing
findCollapsed target (f:fs)
  | fnName == target = Just f
  | otherwise = findCollapsed target fs
  where
    fnName =
      case f of
        CollapsedFunction n _ _ _ _ _ -> n

findFunctionByName :: String -> [Function] -> Maybe Function
findFunctionByName _target [] = Nothing
findFunctionByName target (fn:rest)
  | fnName == target = Just fn
  | otherwise = findFunctionByName target rest
  where
    fnName =
      case fn of
        Function n _ _ -> n

findTypeInfo :: String -> [FunctionTypeInfo] -> Maybe FunctionTypeInfo
findTypeInfo _name [] = Nothing
findTypeInfo target (info:rest)
  | fnName info == target = Just info
  | otherwise = findTypeInfo target rest

compileSourceToCollapsed :: String -> Either String [CollapsedFunction]
compileSourceToCollapsed src = do
  parsed <- parseModule src
  lowered <- lowerModule parsed
  collapseAndVerifyModule lowered

compileFixtureModule :: [(FilePath, String)] -> FilePath -> IO (Either String BS.ByteString)
compileFixtureModule files entryFile = do
  root <- createTempModuleRoot files
  compileEntryModuleToWasm (root </> entryFile) `finally` removePathForcibly root

compileFixtureArtifact :: [(FilePath, String)] -> FilePath -> IO (Either String CompileArtifact)
compileFixtureArtifact files entryFile = do
  root <- createTempModuleRoot files
  compileEntryModule (root </> entryFile) `finally` removePathForcibly root

createTempModuleRoot :: [(FilePath, String)] -> IO FilePath
createTempModuleRoot files = do
  tempRoot <- getTemporaryDirectory
  (seedPath, handle) <- openTempFile tempRoot "clapse-module-fixture-"
  hClose handle
  removePathForcibly seedPath
  createDirectoryIfMissing True seedPath
  forM_ files $ \(relativePath, source) -> do
    let fullPath = seedPath </> relativePath
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFile fullPath source
  pure seedPath

hasRawCallClosure :: [CollapsedFunction] -> Bool
hasRawCallClosure = any fnHasRawCallClosure
  where
    fnHasRawCallClosure :: CollapsedFunction -> Bool
    fnHasRawCallClosure fn =
      any bindIsRawCallClosure (fnBinds fn)
        || any fnHasRawCallClosure (fnLifted fn)

    bindIsRawCallClosure :: Bind -> Bool
    bindIsRawCallClosure b =
      case value b of
        VCallClosure _ _ -> True
        _ -> False

    fnBinds :: CollapsedFunction -> [Bind]
    fnBinds (CollapsedFunction _ _ _ bs _ _) = bs

    fnLifted :: CollapsedFunction -> [CollapsedFunction]
    fnLifted (CollapsedFunction _ _ _ _ _ ls) = ls

moduleHasDirectCall :: String -> [CollapsedFunction] -> Bool
moduleHasDirectCall target = any fnHasDirectCall
  where
    fnHasDirectCall :: CollapsedFunction -> Bool
    fnHasDirectCall fn =
      any bindHasDirectCall (fnBinds fn)
        || any fnHasDirectCall (fnLifted fn)

    bindHasDirectCall :: Bind -> Bool
    bindHasDirectCall b =
      case value b of
        VCallDirect callee _ -> callee == target
        _ -> False

    fnBinds :: CollapsedFunction -> [Bind]
    fnBinds (CollapsedFunction _ _ _ bs _ _) = bs

    fnLifted :: CollapsedFunction -> [CollapsedFunction]
    fnLifted (CollapsedFunction _ _ _ _ _ ls) = ls

functionHasSelfTail :: CollapsedFunction -> Bool
functionHasSelfTail fn =
  any isSelfTail (getBinds fn)
    || any functionHasSelfTail (getLifted fn)
  where
    isSelfTail :: Bind -> Bool
    isSelfTail b =
      case value b of
        VSelfTailCall _ -> True
        _ -> False

    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

    getLifted :: CollapsedFunction -> [CollapsedFunction]
    getLifted (CollapsedFunction _ _ _ _ _ ls) = ls

regionOpCountsAndUsages :: CollapsedFunction -> (Int, Int, Int)
regionOpCountsAndUsages fn =
  let fnBindsList = fnBinds fn
      markBinds = filter isRegionMark fnBindsList
      resetBinds = filter isRegionReset fnBindsList
      markTemps = map (\b -> temp b) markBinds
      markUsages = length [() | Bind _ (VCallDirect "region_reset" [ATemp t]) <- resetBinds, t `elem` markTemps]
   in (length markBinds, length resetBinds, markUsages)
  where
    fnBinds :: CollapsedFunction -> [Bind]
    fnBinds (CollapsedFunction _ _ _ bs _ _) = bs

    isRegionMark :: Bind -> Bool
    isRegionMark b =
      case value b of
        VCallDirect "region_mark" _ -> True
        _ -> False

    isRegionReset :: Bind -> Bool
    isRegionReset b =
      case value b of
        VCallDirect "region_reset" _ -> True
        _ -> False
