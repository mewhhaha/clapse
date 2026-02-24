module Main (main) where

import qualified Data.ByteString as BS
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char (chr)
import Data.List (isInfixOf, isPrefixOf)
import Data.Word (Word32, Word8)
import Clapse.Modules
  ( CompileArtifact(..)
  , CompileDebugArtifact(..)
  , ExportApi(..)
  , compileEntryModule
  , compileEntryModuleDebug
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
  , FunctionAttribute(..)
  , FunctionAttributeValue(..)
  , FunctionAttributePlugin(..)
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
  , parseModuleWithPlugins
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
  , testParseFunctionAttributes
  , testParseFunctionBenchAttribute
  , testParseFunctionMemoAttributePluginValidation
  , testParseFunctionTestAttributePluginValidation
  , testParseFunctionBenchAttributePluginValidation
  , testParseModuleWithCustomAttributePlugin
  , testParseFunctionAttributesPropagateAcrossClauses
  , testParseOrphanFunctionAttributeFails
  , testParseLambda
  , testParseStringLiteral
  , testParseCharLiteral
  , testParseEscapedCharLiteral
  , testParseUnaryNegativeLiteral
  , testParseBinaryMinusUnaffectedByUnaryLiteralSupport
  , testParseCaseMultipleScrutinees
  , testParseCaseCharPattern
  , testParseCaseConstructorPattern
  , testParseCaseConstructorPatternMultiline
  , testParseCaseConstructorPatternExhaustiveSingleScrutinee
  , testParseCaseRequiresCatchAll
  , testParseLetExpression
  , testParseLetMultipleBindings
  , testParseLetFunctionBinding
  , testParseLetMultilineContinuation
  , testParseLetMultilineNoSemicolon
  , testParseLetMultilineOperatorContinuation
  , testParseFunctionGuards
  , testParseFunctionGuardsWithBuiltinOperatorConditions
  , testParseFunctionGuardsWithOperatorConditions
  , testParseNestedCaseChainFromKernelPainPoint
  , testParseMultilineParenthesizedApplicationFromKernelPainPoint
  , testParseInlineCaseDeclarationFromKernelPainPoint
  , testParseNestedCaseInCaseArmBody
  , testParseCaseArmMultilineLetContinuation
  , testParseFunctionPatternClauses
  , testParseDataDeclarationGeneratesFunctions
  , testParseNewtypeDeclarationGeneratesFunctions
  , testParseNewtypeRequiresSingleField
  , testParseDataDeclarationSupportsPascalCase
  , testParseDataDeclarationSupportsPrimitiveBackedSyntax
  , testParseDataDeclarationSupportsLiteralBackedConstructors
  , testParseDataDeclarationRejectsDuplicateLiteralBackedConstructors
  , testParseDataDeclarationSupportsMultiConstructors
  , testParseDataDeclarationGadtStyle
  , testParseBoolDataConstructors
  , testParseTypeAliasDeclaration
  , testParseTypeUnionLiteralMembers
  , testParseTypeUnionSignedIntMembers
  , testParseTypeUnionWithConstructors
  , testParseTypeUnionRejectsDuplicateLiterals
  , testParseTypeSignatureWithOptionalWitness
  , testParseTypeSignatureRequiresFunction
  , testParseCollectionLiteral
  , testParseRejectsDoNotation
  , testParseRejectsAdoNotation
  , testParseClassInstanceDeclarationsRewriteFunctions
  , testParseClassDeclarationRequiresLaws
  , testParseHKTInstanceArityMismatchFails
  , testParseClassDeclarationSupportsOrdAndSliceKinds
  , testParseBuiltinInfixPrecedence
  , testParseBuiltinEqRejectsChain
  , testParseBuiltinRelationalInfixPrecedence
  , testParseBuiltinRelationalRejectsChain
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
  , testFormatParseRoundtripNestedCasePainPoint
  , testFormatParseRoundtripMultilineApplicationPainPoint
  , testFormatParseRoundtripLongLetPainPoint
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
  , testSliceSetReadThenSetReusesBuffer
  , testSliceSetMultipleSetTargetsForceCopy
  , testCollapseInsertsFunctionRegionScopeForLocalAllocation
  , testCollapseSkipsFunctionRegionScopeForExplicitRegionOps
  , testCollapseSkipsFunctionRegionScopeForEscapingAllocation
  , testCollapseSkipsFunctionRegionScopeForEscapingConstructorAllocation
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
  , testInferSourceTypesBoolData
  , testInferSourceTypesLiteralUnionSignatures
  , testInferSourceTypesForOldStyleDataNoTypeParams
  , testInferSourceTypesStringLiteral
  , testInferSourceTypesStringSliceBridge
  , testInferSourceTypesSliceBuiltins
  , testInferSourceTypesSliceEqBuiltin
  , testInferSourceTypesLinearMemoryBuiltins
  , testInferSourceTypesCasePatternConstrainsScrutinee
  , testEvalSourceFunctionWithClosure
  , testEvalSourceFunctionWithCaseExpression
  , testEvalSourceFunctionWithCaseExpressionMultiline
  , testEvalSourceFunctionWithBoolData
  , testEvalSourceFunctionWithMaybeEitherMonads
  , testDifferentialDataAndNoDoSemantics
  , testDifferentialCaseExpressionSemantics
  , testDifferentialTaggedArithmeticWraparoundSemantics
  , testDifferentialDivModNegativeSemantics
  , testEvalCollapsedFunctionWithCurrying
  , testDifferentialMaybeEitherMonadsSemantics
  , testDifferentialSourceCollapsedSemantics
  , testCompileWasmInlinesNumericBuiltins
  , testCompileWasmModule
  , testCompileWasmRejectsTaggedIntLiteralOutOfRange
  , testCompileWasmAcceptsTaggedIntLiteralMaxBoundary
  , testCompileWasmAcceptsTaggedIntLiteralMinBoundary
  , testCompileWasmRejectsTaggedIntLiteralBelowMin
  , testCompileWasmRejectsTaggedIntLiteralOutOfRangeInBuiltinArgs
  , testCompileWasmSupportsMemoAttribute
  , testCompileWasmRejectsMemoOnNonUnaryFunction
  , testCompileWasmRejectsReservedRuntimeExportNames
  , testCompileWasmSupportsCaseExpressions
  , testCompileWasmSupportsCaseExpressionsMultiline
  , testCompileWasmSupportsCaseExpressionBranchThunkDirectCalls
  , testCompileWasmSupportsClosures
  , testCompileWasmSupportsClosuresWithManyCaptures
  , testCompileWasmSupportsMaybeEitherMonads
  , testCompileWasmSupportsCurrying
  , testCompileWasmSupportsDataAndNoDo
  , testCompileWasmSupportsBoolData
  , testCompileWasmSupportsStructWithManyFields
  , testCompileWasmSupportsCollectionLiterals
  , testCompileWasmSupportsStringLiterals
  , testCompileWasmSupportsUtf8StringLiterals
  , testCompileWasmSupportsSliceInteropImports
  , testCompileWasmSupportsSliceEqBuiltin
  , testCompileWasmSupportsSliceSetImport
  , testCompileWasmSupportsLinearMemoryHelpers
  , testCompileWasmStructHelpersInlined
  , testCompileWasmExportsHeapPtr
  , testCompileWasmSupportsHeapGlobalAtom
  , testCompileWasmRejectsHeapPtrAsUserFunction
  , testCompileWasmRejectsUnknownGlobalAtom
  , testBootstrapPhase1FrontendPrimitivesCompiles
  , testBootstrapPhase2CoreDataStructuresCompiles
  , testBootstrapPhase3ModuleGraphCompiles
  , testBootstrapPhase3ModuleGraphRejectsAmbiguousImports
  , testBootstrapPhase5DispatchPilotCompiles
  , testBootstrapPhase6ModuleDispatchCompiles
  , testBootstrapPhase7HostCapabilityCompiles
  , testBootstrapPhase8PatternAndOperatorsCompiles
  , testBootstrapPhase11ParserCombinatorHashAndStream
  , testBootstrapPhase11ParserCombinatorBadType
  , testBootstrapPhase11ParserCombinatorBadTypeUnion
  , testBootstrapPhase11ParserCombinatorSignedIntUnion
  , testBootstrapPhase11ParserCombinatorBadTypeUnionString
  , testParitySupportsTopLevelFunctionAsValue
  , testParityExampleCorpusCompiles
  , testParitySupportsStringCasePatterns
  , testSelfHostBlockerRejectsHostIoBuiltins
  , testCompileEntryModuleHostIoBuiltinImport
  , testCompileEntryModuleDedupesHostIoImport
  , testCompileEntryModuleHostTimeBuiltinImport
  , testCompileEntryModuleLoadsDottedImport
  , testCompileEntryModuleImportedConstructorPattern
  , testCompileEntryModuleAmbiguousConstructorImport
  , testCompileEntryModuleMissingImport
  , testCompileEntryModuleImportCycle
  , testCompileEntryModuleDefaultExports
  , testCompileEntryModuleExplicitExports
  , testCompileEntryModuleRejectsUnknownExport
  , testCompileEntryModuleArtifactExportsIncludeArity
  , testCompileEntryModuleDebugArtifactIncludesStages
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
              [ Function
                  { name = "id"
                  , args = ["x"]
                  , body = Var "x"
                  , attributes = []
                  }
              , Function
                  { name = "add2"
                  , args = ["x"]
                  , body = App (App (Var "add") (Var "x")) (IntLit 2)
                  , attributes = []
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
                  , attributes = []
                  }
              , Function
                  { name = "main"
                  , args = ["y"]
                  , body = App (Var "helper") (Var "y")
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse module with directives" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse module with directives" expected parsed

testParseFunctionAttributes :: IO Bool
testParseFunctionAttributes = do
  let src =
        unlines
          [ "#[memo 100]"
          , "#[test \"fibonacci memoized\"]"
          , "fib x = add x 1"
          ]
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "fib"
                  , args = ["x"]
                  , body = App (App (Var "add") (Var "x")) (IntLit 1)
                  , attributes =
                      [ FunctionAttribute
                          { attributeName = "memo"
                          , attributeValue = Just (AttributeInt 100)
                          }
                      , FunctionAttribute
                          { attributeName = "test"
                          , attributeValue = Just (AttributeString "fibonacci memoized")
                          }
                      ]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse function attributes" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse function attributes" expected parsed

testParseFunctionBenchAttribute :: IO Bool
testParseFunctionBenchAttribute = do
  let src =
        unlines
          [ "#[bench \"fib benchmark\"]"
          , "bench_fib n = fib n"
          ]
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "bench_fib"
                  , args = ["n"]
                  , body = App (Var "fib") (Var "n")
                  , attributes =
                      [ FunctionAttribute
                          { attributeName = "bench"
                          , attributeValue = Just (AttributeString "fib benchmark")
                          }
                      ]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse function bench attribute" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse function bench attribute" expected parsed

testParseFunctionMemoAttributePluginValidation :: IO Bool
testParseFunctionMemoAttributePluginValidation = do
  let src =
        unlines
          [ "#[memo \"bad\"]"
          , "fib x = add x 1"
          ]
  case parseModule src of
    Left err ->
      assertTrue
        "parse function memo attribute validation"
        ("memo attribute requires a non-negative integer size" `isInfixOf` err)
    Right parsed ->
      failTest
        "parse function memo attribute validation"
        ("unexpected parse success: " <> show parsed)

testParseFunctionTestAttributePluginValidation :: IO Bool
testParseFunctionTestAttributePluginValidation = do
  let src =
        unlines
          [ "#[test 100]"
          , "fib x = add x 1"
          ]
  case parseModule src of
    Left err ->
      assertTrue
        "parse function test attribute validation"
        ("test attribute requires a string label" `isInfixOf` err)
    Right parsed ->
      failTest
        "parse function test attribute validation"
        ("unexpected parse success: " <> show parsed)

testParseFunctionBenchAttributePluginValidation :: IO Bool
testParseFunctionBenchAttributePluginValidation = do
  let src =
        unlines
          [ "#[bench 100]"
          , "fib x = add x 1"
          ]
  case parseModule src of
    Left err ->
      assertTrue
        "parse function bench attribute validation"
        ("bench attribute requires a string label" `isInfixOf` err)
    Right parsed ->
      failTest
        "parse function bench attribute validation"
        ("unexpected parse success: " <> show parsed)

testParseModuleWithCustomAttributePlugin :: IO Bool
testParseModuleWithCustomAttributePlugin = do
  let labelPlugin =
        FunctionAttributePlugin
          { pluginName = "label"
          , pluginApply = \fn attr ->
              case attributeValue attr of
                Just (AttributeString label) ->
                  case fn of
                    Function _fnName args0 body0 attrs ->
                      Right
                        (Function
                          { name = _fnName ++ "_" ++ label
                          , args = args0
                          , body = body0
                          , attributes = attrs
                          })
                _ ->
                  Left "label requires string value"
          }
      src =
        unlines
          [ "#[label \"trace\"]"
          , "step n = mul n 2"
          ]
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "step_trace"
                  , args = ["n"]
                  , body = App (App (Var "mul") (Var "n")) (IntLit 2)
                  , attributes =
                      [ FunctionAttribute
                          { attributeName = "label"
                          , attributeValue = Just (AttributeString "trace")
                          }
                      ]
                  }
              ]
          }
  case parseModuleWithPlugins [labelPlugin] src of
    Left err ->
      failTest "parse with custom attribute plugin" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse with custom attribute plugin" expected parsed

testParseFunctionAttributesPropagateAcrossClauses :: IO Bool
testParseFunctionAttributesPropagateAcrossClauses = do
  let src =
        unlines
          [ "#[memo 5]"
          , "count 0 = 1"
          , "count n = n"
          ]
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "count"
                  , args = ["n"]
                  , body = Case [Var "n"]
                      [ CaseArm
                          { armPatterns = [PatInt 0]
                          , armBody = IntLit 1
                          }
                      , CaseArm
                          { armPatterns = [PatVar "n"]
                          , armBody = Var "n"
                          }
                      ]
                  , attributes =
                      [ FunctionAttribute
                          { attributeName = "memo"
                          , attributeValue = Just (AttributeInt 5)
                          }
                      ]
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse function attributes propagate across clauses" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse function attributes propagate across clauses" expected parsed

testParseOrphanFunctionAttributeFails :: IO Bool
testParseOrphanFunctionAttributeFails = do
  let src = "#[memo 4]"
  case parseModule src of
    Left err ->
      assertTrue
        "parse orphan function attribute fails"
        ( "orphaned attribute" `isInfixOf` err
            || "attribute must annotate a function declaration" `isInfixOf` err
        )
    Right parsed ->
      failTest "parse orphan function attribute fails" ("unexpected parse success: " <> show parsed)

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

testFormatParseRoundtripNestedCasePainPoint :: IO Bool
testFormatParseRoundtripNestedCasePainPoint =
  assertFormatParseRoundtrip
    "format/parse roundtrip for nested case pain point"
    ( unlines
        [ "dispatch marker = case (marker == 99) of"
        , "  1 -> 1"
        , "  _ -> case (marker == 102) of"
        , "    1 -> 2"
        , "    _ -> case (marker == 115) of"
        , "      1 -> 3"
        , "      _ -> 0"
        ]
    )

testFormatParseRoundtripMultilineApplicationPainPoint :: IO Bool
testFormatParseRoundtripMultilineApplicationPainPoint =
  assertFormatParseRoundtrip
    "format/parse roundtrip for multiline parenthesized application pain point"
    ( unlines
        [ "main x ="
        , "  add"
        , "    (add x 1)"
        , "    (add x 2)"
        ]
    )

testFormatParseRoundtripLongLetPainPoint :: IO Bool
testFormatParseRoundtripLongLetPainPoint =
  assertFormatParseRoundtrip
    "format/parse roundtrip for long chained let pain point"
    "main x = let half = div x 2; left = add half 1; right = add half 2 in add left right\n"

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
                  , attributes = []
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
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse string literals" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse string literals" expected parsed

testParseCharLiteral :: IO Bool
testParseCharLiteral = do
  let src = "main = 'A'"
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "main"
                  , args = []
                  , body = IntLit 65
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse char literal" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse char literal" expected parsed

testParseEscapedCharLiteral :: IO Bool
testParseEscapedCharLiteral = do
  let src = "main = '\\n'"
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "main"
                  , args = []
                  , body = IntLit 10
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse escaped char literal" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse escaped char literal" expected parsed

testParseUnaryNegativeLiteral :: IO Bool
testParseUnaryNegativeLiteral = do
  let src = "main x = x + -1"
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body = App (App (Var "add") (Var "x")) (IntLit (-1))
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse unary negative literal" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse unary negative literal" expected parsed

testParseBinaryMinusUnaffectedByUnaryLiteralSupport :: IO Bool
testParseBinaryMinusUnaffectedByUnaryLiteralSupport = do
  let src = "main x = x - 1"
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "main"
                  , args = ["x"]
                  , body = App (App (Var "sub") (Var "x")) (IntLit 1)
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse binary minus unaffected by unary literal support" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse binary minus unaffected by unary literal support" expected parsed

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
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse case expression with multiple scrutinees" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse case expression with multiple scrutinees" expected parsed

testParseCaseCharPattern :: IO Bool
testParseCaseCharPattern = do
  let src = "main c = case c of 'a' -> 1; _ -> 0"
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "main"
                  , args = ["c"]
                  , body =
                      Case
                        [Var "c"]
                        [ CaseArm {armPatterns = [PatInt 97], armBody = IntLit 1}
                        , CaseArm {armPatterns = [PatWildcard], armBody = IntLit 0}
                        ]
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse case char pattern" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse case char pattern" expected parsed

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
                  , attributes = []
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
                  , attributes = []
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
                  , attributes = []
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
                  , attributes = []
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
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse let multiline continuation without semicolons" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse let multiline continuation without semicolons" expected parsed

testParseLetMultilineOperatorContinuation :: IO Bool
testParseLetMultilineOperatorContinuation = do
  let src =
        unlines
          [ "infixl 3 <|> = or_else"
          , "main x = let p = eq x 0"
          , "  <|> eq x 1"
          , "  in p"
          ]
  case parseModule src of
    Left err ->
      failTest "parse let multiline operator continuation" ("unexpected parse error: " <> err)
    Right _parsed ->
      assertTrue "parse let multiline operator continuation" True

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
                      App
                        ( App
                            ( App
                                (Var "if")
                                (App (App (Var "eq") (Var "x")) (IntLit 0))
                            )
                            (Lam "__guard_dummy" (IntLit 0))
                        )
                        (Lam "__guard_dummy" (App (App (Var "add") (Var "x")) (Var "y")))
                  , attributes = []
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
                      App
                        ( App
                            ( App
                                (Var "if")
                                ( App
                                    (App (Var "and") (App (App (Var "eq") (Var "x")) (IntLit 0)))
                                    (App (App (Var "eq") (Var "y")) (IntLit 0))
                                )
                            )
                            (Lam "__guard_dummy" (IntLit 0))
                        )
                        (Lam "__guard_dummy" (App (App (Var "add") (Var "x")) (Var "y")))
                  , attributes = []
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
                      App
                        ( App
                            ( App
                                (Var "if")
                                ( App
                                    (App (Var "and") (App (App (Var "eq") (Var "x")) (IntLit 0)))
                                    (App (App (Var "eq") (Var "y")) (IntLit 0))
                                )
                            )
                            (Lam "__guard_dummy" (IntLit 0))
                        )
                        (Lam "__guard_dummy" (App (App (Var "add") (Var "x")) (Var "y")))
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse guarded function declarations with operators" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse guarded function declarations with operators" expected parsed

testParseNestedCaseChainFromKernelPainPoint :: IO Bool
testParseNestedCaseChainFromKernelPainPoint = do
  let src =
        unlines
          [ "dispatch marker = case (marker == 99) of"
          , "  1 -> 1"
          , "  _ -> case (marker == 102) of"
          , "    1 -> 2"
          , "    _ -> case (marker == 115) of"
          , "      1 -> 3"
          , "      _ -> 0"
          ]
  case parseModule src of
    Left err ->
      failTest "parse nested case chain from kernel pain point" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "dispatch" (functions parsed) of
        Just _ ->
          passTest "parse nested case chain from kernel pain point"
        Nothing ->
          failTest "parse nested case chain from kernel pain point" "missing function: dispatch"

testParseMultilineParenthesizedApplicationFromKernelPainPoint :: IO Bool
testParseMultilineParenthesizedApplicationFromKernelPainPoint = do
  let src =
        unlines
          [ "main x ="
          , "  add"
          , "    (add x 1)"
          , "    (add x 2)"
          ]
  case parseModule src of
    Left err ->
      failTest "parse multiline parenthesized application from kernel pain point" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "main" (functions parsed) of
        Just _ ->
          passTest "parse multiline parenthesized application from kernel pain point"
        Nothing ->
          failTest "parse multiline parenthesized application from kernel pain point" "missing function: main"

testParseInlineCaseDeclarationFromKernelPainPoint :: IO Bool
testParseInlineCaseDeclarationFromKernelPainPoint = do
  let src =
        unlines
          [ "step_range in_cells out_cells w h start span = case (eq span 0) (eq span 1) of 1 _ -> out_cells; _ 1 -> slice_set_u8 out_cells start (slice_get_u8 in_cells start); _ _ -> out_cells"
          ]
  case parseModule src of
    Left err ->
      failTest "parse inline case declaration from kernel pain point" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "step_range" (functions parsed) of
        Just _ ->
          passTest "parse inline case declaration from kernel pain point"
        Nothing ->
          failTest "parse inline case declaration from kernel pain point" "missing function: step_range"

testParseNestedCaseInCaseArmBody :: IO Bool
testParseNestedCaseInCaseArmBody = do
  let src =
        unlines
          [ "f x y = case (x == 0) (y == 0) of 1 1 -> case (x == y) of 1 -> 1; _ -> 2; _ _ -> 0"
          ]
  case parseModule src of
    Left err ->
      failTest "parse nested case in case arm body" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "f" (functions parsed) of
        Just _ ->
          passTest "parse nested case in case arm body"
        Nothing ->
          failTest "parse nested case in case arm body" "missing function: f"

testParseCaseArmMultilineLetContinuation :: IO Bool
testParseCaseArmMultilineLetContinuation = do
  let src =
        unlines
          [ "f x y = case (x == 0) (y == 0) of 1 1 -> let a = add x 1;"
          , "  b = add y 2"
          , "  in add a b"
          , "  _ _ -> 0"
          ]
  case parseModule src of
    Left err ->
      failTest "parse case arm multiline let continuation" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "f" (functions parsed) of
        Just _ ->
          passTest "parse case arm multiline let continuation"
        Nothing ->
          failTest "parse case arm multiline let continuation" "missing function: f"

testParseFunctionPatternClauses :: IO Bool
testParseFunctionPatternClauses = do
  let src =
        unlines
          [ "sum2 0 y = y"
          , "sum2 x y = add x y"
          ]
      expected =
        Module
          { signatures = []
          , functions =
              [ Function
                  { name = "sum2"
                  , args = ["x", "y"]
                  , body =
                      Case
                        [ Var "x", Var "y" ]
                        [ CaseArm
                            { armPatterns = [PatInt 0, PatVar "y"]
                            , armBody = Var "y"
                            }
                        , CaseArm
                            { armPatterns = [PatVar "x", PatVar "y"]
                            , armBody = App (App (Var "add") (Var "x")) (Var "y")
                            }
                        ]
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse function pattern clauses" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse function pattern clauses" expected parsed

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
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse data declaration generates functions" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse data declaration generates functions" expected parsed

testParseNewtypeDeclarationGeneratesFunctions :: IO Bool
testParseNewtypeDeclarationGeneratesFunctions = do
  let src = "newtype UserId = UserId i64"
  case parseModule src of
    Left err ->
      failTest "parse newtype declaration generates functions" ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "UserId" (functions parsed) of
        Just Function {args = ctorArgs, body = ctorBody} ->
          case ctorBody of
            App (Var ctorBuiltin) (Var argName) ->
              assertTrue
                "parse newtype declaration generates functions"
                (ctorArgs == ["i64"] && argName == "i64" && "__mk_UserId" `isPrefixOf` ctorBuiltin)
            _ ->
              failTest "parse newtype declaration generates functions" ("unexpected constructor body: " <> show ctorBody)
        _ ->
          failTest "parse newtype declaration generates functions" "missing function: UserId"

testParseNewtypeRequiresSingleField :: IO Bool
testParseNewtypeRequiresSingleField = do
  let src =
        unlines
          [ "newtype Bad = Bad i64 i64"
          , "main x = x"
          ]
  case parseModule src of
    Left err ->
      assertTrue
        "parse newtype declaration requires single field"
        ("newtype declaration" `isInfixOf` err)
    Right parsed ->
      failTest "parse newtype declaration requires single field" ("unexpected parse success: " <> show parsed)

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
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse data declaration supports PascalCase names" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse data declaration supports PascalCase names" expected parsed

testParseDataDeclarationSupportsPrimitiveBackedSyntax :: IO Bool
testParseDataDeclarationSupportsPrimitiveBackedSyntax = do
  let src = "data string = string<slice byte>"
  case parseModule src of
    Left err ->
      failTest
        "parse data declaration with primitive-backed lowercase syntax"
        ("unexpected parse error: " <> err)
    Right parsed ->
      case findFunctionByName "string" (functions parsed) of
        Just Function {args = ctorArgs, body = ctorBody} ->
          case ctorBody of
            App (Var ctorBuiltin) (Var argName) ->
              assertTrue
                "parse data declaration with primitive-backed lowercase syntax"
                (ctorArgs == ["__ctor_field_0"] && argName == "__ctor_field_0" && ctorBuiltin == "__mk_string#string_1_tpar_0_fmap_u")
            _ ->
              failTest
                "parse data declaration with primitive-backed lowercase syntax"
                ("unexpected constructor body: " <> show ctorBody)
        Nothing ->
          failTest
            "parse data declaration with primitive-backed lowercase syntax"
            "missing function: string"

testParseDataDeclarationSupportsLiteralBackedConstructors :: IO Bool
testParseDataDeclarationSupportsLiteralBackedConstructors = do
  let src =
        unlines
          [ "data bool = true<1> | false<0>"
          , "to_i64 b = case b of"
          , "  true -> 1"
          , "  false -> 0"
          ]
  case parseModule src of
    Left err ->
      failTest
        "parse data declaration with literal-backed constructors"
        ("unexpected parse error: " <> err)
    Right parsed ->
      case
        ( findFunctionByName "true" (functions parsed)
        , findFunctionByName "false" (functions parsed)
        , findFunctionByName "to_i64" (functions parsed)
        ) of
        ( Just Function {args = trueArgs, body = trueBody}
          , Just Function {args = falseArgs, body = falseBody}
          , Just Function {body = Case [_] arms}
          ) ->
            assertTrue "parse data declaration with literal-backed constructors" $
              null trueArgs
                && null falseArgs
                && trueBody == IntLit 1
                && falseBody == IntLit 0
                && case arms of
                  [ CaseArm {armPatterns = [PatConstructor ctorTrue _ []], armBody = IntLit 1}
                    , CaseArm {armPatterns = [PatConstructor ctorFalse _ []], armBody = IntLit 0}
                    ] ->
                      ctorTrue == "true" && ctorFalse == "false"
                  _ -> False
        _ ->
          failTest
            "parse data declaration with literal-backed constructors"
            "missing true/false/to_i64 functions"

testParseDataDeclarationRejectsDuplicateLiteralBackedConstructors :: IO Bool
testParseDataDeclarationRejectsDuplicateLiteralBackedConstructors = do
  let src = "data bool = true<1> | false<0> | maybe_true<1>"
  case parseModule src of
    Left err ->
      assertTrue
        "parse data declaration rejects duplicate literal-backed constructors"
        ("duplicate literal-backed constructor value" `isInfixOf` err)
    Right parsed ->
      failTest
        "parse data declaration rejects duplicate literal-backed constructors"
        ("unexpected parse success: " <> show parsed)

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

testParseBoolDataConstructors :: IO Bool
testParseBoolDataConstructors = do
  let src =
        unlines
          [ "data bool = true<1> | false<0>"
          , "to_i64 b = case b of"
          , "  true -> 1"
          , "  false -> 0"
          ]
  case parseModule src of
    Left err ->
      failTest "parse Bool data constructors" ("unexpected parse error: " <> err)
    Right parsed ->
      case
        ( findFunctionByName "true" (functions parsed)
        , findFunctionByName "false" (functions parsed)
        , findFunctionByName "to_i64" (functions parsed)
        ) of
        ( Just Function {args = trueArgs}
          , Just Function {args = falseArgs}
          , Just Function {body = Case [_] arms}
          ) ->
            assertTrue "parse Bool data constructors" $
              null trueArgs
                && null falseArgs
                && case arms of
                  [ CaseArm {armPatterns = [PatConstructor ctorTrue _ []], armBody = IntLit 1}
                    , CaseArm {armPatterns = [PatConstructor ctorFalse _ []], armBody = IntLit 0}
                    ] ->
                      ctorTrue == "true" && ctorFalse == "false"
                  _ -> False
        _ ->
          failTest "parse Bool data constructors" "missing true/false/to_i64 functions"

testParseTypeAliasDeclaration :: IO Bool
testParseTypeAliasDeclaration = do
  let src = "type Digit = <0 | 1 | 2>"
  case parseModule src of
    Left err ->
      failTest "parse type alias declaration" ("unexpected parse error: " <> err)
    Right parsed ->
      assertTrue
        "parse type alias declaration"
        (null (signatures parsed) && null (functions parsed))

testParseTypeUnionLiteralMembers :: IO Bool
testParseTypeUnionLiteralMembers = do
  let src = "type Digit = <0 | 1 | \"2\">"
  case parseModule src of
    Left err ->
      failTest "parse type union with literal members" ("unexpected parse error: " <> err)
    Right parsed ->
      assertTrue
        "parse type union with literal members"
        (null (signatures parsed) && null (functions parsed))

testParseTypeUnionSignedIntMembers :: IO Bool
testParseTypeUnionSignedIntMembers = do
  let src = "type Signed = <-1 | 0 | 1>"
  case parseModule src of
    Left err ->
      failTest "parse type union with signed literal members" ("unexpected parse error: " <> err)
    Right parsed ->
      assertTrue
        "parse type union with signed literal members"
        (null (signatures parsed) && null (functions parsed))

testParseTypeUnionWithConstructors :: IO Bool
testParseTypeUnionWithConstructors = do
  let src = "type Boolish = <true<\"true\"> | false<\"false\">>"
  case parseModule src of
    Left err ->
      assertTrue
        "parse type union with constructor members"
        ("expected literal union member" `isInfixOf` err)
    Right parsed ->
      failTest
        "parse type union with constructor members"
        ("unexpected parse success: " <> show parsed)

testParseTypeUnionRejectsDuplicateLiterals :: IO Bool
testParseTypeUnionRejectsDuplicateLiterals = do
  let src = "type Digit = <0 | 1 | \"2\" | 1>"
  case parseModule src of
    Left err ->
      assertTrue
        "parse type union rejects duplicate literals"
        ("duplicate literal in type union" `isInfixOf` err)
    Right parsed ->
      failTest
        "parse type union rejects duplicate literals"
        ("unexpected parse success: " <> show parsed)

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
                  , attributes = []
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
                  , attributes = []
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
                  , attributes = []
                  }
              , Function
                  { name = "main"
                  , args = ["x"]
                  , body = Var "x"
                  , attributes = []
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

testParseClassDeclarationSupportsOrdAndSliceKinds :: IO Bool
testParseClassDeclarationSupportsOrdAndSliceKinds = do
  let src =
        unlines
          [ "class ord_i64_rules t : ord"
          , "law ord_i64_rules irreflexive_lt = lt x x => 0"
          , "law ord_i64_rules reflexive_le = le x x => 1"
          , "law ord_i64_rules reflexive_ge = ge x x => 1"
          , "law ord_i64_rules antisymmetry = and (le x y) (le y x) => eq x y"
          , "law ord_i64_rules duality = gt x y => lt y x"
          , "instance ord_i64 : ord_i64_rules i64 lt=lt le=le gt=gt ge=ge eq=eq"
          , "class slice_byte_rules s : slice"
          , "law slice_byte_rules set_preserves_len = slice_len (slice_set_u8 s i v) => slice_len s"
          , "law slice_byte_rules get_after_set_same_index = slice_get_u8 (slice_set_u8 s i v) i => v"
          , "instance slice_byte : slice_byte_rules byte slice_len=slice_len slice_get_u8=slice_get_u8 slice_set_u8=slice_set_u8"
          , "main x y = and (lt x y) (ge y x)"
          ]
  case parseModule src of
    Left err ->
      failTest "parse class declarations supports ord and slice kinds" ("unexpected parse error: " <> err)
    Right _parsed ->
      passTest "parse class declarations supports ord and slice kinds"

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
                  , attributes = []
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

testParseBuiltinRelationalInfixPrecedence :: IO Bool
testParseBuiltinRelationalInfixPrecedence = do
  let src = "main x y z = x < y && y >= z"
      expected =
        Module
          { signatures = [], functions =
              [ Function
                  { name = "main"
                  , args = ["x", "y", "z"]
                  , body =
                      App
                        ( App
                            (Var "and")
                            (App (App (Var "lt") (Var "x")) (Var "y"))
                        )
                        (App (App (Var "ge") (Var "y")) (Var "z"))
                  , attributes = []
                  }
              ]
          }
  case parseModule src of
    Left err ->
      failTest "parse builtin relational infix precedence" ("unexpected parse error: " <> err)
    Right parsed ->
      assertEqual "parse builtin relational infix precedence" expected parsed

testParseBuiltinRelationalRejectsChain :: IO Bool
testParseBuiltinRelationalRejectsChain = do
  let src = "main x y z = x < y < z"
  case parseModule src of
    Left err ->
      assertTrue
        "parse builtin non-associative relational rejects chains"
        ("non-associative operator cannot be chained" `isInfixOf` err)
    Right parsed ->
      failTest "parse builtin non-associative relational rejects chains" ("unexpected parse success: " <> show parsed)

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
                  , attributes = []
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
                  , attributes = []
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
                  , attributes = []
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
                  , attributes = []
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
                  , attributes = []
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
          , attributes = []
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
          , attributes = []
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
          , attributes = []
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
          , attributes = []
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
          , attributes = []
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
          , attributes = []
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
          , attributes = []
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
          , attributes = []
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
                  , attributes = []
                  }
              , Function
                  { name = "inc"
                  , args = ["x"]
                  , body = App (Var "add") (Var "x")
                  , attributes = []
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
                  , attributes = []
                  }
              , Function
                  { name = "boom"
                  , args = ["x"]
                  , body =
                      App
                        (App (App (Var "add") (Var "x")) (IntLit 1))
                        (IntLit 2)
                  , attributes = []
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

testSliceSetReadThenSetReusesBuffer :: IO Bool
testSliceSetReadThenSetReusesBuffer = do
  let src =
        unlines
          [ "main idx a b = let s0 = slice_new_u8 16"
          , "  s1 = slice_set_u8 s0 idx a"
          , "  seen = slice_get_u8 s1 idx"
          , "  s2 = slice_set_u8 s1 idx b"
          , "  in add seen (slice_get_u8 s2 idx)"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "slice_set read then set reuses buffer" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "slice_set read then set reuses buffer" "expected function main in collapsed output"
        Just fn ->
          let vals = map value (getBinds fn)
              memcpyCount = length (filter isMemcpy vals)
              sliceSetCount = length (filter isSliceSet vals)
           in assertTrue
                "slice_set read then set reuses buffer"
                (memcpyCount == 0 && sliceSetCount == 2)
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

    isMemcpy :: Value -> Bool
    isMemcpy val =
      case val of
        VCallDirect "memcpy_u8" _ -> True
        _ -> False

    isSliceSet :: Value -> Bool
    isSliceSet val =
      case val of
        VCallDirect "slice_set_u8" _ -> True
        _ -> False

testSliceSetMultipleSetTargetsForceCopy :: IO Bool
testSliceSetMultipleSetTargetsForceCopy = do
  let src =
        unlines
          [ "main idx a b c = let s0 = slice_new_u8 16"
          , "  s1 = slice_set_u8 s0 idx a"
          , "  s2 = slice_set_u8 s1 idx b"
          , "  s3 = slice_set_u8 s1 idx c"
          , "  in add (slice_get_u8 s2 idx) (slice_get_u8 s3 idx)"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest "slice_set multiple set targets force copy" ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "main" collapsed of
        Nothing ->
          failTest "slice_set multiple set targets force copy" "expected function main in collapsed output"
        Just fn ->
          let vals = map value (getBinds fn)
              memcpyCount = length (filter isMemcpy vals)
           in assertTrue
                "slice_set multiple set targets force copy"
                (memcpyCount >= 1)
  where
    getBinds :: CollapsedFunction -> [Bind]
    getBinds (CollapsedFunction _ _ _ bs _ _) = bs

    isMemcpy :: Value -> Bool
    isMemcpy val =
      case val of
        VCallDirect "memcpy_u8" _ -> True
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

testCollapseSkipsFunctionRegionScopeForEscapingConstructorAllocation :: IO Bool
testCollapseSkipsFunctionRegionScopeForEscapingConstructorAllocation = do
  let src =
        unlines
          [ "data Method = Get i64 | Put i64 | UnknownMethod i64"
          , "parse_method code = case code of"
          , "  1 -> Get 0"
          , "  2 -> Put 0"
          , "  _ -> UnknownMethod 0"
          , "main code = parse_method code"
          ]
  case compileSourceToCollapsed src of
    Left err ->
      failTest
        "collapse skips function region scope for escaping constructor allocation"
        ("unexpected pipeline error: " <> err)
    Right collapsed ->
      case findCollapsed "parse_method" collapsed of
        Nothing ->
          failTest
            "collapse skips function region scope for escaping constructor allocation"
            "expected function parse_method in collapsed output"
        Just fn ->
          let (markCount, resetCount, _markUsages) = regionOpCountsAndUsages fn
           in assertTrue
                "collapse skips function region scope for escaping constructor allocation"
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
  assertTrue
    "trait catalog has core traits"
    (all (`elem` names) ["add", "sub", "mul", "div", "ord", "slice", "monoid", "functor", "applicative", "monad"])

testTraitCategoriesAvailable :: IO Bool
testTraitCategoriesAvailable = do
  let arithmeticCount = length (traitsByCategory ArithmeticCategory basicTraits)
      ordCount = length (traitsByCategory OrdCategory basicTraits)
      sliceCount = length (traitsByCategory SliceCategory basicTraits)
      monadCount = length (traitsByCategory MonadCategory basicTraits)
  assertTrue "trait categories filter traits" (arithmeticCount >= 4 && ordCount >= 1 && sliceCount >= 1 && monadCount >= 1)

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

testInferSourceTypesBoolData :: IO Bool
testInferSourceTypesBoolData = do
  let src =
        unlines
          [ "data bool = true<1> | false<0>"
          , "to_i64 b = case b of"
          , "  true -> 1"
          , "  false -> 0"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for Bool data" ("unexpected inference error: " <> err)
    Right infos ->
      case findTypeInfo "to_i64" infos of
        Just toI64Info ->
          let inferred = renderType (fnType toI64Info)
           in assertEqual "infer source types for Bool data" "bool -> i64" inferred
        Nothing ->
          failTest "infer source types for Bool data" "missing inferred type for to_i64"

testInferSourceTypesLiteralUnionSignatures :: IO Bool
testInferSourceTypesLiteralUnionSignatures = do
  let src =
        unlines
          [ "signed_id : <-1 | 0 | 1> -> <-1 | 0 | 1>"
          , "signed_id n = n"
          , "switch : <\"on\" | \"off\"> -> <\"on\" | \"off\">"
          , "switch state = state"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for literal-union signatures" ("unexpected inference error: " <> err)
    Right infos ->
      case (findTypeInfo "signed_id" infos, findTypeInfo "switch" infos) of
        (Just signedInfo, Just switchInfo) ->
          let signedType = renderType (fnType signedInfo)
              switchType = renderType (fnType switchInfo)
           in assertTrue
                "infer source types for literal-union signatures"
                (signedType == "<-1 | 0 | 1> -> <-1 | 0 | 1>" && switchType == "<\"on\" | \"off\"> -> <\"on\" | \"off\">")
        _ ->
          failTest "infer source types for literal-union signatures" "missing inferred type for expected function(s)"

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

testInferSourceTypesStringSliceBridge :: IO Bool
testInferSourceTypesStringSliceBridge = do
  let src =
        unlines
          [ "to_bytes s = str_to_slice s"
          , "to_string bytes = slice_to_string bytes"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for string/slice bridge builtins" ("unexpected inference error: " <> err)
    Right infos ->
      case (findTypeInfo "to_bytes" infos, findTypeInfo "to_string" infos) of
        (Just toBytesInfo, Just toStringInfo) ->
          let toBytesType = renderType (fnType toBytesInfo)
              toStringType = renderType (fnType toStringInfo)
           in assertTrue
                "infer source types for string/slice bridge builtins"
                (toBytesType == "string -> slice byte" && toStringType == "slice byte -> string")
        _ ->
          failTest "infer source types for string/slice bridge builtins" "missing inferred type for expected function(s)"

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

testInferSourceTypesSliceEqBuiltin :: IO Bool
testInferSourceTypesSliceEqBuiltin = do
  let src =
        unlines
          [ "same bytes = slice_eq_u8 bytes bytes"
          ]
  case inferSourceTypes src of
    Left err ->
      failTest "infer source types for slice_eq_u8 builtin" ("unexpected inference error: " <> err)
    Right infos ->
      case findTypeInfo "same" infos of
        Just sameInfo ->
          let sameType = renderType (fnType sameInfo)
           in assertEqual "infer source types for slice_eq_u8 builtin" "slice byte -> bool" sameType
        Nothing ->
          failTest "infer source types for slice_eq_u8 builtin" "missing inferred type for same"

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

testEvalSourceFunctionWithBoolData :: IO Bool
testEvalSourceFunctionWithBoolData = do
  let src =
        unlines
          [ "data bool = true<1> | false<0>"
          , "to_i64 b = case b of"
          , "  true -> 1"
          , "  false -> 0"
          , "main = add (to_i64 true) (to_i64 false)"
          ]
  case parseModule src of
    Left err ->
      failTest "eval source function with Bool data" ("unexpected parse error: " <> err)
    Right modu ->
      case evalSourceFunction modu "main" [] of
        Left err ->
          failTest "eval source function with Bool data" ("unexpected eval error: " <> err)
        Right out ->
          assertEqual "eval source function with Bool data" 1 out

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

testDifferentialTaggedArithmeticWraparoundSemantics :: IO Bool
testDifferentialTaggedArithmeticWraparoundSemantics = do
  let src = "main x = add x 1"
      payloadMax = 1073741823
      payloadMin = -1073741824
  case parseModule src of
    Left err ->
      failTest "differential tagged arithmetic wraparound semantics" ("unexpected parse error: " <> err)
    Right modu -> do
      diffOk <- case differentialCheckSourceCollapsed modu "main" [payloadMax] of
        Left err ->
          failTest "differential tagged arithmetic wraparound semantics" err
        Right () ->
          pure True
      if not diffOk
        then pure False
        else do
          sourceOk <- case evalSourceFunction modu "main" [payloadMax] of
            Left err ->
              failTest "differential tagged arithmetic wraparound semantics" ("unexpected source eval error: " <> err)
            Right out ->
              assertEqual "differential tagged arithmetic wraparound semantics (source)" payloadMin out
          if not sourceOk
            then pure False
            else
              case parseModule src >>= lowerModule >>= collapseAndVerifyModule of
                Left err ->
                  failTest "differential tagged arithmetic wraparound semantics" ("unexpected collapse error: " <> err)
                Right collapsed ->
                  case evalCollapsedFunction collapsed "main" [payloadMax] of
                    Left err ->
                      failTest "differential tagged arithmetic wraparound semantics" ("unexpected collapsed eval error: " <> err)
                    Right out ->
                      assertEqual "differential tagged arithmetic wraparound semantics (collapsed)" payloadMin out

testDifferentialDivModNegativeSemantics :: IO Bool
testDifferentialDivModNegativeSemantics = do
  let src =
        unlines
          [ "main x = add (mul (div (sub 0 x) 10) 100) (mod (sub 0 x) 10)"
          ]
      inputs = [0 .. 32]
  case parseModule src of
    Left err ->
      failTest "differential div/mod negative semantics" ("unexpected parse error: " <> err)
    Right modu ->
      case go modu inputs of
        Left err ->
          failTest "differential div/mod negative semantics" err
        Right () ->
          passTest "differential div/mod negative semantics"
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
          [ "main x = and (eq (add (mul x 2) (sub x 3)) (div x 1)) (eq (mod x 5) (mod x 5))"
          ]
      removedRuntimeFns =
        [ "rt_i32_const"
        , "rt_add"
        , "rt_sub"
        , "rt_mul"
        , "rt_div"
        , "rt_mod"
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

testCompileWasmRejectsTaggedIntLiteralOutOfRange :: IO Bool
testCompileWasmRejectsTaggedIntLiteralOutOfRange = do
  let src =
        unlines
          [ "main _ = 1073741824"
          ]
  case compileSourceToWasm src of
    Left err ->
      assertTrue
        "compile wasm rejects tagged int literal out of range"
        ("tagged i32 payload out of range" `isInfixOf` err)
    Right _ ->
      failTest
        "compile wasm rejects tagged int literal out of range"
        "expected out-of-range tagged literal compile failure"

testCompileWasmAcceptsTaggedIntLiteralMaxBoundary :: IO Bool
testCompileWasmAcceptsTaggedIntLiteralMaxBoundary = do
  let src =
        unlines
          [ "main _ = 1073741823"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest
        "compile wasm accepts tagged int literal max boundary"
        ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm accepts tagged int literal max boundary"
        (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmAcceptsTaggedIntLiteralMinBoundary :: IO Bool
testCompileWasmAcceptsTaggedIntLiteralMinBoundary = do
  let src =
        unlines
          [ "main _ = -1073741824"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest
        "compile wasm accepts tagged int literal min boundary"
        ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm accepts tagged int literal min boundary"
        (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmRejectsTaggedIntLiteralBelowMin :: IO Bool
testCompileWasmRejectsTaggedIntLiteralBelowMin = do
  let src =
        unlines
          [ "main _ = -1073741825"
          ]
  case compileSourceToWasm src of
    Left err ->
      assertTrue
        "compile wasm rejects tagged int literal below min"
        ("tagged i32 payload out of range" `isInfixOf` err)
    Right _ ->
      failTest
        "compile wasm rejects tagged int literal below min"
        "expected below-min tagged literal compile failure"

testCompileWasmRejectsTaggedIntLiteralOutOfRangeInBuiltinArgs :: IO Bool
testCompileWasmRejectsTaggedIntLiteralOutOfRangeInBuiltinArgs = do
  let src =
        unlines
          [ "main bytes = slice_get_u8 bytes 1073741824"
          ]
  case compileSourceToWasm src of
    Left err ->
      assertTrue
        "compile wasm rejects tagged int literal out of range in builtin args"
        ("tagged i32 payload out of range" `isInfixOf` err)
    Right _ ->
      failTest
        "compile wasm rejects tagged int literal out of range in builtin args"
        "expected out-of-range tagged literal compile failure"

testCompileWasmSupportsMemoAttribute :: IO Bool
testCompileWasmSupportsMemoAttribute = do
  let src =
        unlines
          [ "#[memo 256]"
          , "fib n = case n of"
          , "  0 -> 0"
          , "  1 -> 1"
          , "  _ -> add (fib (sub n 1)) (fib (sub n 2))"
          , "main n = fib n"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports memo attribute" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue "compile wasm supports memo attribute" (BS.take 4 wasmBytes == wasmMagic)

testCompileWasmRejectsMemoOnNonUnaryFunction :: IO Bool
testCompileWasmRejectsMemoOnNonUnaryFunction = do
  let src =
        unlines
          [ "#[memo 32]"
          , "add2 x y = add x y"
          , "main x = add2 x 1"
          ]
  case compileSourceToWasm src of
    Left err ->
      assertTrue
        "compile wasm rejects memo on non-unary function"
        ("memo currently supports unary top-level functions only" `isInfixOf` err)
    Right _ ->
      failTest
        "compile wasm rejects memo on non-unary function"
        "expected compile failure for non-unary memo function, but compilation succeeded"

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

testCompileWasmSupportsBoolData :: IO Bool
testCompileWasmSupportsBoolData = do
  let src =
        unlines
          [ "data bool = true<1> | false<0>"
          , "to_i64 b = case b of"
          , "  true -> 1"
          , "  false -> 0"
          , "main x = case x of"
          , "  0 -> to_i64 false"
          , "  _ -> to_i64 true"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm supports Bool data" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue "compile wasm supports Bool data" (BS.take 4 wasmBytes == wasmMagic)

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

testCompileWasmSupportsSliceEqBuiltin :: IO Bool
testCompileWasmSupportsSliceEqBuiltin = do
  let src =
        unlines
          [ "main a b = slice_eq_u8 a b"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest "compile wasm inlines slice_eq_u8 builtin" ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile wasm inlines slice_eq_u8 builtin"
        ( BS.take 4 wasmBytes == wasmMagic
            && not (wasmContainsName wasmBytes "rt_slice_eq_u8")
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

testBootstrapPhase1FrontendPrimitivesCompiles :: IO Bool
testBootstrapPhase1FrontendPrimitivesCompiles = do
  let src =
        unlines
          [ "data Token method path = Tok method path"
          , "is_method t = case t of Tok method _ -> method"
          , "path_value t = case t of Tok _ path -> path"
          , "main x = (is_method (Tok 1 x)) + (path_value (Tok 1 x))"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest
        "bootstrap phase 1 frontend primitives compiles"
        ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "bootstrap phase 1 frontend primitives compiles"
        (BS.take 4 wasmBytes == wasmMagic)

testBootstrapPhase2CoreDataStructuresCompiles :: IO Bool
testBootstrapPhase2CoreDataStructuresCompiles = do
  let src =
        unlines
          [ "data List a = Nil | Cons a (List a)"
          , "sum_three xs = case xs of"
          , "  Nil -> 0"
          , "  Cons a rest -> add_tail a rest"
          , "add_tail a ys = case ys of"
          , "  Nil -> a"
          , "  Cons b tail -> add_tail2 a b tail"
          , "add_tail2 a b zs = case zs of"
          , "  Nil -> a + b"
          , "  Cons c _ -> a + b + c"
          , "mk_three a b c = Cons a (Cons b (Cons c Nil))"
          , "main x = sum_three (mk_three x 2 3)"
          ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  case compileSourceToWasm src of
    Left err ->
      failTest
        "bootstrap phase 2 core data structures compiles"
        ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "bootstrap phase 2 core data structures compiles"
        (BS.take 4 wasmBytes == wasmMagic)

testBootstrapPhase3ModuleGraphCompiles :: IO Bool
testBootstrapPhase3ModuleGraphCompiles = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import util.math"
              , "export main"
              , "main x = double x"
              ]
          )
        , ( "util/math.clapse"
          , unlines
              [ "module util.math"
              , "import util.base"
              , "export double"
              , "double x = (square x) + 1"
              ]
          )
        , ( "util/base.clapse"
          , unlines
              [ "module util.base"
              , "export square"
              , "square x = x * x"
              ]
          )
        ]
      wasmMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest
        "bootstrap phase 3 module graph compiles"
        ("unexpected compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "bootstrap phase 3 module graph compiles"
        (BS.take 4 wasmBytes == wasmMagic)

testBootstrapPhase3ModuleGraphRejectsAmbiguousImports :: IO Bool
testBootstrapPhase3ModuleGraphRejectsAmbiguousImports = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import util.left"
              , "import util.right"
              , "main x = project x"
              ]
          )
        , ( "util/left.clapse"
          , unlines
              [ "module util.left"
              , "export project"
              , "project x = add x 1"
              ]
          )
        , ( "util/right.clapse"
          , unlines
              [ "module util.right"
              , "export project"
              , "project x = mul x 2"
              ]
          )
        ]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      assertTrue
        "bootstrap phase 3 module graph rejects ambiguous imports"
        ("ambiguous import for project" `isInfixOf` err)
    Right _ ->
      failTest
        "bootstrap phase 3 module graph rejects ambiguous imports"
        "expected ambiguous import failure"

testBootstrapPhase5DispatchPilotCompiles :: IO Bool
testBootstrapPhase5DispatchPilotCompiles = do
  src <- readFile "examples/bootstrap_phase5_dispatch_pilot.clapse"
  case compileSourceToWasm src of
    Left err ->
      failTest
        "bootstrap phase 5 dispatch pilot compiles"
        ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "bootstrap phase 5 dispatch pilot compiles"
        (BS.take 4 wasmBytes == BS.pack [0x00, 0x61, 0x73, 0x6d])

testBootstrapPhase6ModuleDispatchCompiles :: IO Bool
testBootstrapPhase6ModuleDispatchCompiles = do
  result <- compileEntryModuleToWasm "examples/bootstrap_phase6_entry.clapse"
  case result of
    Left err ->
      failTest
        "bootstrap phase 6 module dispatch compiles"
        ("unexpected compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "bootstrap phase 6 module dispatch compiles"
        (BS.take 4 wasmBytes == BS.pack [0x00, 0x61, 0x73, 0x6d])

testBootstrapPhase7HostCapabilityCompiles :: IO Bool
testBootstrapPhase7HostCapabilityCompiles = do
  result <- compileEntryModuleToWasm "examples/bootstrap_phase7_host_capability_pilot.clapse"
  case result of
    Left err ->
      failTest
        "bootstrap phase 7 host capability compiles"
        ("unexpected compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "bootstrap phase 7 host capability compiles"
        (BS.take 4 wasmBytes == BS.pack [0x00, 0x61, 0x73, 0x6d])

testBootstrapPhase8PatternAndOperatorsCompiles :: IO Bool
testBootstrapPhase8PatternAndOperatorsCompiles = do
  src <- readFile "examples/bootstrap_phase8_pattern_and_operators.clapse"
  case compileSourceToWasm src of
    Left err ->
      failTest
        "bootstrap phase 8 pattern/operators compiles"
        ("unexpected wasm compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "bootstrap phase 8 pattern/operators compiles"
        (BS.take 4 wasmBytes == BS.pack [0x00, 0x61, 0x73, 0x6d])

testBootstrapPhase11ParserCombinatorHashAndStream :: IO Bool
testBootstrapPhase11ParserCombinatorHashAndStream = do
  src <- readFile "examples/bootstrap_phase11_parser_combinator_pilot.clapse"
  case parseModule src of
    Left err ->
      failTest
        "phase11 hash and stream paths include type declarations"
        ("unexpected parse error: " <> err)
    Right modu ->
      case compileSourceToCollapsed src of
        Left err ->
          failTest
            "phase11 hash and stream paths include type declarations"
            ("unexpected collapse error: " <> err)
        Right collapsed ->
          let hasFn fnName0 =
                case findCollapsed fnName0 collapsed of
                  Just _ -> True
                  Nothing -> False
              hasExpectedFns =
                hasFn "main_stats" && hasFn "main_stream" && hasFn "main"
           in if not hasExpectedFns
                then
                  failTest
                    "phase11 hash and stream paths include type declarations"
                    "expected main_stats, main_stream, and main in collapsed output"
            else
              case evalSourceFunction modu "main" [0] of
                Left err ->
                  failTest
                    "phase11 hash and stream paths include type declarations"
                    ("unexpected eval error for main: " <> err)
                Right mainOut ->
                  case evalSourceFunction modu "main_stats" [0] of
                    Left err ->
                      failTest
                        "phase11 hash and stream paths include type declarations"
                        ("unexpected eval error for main_stats: " <> err)
                    Right mainStatsOut ->
                      case evalSourceFunction modu "main_stream" [0] of
                        Left err ->
                          failTest
                            "phase11 hash and stream paths include type declarations"
                            ("unexpected eval error for main_stream: " <> err)
                        Right mainStreamOut ->
                          assertTrue
                            "phase11 hash and stream paths include type declarations"
                            (mainOut /= 0 && mainStatsOut /= 0 && mainStreamOut /= 0)

testBootstrapPhase11ParserCombinatorBadType :: IO Bool
testBootstrapPhase11ParserCombinatorBadType = do
  src <- readFile "examples/bootstrap_phase11_parser_combinator_pilot.clapse"
  case parseModule src of
    Left err ->
      failTest
        "phase11 bad type declaration rejects malformed source"
        ("unexpected parse error: " <> err)
    Right modu ->
      case compileSourceToCollapsed src of
        Left err ->
          failTest
            "phase11 bad type declaration rejects malformed source"
            ("unexpected collapse error: " <> err)
        Right collapsed ->
          case findCollapsed "main_bad_type" collapsed of
            Nothing ->
              failTest
                "phase11 bad type declaration rejects malformed source"
                "expected main_bad_type in collapsed output"
            Just _ ->
              case evalSourceFunction modu "main_bad_type" [0] of
                Left err ->
                  failTest
                    "phase11 bad type declaration rejects malformed source"
                    ("unexpected eval error for main_bad_type: " <> err)
                Right out ->
                  assertEqual
                    "phase11 bad type declaration rejects malformed source"
                    (-1)
                    out

testBootstrapPhase11ParserCombinatorBadTypeUnion :: IO Bool
testBootstrapPhase11ParserCombinatorBadTypeUnion = do
  src <- readFile "examples/bootstrap_phase11_parser_combinator_pilot.clapse"
  case parseModule src of
    Left err ->
      failTest
        "phase11 bad literal-union type declaration rejects malformed source"
        ("unexpected parse error: " <> err)
    Right modu ->
      case compileSourceToCollapsed src of
        Left err ->
          failTest
            "phase11 bad literal-union type declaration rejects malformed source"
            ("unexpected collapse error: " <> err)
        Right collapsed ->
          case
            ( findCollapsed "main_stats" collapsed
            , findCollapsed "main_bad_type_union" collapsed
            ) of
            (Just _, Just _) ->
              case evalSourceFunction modu "main_bad_type_union" [0] of
                Left err ->
                  failTest
                    "phase11 bad literal-union type declaration rejects malformed source"
                    ("unexpected eval error for main_bad_type_union: " <> err)
                Right out ->
                  assertEqual
                    "phase11 bad literal-union type declaration rejects malformed source"
                    (-1)
                    out
            _ ->
              failTest
                "phase11 bad literal-union type declaration rejects malformed source"
                "expected main_stats and main_bad_type_union in collapsed output"

testBootstrapPhase11ParserCombinatorSignedIntUnion :: IO Bool
testBootstrapPhase11ParserCombinatorSignedIntUnion = do
  src <- readFile "examples/bootstrap_phase11_parser_combinator_pilot.clapse"
  case parseModule src of
    Left err ->
      failTest
        "phase11 signed-int union fixture keeps source/collapsed parity"
        ("unexpected parse error: " <> err)
    Right modu ->
      case compileSourceToCollapsed src of
        Left err ->
          failTest
            "phase11 signed-int union fixture keeps source/collapsed parity"
            ("unexpected collapse error: " <> err)
        Right collapsed ->
          case findCollapsed "main_stats" collapsed of
            Nothing ->
              failTest
                "phase11 signed-int union fixture keeps source/collapsed parity"
                "expected main_stats in collapsed output"
            Just _ ->
              case evalSourceFunction modu "main_stats" [0] of
                Left err ->
                  failTest
                    "phase11 signed-int union fixture keeps source/collapsed parity"
                    ("unexpected eval error for main_stats: " <> err)
                Right out ->
                  assertTrue
                    "phase11 signed-int union fixture keeps source/collapsed parity"
                    (out /= 0)

testBootstrapPhase11ParserCombinatorBadTypeUnionString :: IO Bool
testBootstrapPhase11ParserCombinatorBadTypeUnionString = do
  src <- readFile "examples/bootstrap_phase11_parser_combinator_pilot.clapse"
  case parseModule src of
    Left err ->
      failTest
        "phase11 bad string-literal-union type declaration rejects malformed source"
        ("unexpected parse error: " <> err)
    Right modu ->
      case compileSourceToCollapsed src of
        Left err ->
          failTest
            "phase11 bad string-literal-union type declaration rejects malformed source"
            ("unexpected collapse error: " <> err)
        Right collapsed ->
          case
            ( findCollapsed "main_stats" collapsed
            , findCollapsed "main_bad_type_union_string" collapsed
            ) of
            (Just _, Just _) ->
              case evalSourceFunction modu "main_bad_type_union_string" [0] of
                Left err ->
                  failTest
                    "phase11 bad string-literal-union type declaration rejects malformed source"
                    ("unexpected eval error for main_bad_type_union_string: " <> err)
                Right out ->
                  assertEqual
                    "phase11 bad string-literal-union type declaration rejects malformed source"
                    (-1)
                    out
            _ ->
              failTest
                "phase11 bad string-literal-union type declaration rejects malformed source"
                "expected main_stats and main_bad_type_union_string in collapsed output"

testParitySupportsTopLevelFunctionAsValue :: IO Bool
testParitySupportsTopLevelFunctionAsValue = do
  let src =
        unlines
          [ "twice f x = f (f x)"
          , "inc x = x + 1"
          , "main x = twice inc x"
          ]
  case compileSourceToWasm src of
    Left err ->
      failTest
        "parity: supports top-level function as value"
        ("unexpected compile error: " <> err)
    Right _ -> do
      case parseModule src of
        Left err ->
          failTest
            "parity: supports top-level function as value"
            ("unexpected parse error: " <> err)
        Right modu ->
          case differentialCheckSourceCollapsed modu "main" [7] of
            Left err ->
              failTest
                "parity: supports top-level function as value"
                ("unexpected differential failure: " <> err)
            Right () ->
              assertTrue
                "parity: supports top-level function as value"
                True

testParityExampleCorpusCompiles :: IO Bool
testParityExampleCorpusCompiles = do
  let directExamples =
        [ "examples/identity.clapse"
        , "examples/currying.clapse"
        , "examples/closures.clapse"
        , "examples/case_of.clapse"
        , "examples/data.clapse"
        , "examples/monads_maybe_either.clapse"
        , "examples/bootstrap_phase1_frontend_primitives.clapse"
        , "examples/bootstrap_phase2_core_data_structures.clapse"
        , "examples/bootstrap_phase4_parser_pilot.clapse"
        , "examples/bootstrap_phase5_dispatch_pilot.clapse"
        , "examples/bootstrap_phase8_pattern_and_operators.clapse"
        ]
  directOk <- and <$> traverse compileDirectExample directExamples
  modulePhase3Ok <- compileModuleEntry "examples/bootstrap_phase3_entry.clapse"
  modulePhase6Ok <- compileModuleEntry "examples/bootstrap_phase6_entry.clapse"
  modulePhase7Ok <- compileModuleEntry "examples/bootstrap_phase7_host_capability_pilot.clapse"
  assertTrue
    "parity: example corpus compiles"
    (directOk && modulePhase3Ok && modulePhase6Ok && modulePhase7Ok)
  where
    compileDirectExample :: FilePath -> IO Bool
    compileDirectExample path = do
      src <- readFile path
      case compileSourceToWasm src of
        Left err ->
          failTest
            "parity: example corpus compiles"
            ("compile failed for " <> path <> ": " <> err)
        Right wasmBytes ->
          assertTrue
            "parity: example corpus compiles"
            (BS.take 4 wasmBytes == BS.pack [0x00, 0x61, 0x73, 0x6d])

    compileModuleEntry :: FilePath -> IO Bool
    compileModuleEntry path = do
      result <- compileEntryModuleToWasm path
      case result of
        Left err ->
          failTest
            "parity: example corpus compiles"
            ("module compile failed for " <> path <> ": " <> err)
        Right wasmBytes ->
          assertTrue
            "parity: example corpus compiles"
            (BS.take 4 wasmBytes == BS.pack [0x00, 0x61, 0x73, 0x6d])

testParitySupportsStringCasePatterns :: IO Bool
testParitySupportsStringCasePatterns = do
  let src =
        unlines
          [ "route s = case s of"
          , "  \"GET\" -> 1"
          , "  _ -> 0"
          , "main _ = route \"GET\""
          ]
  case compileSourceToWasm src of
    Left err ->
      failTest
        "parity: supports string case patterns"
        ("unexpected compile error: " <> err)
    Right _ ->
      case parseModule src of
        Left err ->
          failTest
            "parity: supports string case patterns"
            ("unexpected parse error: " <> err)
        Right modu ->
          case differentialCheckSourceCollapsed modu "main" [0] of
            Left err ->
              failTest
                "parity: supports string case patterns"
                ("unexpected differential failure: " <> err)
            Right () ->
              assertTrue
                "parity: supports string case patterns"
                True

testSelfHostBlockerRejectsHostIoBuiltins :: IO Bool
testSelfHostBlockerRejectsHostIoBuiltins = do
  let src =
        unlines
          [ "main x = read_file x"
          ]
  case compileSourceToWasm src of
    Left err ->
      assertTrue
        "self-host blocker: rejects host io builtins"
        ("wasm backend: unknown direct callee: read_file" `isInfixOf` err)
    Right _ ->
      failTest
        "self-host blocker: rejects host io builtins"
        "expected compile failure for unknown host IO builtin"

testCompileEntryModuleHostIoBuiltinImport :: IO Bool
testCompileEntryModuleHostIoBuiltinImport = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import host.io"
              , "main path = read_file path"
              ]
          )
        ]
      expectedMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module host.io builtin import" ("unexpected compile error: " <> err)
    Right wasmBytes ->
      case parseWasmImports wasmBytes of
        Left err ->
          failTest "compile entry module host.io builtin import" err
        Right imports ->
          let hostReadFileImports =
                [ ()
                | (modName, fieldName, kind) <- imports
                , modName == "host"
                , fieldName == "read_file"
                , kind == 0
                ]
           in assertTrue
                "compile entry module host.io builtin import"
                (BS.take 4 wasmBytes == expectedMagic && length hostReadFileImports == 1)

testCompileEntryModuleDedupesHostIoImport :: IO Bool
testCompileEntryModuleDedupesHostIoImport = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import host.io"
              , "import host.io"
              , "main path = read_file path"
              ]
          )
        ]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module dedupes host.io import" ("unexpected compile error: " <> err)
    Right wasmBytes ->
      case parseWasmImports wasmBytes of
        Left err ->
          failTest "compile entry module dedupes host.io import" err
        Right imports ->
          let hostReadFileImports =
                [ ()
                | (modName, fieldName, kind) <- imports
                , modName == "host"
                , fieldName == "read_file"
                , kind == 0
                ]
           in assertEqual
                "compile entry module dedupes host.io import"
                1
                (length hostReadFileImports)

testCompileEntryModuleHostTimeBuiltinImport :: IO Bool
testCompileEntryModuleHostTimeBuiltinImport = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import host.time"
              , "main x = unix_time_ms x"
              ]
          )
        ]
      expectedMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module host.time builtin import" ("unexpected compile error: " <> err)
    Right wasmBytes ->
      case parseWasmImports wasmBytes of
        Left err ->
          failTest "compile entry module host.time builtin import" err
        Right imports ->
          let hostTimeImports =
                [ ()
                | (modName, fieldName, kind) <- imports
                , modName == "host"
                , fieldName == "unix_time_ms"
                , kind == 0
                ]
           in assertTrue
                "compile entry module host.time builtin import"
                (BS.take 4 wasmBytes == expectedMagic && length hostTimeImports == 1)

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

testCompileEntryModuleImportedConstructorPattern :: IO Bool
testCompileEntryModuleImportedConstructorPattern = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import prelude"
              , "main x = case x of"
              , "  true -> 1"
              , "  _ -> 0"
              ]
          )
        , ( "prelude.clapse"
          , unlines
              [ "module prelude"
              , "data bool = true<1> | false<0>"
              ]
          )
        ]
      expectedMagic = BS.pack [0x00, 0x61, 0x73, 0x6d]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module imported constructor pattern" ("unexpected compile error: " <> err)
    Right wasmBytes ->
      assertTrue
        "compile entry module imported constructor pattern"
        (BS.take 4 wasmBytes == expectedMagic)

testCompileEntryModuleAmbiguousConstructorImport :: IO Bool
testCompileEntryModuleAmbiguousConstructorImport = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "import a"
              , "import b"
              , "main x = case x of"
              , "  true -> 1"
              , "  _ -> 0"
              ]
          )
        , ( "a.clapse"
          , unlines
              [ "module a"
              , "data bool = true<1> | false<0>"
              ]
          )
        , ( "b.clapse"
          , unlines
              [ "module b"
              , "data bool = true<1> | false<0>"
              ]
          )
        ]
  result <- compileFixtureModule files "entry.clapse"
  case result of
    Left err ->
      assertTrue
        "compile entry module rejects ambiguous constructor import"
        ( "ambiguous constructor import for true" `isInfixOf` err
            || "ambiguous constructor import for false" `isInfixOf` err
            || "ambiguous import for true" `isInfixOf` err
            || "ambiguous import for false" `isInfixOf` err
        )
    Right _ ->
      failTest
        "compile entry module rejects ambiguous constructor import"
        "expected ambiguous constructor import failure"

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

testCompileEntryModuleDebugArtifactIncludesStages :: IO Bool
testCompileEntryModuleDebugArtifactIncludesStages = do
  let files =
        [ ( "entry.clapse"
          , unlines
              [ "module entry"
              , "export main"
              , "main x y = add x y"
              ]
          )
        ]
  result <- compileFixtureDebugArtifact files "entry.clapse"
  case result of
    Left err ->
      failTest "compile entry module debug artifact includes stages" ("unexpected compile error: " <> err)
    Right dbg ->
      let hasFunctions =
            case debugMergedModule dbg of
              Module {functions = fs} -> not (null fs)
          hasTypes =
            case debugTypeInfo dbg of
              Just infos -> not (null infos)
              Nothing -> False
          hasNoTypeError =
            case debugTypeInfoError dbg of
              Nothing -> True
              Just _ -> False
          hasLowered = not (null (debugLowered dbg))
          hasCollapsed = not (null (debugCollapsed dbg))
          hasExports = debugExports dbg == [ExportApi {exportName = "main", exportArity = 2}]
          hasWasmHeader = BS.take 4 (debugWasm dbg) == BS.pack [0x00, 0x61, 0x73, 0x6d]
       in assertTrue
            "compile entry module debug artifact includes stages"
            (hasFunctions && hasTypes && hasNoTypeError && hasLowered && hasCollapsed && hasExports && hasWasmHeader)

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

parseWasmImports :: BS.ByteString -> Either String [(String, String, Word8)]
parseWasmImports wasmBytes = do
  let wasmOffset = 8
  importSection <- findSection wasmBytes wasmOffset sectionImport
  parseImportSection importSection
    `orElse` []
  where
    orElse :: Either String a -> a -> Either String a
    orElse action fallback =
      case action of
        Left _ -> Right fallback
        Right value -> Right value

parseImportSection :: BS.ByteString -> Either String [(String, String, Word8)]
parseImportSection payload = do
  (count, offsetAfterCount) <- readU32 payload 0
  let importCount = fromIntegral count
  foldM parseImport ([], fromIntegral offsetAfterCount) [1 .. importCount]
    >>= (pure . reverse . fst)
  where
    parseImport
      :: ([(String, String, Word8)], Int)
      -> Word32
      -> Either String ([(String, String, Word8)], Int)
    parseImport (acc, offset) _ = do
      (moduleLen, off1) <- readU32 payload offset
      let moduleBytes = BS.take (fromIntegral moduleLen) (BS.drop off1 payload)
          off2 = off1 + fromIntegral moduleLen
      (fieldLen, off3) <- readU32 payload off2
      let fieldBytes = BS.take (fromIntegral fieldLen) (BS.drop off3 payload)
          off4 = off3 + fromIntegral fieldLen
      kind <- readByte payload off4
      nextOffset <-
        case kind of
          0x00 -> snd <$> readU32 payload (off4 + 1)
          0x01 -> skipTableType payload (off4 + 1)
          0x02 -> skipLimits payload (off4 + 1)
          0x03 -> pure (off4 + 2)
          _ -> Left ("unknown import kind: " <> show kind)
      let moduleName = map (chr . fromIntegral) (BS.unpack moduleBytes)
          fieldName = map (chr . fromIntegral) (BS.unpack fieldBytes)
      pure ((moduleName, fieldName, kind) : acc, nextOffset)

skipTableType :: BS.ByteString -> Int -> Either String Int
skipTableType payload offset = do
  _ <- readByte payload offset
  skipLimits payload (offset + 1)

skipLimits :: BS.ByteString -> Int -> Either String Int
skipLimits payload offset = do
  flags <- readByte payload offset
  (_, off1) <- readU32 payload (offset + 1)
  case flags of
    0x00 -> Right off1
    0x01 -> snd <$> readU32 payload off1
    _ -> Left ("unsupported limits flag: " <> show flags)

sectionExport :: Word8
sectionExport = 7

sectionImport :: Word8
sectionImport = 2

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
        Function n _ _ _ -> n

findTypeInfo :: String -> [FunctionTypeInfo] -> Maybe FunctionTypeInfo
findTypeInfo _name [] = Nothing
findTypeInfo target (info:rest)
  | fnName info == target = Just info
  | otherwise = findTypeInfo target rest

assertFormatParseRoundtrip :: String -> String -> IO Bool
assertFormatParseRoundtrip label src =
  case formatSource src of
    Left err ->
      failTest label ("unexpected format error on first pass: " <> err)
    Right formatted1 ->
      case parseModule formatted1 of
        Left err ->
          failTest label ("formatted source did not parse: " <> err)
        Right _ ->
          case formatSource formatted1 of
            Left err ->
              failTest label ("unexpected format error on second pass: " <> err)
            Right formatted2 ->
              assertEqual label formatted1 formatted2

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

compileFixtureDebugArtifact :: [(FilePath, String)] -> FilePath -> IO (Either String CompileDebugArtifact)
compileFixtureDebugArtifact files entryFile = do
  root <- createTempModuleRoot files
  compileEntryModuleDebug (root </> entryFile) `finally` removePathForcibly root

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
