module Clapse.CollapseIR
  ( Atom(..)
  , Value(..)
  , Bind(..)
  , CollapsedFunction(..)
  , collapseFunction
  , collapseModule
  , verifyCollapsedFunction
  , verifyCollapsedModule
  , collapseAndVerifyFunction
  , collapseAndVerifyModule
  , collapseAndVerifyModuleFromRoots
  , pruneDeadFunctionsFromRoots
  ) where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.List (isInfixOf, isPrefixOf, sort)

import Clapse.Lowering (FlatFunction(FlatFunction), Op(..))
import Clapse.Syntax (Name)

type ArityEnv = [(Name, Int)]

data Atom
  = AConstI32 Int
  | AConstString String
  | ALocal Int
  | ATemp Int
  | AGlobal Name
  deriving (Eq, Show)

data Value
  = VClosure Name [Atom]
  | VCallDirect Name [Atom]
  | VCurryDirect Name [Atom]
  | VCallClosure Atom [Atom]
  | VApply Atom Atom
  | VSelfTailCall [Atom]
  deriving (Eq, Show)

data Bind = Bind
  { temp :: Int
  , value :: Value
  }
  deriving (Eq, Show)

data CollapsedFunction = CollapsedFunction
  { name :: Name
  , arity :: Int
  , captureArity :: Int
  , binds :: [Bind]
  , result :: Atom
  , lifted :: [CollapsedFunction]
  }
  deriving (Eq, Show)

data CollapseState = CollapseState
  { stack :: [Atom]
  , bindsRev :: [Bind]
  , nextTemp :: Int
  }
  deriving (Eq, Show)

data RewriteState = RewriteState
  { rewriteNext :: Int
  , rewriteBindsRev :: [Bind]
  , rewriteTempMap :: [(Int, Atom)]
  , rewriteValueMap :: [(Int, Value)]
  }
  deriving (Eq, Show)

data ConstAtom
  = ConstI32 Int
  | ConstString String
  deriving (Eq, Show)

type SpecKey = (Name, [Maybe ConstAtom])

data SpecializeState = SpecializeState
  { specNext :: Int
  , specCache :: [(SpecKey, Name)]
  , specFnsRev :: [CollapsedFunction]
  }
  deriving (Eq, Show)

data InlineState = InlineState
  { inlineNext :: Int
  , inlineBindsRev :: [Bind]
  , inlineTempMap :: [(Int, Atom)]
  }
  deriving (Eq, Show)

data EscapeAnalysis = EscapeAnalysis
  { nonEscapingClosureTemps :: [Int]
  , nonEscapingStructTemps :: [Int]
  }
  deriving (Eq, Show)

data StructShape = StructShape
  { structTag :: Name
  , structFields :: [Atom]
  }
  deriving (Eq, Show)

data EscapeFlattenState = EscapeFlattenState
  { flattenNext :: Int
  , flattenBindsRev :: [Bind]
  , flattenTempMap :: [(Int, Atom)]
  , flattenStructMap :: [(Int, StructShape)]
  }
  deriving (Eq, Show)

data SliceUseKind
  = SliceUseAsSetTarget
  | SliceUseOther
  deriving (Eq, Show)

data SliceRewriteState = SliceRewriteState
  { sliceNext :: Int
  , sliceBindsRev :: [Bind]
  , sliceTempMap :: [(Int, Atom)]
  , sliceUniqueTemps :: [Int]
  }
  deriving (Eq, Show)

data CaptureTrimPlan = CaptureTrimPlan
  { trimOldCaptureArity :: Int
  , trimKeepCaptureIndices :: [Int]
  }
  deriving (Eq, Show)

data HotUncurryState = HotUncurryState
  { hotNext :: Int
  , hotBindsRev :: [Bind]
  , hotTempMap :: [(Int, Atom)]
  }
  deriving (Eq, Show)

collapseModule :: [FlatFunction] -> Either String [CollapsedFunction]
collapseModule = traverse collapseFunction

collapseFunction :: FlatFunction -> Either String CollapsedFunction
collapseFunction (FlatFunction fnName fnArity fnCaptureArity fnOps fnLifted) = do
  collapsedLifted <- traverse collapseFunction fnLifted
  (collapsedBinds, out) <- collapseOps fnOps
  pure
    CollapsedFunction
      { name = fnName
      , arity = fnArity
      , captureArity = fnCaptureArity
      , binds = collapsedBinds
      , result = out
      , lifted = collapsedLifted
      }

collapseAndVerifyModule :: [FlatFunction] -> Either String [CollapsedFunction]
collapseAndVerifyModule lowered =
  collapseAndVerifyModuleFromRoots (map flatFunctionName lowered) lowered
  where
    flatFunctionName :: FlatFunction -> Name
    flatFunctionName (FlatFunction fnName _ _ _ _) = fnName

collapseAndVerifyModuleFromRoots :: [Name] -> [FlatFunction] -> Either String [CollapsedFunction]
collapseAndVerifyModuleFromRoots roots lowered = do
  env0 <- buildArityEnvFromFlat lowered
  collapsed <- collapseModule lowered
  normalized0 <- normalizeCurryingModule env0 collapsed
  specialized <- specializeConstantDirectCallsModule normalized0
  env1 <- buildArityEnvFromCollapsed specialized
  normalized1 <- normalizeCurryingModule env1 specialized
  inlined <- inlineSmallDirectCallsModule normalized1
  env2 <- buildArityEnvFromCollapsed inlined
  normalized2 <- normalizeCurryingModule env2 inlined
  inlined2 <- inlineSmallDirectCallsModule normalized2
  env3 <- buildArityEnvFromCollapsed inlined2
  normalized3 <- normalizeCurryingModule env3 inlined2
  escapeFlattened <- optimizeEscapeLifetimeModule normalized3
  captureFlattened <- flattenClosureCaptureLayoutsModule escapeFlattened
  hotUncurried <- uncurryHotApplyWrappersModule captureFlattened
  stabilized <- normalizeInlineRoundsModule 6 hotUncurried
  pruned <- pruneDeadFunctionsFromRoots roots stabilized
  let tailOptimized = tailOptimizeModule pruned
  verifyCollapsedModule tailOptimized
  pure tailOptimized

collapseAndVerifyFunction :: FlatFunction -> Either String CollapsedFunction
collapseAndVerifyFunction lowered = do
  env <- buildArityEnvFromFlat [lowered]
  collapsed <- collapseFunction lowered
  normalized <- normalizeCurryingFunction env collapsed
  escapeFlattened <- optimizeEscapeLifetimeFunction normalized
  let capturePlans = buildCaptureTrimPlans [escapeFlattened]
  captureFlattened <- flattenClosureCaptureLayoutsFunction capturePlans escapeFlattened
  let callCounts = buildDirectCallCounts [captureFlattened]
      wrappers = buildHotWrapperArityMap callCounts [captureFlattened]
  hotUncurried <- uncurryHotApplyWrappersFunction wrappers captureFlattened
  stabilized <- normalizeInlineRoundsFunction 6 hotUncurried
  let tailOptimized = tailOptimizeFunction stabilized
  verifyCollapsedFunction tailOptimized
  env1 <- buildArityEnvFromCollapsed [tailOptimized]
  verifyCurryingFunction env1 tailOptimized
  pure tailOptimized

normalizeInlineRoundsModule :: Int -> [CollapsedFunction] -> Either String [CollapsedFunction]
normalizeInlineRoundsModule rounds fns
  | rounds <= 0 = normalizeWithFreshEnv fns
  | otherwise = do
      normalized <- normalizeWithFreshEnv fns
      inlined <- inlineSmallDirectCallsModule normalized
      normalizeInlineRoundsModule (rounds - 1) inlined
  where
    normalizeWithFreshEnv :: [CollapsedFunction] -> Either String [CollapsedFunction]
    normalizeWithFreshEnv nextFns = do
      env <- buildArityEnvFromCollapsed nextFns
      normalizeCurryingModule env nextFns

normalizeInlineRoundsFunction :: Int -> CollapsedFunction -> Either String CollapsedFunction
normalizeInlineRoundsFunction rounds fn = do
  outFns <- normalizeInlineRoundsModule rounds [fn]
  case outFns of
    [out] -> pure out
    _ -> Left "collapse pipeline invariant failed: expected singleton function result"

collapseOps :: [Op] -> Either String ([Bind], Atom)
collapseOps ops = do
  st <- foldM stepOp initialState ops
  case stack st of
    [out] ->
      Right (reverse (bindsRev st), out)
    [] ->
      Left "collapse failed: function produced no result value"
    extra ->
      Left ("collapse failed: function left extra stack values: " <> show (length extra))

initialState :: CollapseState
initialState = CollapseState {stack = [], bindsRev = [], nextTemp = 0}

stepOp :: CollapseState -> Op -> Either String CollapseState
stepOp st op =
  case op of
    PushI32 n ->
      Right (push (AConstI32 n) st)
    PushString s ->
      Right (push (AConstString s) st)
    LocalGet i ->
      Right (push (ALocal i) st)
    MakeClosure fn captureLocals -> do
      let captureAtoms = map ALocal captureLocals
      let (tmp, st1) = emit (VClosure fn captureAtoms) st
      Right (push tmp st1)
    Call fn argc -> do
      (args, st1) <- popMany argc "direct call arguments" st
      let (tmp, st2) = emit (VCallDirect fn args) st1
      Right (push tmp st2)
    CallClosure argc -> do
      (args, st1) <- popMany argc "closure call arguments" st
      (callee, st2) <- popOne "closure call callee" st1
      let (tmp, st3) = emit (VCallClosure callee args) st2
      Right (push tmp st3)

push :: Atom -> CollapseState -> CollapseState
push atom st = st {stack = atom : stack st}

popOne :: String -> CollapseState -> Either String (Atom, CollapseState)
popOne label st =
  case stack st of
    [] ->
      Left ("collapse failed: stack underflow while reading " <> label)
    x:xs ->
      Right (x, st {stack = xs})

popMany :: Int -> String -> CollapseState -> Either String ([Atom], CollapseState)
popMany n label st
  | n < 0 =
      Left ("collapse failed: negative arity while reading " <> label)
  | otherwise =
      go n [] st
  where
    go :: Int -> [Atom] -> CollapseState -> Either String ([Atom], CollapseState)
    go needed acc cur
      | needed == 0 = Right (acc, cur)
      | otherwise = do
          (x, cur1) <- popOne label cur
          go (needed - 1) (x : acc) cur1

emit :: Value -> CollapseState -> (Atom, CollapseState)
emit val st =
  let t = nextTemp st
      b = Bind {temp = t, value = val}
   in (ATemp t, st {nextTemp = t + 1, bindsRev = b : bindsRev st})

normalizeCurryingModule :: ArityEnv -> [CollapsedFunction] -> Either String [CollapsedFunction]
normalizeCurryingModule env = traverse (normalizeCurryingFunction env)

normalizeCurryingFunction :: ArityEnv -> CollapsedFunction -> Either String CollapsedFunction
normalizeCurryingFunction env fn = do
  (newBindsRaw, tempMap) <- rewriteBinds env (binds fn)
  remappedResult <- remapAtom tempMap (result fn)
  let (newBinds, newResult) = pruneDeadTemps newBindsRaw remappedResult
  newLifted <- traverse (normalizeCurryingFunction env) (lifted fn)
  pure
    CollapsedFunction
      { name = name fn
      , arity = arity fn
      , captureArity = captureArity fn
      , binds = newBinds
      , result = newResult
      , lifted = newLifted
      }

rewriteBinds :: ArityEnv -> [Bind] -> Either String ([Bind], [(Int, Atom)])
rewriteBinds env allBinds = do
  st <- foldM (rewriteBind env) initialRewrite allBinds
  pure (reverse (rewriteBindsRev st), rewriteTempMap st)
  where
    initialRewrite =
      RewriteState
        { rewriteNext = 0
        , rewriteBindsRev = []
        , rewriteTempMap = []
        , rewriteValueMap = []
        }

rewriteBind :: ArityEnv -> RewriteState -> Bind -> Either String RewriteState
rewriteBind env st b = do
  val <- remapValue (rewriteTempMap st) (value b)
  (outAtom, produced, nextIx) <- normalizeValue env (rewriteValueMap st) (rewriteNext st) val
  let producedValues = map (\pb -> (temp pb, value pb)) produced
  let updated =
        RewriteState
          { rewriteNext = nextIx
          , rewriteBindsRev = reverse produced <> rewriteBindsRev st
          , rewriteTempMap = (temp b, outAtom) : rewriteTempMap st
          , rewriteValueMap = producedValues <> rewriteValueMap st
          }
  pure updated

normalizeValue :: ArityEnv -> [(Int, Value)] -> Int -> Value -> Either String (Atom, [Bind], Int)
normalizeValue env valueMap nextIx val =
  case val of
    VClosure fn captures ->
      pure (emitOne nextIx (VClosure fn captures))
    VCallDirect fn args ->
      pure (normalizeDirectCall env nextIx fn args)
    VCurryDirect fn args ->
      pure (emitOne nextIx (VCurryDirect fn args))
    VCallClosure callee args ->
      emitApplyChainOptimized env valueMap nextIx callee args
    VApply callee arg ->
      optimizeApplyValue env valueMap nextIx callee arg
    VSelfTailCall args ->
      pure (emitOne nextIx (VSelfTailCall args))

emitOne :: Int -> Value -> (Atom, [Bind], Int)
emitOne nextIx val =
  let out = ATemp nextIx
      b = Bind {temp = nextIx, value = val}
   in (out, [b], nextIx + 1)

emitApplyChain :: Int -> Atom -> [Atom] -> (Atom, [Bind], Int)
emitApplyChain nextIx callee args =
  case args of
    [] ->
      (callee, [], nextIx)
    arg1:rest -> do
      let (out1, b1, next1) = emitOne nextIx (VApply callee arg1)
          (out2, b2, next2) = emitApplyChain next1 out1 rest
      (out2, b1 <> b2, next2)

emitApplyChainOptimized :: ArityEnv -> [(Int, Value)] -> Int -> Atom -> [Atom] -> Either String (Atom, [Bind], Int)
emitApplyChainOptimized env valueMap nextIx callee args =
  case args of
    [] ->
      pure (callee, [], nextIx)
    _ ->
      do
        st <- foldM step (callee, [], nextIx, valueMap) args
        let (out, produced, nextFinal, _mapFinal) = st
        pure (out, produced, nextFinal)
  where
    step :: (Atom, [Bind], Int, [(Int, Value)]) -> Atom -> Either String (Atom, [Bind], Int, [(Int, Value)])
    step (curCallee, producedSoFar, curNext, curMap) arg = do
      (out, newBinds, next1) <- optimizeApplyValue env curMap curNext curCallee arg
      let producedValues = map (\pb -> (temp pb, value pb)) newBinds
          nextMap = producedValues <> curMap
      pure (out, producedSoFar <> newBinds, next1, nextMap)

normalizeDirectCall :: ArityEnv -> Int -> Name -> [Atom] -> (Atom, [Bind], Int)
normalizeDirectCall env nextIx fn args =
  case normalizeCoreBuiltin nextIx fn args of
    Just out ->
      out
    Nothing ->
      case lookup fn env of
        Nothing ->
          emitOne nextIx (VCallDirect fn args)
        Just fnArity
          | length args < fnArity ->
              emitOne nextIx (VCurryDirect fn args)
          | length args == fnArity ->
              emitOne nextIx (VCallDirect fn args)
          | otherwise ->
              let (leadingArgs, extraArgs) = splitAt fnArity args
                  (callOut, callBinds, next1) = emitOne nextIx (VCallDirect fn leadingArgs)
                  (finalOut, extraBinds, next2) = emitApplyChain next1 callOut extraArgs
               in (finalOut, callBinds <> extraBinds, next2)

normalizeCoreBuiltin :: Int -> Name -> [Atom] -> Maybe (Atom, [Bind], Int)
normalizeCoreBuiltin nextIx fn args =
  case (fn, args) of
    ("pure", [x]) ->
      Just (x, [], nextIx)
    ("fmap", [f, x]) ->
      Just (emitApplyChain nextIx f [x])
    ("ap", [f, x]) ->
      Just (emitApplyChain nextIx f [x])
    ("bind", [m, f]) ->
      Just (emitApplyChain nextIx f [m])
    _ ->
      Nothing

optimizeApplyValue :: ArityEnv -> [(Int, Value)] -> Int -> Atom -> Atom -> Either String (Atom, [Bind], Int)
optimizeApplyValue env valueMap nextIx callee arg =
  case callee of
    ATemp t ->
      case lookup t valueMap of
        Just (VClosure fn captures) ->
          pure (normalizeDirectCall env nextIx fn (captures <> [arg]))
        Just (VCurryDirect fn args) ->
          pure (normalizeDirectCall env nextIx fn (args <> [arg]))
        _ ->
          pure (emitOne nextIx (VApply callee arg))
    _ ->
      pure (emitOne nextIx (VApply callee arg))

pruneDeadTemps :: [Bind] -> Atom -> ([Bind], Atom)
pruneDeadTemps allBinds out =
  let liveTemps = collectLiveTemps allBinds (effectfulBuiltinTemps allBinds <> tempFromAtom out)
      keptBinds = filter (\b -> temp b `elem` liveTemps) allBinds
      renumberMap = zip (map temp keptBinds) [0 ..]
      remappedBinds = map (remapBind renumberMap) keptBinds
      remappedResult = remapAtomCompact renumberMap out
   in (remappedBinds, remappedResult)

effectfulBuiltinTemps :: [Bind] -> [Int]
effectfulBuiltinTemps = map temp . filter (isEffectfulBuiltinValue . value)

isEffectfulBuiltinValue :: Value -> Bool
isEffectfulBuiltinValue val =
  case val of
    VCallDirect callee _ ->
      isEffectfulMemoryBuiltin callee
    _ ->
      False

collectLiveTemps :: [Bind] -> [Int] -> [Int]
collectLiveTemps allBinds seedTemps = go seedTemps seedTemps
  where
    bindMap = map (\b -> (temp b, value b)) allBinds

    go :: [Int] -> [Int] -> [Int]
    go [] live = live
    go (t:todo) live =
      case lookup t bindMap of
        Nothing ->
          go todo live
        Just val ->
          let newDeps = filter (`notElem` live) (tempsFromValue val)
           in go (todo <> newDeps) (live <> newDeps)

remapBind :: [(Int, Int)] -> Bind -> Bind
remapBind renumberMap b =
  Bind
    { temp = remapTemp renumberMap (temp b)
    , value = remapValueCompact renumberMap (value b)
    }

remapTemp :: [(Int, Int)] -> Int -> Int
remapTemp renumberMap t =
  case lookup t renumberMap of
    Just newT -> newT
    Nothing -> t

remapAtomCompact :: [(Int, Int)] -> Atom -> Atom
remapAtomCompact renumberMap atom =
  case atom of
    ATemp t ->
      ATemp (remapTemp renumberMap t)
    _ ->
      atom

remapValueCompact :: [(Int, Int)] -> Value -> Value
remapValueCompact renumberMap val =
  case val of
    VClosure fn captures ->
      VClosure fn (map (remapAtomCompact renumberMap) captures)
    VCallDirect fn args ->
      VCallDirect fn (map (remapAtomCompact renumberMap) args)
    VCurryDirect fn args ->
      VCurryDirect fn (map (remapAtomCompact renumberMap) args)
    VCallClosure callee args ->
      VCallClosure (remapAtomCompact renumberMap callee) (map (remapAtomCompact renumberMap) args)
    VApply callee arg ->
      VApply (remapAtomCompact renumberMap callee) (remapAtomCompact renumberMap arg)
    VSelfTailCall args ->
      VSelfTailCall (map (remapAtomCompact renumberMap) args)

remapValue :: [(Int, Atom)] -> Value -> Either String Value
remapValue tempMap val =
  case val of
    VClosure fn captures ->
      VClosure fn <$> traverse (remapAtom tempMap) captures
    VCallDirect fn args ->
      VCallDirect fn <$> traverse (remapAtom tempMap) args
    VCurryDirect fn args ->
      VCurryDirect fn <$> traverse (remapAtom tempMap) args
    VCallClosure callee args ->
      VCallClosure <$> remapAtom tempMap callee <*> traverse (remapAtom tempMap) args
    VApply callee arg ->
      VApply <$> remapAtom tempMap callee <*> remapAtom tempMap arg
    VSelfTailCall args ->
      VSelfTailCall <$> traverse (remapAtom tempMap) args

optimizeEscapeLifetimeModule :: [CollapsedFunction] -> Either String [CollapsedFunction]
optimizeEscapeLifetimeModule = traverse optimizeEscapeLifetimeFunction

optimizeEscapeLifetimeFunction :: CollapsedFunction -> Either String CollapsedFunction
optimizeEscapeLifetimeFunction fn = do
  let analysis = analyzeEscapeLifetimesFunction fn
      initialFlattenState =
        EscapeFlattenState
          { flattenNext = 0
          , flattenBindsRev = []
          , flattenTempMap = []
          , flattenStructMap = []
          }
  st <- foldM (flattenStructBind analysis) initialFlattenState (binds fn)
  remappedResult <- remapAtom (flattenTempMap st) (result fn)
  liftedOut <- traverse optimizeEscapeLifetimeFunction (lifted fn)
  let rawFn =
        fn
          { binds = reverse (flattenBindsRev st)
          , result = remappedResult
          , lifted = liftedOut
          }
  sliceRewritten <- rewriteSliceSetOwnershipFunction rawFn
  let withRegionScope =
        if enableFunctionRegionScopeInsertion
          then insertFunctionRegionScope sliceRewritten
          else sliceRewritten
  pure (pruneFunctionTemps withRegionScope)

enableFunctionRegionScopeInsertion :: Bool
enableFunctionRegionScopeInsertion = False

flattenStructBind :: EscapeAnalysis -> EscapeFlattenState -> Bind -> Either String EscapeFlattenState
flattenStructBind analysis st b = do
  val <- remapValue (flattenTempMap st) (value b)
  case tryFlattenStructProjection (flattenStructMap st) val of
    Just aliasAtom ->
      pure
        st
          { flattenTempMap = (temp b, aliasAtom) : flattenTempMap st
          }
    Nothing ->
      let nextTempIx = flattenNext st
          nextBind = Bind {temp = nextTempIx, value = val}
          nextStructMap =
            case valueToStructShape val of
              Just shape
                | temp b `elem` nonEscapingStructTemps analysis ->
                    (nextTempIx, shape) : flattenStructMap st
              _ ->
                flattenStructMap st
       in pure
            EscapeFlattenState
              { flattenNext = nextTempIx + 1
              , flattenBindsRev = nextBind : flattenBindsRev st
              , flattenTempMap = (temp b, ATemp nextTempIx) : flattenTempMap st
              , flattenStructMap = nextStructMap
              }

rewriteSliceSetOwnershipFunction :: CollapsedFunction -> Either String CollapsedFunction
rewriteSliceSetOwnershipFunction fn
  | not (functionBodyIsScopeSafe fn) = Right fn
rewriteSliceSetOwnershipFunction fn = do
  let useKinds = collectSliceUseKinds fn
      initialSliceState =
        SliceRewriteState
          { sliceNext = 0
          , sliceBindsRev = []
          , sliceTempMap = []
          , sliceUniqueTemps = []
          }
  st <- foldM (rewriteSliceOwnershipBind useKinds) initialSliceState (binds fn)
  nextResult <- remapAtom (sliceTempMap st) (result fn)
  pure
    fn
      { binds = reverse (sliceBindsRev st)
      , result = nextResult
      }

rewriteSliceOwnershipBind ::
  [(Int, [SliceUseKind])] ->
  SliceRewriteState ->
  Bind ->
  Either String SliceRewriteState
rewriteSliceOwnershipBind useKinds st b = do
  remapped <- remapValue (sliceTempMap st) (value b)
  case (value b, remapped) of
    (VCallDirect "slice_set_u8" [origTarget, _origIx, _origVal], VCallDirect "slice_set_u8" [target, ix, valArg])
      | shouldReuseSliceTarget useKinds st origTarget target ->
          emitSliceRewriteOutput st b (VCallDirect "slice_set_u8" [target, ix, valArg]) True
      | otherwise ->
          let (outAtom, produced, nextIx) = emitSliceCopyAndSet (sliceNext st) target ix valArg
           in pure
                SliceRewriteState
                  { sliceNext = nextIx
                  , sliceBindsRev = reverse produced <> sliceBindsRev st
                  , sliceTempMap = (temp b, outAtom) : sliceTempMap st
                  , sliceUniqueTemps = addUniqueSliceTemp (atomToTemp outAtom) (sliceUniqueTemps st)
                  }
    _ ->
      emitSliceRewriteOutput st b remapped (valueProducesUniqueSlice remapped)

emitSliceRewriteOutput ::
  SliceRewriteState ->
  Bind ->
  Value ->
  Bool ->
  Either String SliceRewriteState
emitSliceRewriteOutput st b nextValue markUnique =
  let outTemp = sliceNext st
      outAtom = ATemp outTemp
      nextBind = Bind {temp = outTemp, value = nextValue}
      nextUnique =
        if markUnique
          then addUniqueSliceTemp (Just outTemp) (sliceUniqueTemps st)
          else sliceUniqueTemps st
   in Right
        SliceRewriteState
          { sliceNext = outTemp + 1
          , sliceBindsRev = nextBind : sliceBindsRev st
          , sliceTempMap = (temp b, outAtom) : sliceTempMap st
          , sliceUniqueTemps = nextUnique
          }

shouldReuseSliceTarget ::
  [(Int, [SliceUseKind])] ->
  SliceRewriteState ->
  Atom ->
  Atom ->
  Bool
shouldReuseSliceTarget useKinds st origTarget remappedTarget =
  case (origTarget, remappedTarget) of
    (ATemp oldTemp, ATemp newTemp) ->
      isTempOnlySetTargetUse useKinds oldTemp && newTemp `elem` sliceUniqueTemps st
    _ ->
      False

emitSliceCopyAndSet :: Int -> Atom -> Atom -> Atom -> (Atom, [Bind], Int)
emitSliceCopyAndSet nextIx target ix valArg =
  let (lenAtom, lenBind, next1) = emitOne nextIx (VCallDirect "slice_len_raw" [target])
      (copyAtom, copyBind, next2) = emitOne next1 (VCallDirect "slice_new_u8" [lenAtom])
      (srcAtom, srcBind, next3) = emitOne next2 (VCallDirect "slice_data_ptr" [target])
      (dstAtom, dstBind, next4) = emitOne next3 (VCallDirect "slice_data_ptr" [copyAtom])
      (_copyDone, memcpyBind, next5) = emitOne next4 (VCallDirect "memcpy_u8" [dstAtom, srcAtom, lenAtom])
      (setAtom, setBind, next6) = emitOne next5 (VCallDirect "slice_set_u8" [copyAtom, ix, valArg])
   in (setAtom, lenBind <> copyBind <> srcBind <> dstBind <> memcpyBind <> setBind, next6)

valueProducesUniqueSlice :: Value -> Bool
valueProducesUniqueSlice val =
  case val of
    VCallDirect "slice_new_u8" [_len] ->
      True
    _ ->
      False

atomToTemp :: Atom -> Maybe Int
atomToTemp atom =
  case atom of
    ATemp t -> Just t
    _ -> Nothing

addUniqueSliceTemp :: Maybe Int -> [Int] -> [Int]
addUniqueSliceTemp maybeTemp seen =
  case maybeTemp of
    Nothing -> seen
    Just t ->
      if t `elem` seen
        then seen
        else t : seen

collectSliceUseKinds :: CollapsedFunction -> [(Int, [SliceUseKind])]
collectSliceUseKinds fn =
  foldl recordSliceUse [] allUses
  where
    bindUses = concatMap (sliceUsesFromValue . value) (binds fn)
    resultUses = map (\t -> (t, SliceUseOther)) (tempFromAtom (result fn))
    allUses = bindUses <> resultUses

sliceUsesFromValue :: Value -> [(Int, SliceUseKind)]
sliceUsesFromValue val =
  case val of
    VCallDirect "slice_set_u8" args ->
      case args of
        target:rest ->
          sliceTargetUse target <> map (\t -> (t, SliceUseOther)) (foldMap tempFromAtom rest)
        _ ->
          map (\t -> (t, SliceUseOther)) (tempsFromValue val)
    _ ->
      map (\t -> (t, SliceUseOther)) (tempsFromValue val)

sliceTargetUse :: Atom -> [(Int, SliceUseKind)]
sliceTargetUse atom =
  case atom of
    ATemp t -> [(t, SliceUseAsSetTarget)]
    _ -> []

recordSliceUse :: [(Int, [SliceUseKind])] -> (Int, SliceUseKind) -> [(Int, [SliceUseKind])]
recordSliceUse acc (t, useKind) =
  case lookup t acc of
    Nothing ->
      (t, [useKind]) : acc
    Just kinds ->
      (t, kinds <> [useKind]) : filter (\(k, _) -> k /= t) acc

isTempOnlySetTargetUse :: [(Int, [SliceUseKind])] -> Int -> Bool
isTempOnlySetTargetUse useKinds t =
  case lookup t useKinds of
    Just [SliceUseAsSetTarget] -> True
    _ -> False

insertFunctionRegionScope :: CollapsedFunction -> CollapsedFunction
insertFunctionRegionScope fn =
  case shouldInsertFunctionRegionScope fn of
    Nothing ->
      fn
    Just (markTemp, resetTemp) ->
      let markBind = Bind {temp = markTemp, value = VCallDirect "region_mark" [AConstI32 0]}
          resetBind = Bind {temp = resetTemp, value = VCallDirect "region_reset" [ATemp markTemp]}
       in fn {binds = markBind : binds fn <> [resetBind]}

shouldInsertFunctionRegionScope :: CollapsedFunction -> Maybe (Int, Int)
shouldInsertFunctionRegionScope fn
  | null allocTemps = Nothing
  | hasExplicitRegionOps fn = Nothing
  | not (functionBodyIsScopeSafe fn) = Nothing
  | any (`elem` resultReachableTemps) allocTemps = Nothing
  | any resultTempMayAllocate resultReachableTemps = Nothing
  | not (all (allocationTempHasOnlyLocalUses fn) allocTemps) = Nothing
  | otherwise =
      Just (next, next + 1)
  where
    allBinds = binds fn
    bindMap = map (\b -> (temp b, value b)) allBinds
    allocTemps = map temp (filter (isScopeCandidateAlloc . value) allBinds)
    resultReachableTemps = collectLiveTemps allBinds (tempFromAtom (result fn))
    next = nextTempIndex allBinds

    resultTempMayAllocate :: Int -> Bool
    resultTempMayAllocate t =
      case lookup t bindMap of
        Nothing ->
          True
        Just val ->
          valueMayAllocate val

functionBodyIsScopeSafe :: CollapsedFunction -> Bool
functionBodyIsScopeSafe fn = all bindScopeSafe (binds fn)
  where
    bindScopeSafe :: Bind -> Bool
    bindScopeSafe b =
      case value b of
        VCallDirect callee _ ->
          isBuiltinName callee
        _ ->
          False

allocationTempHasOnlyLocalUses :: CollapsedFunction -> Int -> Bool
allocationTempHasOnlyLocalUses fn t =
  let uses = collectTempUses t fn
   in not (null uses) && all allocationUseIsLocal uses

allocationUseIsLocal :: TempUse -> Bool
allocationUseIsLocal useTag =
  case useTag of
    TempUseDirectArg callee ->
      callee `elem` localConsumers
        || "__get_" `isPrefixOf` callee
        || "__is_" `isPrefixOf` callee
    _ ->
      False
  where
    localConsumers =
      [ "add"
      , "sub"
      , "mul"
      , "div"
      , "eq"
      , "and"
      , "if"
      , "slice_len"
      , "slice_get_u8"
      , "slice_set_u8"
      , "slice_data_ptr"
      , "slice_len_raw"
      , "memcpy_u8"
      , "memset_u8"
      , "struct_tag"
      ]

hasExplicitRegionOps :: CollapsedFunction -> Bool
hasExplicitRegionOps fn = any bindHasRegionOp (binds fn)
  where
    bindHasRegionOp :: Bind -> Bool
    bindHasRegionOp b =
      case value b of
        VCallDirect "region_mark" _ -> True
        VCallDirect "region_reset" _ -> True
        _ -> False

isScopeCandidateAlloc :: Value -> Bool
isScopeCandidateAlloc val =
  case val of
    VCallDirect callee _ ->
      callee == "slice_new_u8" || callee == "region_alloc"
    _ ->
      False

valueMayAllocate :: Value -> Bool
valueMayAllocate val =
  case val of
    VClosure _ _ ->
      True
    VCurryDirect _ _ ->
      True
    VCallDirect callee _ ->
      not (isKnownNoAllocBuiltin callee)
    VCallClosure _ _ ->
      True
    VApply _ _ ->
      True
    VSelfTailCall _ ->
      True

isKnownNoAllocBuiltin :: Name -> Bool
isKnownNoAllocBuiltin callee =
  callee `elem` knownNoAlloc
    || "__get_" `isPrefixOf` callee
    || "__is_" `isPrefixOf` callee
  where
    knownNoAlloc =
      [ "add"
      , "sub"
      , "mul"
      , "div"
      , "eq"
      , "and"
      , "if"
      , "slice_len"
      , "slice_get_u8"
      , "slice_set_u8"
      , "slice_data_ptr"
      , "slice_len_raw"
      , "memcpy_u8"
      , "memset_u8"
      , "region_mark"
      , "region_reset"
      , "struct_tag"
      ]

nextTempIndex :: [Bind] -> Int
nextTempIndex allBinds =
  case map temp allBinds of
    [] -> 0
    temps -> 1 + maximum temps

tryFlattenStructProjection :: [(Int, StructShape)] -> Value -> Maybe Atom
tryFlattenStructProjection structMap val =
  case val of
    VCallDirect callee [target] ->
      case target of
        ATemp t ->
          case lookup t structMap of
            Nothing ->
              Nothing
            Just shape ->
              case parseGetBuiltin callee of
                Just (expectedTag, idx)
                  | expectedTag == structTag shape ->
                      indexAt (structFields shape) idx
                  | otherwise ->
                      Nothing
                Nothing ->
                  case parseIsBuiltin callee of
                    Just expectedTag ->
                      Just (AConstI32 (if expectedTag == structTag shape then 1 else 0))
                    Nothing ->
                      Nothing
        _ ->
          Nothing
    _ ->
      Nothing

valueToStructShape :: Value -> Maybe StructShape
valueToStructShape val =
  case val of
    VCallDirect callee args ->
      case parseMkBuiltin callee of
        Just (tag, fieldCount)
          | fieldCount == length args ->
              Just StructShape {structTag = tag, structFields = args}
        _ ->
          Nothing
    _ ->
      Nothing

analyzeEscapeLifetimesFunction :: CollapsedFunction -> EscapeAnalysis
analyzeEscapeLifetimesFunction fn =
  let allBinds = binds fn
      closureTemps =
        [ temp b
        | b <- allBinds
        , case value b of
            VClosure _ _ -> True
            VCurryDirect _ _ -> True
            _ -> False
        ]
      structTemps =
        [ temp b
        | b <- allBinds
        , case value b of
            VCallDirect callee _ ->
              case parseMkBuiltin callee of
                Just _ -> True
                Nothing -> False
            _ -> False
        ]
      nonEscapingClosures =
        [ t
        | t <- closureTemps
        , let uses = collectTempUses t fn
        , not (null uses)
        , all closureUseIsLocal uses
        ]
      nonEscapingStructs =
        [ t
        | t <- structTemps
        , let uses = collectTempUses t fn
        , not (null uses)
        , all structUseIsLocal uses
        ]
   in EscapeAnalysis
        { nonEscapingClosureTemps = sort (uniqueInts nonEscapingClosures)
        , nonEscapingStructTemps = sort (uniqueInts nonEscapingStructs)
        }

data TempUse
  = TempUseResult
  | TempUseClosureCapture
  | TempUseDirectArg Name
  | TempUseCallClosureCallee
  | TempUseCallClosureArg
  | TempUseApplyCallee
  | TempUseApplyArg
  | TempUseTailArg
  deriving (Eq, Show)

collectTempUses :: Int -> CollapsedFunction -> [TempUse]
collectTempUses target fn =
  foldMap (usesInBind target) (binds fn)
    <> usesInResult target (result fn)

usesInBind :: Int -> Bind -> [TempUse]
usesInBind target b = usesInValue target (value b)

usesInResult :: Int -> Atom -> [TempUse]
usesInResult target atom =
  if atomHasTemp target atom
    then [TempUseResult]
    else []

usesInValue :: Int -> Value -> [TempUse]
usesInValue target val =
  case val of
    VClosure _ captures ->
      foldMap (markIfContains target TempUseClosureCapture) captures
    VCallDirect callee args ->
      foldMap (markIfContains target (TempUseDirectArg callee)) args
    VCurryDirect callee args ->
      foldMap (markIfContains target (TempUseDirectArg callee)) args
    VCallClosure callee args ->
      markIfContains target TempUseCallClosureCallee callee
        <> foldMap (markIfContains target TempUseCallClosureArg) args
    VApply callee arg ->
      markIfContains target TempUseApplyCallee callee
        <> markIfContains target TempUseApplyArg arg
    VSelfTailCall args ->
      foldMap (markIfContains target TempUseTailArg) args

markIfContains :: Int -> TempUse -> Atom -> [TempUse]
markIfContains target useTag atom =
  if atomHasTemp target atom
    then [useTag]
    else []

atomHasTemp :: Int -> Atom -> Bool
atomHasTemp target atom =
  case atom of
    ATemp t -> t == target
    _ -> False

closureUseIsLocal :: TempUse -> Bool
closureUseIsLocal useTag =
  case useTag of
    TempUseApplyCallee -> True
    TempUseCallClosureCallee -> True
    _ -> False

structUseIsLocal :: TempUse -> Bool
structUseIsLocal useTag =
  case useTag of
    TempUseDirectArg callee ->
      case parseGetBuiltin callee of
        Just _ -> True
        Nothing ->
          case parseIsBuiltin callee of
            Just _ -> True
            Nothing -> False
    _ -> False

flattenClosureCaptureLayoutsModule :: [CollapsedFunction] -> Either String [CollapsedFunction]
flattenClosureCaptureLayoutsModule topLevelFns = do
  let plans = buildCaptureTrimPlans topLevelFns
  traverse (flattenClosureCaptureLayoutsFunction plans) topLevelFns

flattenClosureCaptureLayoutsFunction ::
  [(Name, CaptureTrimPlan)] ->
  CollapsedFunction ->
  Either String CollapsedFunction
flattenClosureCaptureLayoutsFunction plans fn = do
  nextBinds <- traverse (rewriteCaptureLayoutBind plans selfPlan) (binds fn)
  nextResult <- rewriteCaptureLayoutAtom selfPlan (result fn)
  nextLifted <- traverse (flattenClosureCaptureLayoutsFunction plans) (lifted fn)
  let nextCaptureArity =
        case selfPlan of
          Nothing -> captureArity fn
          Just plan -> length (trimKeepCaptureIndices plan)
      rawFn =
        fn
          { captureArity = nextCaptureArity
          , binds = nextBinds
          , result = nextResult
          , lifted = nextLifted
          }
  pure (pruneFunctionTemps rawFn)
  where
    selfPlan = lookup (name fn) plans

rewriteCaptureLayoutBind ::
  [(Name, CaptureTrimPlan)] ->
  Maybe CaptureTrimPlan ->
  Bind ->
  Either String Bind
rewriteCaptureLayoutBind plans selfPlan b = do
  nextValue <- rewriteCaptureLayoutValue plans selfPlan (value b)
  pure b {value = nextValue}

rewriteCaptureLayoutValue ::
  [(Name, CaptureTrimPlan)] ->
  Maybe CaptureTrimPlan ->
  Value ->
  Either String Value
rewriteCaptureLayoutValue plans selfPlan val =
  case val of
    VClosure callee captures ->
      VClosure callee
        <$> traverse (rewriteCaptureLayoutAtom selfPlan) (remapCalleeArgs plans callee captures)
    VCallDirect callee args ->
      VCallDirect callee
        <$> traverse (rewriteCaptureLayoutAtom selfPlan) (remapCalleeArgs plans callee args)
    VCurryDirect callee args ->
      VCurryDirect callee
        <$> traverse (rewriteCaptureLayoutAtom selfPlan) (remapCalleeArgs plans callee args)
    VCallClosure callee args ->
      VCallClosure
        <$> rewriteCaptureLayoutAtom selfPlan callee
        <*> traverse (rewriteCaptureLayoutAtom selfPlan) args
    VApply callee arg ->
      VApply
        <$> rewriteCaptureLayoutAtom selfPlan callee
        <*> rewriteCaptureLayoutAtom selfPlan arg
    VSelfTailCall args ->
      VSelfTailCall <$> traverse (rewriteCaptureLayoutAtom selfPlan) args

rewriteCaptureLayoutAtom :: Maybe CaptureTrimPlan -> Atom -> Either String Atom
rewriteCaptureLayoutAtom selfPlan atom =
  case (selfPlan, atom) of
    (Just plan, ALocal ix) ->
      Right (ALocal (remapLocalIndex plan ix))
    _ ->
      Right atom

remapCalleeArgs :: [(Name, CaptureTrimPlan)] -> Name -> [Atom] -> [Atom]
remapCalleeArgs plans callee args =
  case lookup callee plans of
    Nothing -> args
    Just plan -> remapPrefixArgs plan args

remapPrefixArgs :: CaptureTrimPlan -> [Atom] -> [Atom]
remapPrefixArgs plan args =
  let oldCaptureN = trimOldCaptureArity plan
      keepCaptureIx = trimKeepCaptureIndices plan
      providedCaptureN = min oldCaptureN (length args)
      providedCaptures = zip [0 :: Int ..] (take providedCaptureN args)
      keptCaptures = [arg | (ix, arg) <- providedCaptures, ix `elem` keepCaptureIx]
      params = drop providedCaptureN args
   in keptCaptures <> params

remapLocalIndex :: CaptureTrimPlan -> Int -> Int
remapLocalIndex plan ix
  | ix < oldCaptureN =
      case lookup ix keepMap of
        Just out -> out
        Nothing -> ix
  | otherwise =
      newCaptureN + (ix - oldCaptureN)
  where
    oldCaptureN = trimOldCaptureArity plan
    keepMap = zip (trimKeepCaptureIndices plan) [0 ..]
    newCaptureN = length (trimKeepCaptureIndices plan)

buildCaptureTrimPlans :: [CollapsedFunction] -> [(Name, CaptureTrimPlan)]
buildCaptureTrimPlans topLevelFns =
  foldMap collectPlan (flattenCollapsedFunctions topLevelFns)
  where
    collectPlan :: CollapsedFunction -> [(Name, CaptureTrimPlan)]
    collectPlan fn =
      case buildCaptureTrimPlan fn of
        Nothing -> []
        Just plan -> [(name fn, plan)]

buildCaptureTrimPlan :: CollapsedFunction -> Maybe CaptureTrimPlan
buildCaptureTrimPlan fn
  | oldCaptureN <= 0 = Nothing
  | length keptCaptures == oldCaptureN = Nothing
  | otherwise =
      Just
        CaptureTrimPlan
          { trimOldCaptureArity = oldCaptureN
          , trimKeepCaptureIndices = keptCaptures
          }
  where
    oldCaptureN = captureArity fn
    usedLocals =
      sort
        ( uniqueInts
            ( foldMap localsFromValue (map value (binds fn))
                <> localsFromAtom (result fn)
            )
        )
    keptCaptures = filter (`elem` usedLocals) [0 .. oldCaptureN - 1]

localsFromValue :: Value -> [Int]
localsFromValue val = foldMap localsFromAtom (atomsInValue val)

localsFromAtom :: Atom -> [Int]
localsFromAtom atom =
  case atom of
    ALocal ix -> [ix]
    _ -> []

uniqueInts :: [Int] -> [Int]
uniqueInts = reverse . go [] []
  where
    go :: [Int] -> [Int] -> [Int] -> [Int]
    go _seen acc [] = acc
    go seen acc (x:xs)
      | x `elem` seen = go seen acc xs
      | otherwise = go (x : seen) (x : acc) xs

uncurryHotApplyWrappersModule :: [CollapsedFunction] -> Either String [CollapsedFunction]
uncurryHotApplyWrappersModule topLevelFns = do
  let callCounts = buildDirectCallCounts topLevelFns
      wrappers = buildHotWrapperArityMap callCounts topLevelFns
  traverse (uncurryHotApplyWrappersFunction wrappers) topLevelFns

uncurryHotApplyWrappersFunction ::
  [(Name, Int)] ->
  CollapsedFunction ->
  Either String CollapsedFunction
uncurryHotApplyWrappersFunction wrappers fn = do
  let initialHotState =
        HotUncurryState
          { hotNext = 0
          , hotBindsRev = []
          , hotTempMap = []
          }
  st <- foldM (rewriteHotUncurryBind wrappers) initialHotState (binds fn)
  remappedResult <- remapAtom (hotTempMap st) (result fn)
  nextLifted <- traverse (uncurryHotApplyWrappersFunction wrappers) (lifted fn)
  let rawFn =
        fn
          { binds = reverse (hotBindsRev st)
          , result = remappedResult
          , lifted = nextLifted
          }
  pure (pruneFunctionTemps rawFn)

rewriteHotUncurryBind ::
  [(Name, Int)] ->
  HotUncurryState ->
  Bind ->
  Either String HotUncurryState
rewriteHotUncurryBind wrappers st b = do
  val <- remapValue (hotTempMap st) (value b)
  case val of
    VCallDirect callee args
      | Just wrapperArity <- lookup callee wrappers
      , length args == wrapperArity ->
          case args of
            [] ->
              emitHotBind st b val
            f:rest ->
              let (outAtom, produced, nextIx) = emitApplyChain (hotNext st) f rest
               in pure
                    HotUncurryState
                      { hotNext = nextIx
                      , hotBindsRev = reverse produced <> hotBindsRev st
                      , hotTempMap = (temp b, outAtom) : hotTempMap st
                      }
    _ ->
      emitHotBind st b val

emitHotBind :: HotUncurryState -> Bind -> Value -> Either String HotUncurryState
emitHotBind st b nextValue =
  let nextTempIx = hotNext st
      nextBind = Bind {temp = nextTempIx, value = nextValue}
   in Right
        HotUncurryState
          { hotNext = nextTempIx + 1
          , hotBindsRev = nextBind : hotBindsRev st
          , hotTempMap = (temp b, ATemp nextTempIx) : hotTempMap st
          }

buildHotWrapperArityMap :: [(Name, Int)] -> [CollapsedFunction] -> [(Name, Int)]
buildHotWrapperArityMap callCounts topLevelFns =
  [ (name fn, arity fn)
  | fn <- flattenCollapsedFunctions topLevelFns
  , isHotApplyWrapper callCounts fn
  ]

isHotApplyWrapper :: [(Name, Int)] -> CollapsedFunction -> Bool
isHotApplyWrapper callCounts fn =
  captureArity fn == 0
    && null (lifted fn)
    && arity fn >= 2
    && lookupCount (name fn) callCounts >= hotWrapperThreshold
    && matchesApplyChainWrapper fn
  where
    hotWrapperThreshold = 3

matchesApplyChainWrapper :: CollapsedFunction -> Bool
matchesApplyChainWrapper fn =
  matchesUnaryApplyChain fn || matchesBulkCallClosure fn
  where
    matchesUnaryApplyChain :: CollapsedFunction -> Bool
    matchesUnaryApplyChain curFn = go (ALocal 0) 1 (binds curFn)
      where
        go :: Atom -> Int -> [Bind] -> Bool
        go expectedCallee nextArgIx remaining
          | nextArgIx >= arity curFn =
              null remaining && result curFn == expectedCallee
          | otherwise =
              case remaining of
                [] ->
                  False
                b:rest ->
                  case value b of
                    VApply callee (ALocal argIx)
                      | callee == expectedCallee && argIx == nextArgIx ->
                          go (ATemp (temp b)) (nextArgIx + 1) rest
                    VCallClosure callee [ALocal argIx]
                      | callee == expectedCallee && argIx == nextArgIx ->
                          go (ATemp (temp b)) (nextArgIx + 1) rest
                    _ ->
                      False

    matchesBulkCallClosure :: CollapsedFunction -> Bool
    matchesBulkCallClosure curFn =
      case binds curFn of
        [b] ->
          case value b of
            VCallClosure (ALocal 0) callArgs ->
              callArgs == map ALocal [1 .. arity curFn - 1]
                && result curFn == ATemp (temp b)
            _ ->
              False
        _ ->
          False

buildDirectCallCounts :: [CollapsedFunction] -> [(Name, Int)]
buildDirectCallCounts topLevelFns = foldl bump [] allDirectCalls
  where
    allDirectCalls =
      foldMap collectFunctionCalls (flattenCollapsedFunctions topLevelFns)

    collectFunctionCalls :: CollapsedFunction -> [Name]
    collectFunctionCalls fn = foldMap collectValueCall (map value (binds fn))

    collectValueCall :: Value -> [Name]
    collectValueCall val =
      case val of
        VCallDirect callee _ -> [callee]
        _ -> []

    bump :: [(Name, Int)] -> Name -> [(Name, Int)]
    bump counts callee =
      case lookup callee counts of
        Nothing -> (callee, 1) : counts
        Just n -> (callee, n + 1) : filter (\(k, _v) -> k /= callee) counts

lookupCount :: Name -> [(Name, Int)] -> Int
lookupCount key counts =
  case lookup key counts of
    Just n -> n
    Nothing -> 0

specializeConstantDirectCallsModule :: [CollapsedFunction] -> Either String [CollapsedFunction]
specializeConstantDirectCallsModule topLevelFns = do
  let fnMap = buildFunctionMap topLevelFns
      initialSpecializeState =
        SpecializeState
          { specNext = 0
          , specCache = []
          , specFnsRev = []
          }
  (rewrittenTopLevel, finalState) <- rewriteSpecializedFunctions fnMap initialSpecializeState topLevelFns
  pure (rewrittenTopLevel <> reverse (specFnsRev finalState))

rewriteSpecializedFunctions ::
  [(Name, CollapsedFunction)] ->
  SpecializeState ->
  [CollapsedFunction] ->
  Either String ([CollapsedFunction], SpecializeState)
rewriteSpecializedFunctions _fnMap st [] = Right ([], st)
rewriteSpecializedFunctions fnMap st (fn:rest) = do
  (fnOut, st1) <- rewriteSpecializedFunction fnMap st fn
  (restOut, st2) <- rewriteSpecializedFunctions fnMap st1 rest
  pure (fnOut : restOut, st2)

rewriteSpecializedFunction ::
  [(Name, CollapsedFunction)] ->
  SpecializeState ->
  CollapsedFunction ->
  Either String (CollapsedFunction, SpecializeState)
rewriteSpecializedFunction fnMap st fn = do
  (nextBinds, st1) <- rewriteSpecializedBinds fnMap st (name fn) (binds fn)
  (nextLifted, st2) <- rewriteSpecializedFunctions fnMap st1 (lifted fn)
  pure
    ( fn
        { binds = nextBinds
        , lifted = nextLifted
        }
    , st2
    )

rewriteSpecializedBinds ::
  [(Name, CollapsedFunction)] ->
  SpecializeState ->
  Name ->
  [Bind] ->
  Either String ([Bind], SpecializeState)
rewriteSpecializedBinds _fnMap st _owner [] = Right ([], st)
rewriteSpecializedBinds fnMap st owner (b:rest) = do
  (nextValue, st1) <- rewriteSpecializedValue fnMap st owner (value b)
  let nextBind = b {value = nextValue}
  (restBinds, st2) <- rewriteSpecializedBinds fnMap st1 owner rest
  pure (nextBind : restBinds, st2)

rewriteSpecializedValue ::
  [(Name, CollapsedFunction)] ->
  SpecializeState ->
  Name ->
  Value ->
  Either String (Value, SpecializeState)
rewriteSpecializedValue fnMap st owner val =
  case val of
    VCallDirect callee args ->
      case maybeSpecializeDirectCall fnMap st owner callee args of
        Nothing ->
          Right (val, st)
        Just out ->
          out
    _ ->
      Right (val, st)

maybeSpecializeDirectCall ::
  [(Name, CollapsedFunction)] ->
  SpecializeState ->
  Name ->
  Name ->
  [Atom] ->
  Maybe (Either String (Value, SpecializeState))
maybeSpecializeDirectCall fnMap st owner callee args = do
  calleeFn <- lookup callee fnMap
  if not (isSpecializeCandidate owner calleeFn args)
    then Nothing
    else
      let key = (callee, map atomToConst args)
       in case lookup key (specCache st) of
            Just existingName ->
              let nextArgs = nonConstantArgs args
               in Just (Right (VCallDirect existingName nextArgs, st))
            Nothing ->
              Just $ do
                (nextName, nextSpecCounter) <- freshSpecializedName fnMap st callee
                specFnRaw <- buildSpecializedFunction nextName calleeFn args
                let specFn = pruneFunctionTemps specFnRaw
                    nextState =
                      st
                        { specNext = nextSpecCounter
                        , specCache = (key, nextName) : specCache st
                        , specFnsRev = specFn : specFnsRev st
                        }
                pure (VCallDirect nextName (nonConstantArgs args), nextState)

isSpecializeCandidate :: Name -> CollapsedFunction -> [Atom] -> Bool
isSpecializeCandidate owner calleeFn args =
  name calleeFn /= owner
    && null (lifted calleeFn)
    && length args == localCount calleeFn
    && any isConstantAtom args
    && bindCount > 0
    && bindCount <= 3
    && not (functionCallsName (name calleeFn) calleeFn)
    && all (inlineFriendlyValue . value) (binds calleeFn)
  where
    bindCount = length (binds calleeFn)

freshSpecializedName ::
  [(Name, CollapsedFunction)] ->
  SpecializeState ->
  Name ->
  Either String (Name, Int)
freshSpecializedName fnMap st callee = go (specNext st)
  where
    existingNames = map fst fnMap <> map snd (specCache st)

    go :: Int -> Either String (Name, Int)
    go ix =
      let candidate = callee <> "__spec_" <> show ix
       in if candidate `elem` existingNames
            then go (ix + 1)
            else Right (candidate, ix + 1)

buildSpecializedFunction :: Name -> CollapsedFunction -> [Atom] -> Either String CollapsedFunction
buildSpecializedFunction newName calleeFn callArgs = do
  let localMap = buildSpecializedLocalMap callArgs
      nextArity = length (nonConstantArgs callArgs)
  nextBinds <- traverse (specializeBind localMap) (binds calleeFn)
  nextResult <- specializeAtom localMap [] (result calleeFn)
  pure
    CollapsedFunction
      { name = newName
      , arity = nextArity
      , captureArity = 0
      , binds = nextBinds
      , result = nextResult
      , lifted = []
      }

buildSpecializedLocalMap :: [Atom] -> [(Int, Atom)]
buildSpecializedLocalMap callArgs = reverse localMapRev
  where
    (_nextIx, localMapRev) = foldl step (0, []) (zip [0 :: Int ..] callArgs)

    step :: (Int, [(Int, Atom)]) -> (Int, Atom) -> (Int, [(Int, Atom)])
    step (nextParamIx, acc) (oldIx, atom) =
      if isConstantAtom atom
        then (nextParamIx, (oldIx, atom) : acc)
        else (nextParamIx + 1, (oldIx, ALocal nextParamIx) : acc)

specializeBind :: [(Int, Atom)] -> Bind -> Either String Bind
specializeBind localMap b = do
  nextValue <- specializeValue localMap [] (value b)
  pure b {value = nextValue}

specializeValue :: [(Int, Atom)] -> [(Int, Atom)] -> Value -> Either String Value
specializeValue localMap tempMap val =
  case val of
    VClosure callee captures ->
      VClosure callee <$> traverse (specializeAtom localMap tempMap) captures
    VCallDirect callee args ->
      VCallDirect callee <$> traverse (specializeAtom localMap tempMap) args
    VCurryDirect callee args ->
      VCurryDirect callee <$> traverse (specializeAtom localMap tempMap) args
    VCallClosure callee args ->
      VCallClosure <$> specializeAtom localMap tempMap callee <*> traverse (specializeAtom localMap tempMap) args
    VApply callee arg ->
      VApply <$> specializeAtom localMap tempMap callee <*> specializeAtom localMap tempMap arg
    VSelfTailCall args ->
      VSelfTailCall <$> traverse (specializeAtom localMap tempMap) args

specializeAtom :: [(Int, Atom)] -> [(Int, Atom)] -> Atom -> Either String Atom
specializeAtom localMap tempMap atom =
  case atom of
    ALocal ix ->
      case lookup ix localMap of
        Just mapped -> Right mapped
        Nothing ->
          Left ("specialization failed: unknown local index " <> show ix)
    ATemp t ->
      case lookup t tempMap of
        Just mapped -> Right mapped
        Nothing -> Right (ATemp t)
    _ ->
      Right atom

atomToConst :: Atom -> Maybe ConstAtom
atomToConst atom =
  case atom of
    AConstI32 n -> Just (ConstI32 n)
    AConstString s -> Just (ConstString s)
    _ -> Nothing

isConstantAtom :: Atom -> Bool
isConstantAtom atom =
  case atomToConst atom of
    Just _ -> True
    Nothing -> False

nonConstantArgs :: [Atom] -> [Atom]
nonConstantArgs = filter (not . isConstantAtom)

inlineSmallDirectCallsModule :: [CollapsedFunction] -> Either String [CollapsedFunction]
inlineSmallDirectCallsModule topLevelFns = do
  let fnMap = buildFunctionMap topLevelFns
  traverse (inlineSmallDirectCallsFunction fnMap) topLevelFns

inlineSmallDirectCallsFunction ::
  [(Name, CollapsedFunction)] ->
  CollapsedFunction ->
  Either String CollapsedFunction
inlineSmallDirectCallsFunction fnMap fn = do
  (rawBinds, rawResult) <- inlineSmallDirectCallsInBody fnMap (name fn) (binds fn) (result fn)
  let (nextBinds, nextResult) = pruneDeadTemps rawBinds rawResult
  nextLifted <- traverse (inlineSmallDirectCallsFunction fnMap) (lifted fn)
  pure
    fn
      { binds = nextBinds
      , result = nextResult
      , lifted = nextLifted
      }

inlineSmallDirectCallsInBody ::
  [(Name, CollapsedFunction)] ->
  Name ->
  [Bind] ->
  Atom ->
  Either String ([Bind], Atom)
inlineSmallDirectCallsInBody fnMap owner allBinds out = do
  st <- foldM step initialInlineState allBinds
  outRemapped <- remapAtom (inlineTempMap st) out
  pure (reverse (inlineBindsRev st), outRemapped)
  where
    initialInlineState =
      InlineState
        { inlineNext = 0
        , inlineBindsRev = []
        , inlineTempMap = []
        }

    step :: InlineState -> Bind -> Either String InlineState
    step st b = do
      remappedValue <- remapValue (inlineTempMap st) (value b)
      case remappedValue of
        VCallDirect callee args ->
          case lookup callee fnMap of
            Just calleeFn
              | isInlineCandidate owner calleeFn args -> do
                  (inlineOut, inlineBinds, nextIx) <- inlineCallAtTemp (inlineNext st) calleeFn args
                  pure
                    InlineState
                      { inlineNext = nextIx
                      , inlineBindsRev = reverse inlineBinds <> inlineBindsRev st
                      , inlineTempMap = (temp b, inlineOut) : inlineTempMap st
                      }
            _ ->
              emitOriginal st b remappedValue
        _ ->
          emitOriginal st b remappedValue

emitOriginal :: InlineState -> Bind -> Value -> Either String InlineState
emitOriginal st b remappedValue =
  let nextTempIx = inlineNext st
      nextBind = Bind {temp = nextTempIx, value = remappedValue}
   in Right
        InlineState
          { inlineNext = nextTempIx + 1
          , inlineBindsRev = nextBind : inlineBindsRev st
          , inlineTempMap = (temp b, ATemp nextTempIx) : inlineTempMap st
          }

inlineCallAtTemp :: Int -> CollapsedFunction -> [Atom] -> Either String (Atom, [Bind], Int)
inlineCallAtTemp nextIx calleeFn callArgs = do
  (nextTempMap, bindsRevAcc, nextIxOut) <- foldM step ([], [], nextIx) (binds calleeFn)
  outAtom <- inlineAtom localMap nextTempMap (result calleeFn)
  pure (outAtom, reverse bindsRevAcc, nextIxOut)
  where
    localMap = zip [0 :: Int ..] callArgs

    step ::
      ([(Int, Atom)], [Bind], Int) ->
      Bind ->
      Either String ([(Int, Atom)], [Bind], Int)
    step (tempMapAcc, bindsRevAcc, ix) b = do
      nextValue <- inlineValue localMap tempMapAcc (value b)
      let nextBind = Bind {temp = ix, value = nextValue}
          nextTempMap = (temp b, ATemp ix) : tempMapAcc
      pure (nextTempMap, nextBind : bindsRevAcc, ix + 1)

inlineValue :: [(Int, Atom)] -> [(Int, Atom)] -> Value -> Either String Value
inlineValue localMap tempMap val =
  case val of
    VClosure callee captures ->
      VClosure callee <$> traverse (inlineAtom localMap tempMap) captures
    VCallDirect callee args ->
      VCallDirect callee <$> traverse (inlineAtom localMap tempMap) args
    VCurryDirect callee args ->
      VCurryDirect callee <$> traverse (inlineAtom localMap tempMap) args
    VCallClosure callee args ->
      VCallClosure <$> inlineAtom localMap tempMap callee <*> traverse (inlineAtom localMap tempMap) args
    VApply callee arg ->
      VApply <$> inlineAtom localMap tempMap callee <*> inlineAtom localMap tempMap arg
    VSelfTailCall args ->
      VSelfTailCall <$> traverse (inlineAtom localMap tempMap) args

inlineAtom :: [(Int, Atom)] -> [(Int, Atom)] -> Atom -> Either String Atom
inlineAtom localMap tempMap atom =
  case atom of
    ALocal ix ->
      case lookup ix localMap of
        Just mapped ->
          Right mapped
        Nothing ->
          Left ("inline failed: unknown local index " <> show ix)
    ATemp t ->
      case lookup t tempMap of
        Just mapped ->
          Right mapped
        Nothing ->
          Left ("inline failed: unknown temporary t" <> show t)
    _ ->
      Right atom

isInlineCandidate :: Name -> CollapsedFunction -> [Atom] -> Bool
isInlineCandidate owner calleeFn args =
  name calleeFn /= owner
    && length args == localCount calleeFn
    && bindCount > 0
    && bindCount <= inlineBindBudget calleeFn
    && not (functionCallsName (name calleeFn) calleeFn)
    && all (inlineFriendlyValue . value) (binds calleeFn)
  where
    bindCount = length (binds calleeFn)

inlineBindBudget :: CollapsedFunction -> Int
inlineBindBudget fn
  | isLamLikeName (name fn) = 24
  | otherwise = 3

inlineFriendlyValue :: Value -> Bool
inlineFriendlyValue val =
  case val of
    VClosure _ _ ->
      True
    VCallDirect callee _ ->
      isBuiltinName callee || isLamLikeName callee
    VCurryDirect callee _ ->
      isBuiltinName callee || isLamLikeName callee
    VCallClosure _ _ ->
      True
    VApply _ _ ->
      True
    _ ->
      False

isLamLikeName :: Name -> Bool
isLamLikeName n = "$lam" `isInfixOf` n

isBuiltinName :: Name -> Bool
isBuiltinName n =
  n `elem` coreBuiltins
    || "__mk_" `isPrefixOf` n
    || "__get_" `isPrefixOf` n
    || "__is_" `isPrefixOf` n
  where
    coreBuiltins =
      [ "add"
      , "sub"
      , "mul"
      , "div"
      , "eq"
      , "and"
      , "if"
      , "pure"
      , "bind"
      , "fmap"
      , "ap"
      , "slice_len"
      , "slice_get_u8"
      , "slice_set_u8"
      , "slice_new_u8"
      , "slice_data_ptr"
      , "slice_len_raw"
      , "region_mark"
      , "region_alloc"
      , "region_reset"
      , "memcpy_u8"
      , "memset_u8"
      , "struct_tag"
      , "collection_empty"
      , "collection_extend"
      ]

isEffectfulMemoryBuiltin :: Name -> Bool
isEffectfulMemoryBuiltin n =
  n `elem`
    [ "region_reset"
    , "memcpy_u8"
    , "memset_u8"
    , "slice_set_u8"
    ]

functionCallsName :: Name -> CollapsedFunction -> Bool
functionCallsName target fn = any bindCallsSelf (binds fn)
  where
    bindCallsSelf :: Bind -> Bool
    bindCallsSelf b =
      valueCallsName target (value b)

valueCallsName :: Name -> Value -> Bool
valueCallsName target val =
  case val of
    VClosure callee _ ->
      callee == target
    VCallDirect callee _ ->
      callee == target
    VCurryDirect callee _ ->
      callee == target
    _ ->
      False

pruneDeadFunctionsFromRoots :: [Name] -> [CollapsedFunction] -> Either String [CollapsedFunction]
pruneDeadFunctionsFromRoots roots topLevelFns = do
  let fnMap = buildFunctionMap topLevelFns
  traverse_ (assertFunctionExists fnMap) roots
  reachable <- collectReachableNames fnMap roots []
  pure (filterMap (pruneFunctionTree reachable) topLevelFns)

assertFunctionExists :: [(Name, CollapsedFunction)] -> Name -> Either String ()
assertFunctionExists fnMap rootName =
  case lookup rootName fnMap of
    Nothing ->
      Left ("collapse pruning failed: unknown root function: " <> rootName)
    Just _ ->
      Right ()

collectReachableNames ::
  [(Name, CollapsedFunction)] ->
  [Name] ->
  [Name] ->
  Either String [Name]
collectReachableNames _fnMap [] seen = Right seen
collectReachableNames fnMap (next:rest) seen
  | next `elem` seen =
      collectReachableNames fnMap rest seen
  | otherwise =
      case lookup next fnMap of
        Nothing ->
          collectReachableNames fnMap rest seen
        Just fn ->
          let knownNames = map fst fnMap
              deps = filter (`elem` knownNames) (functionDeps fn)
           in collectReachableNames fnMap (rest <> deps) (next : seen)

functionDeps :: CollapsedFunction -> [Name]
functionDeps fn = uniqueNames (foldMap (valueDeps . value) (binds fn))

valueDeps :: Value -> [Name]
valueDeps val =
  case val of
    VClosure callee _ ->
      [callee]
    VCallDirect callee _ ->
      [callee]
    VCurryDirect callee _ ->
      [callee]
    _ ->
      []

pruneFunctionTree :: [Name] -> CollapsedFunction -> Maybe CollapsedFunction
pruneFunctionTree reachable fn =
  let nextLifted = filterMap (pruneFunctionTree reachable) (lifted fn)
      keepSelf = name fn `elem` reachable || not (null nextLifted)
      keptFn
        | name fn `elem` reachable =
            fn {lifted = nextLifted}
        | otherwise =
            fn
              { binds = []
              , result = AConstI32 0
              , lifted = nextLifted
              }
   in if keepSelf
        then Just keptFn
        else Nothing

pruneFunctionTemps :: CollapsedFunction -> CollapsedFunction
pruneFunctionTemps fn =
  let (nextBinds, nextResult) = pruneDeadTemps (binds fn) (result fn)
   in fn
        { binds = nextBinds
        , result = nextResult
        }

indexAt :: [a] -> Int -> Maybe a
indexAt xs ix
  | ix < 0 = Nothing
  | otherwise = go ix xs
  where
    go :: Int -> [a] -> Maybe a
    go _ [] = Nothing
    go 0 (x:_) = Just x
    go n (_:rest) = go (n - 1) rest

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

buildFunctionMap :: [CollapsedFunction] -> [(Name, CollapsedFunction)]
buildFunctionMap = map (\fn -> (name fn, fn)) . flattenCollapsedFunctions

flattenCollapsedFunctions :: [CollapsedFunction] -> [CollapsedFunction]
flattenCollapsedFunctions = concatMap flattenOne
  where
    flattenOne :: CollapsedFunction -> [CollapsedFunction]
    flattenOne fn = fn : flattenCollapsedFunctions (lifted fn)

uniqueNames :: [Name] -> [Name]
uniqueNames = reverse . go [] []
  where
    go :: [Name] -> [Name] -> [Name] -> [Name]
    go _seen acc [] = acc
    go seen acc (x:xs)
      | x `elem` seen = go seen acc xs
      | otherwise = go (x : seen) (x : acc) xs

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _f [] = []
filterMap f (x:xs) =
  case f x of
    Nothing -> filterMap f xs
    Just out -> out : filterMap f xs

tailOptimizeModule :: [CollapsedFunction] -> [CollapsedFunction]
tailOptimizeModule = map tailOptimizeFunction

tailOptimizeFunction :: CollapsedFunction -> CollapsedFunction
tailOptimizeFunction fn =
  let optimizedLifted = map tailOptimizeFunction (lifted fn)
      optimizedBinds = tailOptimizeBinds (name fn) (arity fn) (result fn) (binds fn)
   in CollapsedFunction
        { name = name fn
        , arity = arity fn
        , captureArity = captureArity fn
        , binds = optimizedBinds
        , result = result fn
        , lifted = optimizedLifted
        }

tailOptimizeBinds :: Name -> Int -> Atom -> [Bind] -> [Bind]
tailOptimizeBinds fnName fnArity fnResult allBinds =
  case unsnoc allBinds of
    Nothing ->
      allBinds
    Just (prefix, lastBind)
      | temp lastBind /= resultTemp fnResult ->
          allBinds
      | otherwise ->
          case value lastBind of
            VCallDirect callee args
              | callee == fnName && length args == fnArity ->
                  prefix <> [lastBind {value = VSelfTailCall args}]
              | otherwise ->
                  allBinds
            _ ->
              allBinds

resultTemp :: Atom -> Int
resultTemp atom =
  case atom of
    ATemp t -> t
    _ -> -1

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = do
  (initRest, lastElem) <- unsnoc xs
  pure (x : initRest, lastElem)

remapAtom :: [(Int, Atom)] -> Atom -> Either String Atom
remapAtom tempMap atom =
  case atom of
    AConstI32 _ ->
      Right atom
    AConstString _ ->
      Right atom
    ALocal _ ->
      Right atom
    AGlobal _ ->
      Right atom
    ATemp t ->
      case lookup t tempMap of
        Just mapped ->
          Right mapped
        Nothing ->
          Left ("collapse normalization failed: unknown temp t" <> show t)

verifyCollapsedModule :: [CollapsedFunction] -> Either String ()
verifyCollapsedModule fns = do
  env <- buildArityEnvFromCollapsed fns
  traverse_ verifyCollapsedFunction fns
  traverse_ (verifyCurryingFunction env) fns

verifyCollapsedFunction :: CollapsedFunction -> Either String ()
verifyCollapsedFunction fn = do
  verifyHeader fn
  verifyTempShape (binds fn)
  verifyUseSites fn
  verifyNoDeadTemps fn
  verifyTailCalls fn
  traverse_ verifyCollapsedFunction (lifted fn)

verifyHeader :: CollapsedFunction -> Either String ()
verifyHeader fn
  | arity fn < 0 =
      Left "collapsed IR invalid: arity cannot be negative"
  | captureArity fn < 0 =
      Left "collapsed IR invalid: capture arity cannot be negative"
  | otherwise =
      Right ()

verifyTempShape :: [Bind] -> Either String ()
verifyTempShape allBinds =
  let temps = map temp allBinds
      expected = [0 .. length allBinds - 1]
   in if temps == expected
        then Right ()
        else Left "collapsed IR is not normalized: temps must be contiguous from 0"

verifyUseSites :: CollapsedFunction -> Either String ()
verifyUseSites fn = do
  let localN = localCount fn
      allTemps = map temp (binds fn)
  _ <- verifyBindUses localN [] (binds fn)
  verifyAtomInScope localN allTemps (result fn)
  pure ()

verifyBindUses :: Int -> [Int] -> [Bind] -> Either String [Int]
verifyBindUses _localN seen [] = Right seen
verifyBindUses localN seen (b:bs) = do
  let atoms = atomsInValue (value b)
  traverse_ (verifyAtomInScope localN seen) atoms
  case value b of
    VCallClosure callee _ ->
      checkCallable callee
    VApply callee _ ->
      checkCallable callee
    _ ->
      pure ()
  verifyBindUses localN (temp b : seen) bs

checkCallable :: Atom -> Either String ()
checkCallable atom =
  case atom of
    AConstI32 _ ->
      Left "collapsed IR invalid: callee cannot be a constant"
    AConstString _ ->
      Left "collapsed IR invalid: callee cannot be a constant"
    _ ->
      Right ()

verifyAtomInScope :: Int -> [Int] -> Atom -> Either String ()
verifyAtomInScope localN tempScope atom =
  case atom of
    AConstI32 _ ->
      Right ()
    AConstString _ ->
      Right ()
    AGlobal _ ->
      Right ()
    ALocal i
      | i < 0 ->
          Left "collapsed IR invalid: local index cannot be negative"
      | i >= localN ->
          Left ("collapsed IR invalid: local index out of range: " <> show i)
      | otherwise ->
          Right ()
    ATemp t
      | t `elem` tempScope ->
          Right ()
      | otherwise ->
          Left ("collapsed IR invalid: temp used out of scope: t" <> show t)

verifyNoDeadTemps :: CollapsedFunction -> Either String ()
verifyNoDeadTemps fn =
  let allTemps = map temp (binds fn)
      usedTemps =
        foldMap tempsFromValue (map value (binds fn))
          <> effectfulBuiltinTemps (binds fn)
          <> tempFromAtom (result fn)
      deadTemps = filter (`notElem` usedTemps) allTemps
   in case deadTemps of
        [] ->
          Right ()
        _ ->
          Left ("collapsed IR is not minimal: dead temp(s): " <> show deadTemps)

verifyCurryingFunction :: ArityEnv -> CollapsedFunction -> Either String ()
verifyCurryingFunction env fn = do
  traverse_ (verifyCurryingValue env) (map value (binds fn))
  traverse_ (verifyCurryingFunction env) (lifted fn)

verifyCurryingValue :: ArityEnv -> Value -> Either String ()
verifyCurryingValue env val =
  case val of
    VCallDirect fn args ->
      case lookup fn env of
        Nothing ->
          Right ()
        Just fnArity
          | length args == fnArity ->
              Right ()
          | otherwise ->
              Left
                ( "collapsed IR is not optimal: direct call to "
                    <> fn
                    <> " has "
                    <> show (length args)
                    <> " args but arity is "
                    <> show fnArity
                )
    VCurryDirect fn args ->
      case lookup fn env of
        Nothing ->
          Left ("collapsed IR invalid: curried call target not found: " <> fn)
        Just fnArity
          | length args < fnArity ->
              Right ()
          | otherwise ->
              Left
                ( "collapsed IR is not optimal: curry call to "
                    <> fn
                    <> " must provide fewer than arity "
                    <> show fnArity
                )
    _ ->
      Right ()

verifyTailCalls :: CollapsedFunction -> Either String ()
verifyTailCalls fn = traverse_ verifyTailValue (map value (binds fn))
  where
    verifyTailValue :: Value -> Either String ()
    verifyTailValue val =
      case val of
        VSelfTailCall args
          | length args == arity fn ->
              Right ()
          | otherwise ->
              Left
                ( "collapsed IR invalid: self tail call in "
                    <> name fn
                    <> " must have "
                    <> show (arity fn)
                    <> " args but has "
                    <> show (length args)
                )
        _ ->
          Right ()

localCount :: CollapsedFunction -> Int
localCount fn = captureArity fn + arity fn

atomsInValue :: Value -> [Atom]
atomsInValue val =
  case val of
    VClosure _ captures ->
      captures
    VCallDirect _ args ->
      args
    VCurryDirect _ args ->
      args
    VCallClosure callee args ->
      callee : args
    VApply callee arg ->
      [callee, arg]
    VSelfTailCall args ->
      args

tempsFromValue :: Value -> [Int]
tempsFromValue val = foldMap tempFromAtom (atomsInValue val)

tempFromAtom :: Atom -> [Int]
tempFromAtom atom =
  case atom of
    ATemp t -> [t]
    _ -> []

buildArityEnvFromFlat :: [FlatFunction] -> Either String ArityEnv
buildArityEnvFromFlat allFns = buildArityEnv (concatMap collectFlatSigs allFns)

collectFlatSigs :: FlatFunction -> ArityEnv
collectFlatSigs (FlatFunction fnName fnArity fnCaptureArity _ fnLifted) =
  (fnName, fnArity + fnCaptureArity) : concatMap collectFlatSigs fnLifted

buildArityEnvFromCollapsed :: [CollapsedFunction] -> Either String ArityEnv
buildArityEnvFromCollapsed allFns = buildArityEnv (concatMap collectCollapsedSigs allFns)

collectCollapsedSigs :: CollapsedFunction -> ArityEnv
collectCollapsedSigs fn =
  (name fn, arity fn + captureArity fn) : concatMap collectCollapsedSigs (lifted fn)

buildArityEnv :: ArityEnv -> Either String ArityEnv
buildArityEnv = go [] []
  where
    go :: [Name] -> ArityEnv -> ArityEnv -> Either String ArityEnv
    go _seen acc [] = Right (reverse acc)
    go seen acc ((fnName, fnArity):rest)
      | fnArity < 0 =
          Left ("function arity cannot be negative: " <> fnName)
      | fnName `elem` seen =
          Left ("duplicate function name in arity environment: " <> fnName)
      | otherwise =
          go (fnName : seen) ((fnName, fnArity) : acc) rest
