module Clapse.Tools.Bench
  ( runBench
  ) where

import Data.Bits ((.&.))
import System.CPUTime (getCPUTime)

import MyLib
  ( Atom(..)
  , Bind(..)
  , CollapsedFunction(..)
  , Value(..)
  , collapseAndVerifyModuleFromRoots
  , evalCollapsedFunction
  , lowerModule
  , parseModule
  )

data Cost = Cost
  { bindCount :: Int
  , directCount :: Int
  , applyCount :: Int
  , closureCount :: Int
  }
  deriving (Eq, Show)

data Bar = Bar
  { maxRuntimeGapToHand :: Double
  , maxStaticGapToHand :: Double
  , maxStaticGapToLower :: Double
  }
  deriving (Eq, Show)

defaultBar :: Bar
defaultBar =
  Bar
    { maxRuntimeGapToHand = 1.2
    , maxStaticGapToHand = 1.2
    , maxStaticGapToLower = 1.35
    }

runBench :: Int -> IO ()
runBench iterations = do
  handCollapsed <- compileCollapsed handSource "hand"
  abstractionCollapsed <- compileCollapsed abstractionSource "abstraction"
  case verifyParity handCollapsed abstractionCollapsed of
    Left err ->
      putStrLn ("bench parity check failed: " <> err)
    Right () -> do
      (handMs, handChecksum) <- timeCase iterations handCollapsed "hand"
      (absMs, absChecksum) <- timeCase iterations abstractionCollapsed "abstraction"
      let runtimeGapToHand = ratio absMs handMs
          handCost = collectCost handCollapsed
          abstractionCost = collectCost abstractionCollapsed
          lowerBound = lowerBoundScore abstractionCost
          handScore = scoreCost handCost
          abstractionScore = scoreCost abstractionCost
          staticGapToLower = ratio (fromIntegral abstractionScore) (fromIntegral lowerBound)
          staticGapToHand = ratio (fromIntegral abstractionScore) (fromIntegral handScore)
          noCollapsiblePatterns = not (hasCollapsiblePatterns abstractionCollapsed)
          barChecks =
            [ ("no_collapsible_patterns", noCollapsiblePatterns)
            , ("static_gap_to_lower<=1.35", staticGapToLower <= maxStaticGapToLower defaultBar)
            , ("static_gap_to_hand<=1.20", staticGapToHand <= maxStaticGapToHand defaultBar)
            , ("runtime_gap_to_hand<=1.20", runtimeGapToHand <= maxRuntimeGapToHand defaultBar)
            ]
      putStrLn ("iterations: " <> show iterations)
      putStrLn "parity: ok for inputs 0..127"
      putStrLn ("hand:        " <> fmtMs handMs <> " ms  checksum=" <> show handChecksum)
      putStrLn ("abstraction: " <> fmtMs absMs <> " ms  checksum=" <> show absChecksum)
      putStrLn ("runtime gap_to_hand: " <> show runtimeGapToHand <> "x")
      putStrLn ("cost hand:        " <> show handCost <> " score=" <> show handScore)
      putStrLn ("cost abstraction: " <> show abstractionCost <> " score=" <> show abstractionScore)
      putStrLn ("cost lower_bound score: " <> show lowerBound)
      putStrLn ("static gap_to_hand: " <> show staticGapToHand <> "x")
      putStrLn ("static gap_to_lower: " <> show staticGapToLower <> "x")
      putStrLn ("collapsible patterns remain: " <> show (not noCollapsiblePatterns))
      reportBar barChecks
      if all snd barChecks
        then putStrLn "optimization bar: PASS"
        else fail "optimization bar: FAIL"

compileCollapsed :: String -> String -> IO [CollapsedFunction]
compileCollapsed src root =
  case parseModule src >>= lowerModule >>= collapseAndVerifyModuleFromRoots [root] of
    Left err -> fail ("bench compile error: " <> err)
    Right collapsed -> pure collapsed

verifyParity :: [CollapsedFunction] -> [CollapsedFunction] -> Either String ()
verifyParity handCollapsed abstractionCollapsed = go 0
  where
    go :: Int -> Either String ()
    go i
      | i > 127 = Right ()
      | otherwise = do
          handOut <- evalCollapsedFunction handCollapsed "hand" [i]
          absOut <- evalCollapsedFunction abstractionCollapsed "abstraction" [i]
          if handOut == absOut
            then go (i + 1)
            else
              Left
                ( "mismatch at input "
                    <> show i
                    <> ": hand="
                    <> show handOut
                    <> " abstraction="
                    <> show absOut
                )

timeCase :: Int -> [CollapsedFunction] -> String -> IO (Double, Int)
timeCase iterations collapsed entry = do
  start <- getCPUTime
  checksum <-
    case runWork 0 0 of
      Left err -> fail ("bench eval error in " <> entry <> ": " <> err)
      Right out -> pure out
  end <- getCPUTime
  let elapsedPs = end - start
      elapsedMs = fromIntegral elapsedPs / 1.0e9
  pure (elapsedMs, checksum)
  where
    runWork :: Int -> Int -> Either String Int
    runWork i acc
      | i >= iterations = Right acc
      | otherwise = do
          let arg = i .&. 1023
          out <- evalCollapsedFunction collapsed entry [arg]
          let acc' = acc + out
          acc' `seq` runWork (i + 1) acc'

fmtMs :: Double -> String
fmtMs x = show (fromInteger (round (x * 1000)) / 1000 :: Double)

ratio :: Double -> Double -> Double
ratio _ numeratorDenominator | numeratorDenominator <= 0 = 0
ratio numerator denominator = numerator / denominator

scoreCost :: Cost -> Int
scoreCost cost =
  bindCount cost
    + (2 * directCount cost)
    + (4 * applyCount cost)
    + (6 * closureCount cost)

-- Theoretical floor under this simple model: keep binds/direct calls, remove closure/apply overhead.
lowerBoundScore :: Cost -> Int
lowerBoundScore cost = max 1 (bindCount cost + (2 * directCount cost))

collectCost :: [CollapsedFunction] -> Cost
collectCost = foldr addCost zeroCost . map collectFunctionCost

collectFunctionCost :: CollapsedFunction -> Cost
collectFunctionCost (CollapsedFunction _ _ _ bs _ ls) =
  foldr addCost (foldr addCost zeroCost (map collectFunctionCost ls)) (map costFromBind bs)

costFromBind :: Bind -> Cost
costFromBind (Bind _ v) =
  case v of
    VCallDirect _ _ -> Cost 1 1 0 0
    VApply _ _ -> Cost 1 0 1 0
    VClosure _ _ -> Cost 1 0 0 1
    VCurryDirect _ _ -> Cost 1 0 0 1
    VCallClosure _ _ -> Cost 1 0 1 0
    VSelfTailCall _ -> Cost 1 1 0 0

zeroCost :: Cost
zeroCost = Cost 0 0 0 0

addCost :: Cost -> Cost -> Cost
addCost a b =
  Cost
    { bindCount = bindCount a + bindCount b
    , directCount = directCount a + directCount b
    , applyCount = applyCount a + applyCount b
    , closureCount = closureCount a + closureCount b
    }

hasCollapsiblePatterns :: [CollapsedFunction] -> Bool
hasCollapsiblePatterns = any fnHasCollapsible
  where
    fnHasCollapsible :: CollapsedFunction -> Bool
    fnHasCollapsible (CollapsedFunction _ _ _ bs _ ls) =
      bindsHaveCollapsible bs || any fnHasCollapsible ls

bindsHaveCollapsible :: [Bind] -> Bool
bindsHaveCollapsible = go []
  where
    go :: [(Int, Value)] -> [Bind] -> Bool
    go _ [] = False
    go seen (b:rest) =
      let hasNow = valueIsCollapsible seen (value b)
          seen' = (temp b, value b) : seen
       in hasNow || go seen' rest

valueIsCollapsible :: [(Int, Value)] -> Value -> Bool
valueIsCollapsible seen val =
  case val of
    VApply (ATemp t) _ ->
      case lookup t seen of
        Just (VClosure _ _) -> True
        Just (VCurryDirect _ _) -> True
        _ -> False
    VCallClosure _ _ ->
      True
    _ ->
      False

reportBar :: [(String, Bool)] -> IO ()
reportBar checks = mapM_ report checks
  where
    report :: (String, Bool) -> IO ()
    report (label, ok) =
      putStrLn
        ( "bar "
            <> label
            <> ": "
            <> if ok then "PASS" else "FAIL"
        )

handSource :: String
handSource = "hand x = add (mul x x) 1"

abstractionSource :: String
abstractionSource =
  unlines
    [ "plus a b = add a b"
    , "square x = mul x x"
    , "apply f x = f x"
    , "abstraction x = apply (plus 1) (square x)"
    ]
