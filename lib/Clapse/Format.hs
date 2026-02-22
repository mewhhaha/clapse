module Clapse.Format
  ( formatSource
  ) where

import Clapse.Syntax (parseModule)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf)

data LetHead
  = LetHeaderOnly
  | LetInlineBinding String

data LetDescriptor = LetDescriptor
  { descLetColumn :: Int
  , descLetPrefix :: String
  , descLetHead :: LetHead
  }

-- Mostly-conservative formatter:
-- 1) validate syntax via parser
-- 2) preserve declaration/layout structure (except long inline let-chain expansion)
-- 3) normalize redundant horizontal whitespace outside strings/comments
-- 4) normalize final newline
formatSource :: String -> Either String String
formatSource src = do
  _ <- parseModule src
  let formatted = normalizeSource src
  _ <- parseModule formatted
  pure formatted

normalizeSource :: String -> String
normalizeSource src =
  let normalizedNewlines = normalizeNewlineChars src
      cleaned = map normalizeLine (lines normalizedNewlines)
      laidOut =
        layoutLetBlocksHaskell
          ( pushDownInlineLetHeaders
              (expandInlineLets (indentCaseArmLetContinuations (expandInlineCases cleaned)))
          )
      body = dropWhileEnd null laidOut
   in case body of
        [] -> ""
        _ -> unlines body

normalizeNewlineChars :: String -> String
normalizeNewlineChars [] = []
normalizeNewlineChars ('\r':'\n':rest) = '\n' : normalizeNewlineChars rest
normalizeNewlineChars ('\r':rest) = '\n' : normalizeNewlineChars rest
normalizeNewlineChars (c:rest) = c : normalizeNewlineChars rest

trimTrailingHorizontalWhitespace :: String -> String
trimTrailingHorizontalWhitespace =
  reverse . dropWhile isHorizontalWhitespace . reverse

normalizeLine :: String -> String
normalizeLine line =
  let (codePrefix, commentSuffix) = splitCommentOutsideString line
      (indent, codeBody) = span isHorizontalWhitespace codePrefix
      collapsedBody = collapseInnerWhitespace codeBody
      trimmedCode = trimTrailingHorizontalWhitespace collapsedBody
      renderedCode = indent <> trimmedCode
   in case commentSuffix of
        [] -> renderedCode
        _ ->
          if null trimmedCode
            then indent <> commentSuffix
            else renderedCode <> " " <> commentSuffix

expandInlineCases :: [String] -> [String]
expandInlineCases [] = []
expandInlineCases (line:rest) =
  case maybeExpandInlineCaseLine line of
    Nothing -> line : expandInlineCases rest
    Just expanded -> expanded <> expandInlineCases rest

maybeExpandInlineCaseLine :: String -> Maybe [String]
maybeExpandInlineCaseLine line =
  let (codePrefix, commentSuffix) = splitCommentOutsideString line
   in if not (null commentSuffix)
        then Nothing
        else
          let (indent, codeBody) = span isHorizontalWhitespace codePrefix
           in maybeExpandedCaseLayout indent codeBody

maybeExpandedCaseLayout :: String -> String -> Maybe [String]
maybeExpandedCaseLayout indent codeBody = do
  (prefixRaw, headerRaw, armsRaw) <- findCaseLayout codeBody
  let hadTrailingSeparator = endsWith ";" (trimHorizontal armsRaw)
      prefix = trimTrailingHorizontalWhitespace prefixRaw
      header = trimTrailingHorizontalWhitespace headerRaw
      armPieces = filter (not . null) (map trimHorizontal (splitTopLevelSemicolons armsRaw))
      arms =
        case reverse armPieces of
          [] -> []
          lastArm:restRev ->
            let lastArmTrimmed = trimHorizontal lastArm
                lastArmOut =
                  if hadTrailingSeparator && not (endsWith ";" lastArmTrimmed)
                    then lastArm <> ";"
                    else lastArm
             in reverse (lastArmOut : restRev)
      shouldExpand = length arms >= 2
  if not shouldExpand
    then Nothing
    else
      let firstLine =
            if null prefix
              then indent <> header
              else indent <> prefix <> " " <> header
          armLines = map (\arm -> indent <> "  " <> arm) arms
       in Just (firstLine : armLines)

findCaseLayout :: String -> Maybe (String, String, String)
findCaseLayout codeBody = do
  (caseStart, _caseEnd, rest) <- firstCaseToken (keywordTokensOutsideString codeBody)
  (_ofStart, ofEnd) <- matchingOfToken 1 rest
  let prefixRaw = take caseStart codeBody
      prefixTrimmed = trimTrailingHorizontalWhitespace prefixRaw
      headerRaw = take (ofEnd - caseStart) (drop caseStart codeBody)
      armsRaw = drop ofEnd codeBody
      allowedPrefix = endsWith "=" prefixTrimmed || endsWith "->" prefixTrimmed
  if not allowedPrefix
    then Nothing
    else Just (prefixRaw, headerRaw, armsRaw)

firstCaseToken :: [(String, Int, Int)] -> Maybe (Int, Int, [(String, Int, Int)])
firstCaseToken [] = Nothing
firstCaseToken ((tok, startIx, endIx):rest)
  | tok == "case" = Just (startIx, endIx, rest)
  | otherwise = firstCaseToken rest

matchingOfToken :: Int -> [(String, Int, Int)] -> Maybe (Int, Int)
matchingOfToken _ [] = Nothing
matchingOfToken depth ((tok, startIx, endIx):rest)
  | tok == "case" = matchingOfToken (depth + 1) rest
  | tok == "of" =
      if depth == 1
        then Just (startIx, endIx)
        else matchingOfToken (depth - 1) rest
  | otherwise = matchingOfToken depth rest

indentCaseArmLetContinuations :: [String] -> [String]
indentCaseArmLetContinuations = go
  where
    go :: [String] -> [String]
    go [] = []
    go (line:rest) =
      case splitCaseArmInlineLetHeader line of
        Nothing ->
          line : go rest
        Just (armIndent, armLine, letLine) ->
          let (continuationLines, restLines) = span (isCaseArmLetContinuation armIndent) rest
              indentedContinuations = map ("  " <>) continuationLines
           in armLine : letLine : indentedContinuations <> go restLines

    isCaseArmLetContinuation :: String -> String -> Bool
    isCaseArmLetContinuation armIndent raw =
      let trimmed = trimHorizontal raw
          currentIndent = takeWhile isHorizontalWhitespace raw
       in not (null trimmed)
            && currentIndent == armIndent
            && not ("->" `isInfixOf` trimmed)

    splitCaseArmInlineLetHeader :: String -> Maybe (String, String, String)
    splitCaseArmInlineLetHeader raw = do
      splitIx <- indexOfInfix "-> let " raw
      let trimmed = trimHorizontal raw
      if not (endsWith ";" trimmed)
        then Nothing
        else
          let armIndent = takeWhile isHorizontalWhitespace raw
              (prefix, markerAndRest) = splitAt splitIx raw
              letBinding = drop (length "-> let ") markerAndRest
              armLine = prefix <> "->"
              letLine = armIndent <> "  let " <> letBinding
           in Just (armIndent, armLine, letLine)

pushDownInlineLetHeaders :: [String] -> [String]
pushDownInlineLetHeaders [] = []
pushDownInlineLetHeaders [line] = [line]
pushDownInlineLetHeaders (line:next:rest) =
  case splitFunctionInlineLetHeader line next of
    Nothing ->
      line : pushDownInlineLetHeaders (next : rest)
    Just (headerLine, firstBindingLine) ->
      headerLine : firstBindingLine : pushDownInlineLetHeaders (next : rest)
  where
    splitFunctionInlineLetHeader :: String -> String -> Maybe (String, String)
    splitFunctionInlineLetHeader raw lookahead = do
      if not (isIndentedNonBlank lookahead)
        then Nothing
        else
          case splitMarker raw of
            Nothing -> Nothing
            Just (marker, splitIx, headerSuffix) ->
              let baseIndent = takeWhile isHorizontalWhitespace raw
                  (prefix, markerAndRest) = splitAt splitIx raw
                  firstBinding = drop (length marker) markerAndRest
                  headerLine = prefix <> headerSuffix
                  bindingLine = baseIndent <> "  " <> firstBinding
               in Just (headerLine, bindingLine)

    splitMarker :: String -> Maybe (String, Int, String)
    splitMarker raw =
      case indexOfInfix "= let " raw of
        Just ix -> Just ("= let ", ix, "= let")
        Nothing ->
          case indexOfInfix "in let " raw of
            Just ix -> Just ("in let ", ix, "in let")
            Nothing -> Nothing

    isIndentedNonBlank :: String -> Bool
    isIndentedNonBlank raw =
      case raw of
        [] -> False
        c:_ -> isHorizontalWhitespace c && not (null (trimHorizontal raw))

layoutLetBlocksHaskell :: [String] -> [String]
layoutLetBlocksHaskell srcLines =
  let next = layoutLetBlocksHaskellPass srcLines
   in if next == srcLines
        then next
        else layoutLetBlocksHaskell next

layoutLetBlocksHaskellPass :: [String] -> [String]
layoutLetBlocksHaskellPass = go
  where
    go :: [String] -> [String]
    go [] = []
    go (line:rest) =
      case rewriteLetBlock line rest of
        Nothing ->
          line : go rest
        Just (rewritten, next) ->
          rewritten <> go next

    rewriteLetBlock :: String -> [String] -> Maybe ([String], [String])
    rewriteLetBlock line0 rest0 = do
      let descriptor = detectLetDescriptor line0
      desc <- descriptor
      firstCont <- firstNonBlank rest0
      let contIndent = takeWhile isHorizontalWhitespace firstCont
      if null contIndent
        then Nothing
        else do
          let (sameIndentBlock, restAfterBlock) = span (isAtLeastIndentNonBlank contIndent) rest0
          if null sameIndentBlock
            then Nothing
            else do
              let (firstBinding, remainingForIn) =
                    case descLetHead desc of
                      LetHeaderOnly ->
                        case sameIndentBlock of
                          [] ->
                            ("", [])
                          b0:bs ->
                            (stripBindingSuffix (trimHorizontal b0), bs)
                      LetInlineBinding binding ->
                        (binding, sameIndentBlock)
              if null firstBinding
                then Nothing
                else do
                  (restBindings, inBody, remainingBlockLines) <- splitAtInLine remainingForIn
                  let letCol = descLetColumn desc
                      prefix = descLetPrefix desc
                      prefixTrimmed = trimTrailingHorizontalWhitespace prefix
                      baseIndentLen = length (takeWhile isHorizontalWhitespace prefix)
                      breakAfterEq = endsWith "=" prefixTrimmed
                      letIndent =
                        if breakAfterEq
                          then replicate (baseIndentLen + 2) ' '
                          else ""
                      inIndent =
                        if breakAfterEq
                          then letIndent
                          else replicate letCol ' '
                      firstLineOut =
                        if breakAfterEq
                          then prefixTrimmed
                          else prefix <> "let " <> firstBinding
                      bindingLinesOut =
                        if breakAfterEq
                          then
                            let firstBindingLine = letIndent <> "let " <> firstBinding
                                restBindingIndent = replicate (baseIndentLen + 6) ' '
                                restBindingLines = map (\b -> restBindingIndent <> b) restBindings
                             in firstBindingLine : restBindingLines
                          else
                            let inlineBindingIndent = replicate (letCol + 4) ' '
                             in map (\b -> inlineBindingIndent <> b) restBindings
                      inLineOut =
                        if null inBody
                          then inIndent <> "in"
                          else inIndent <> "in " <> inBody
                      deferInLine = inBody == "let"
                      remaining =
                        if deferInLine
                          then inLineOut : (remainingBlockLines <> restAfterBlock)
                          else remainingBlockLines <> restAfterBlock
                      rendered =
                        if deferInLine
                          then firstLineOut : bindingLinesOut
                          else firstLineOut : bindingLinesOut <> [inLineOut]
                  Just (rendered, remaining)

    splitAtInLine :: [String] -> Maybe ([String], String, [String])
    splitAtInLine rows = goRows [] rows
      where
        goRows :: [String] -> [String] -> Maybe ([String], String, [String])
        goRows _ [] = Nothing
        goRows acc (r:rs) =
          let trimmed = trimHorizontal r
           in case parseInLine trimmed of
                Just body ->
                  let bindings = map (stripBindingSuffix . trimHorizontal) (reverse acc)
                   in Just (bindings, body, rs)
                Nothing ->
                  goRows (r : acc) rs

    parseInLine :: String -> Maybe String
    parseInLine trimmed
      | trimmed == "in" = Just ""
      | "in " `isPrefixOf` trimmed = Just (trimHorizontal (drop 3 trimmed))
      | otherwise = Nothing

    isAtLeastIndentNonBlank :: String -> String -> Bool
    isAtLeastIndentNonBlank indent raw =
      let currentIndent = takeWhile isHorizontalWhitespace raw
          depthOk =
            length currentIndent >= length indent
              && take (length indent) currentIndent == indent
       in not (null (trimHorizontal raw)) && depthOk

    firstNonBlank :: [String] -> Maybe String
    firstNonBlank [] = Nothing
    firstNonBlank (r:rs)
      | null (trimHorizontal r) = firstNonBlank rs
      | otherwise = Just r

    stripBindingSuffix :: String -> String
    stripBindingSuffix raw =
      let trimmed = trimHorizontal raw
       in if endsWith ";" trimmed
            then trimHorizontal (take (length trimmed - 1) trimmed)
            else trimmed

    detectLetDescriptor :: String -> Maybe LetDescriptor
    detectLetDescriptor raw = do
      (startIx, endIx) <- lastLetToken raw
      let prefix = take startIx raw
          suffix = trimHorizontal (drop endIx raw)
      if null suffix
        then
          Just
            LetDescriptor
              { descLetColumn = startIx
              , descLetPrefix = prefix
              , descLetHead = LetHeaderOnly
              }
        else
          if endsWith ";" suffix
            then
              let firstBinding = stripBindingSuffix suffix
               in if null firstBinding
                    then Nothing
                    else
                      Just
                        LetDescriptor
                          { descLetColumn = startIx
                          , descLetPrefix = prefix
                          , descLetHead = LetInlineBinding firstBinding
                          }
            else
              Nothing

    lastLetToken :: String -> Maybe (Int, Int)
    lastLetToken raw =
      case [(startIx, endIx) | (tok, startIx, endIx) <- keywordTokensOutsideString raw, tok == "let"] of
        [] -> Nothing
        xs -> Just (last xs)

expandInlineLets :: [String] -> [String]
expandInlineLets srcLines =
  let next = expandInlineLetsPass srcLines
   in if next == srcLines
        then next
        else expandInlineLets next

expandInlineLetsPass :: [String] -> [String]
expandInlineLetsPass [] = []
expandInlineLetsPass (line:rest) =
  case maybeExpandInlineLetLine line of
    Nothing -> line : expandInlineLetsPass rest
    Just expanded -> expanded <> expandInlineLetsPass rest

maybeExpandInlineLetLine :: String -> Maybe [String]
maybeExpandInlineLetLine line =
  let (codePrefix, commentSuffix) = splitCommentOutsideString line
   in if not (null commentSuffix)
        then Nothing
        else
          let (indent, codeBody) = span isHorizontalWhitespace codePrefix
           in maybeExpandedLetLayout indent codeBody

maybeExpandedLetLayout :: String -> String -> Maybe [String]
maybeExpandedLetLayout indent codeBody = do
  (prefixRaw, bindingsRaw, bodyRaw) <- findLetLayout codeBody
  let bindings = filter (not . null) (map trimHorizontal (splitSemicolonsOutsideString bindingsRaw))
      body = trimHorizontal bodyRaw
      prefix = trimTrailingHorizontalWhitespace prefixRaw
      hasMultipleBindings = length bindings >= 2
      bodyContainsNestedLet = "let " `isPrefixOf` body || " let " `isInfixOf` body
      shouldExpand = length codeBody >= 80 && (hasMultipleBindings || bodyContainsNestedLet)
      insideCaseArm = "->" `isInfixOf` prefix
  if not shouldExpand || null body || insideCaseArm
    then Nothing
    else
      case bindings of
        [] -> Nothing
        firstBinding:restBindings ->
          let firstLine =
                indent
                  <> (if null prefix then "let " else prefix <> " let ")
                  <> firstBinding
                  <> if null restBindings then "" else ";"
              renderedBindings = renderRestBindings indent restBindings
              inLine = indent <> "  in " <> body
           in Just (firstLine : renderedBindings <> [inLine])

renderRestBindings :: String -> [String] -> [String]
renderRestBindings _ [] = []
renderRestBindings indent [b] = [indent <> "  " <> b]
renderRestBindings indent (b:bs) =
  (indent <> "  " <> b <> ";") : renderRestBindings indent bs

findLetLayout :: String -> Maybe (String, String, String)
findLetLayout codeBody = do
  (letStart, letEnd, rest) <- firstLetToken (keywordTokensOutsideString codeBody)
  (inStart, inEnd) <- matchingInToken 1 rest
  let prefix = take letStart codeBody
      bindingsRaw = take (inStart - letEnd) (drop letEnd codeBody)
      bodyRaw = drop inEnd codeBody
  Just (prefix, bindingsRaw, bodyRaw)

firstLetToken :: [(String, Int, Int)] -> Maybe (Int, Int, [(String, Int, Int)])
firstLetToken [] = Nothing
firstLetToken ((tok, startIx, endIx):rest)
  | tok == "let" = Just (startIx, endIx, rest)
  | otherwise = firstLetToken rest

matchingInToken :: Int -> [(String, Int, Int)] -> Maybe (Int, Int)
matchingInToken _ [] = Nothing
matchingInToken depth ((tok, startIx, endIx):rest)
  | tok == "let" = matchingInToken (depth + 1) rest
  | tok == "in" =
      if depth == 1
        then Just (startIx, endIx)
        else matchingInToken (depth - 1) rest
  | otherwise = matchingInToken depth rest

keywordTokensOutsideString :: String -> [(String, Int, Int)]
keywordTokensOutsideString src = reverse (go 0 False False Nothing [] [] src)
  where
    go :: Int -> Bool -> Bool -> Maybe Int -> String -> [(String, Int, Int)] -> String -> [(String, Int, Int)]
    go ix _inString _escaped tokenStart tokenRev acc [] =
      finalize ix tokenStart tokenRev acc
    go ix inString escaped tokenStart tokenRev acc (c:cs)
      | inString =
          if escaped
            then go (ix + 1) True False tokenStart tokenRev acc cs
            else
              if c == '\\'
                then go (ix + 1) True True tokenStart tokenRev acc cs
                else
                  if c == '"'
                    then go (ix + 1) False False Nothing [] (finalize ix tokenStart tokenRev acc) cs
                    else go (ix + 1) True False tokenStart tokenRev acc cs
      | c == '"' =
          go (ix + 1) True False Nothing [] (finalize ix tokenStart tokenRev acc) cs
      | isKeywordChar c =
          case tokenStart of
            Nothing -> go (ix + 1) False False (Just ix) [toLower c] acc cs
            Just _ -> go (ix + 1) False False tokenStart (toLower c : tokenRev) acc cs
      | otherwise =
          go (ix + 1) False False Nothing [] (finalize ix tokenStart tokenRev acc) cs

    finalize :: Int -> Maybe Int -> String -> [(String, Int, Int)] -> [(String, Int, Int)]
    finalize _ Nothing _ acc = acc
    finalize ix (Just startIx) tokenRev acc = (reverse tokenRev, startIx, ix) : acc

splitSemicolonsOutsideString :: String -> [String]
splitSemicolonsOutsideString = splitTopLevelSemicolons

splitTopLevelSemicolons :: String -> [String]
splitTopLevelSemicolons src = go False False [] [] 0 0 0 0 0 src
  where
    go :: Bool -> Bool -> String -> String -> Int -> Int -> Int -> Int -> Int -> String -> [String]
    go _inString _escaped _tokenRev chunkRev _parens _brackets _braces _letDepth _caseDepth [] =
      [reverse chunkRev]
    go inString escaped tokenRev chunkRev parens brackets braces letDepth caseDepth (c:cs)
      | inString =
          if escaped
            then go True False tokenRev (c : chunkRev) parens brackets braces letDepth caseDepth cs
            else
              if c == '\\'
                then go True True tokenRev (c : chunkRev) parens brackets braces letDepth caseDepth cs
                else
                  if c == '"'
                    then go False False tokenRev (c : chunkRev) parens brackets braces letDepth caseDepth cs
                    else go True False tokenRev (c : chunkRev) parens brackets braces letDepth caseDepth cs
      | c == '"' =
          let (letDepth', caseDepth') = consumeKeywordToken tokenRev letDepth caseDepth
           in go True False [] (c : chunkRev) parens brackets braces letDepth' caseDepth' cs
      | isKeywordChar c =
          go False False (toLower c : tokenRev) (c : chunkRev) parens brackets braces letDepth caseDepth cs
      | otherwise =
          let (letDepth', caseDepth') = consumeKeywordToken tokenRev letDepth caseDepth
              atTopLevel =
                parens == 0
                  && brackets == 0
                  && braces == 0
                  && letDepth' == 0
                  && caseDepth' == 0
              (parens', brackets', braces') = updateDelimDepths parens brackets braces c
           in if c == ';' && atTopLevel
                then
                  let piece = reverse chunkRev
                      next = go False False [] [] parens' brackets' braces' letDepth' caseDepth' cs
                   in piece : next
                else
                  go False False [] (c : chunkRev) parens' brackets' braces' letDepth' caseDepth' cs

    consumeKeywordToken :: String -> Int -> Int -> (Int, Int)
    consumeKeywordToken [] letDepth caseDepth = (letDepth, caseDepth)
    consumeKeywordToken tokenRev letDepth caseDepth =
      let token = reverse tokenRev
          (letDepth', caseDepth') =
            case token of
              "let" -> (letDepth + 1, caseDepth)
              "in" ->
                if letDepth > 0
                  then (letDepth - 1, caseDepth)
                  else (letDepth, caseDepth)
              "case" -> (letDepth, caseDepth + 1)
              "of" ->
                if caseDepth > 0
                  then (letDepth, caseDepth - 1)
                  else (letDepth, caseDepth)
              _ -> (letDepth, caseDepth)
       in (letDepth', caseDepth')

    updateDelimDepths :: Int -> Int -> Int -> Char -> (Int, Int, Int)
    updateDelimDepths parens brackets braces c =
      case c of
        '(' -> (parens + 1, brackets, braces)
        ')' -> (max 0 (parens - 1), brackets, braces)
        '[' -> (parens, brackets + 1, braces)
        ']' -> (parens, max 0 (brackets - 1), braces)
        '{' -> (parens, brackets, braces + 1)
        '}' -> (parens, brackets, max 0 (braces - 1))
        _ -> (parens, brackets, braces)

trimHorizontal :: String -> String
trimHorizontal = dropWhileEnd isHorizontalWhitespace . dropWhile isHorizontalWhitespace

isKeywordChar :: Char -> Bool
isKeywordChar c =
  isAsciiLower c
    || isAsciiUpper c
    || isDigit c
    || c == '_'
    || c == '\''

endsWith :: String -> String -> Bool
endsWith suffix src =
  let n = length suffix
      m = length src
   in n <= m && drop (m - n) src == suffix

indexOfInfix :: String -> String -> Maybe Int
indexOfInfix needle src = go 0 src
  where
    go :: Int -> String -> Maybe Int
    go _ [] = Nothing
    go ix rest =
      if needle `isPrefixOf` rest
        then Just ix
        else
          case rest of
            _:xs -> go (ix + 1) xs

splitCommentOutsideString :: String -> (String, String)
splitCommentOutsideString = go False False []
  where
    go :: Bool -> Bool -> String -> String -> (String, String)
    go _inString _escaped acc [] = (reverse acc, [])
    go inString _escaped acc ('-':'-':rest)
      | not inString = (reverse acc, "--" <> rest)
    go inString escaped acc (c:rest)
      | inString =
          let escaped' = if escaped then False else c == '\\'
              inString' = not escaped && c /= '"' || escaped
              inStringOut = if c == '"' && not escaped then False else inString'
           in go inStringOut escaped' (c:acc) rest
      | c == '"' =
          go True False (c:acc) rest
      | otherwise =
          go False False (c:acc) rest

collapseInnerWhitespace :: String -> String
collapseInnerWhitespace = go False False False []
  where
    go :: Bool -> Bool -> Bool -> String -> String -> String
    go _inString _escaped _prevWasSpace acc [] = reverse acc
    go inString escaped prevWasSpace acc (c:rest)
      | inString =
          let escaped' = if escaped then False else c == '\\'
              inString' = if c == '"' && not escaped then False else True
           in go inString' escaped' False (c:acc) rest
      | c == '"' =
          go True False False (c:acc) rest
      | isHorizontalWhitespace c =
          if prevWasSpace
            then go False False True acc rest
            else go False False True (' ':acc) rest
      | otherwise =
          go False False False (c:acc) rest

isHorizontalWhitespace :: Char -> Bool
isHorizontalWhitespace c =
  c == ' ' || c == '\t' || c == '\f'
