module Clapse.Tools.Lsp
  ( runLsp
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)

import Clapse.Tools.Format (formatSource)
import MyLib
  ( FunctionTypeInfo(..)
  , inferSourceTypes
  , parseFunctionLine
  , parseModule
  , renderType
  )
import System.IO (BufferMode(NoBuffering), hIsEOF, hSetBuffering, stdin, stdout)

data LspState = LspState
  { didShutdown :: Bool
  , docs :: [(String, String)]
  }

data Token = Token
  { tokText :: String
  , tokStart :: Int
  , tokEnd :: Int
  }

data FunctionSite = FunctionSite
  { siteLine :: Int
  , siteFnToken :: Token
  , siteArgTokens :: [Token]
  }

data Diagnostic = Diagnostic
  { diagLine :: Int
  , diagChar :: Int
  , diagSeverity :: Int
  , diagMessage :: String
  }

runLsp :: IO ()
runLsp = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  loop LspState {didShutdown = False, docs = []}

loop :: LspState -> IO ()
loop st = do
  msg <- readMessage
  case msg of
    Nothing -> pure ()
    Just raw -> do
      next <- handleMessage st raw
      case next of
        Nothing -> pure ()
        Just st' -> loop st'

handleMessage :: LspState -> String -> IO (Maybe LspState)
handleMessage st raw =
  case extractMethod raw of
    Just "initialize" -> do
      sendResponse raw initializeResult
      pure (Just st)
    Just "shutdown" -> do
      sendResponse raw "null"
      pure (Just st {didShutdown = True})
    Just "textDocument/didOpen" -> do
      st' <- handleDocumentUpdate st raw
      pure (Just st')
    Just "textDocument/didChange" -> do
      st' <- handleDocumentUpdate st raw
      pure (Just st')
    Just "textDocument/formatting" -> do
      handleFormatting st raw
      pure (Just st)
    Just "textDocument/hover" -> do
      handleHover st raw
      pure (Just st)
    Just "exit" ->
      if didShutdown st then pure Nothing else pure Nothing
    _ ->
      pure (Just st)

initializeResult :: String
initializeResult =
  "{\"capabilities\":{\"textDocumentSync\":1,\"documentFormattingProvider\":true,\"hoverProvider\":true}}"

handleDocumentUpdate :: LspState -> String -> IO LspState
handleDocumentUpdate st raw =
  case (extractQuotedField "uri" raw, extractQuotedField "text" raw) of
    (Just uriRaw, Just textRaw) -> do
      let uri = decodeOrRaw uriRaw
          text = decodeOrRaw textRaw
          nextDocs = upsertDoc uri text (docs st)
      publishDiagnostics uri text
      pure st {docs = nextDocs}
    _ ->
      pure st

handleFormatting :: LspState -> String -> IO ()
handleFormatting st raw =
  case extractQuotedField "uri" raw of
    Nothing ->
      sendResponse raw "[]"
    Just uriRaw -> do
      let uri = decodeOrRaw uriRaw
      case lookup uri (docs st) of
        Nothing ->
          sendResponse raw "[]"
        Just src ->
          case formatSource src of
            Left _ -> sendResponse raw "[]"
            Right formatted ->
              if formatted == src
                then sendResponse raw "[]"
                else sendResponse raw (formattingEdits src formatted)

handleHover :: LspState -> String -> IO ()
handleHover st raw =
  case (extractQuotedField "uri" raw, extractPosition raw) of
    (Just uriRaw, Just (lineNo, charNo)) -> do
      let uri = decodeOrRaw uriRaw
      case lookup uri (docs st) of
        Nothing -> sendResponse raw "null"
        Just src ->
          case hoverPayload src lineNo charNo of
            Nothing -> sendResponse raw "null"
            Just payload -> sendResponse raw payload
    _ ->
      sendResponse raw "null"

formattingEdits :: String -> String -> String
formattingEdits oldSrc newSrc =
  let (endLine, endChar) = endPosition oldSrc
      escaped = escapeJson newSrc
   in
    "[{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":"
      <> show endLine
      <> ",\"character\":"
      <> show endChar
      <> "}},\"newText\":\""
      <> escaped
      <> "\"}]"

publishDiagnostics :: String -> String -> IO ()
publishDiagnostics uri src =
  let payload = diagnosticsPayload uri (diagnosticsForSource src)
   in writeNotification payload

diagnosticsForSource :: String -> [Diagnostic]
diagnosticsForSource src =
  case parseModule src of
    Left parseErr ->
      [ Diagnostic
          { diagLine = max 0 (parseErrorLine parseErr - 1)
          , diagChar = 0
          , diagSeverity = 1
          , diagMessage = parseErr
          }
      ]
    Right _ ->
      case inferSourceTypes src of
        Left typeErr ->
          [ Diagnostic
              { diagLine = 0
              , diagChar = 0
              , diagSeverity = 1
              , diagMessage = typeErr
              }
          ]
        Right _ ->
          []

diagnosticsPayload :: String -> [Diagnostic] -> String
diagnosticsPayload uri ds =
  let dsJson = "[" <> joinWith "," (map diagnosticJson ds) <> "]"
   in "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\""
        <> escapeJson uri
        <> "\",\"diagnostics\":"
        <> dsJson
        <> "}}"

diagnosticJson :: Diagnostic -> String
diagnosticJson d =
  "{\"range\":{\"start\":{\"line\":"
    <> show (diagLine d)
    <> ",\"character\":"
    <> show (diagChar d)
    <> "},\"end\":{\"line\":"
    <> show (diagLine d)
    <> ",\"character\":"
    <> show (diagChar d + 1)
    <> "}},\"severity\":"
    <> show (diagSeverity d)
    <> ",\"source\":\"clapse\",\"message\":\""
    <> escapeJson (diagMessage d)
    <> "\"}"

hoverPayload :: String -> Int -> Int -> Maybe String
hoverPayload src lineNo charNo = do
  word <- identifierAt src lineNo charNo
  typeInfos <- either (const Nothing) Just (inferSourceTypes src)
  let sites = functionSites src
  msg <- hoverMessage typeInfos sites lineNo word
  Just
    ("{\"contents\":{\"kind\":\"plaintext\",\"value\":\""
      <> escapeJson msg
      <> "\"}}"
    )

hoverMessage :: [FunctionTypeInfo] -> [FunctionSite] -> Int -> String -> Maybe String
hoverMessage infos sites lineNo ident =
  case hoverParamType infos sites lineNo ident of
    Just t -> Just (ident <> " : " <> t)
    Nothing ->
      case lookupFnInfo infos ident of
        Just info -> Just (ident <> " : " <> renderType (fnType info))
        Nothing -> Nothing

hoverParamType :: [FunctionTypeInfo] -> [FunctionSite] -> Int -> String -> Maybe String
hoverParamType infos sites lineNo ident = do
  site <- findSiteByLine sites lineNo
  info <- lookupFnInfo infos (tokText (siteFnToken site))
  idx <- findArgIndex (siteArgTokens site) ident
  ty <- lookupIndex idx (fnArgTypes info)
  Just (renderType ty)

functionSites :: String -> [FunctionSite]
functionSites src =
  mapMaybe lineSite (zip [0 :: Int ..] (splitLines src))
  where
    lineSite :: (Int, String) -> Maybe FunctionSite
    lineSite (lineNo, rawLine) =
      let lineNoComment = stripComment rawLine
          cleaned = trim lineNoComment
       in case parseFunctionLine cleaned of
            Left _ -> Nothing
            Right _ -> buildSite lineNo lineNoComment

buildSite :: Int -> String -> Maybe FunctionSite
buildSite lineNo rawLine = do
  eqPos <- findCharIndex '=' rawLine
  let tokens = filter (\t -> tokEnd t <= eqPos) (tokenizeIdentifiers rawLine)
  case tokens of
    [] -> Nothing
    fnTok:argToks ->
      Just
        FunctionSite
          { siteLine = lineNo
          , siteFnToken = fnTok
          , siteArgTokens = argToks
          }

tokenizeIdentifiers :: String -> [Token]
tokenizeIdentifiers = go 0
  where
    go :: Int -> String -> [Token]
    go _ [] = []
    go ix s@(c:cs)
      | isIdentifierStart c =
          let (name, rest, endIx) = consumeIdent ix s
           in Token {tokText = name, tokStart = ix, tokEnd = endIx} : go endIx rest
      | otherwise =
          go (ix + 1) cs

consumeIdent :: Int -> String -> (String, String, Int)
consumeIdent start s =
  let (name, rest) = span isIdentifierChar s
      endIx = start + length name
   in (name, rest, endIx)

identifierAt :: String -> Int -> Int -> Maybe String
identifierAt src lineNo charNo = do
  line <- lookupIndex lineNo (splitLines src)
  let toks = tokenizeIdentifiers line
  tok <- findTokenAt toks charNo
  Just (tokText tok)

findTokenAt :: [Token] -> Int -> Maybe Token
findTokenAt [] _ = Nothing
findTokenAt (t:ts) pos
  | tokStart t <= pos && pos < tokEnd t = Just t
  | otherwise = findTokenAt ts pos

lookupFnInfo :: [FunctionTypeInfo] -> String -> Maybe FunctionTypeInfo
lookupFnInfo [] _ = Nothing
lookupFnInfo (i:is) key
  | fnName i == key = Just i
  | otherwise = lookupFnInfo is key

findSiteByLine :: [FunctionSite] -> Int -> Maybe FunctionSite
findSiteByLine [] _ = Nothing
findSiteByLine (s:ss) lineNo
  | siteLine s == lineNo = Just s
  | otherwise = findSiteByLine ss lineNo

findArgIndex :: [Token] -> String -> Maybe Int
findArgIndex = go 0
  where
    go :: Int -> [Token] -> String -> Maybe Int
    go _ [] _ = Nothing
    go i (t:ts) ident
      | tokText t == ident = Just i
      | otherwise = go (i + 1) ts ident

parseErrorLine :: String -> Int
parseErrorLine err =
  case findAfter "line " err of
    Nothing -> 1
    Just rest ->
      case span isDigit rest of
        ([], _) -> 1
        (digits, _) ->
          case reads digits of
            [(n, "")] -> n
            _ -> 1

extractPosition :: String -> Maybe (Int, Int)
extractPosition json = do
  posChunk <- findAfter "\"position\":{" json
  lineNo <- extractFirstNumberAfter "\"line\":" posChunk
  charNo <- extractFirstNumberAfter "\"character\":" posChunk
  Just (lineNo, charNo)

extractFirstNumberAfter :: String -> String -> Maybe Int
extractFirstNumberAfter needle haystack = do
  rest <- findAfter needle haystack
  let digits = takeWhile isDigit (dropWhile isSpace rest)
  if null digits
    then Nothing
    else
      case reads digits of
        [(n, "")] -> Just n
        _ -> Nothing

endPosition :: String -> (Int, Int)
endPosition src =
  let ls = splitLines src
   in case ls of
        [] -> (0, 0)
        _ -> (length ls - 1, length (last ls))

splitLines :: String -> [String]
splitLines [] = [""]
splitLines s = go [] [] s
  where
    go :: [String] -> String -> String -> [String]
    go acc cur [] = reverse (reverse cur : acc)
    go acc cur (c:cs)
      | c == '\n' = go (reverse cur : acc) [] cs
      | otherwise = go acc (c : cur) cs

upsertDoc :: String -> String -> [(String, String)] -> [(String, String)]
upsertDoc uri text allDocs =
  (uri, text) : filter (\(u, _) -> u /= uri) allDocs

sendResponse :: String -> String -> IO ()
sendResponse requestJson resultJson =
  case extractIdRaw requestJson of
    Nothing -> pure ()
    Just rid ->
      writeMessage
        ( "{\"jsonrpc\":\"2.0\",\"id\":"
            <> rid
            <> ",\"result\":"
            <> resultJson
            <> "}"
        )

writeNotification :: String -> IO ()
writeNotification = writeMessage

readMessage :: IO (Maybe String)
readMessage = do
  headers <- readHeaders []
  case headers of
    Nothing -> pure Nothing
    Just hdrs ->
      case parseContentLength hdrs of
        Nothing -> pure Nothing
        Just n -> do
          body <- BS.hGet stdin n
          if BS.length body /= n
            then pure Nothing
            else pure (Just (C8.unpack body))

readHeaders :: [String] -> IO (Maybe [String])
readHeaders acc = do
  eof <- hIsEOF stdin
  if eof
    then pure Nothing
    else do
      lineBs <- C8.hGetLine stdin
      if BS.null lineBs
        then pure (Just (reverse acc))
        else do
          let line = trim (stripCR (C8.unpack lineBs))
          if null line
            then pure (Just (reverse acc))
            else readHeaders (line : acc)

stripCR :: String -> String
stripCR [] = []
stripCR xs =
  case reverse xs of
    '\r':rest -> reverse rest
    _ -> xs

stripComment :: String -> String
stripComment [] = []
stripComment ('-':'-':_) = []
stripComment (c:cs) = c : stripComment cs

parseContentLength :: [String] -> Maybe Int
parseContentLength [] = Nothing
parseContentLength (h:hs) =
  case splitHeader h of
    Just (k, v)
      | k == "Content-Length" ->
          case reads v of
            [(n, "")] -> Just n
            _ -> Nothing
      | otherwise ->
          parseContentLength hs
    Nothing ->
      parseContentLength hs

splitHeader :: String -> Maybe (String, String)
splitHeader header =
  let (k, rest) = break (== ':') header
   in case rest of
        [] -> Nothing
        (_:v) -> Just (trim k, trim v)

writeMessage :: String -> IO ()
writeMessage payload = do
  let body = C8.pack payload
      header = C8.pack ("Content-Length: " <> show (BS.length body) <> "\r\n\r\n")
  BS.putStr header
  BS.putStr body

extractMethod :: String -> Maybe String
extractMethod = extractQuotedField "method"

extractIdRaw :: String -> Maybe String
extractIdRaw json = do
  start <- findAfter "\"id\":" json
  let valueStart = dropWhile isSpace start
  consumeJsonValue valueStart

extractQuotedField :: String -> String -> Maybe String
extractQuotedField field json = do
  start <- findAfter ("\"" <> field <> "\":\"") json
  takeUntilUnescapedQuote start

findAfter :: String -> String -> Maybe String
findAfter needle haystack = go haystack
  where
    go [] = Nothing
    go s@(_:rest)
      | needle `isPrefixOf` s = Just (drop (length needle) s)
      | otherwise = go rest

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

takeUntilUnescapedQuote :: String -> Maybe String
takeUntilUnescapedQuote = go False []
  where
    go :: Bool -> String -> String -> Maybe String
    go _escaped _acc [] = Nothing
    go escaped acc (c:cs)
      | c == '"' && not escaped = Just (reverse acc)
      | c == '\\' && not escaped = go True (c : acc) cs
      | otherwise = go False (c : acc) cs

consumeJsonValue :: String -> Maybe String
consumeJsonValue [] = Nothing
consumeJsonValue (c:cs)
  | c == '"' = do
      rest <- takeQuotedWithQuotes cs
      Just ("\"" <> rest)
  | c == '[' = takeBracketed '[' ']' 1 ['['] cs
  | c == '{' = takeBracketed '{' '}' 1 ['{'] cs
  | otherwise = Just (trimRight (takeSimpleValue (c : cs)))

takeQuotedWithQuotes :: String -> Maybe String
takeQuotedWithQuotes = go False []
  where
    go :: Bool -> String -> String -> Maybe String
    go _escaped _acc [] = Nothing
    go escaped acc (c:cs)
      | c == '"' && not escaped = Just (reverse ('"' : c : acc))
      | c == '\\' && not escaped = go True (c : acc) cs
      | otherwise = go False (c : acc) cs

takeBracketed :: Char -> Char -> Int -> String -> String -> Maybe String
takeBracketed _open _close depth acc [] =
  if depth == 0 then Just (reverse acc) else Nothing
takeBracketed open close depth acc (c:cs)
  | c == open =
      takeBracketed open close (depth + 1) (c : acc) cs
  | c == close =
      let depth' = depth - 1
          acc' = c : acc
       in if depth' == 0
            then Just (reverse acc')
            else takeBracketed open close depth' acc' cs
  | otherwise =
      takeBracketed open close depth (c : acc) cs

takeSimpleValue :: String -> String
takeSimpleValue [] = []
takeSimpleValue (c:cs)
  | c == ',' || c == '}' = []
  | otherwise = c : takeSimpleValue cs

decodeOrRaw :: String -> String
decodeOrRaw raw =
  case decodeJsonString raw of
    Right decoded -> decoded
    Left _ -> raw

decodeJsonString :: String -> Either String String
decodeJsonString = go []
  where
    go :: String -> String -> Either String String
    go acc [] = Right (reverse acc)
    go acc ('\\':'n':xs) = go ('\n' : acc) xs
    go acc ('\\':'r':xs) = go ('\r' : acc) xs
    go acc ('\\':'t':xs) = go ('\t' : acc) xs
    go acc ('\\':'"':xs) = go ('"' : acc) xs
    go acc ('\\':'\\':xs) = go ('\\' : acc) xs
    go acc ('\\':'/':xs) = go ('/' : acc) xs
    go _ ('\\':'u':_) = Left "unicode escapes are not supported"
    go _ ('\\':_) = Left "unsupported json escape"
    go acc (x:xs) = go (x : acc) xs

escapeJson :: String -> String
escapeJson [] = []
escapeJson (c:cs)
  | c == '"' = '\\' : '"' : escapeJson cs
  | c == '\\' = '\\' : '\\' : escapeJson cs
  | c == '\n' = '\\' : 'n' : escapeJson cs
  | c == '\r' = '\\' : 'r' : escapeJson cs
  | c == '\t' = '\\' : 't' : escapeJson cs
  | otherwise = c : escapeJson cs

trim :: String -> String
trim = trimRight . dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse

isIdentifierStart :: Char -> Bool
isIdentifierStart c = isAsciiLower c || isAsciiUpper c || c == '_'

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isIdentifierStart c || isDigit c || c == '\''

findCharIndex :: Char -> String -> Maybe Int
findCharIndex target = go 0
  where
    go :: Int -> String -> Maybe Int
    go _ [] = Nothing
    go i (c:cs)
      | c == target = Just i
      | otherwise = go (i + 1) cs

lookupIndex :: Int -> [a] -> Maybe a
lookupIndex idx xs
  | idx < 0 = Nothing
  | otherwise =
      case drop idx xs of
        y:_ -> Just y
        [] -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _f [] = []
mapMaybe f (x:xs) =
  case f x of
    Nothing -> mapMaybe f xs
    Just y -> y : mapMaybe f xs

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x:xs) = x <> sep <> joinWith sep xs
