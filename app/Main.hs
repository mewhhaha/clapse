module Main (main) where

import Clapse.Tools.Bench (runBench)
import Clapse.Tools.Format (formatFileInPlace, formatFileToStdout, formatStdinToStdout)
import Clapse.Tools.Lsp (runLsp)
import Clapse.Tools.Selfhost (writeSelfhostArtifacts)
import Clapse.Modules (CompileArtifact(..), compileEntryModule, renderTypeScriptBindings)
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension, takeDirectory)
import System.IO (hPutStrLn, stderr)
import Control.Monad (unless)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["format", "--stdin"] ->
      formatStdinToStdout
    ["format", "--write", path] ->
      formatFileInPlace path
    ["format", path] ->
      formatFileToStdout path
    ["compile", inputPath] ->
      compileFile inputPath (replaceExtension inputPath "wasm")
    ["compile", inputPath, outputPath] ->
      compileFile inputPath outputPath
    ["bench"] ->
      runBench 200000
    ["bench", iterationsArg] ->
      case reads iterationsArg of
        [(n, "")] | n > 0 ->
          runBench n
        _ -> do
          hPutStrLn stderr ("bench error: iterations must be a positive integer, got: " <> iterationsArg)
          exitFailure
    ["lsp"] ->
      runLsp
    ["lsp", "--stdio"] ->
      runLsp
    ["selfhost-artifacts", inputPath, outDir] ->
      writeArtifacts inputPath outDir
    _ -> do
      hPutStrLn stderr usage
      exitFailure

usage :: String
usage =
  unlines
    [ "clapse: compiler + formatter + lsp"
    , ""
    , "Usage:"
    , "  clapse format <file>"
    , "  clapse format --write <file>"
    , "  clapse format --stdin"
    , "  clapse compile <input.clapse> [output.wasm]"
    , "    writes <outputPath> and <outputPath with .d.ts extension> (bindings from collapsed IR exports)"
    , "  clapse bench [iterations]"
    , "  clapse lsp"
    , "  clapse lsp --stdio"
    , "  clapse selfhost-artifacts <input.clapse> <out-dir>"
    ]

compileFile :: FilePath -> FilePath -> IO ()
compileFile inputPath outputPath = do
  result <- compileEntryModule inputPath
  case result of
    Left err -> do
      hPutStrLn stderr ("compile error in " <> inputPath <> ": " <> err)
      exitFailure
    Right artifact -> do
      let outDir = takeDirectory outputPath
          tsPath = replaceExtension outputPath "d.ts"
      unless (outDir == "." || null outDir) (createDirectoryIfMissing True outDir)
      BS.writeFile outputPath (artifactWasm artifact)
      writeFile tsPath (renderTypeScriptBindings (artifactExports artifact))

writeArtifacts :: FilePath -> FilePath -> IO ()
writeArtifacts inputPath outDir = do
  result <- writeSelfhostArtifacts inputPath outDir
  case result of
    Left err -> do
      hPutStrLn stderr ("selfhost-artifacts error in " <> inputPath <> ": " <> err)
      exitFailure
    Right _ ->
      pure ()
