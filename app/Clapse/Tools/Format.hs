module Clapse.Tools.Format
  ( formatSource
  , formatFileToStdout
  , formatFileInPlace
  , formatStdinToStdout
  ) where

import MyLib (formatSource)
import Control.Exception (evaluate)
import Control.Monad (void)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

formatFileToStdout :: FilePath -> IO ()
formatFileToStdout path = do
  src <- readFile path
  case formatSource src of
    Left err -> do
      hPutStrLn stderr ("format error in " <> path <> ": " <> err)
      exitFailure
    Right out ->
      putStr out

formatFileInPlace :: FilePath -> IO ()
formatFileInPlace path = do
  src <- readFile path
  case formatSource src of
    Left err -> do
      hPutStrLn stderr ("format error in " <> path <> ": " <> err)
      exitFailure
    Right out -> do
      -- Force formatted content before opening the same file for writing.
      -- This avoids lazy IO truncation hazards on in-place format.
      void (evaluate (length out))
      writeFile path out

formatStdinToStdout :: IO ()
formatStdinToStdout = do
  src <- getContents
  case formatSource src of
    Left err -> do
      hPutStrLn stderr ("format error: " <> err)
      exitFailure
    Right out ->
      putStr out
