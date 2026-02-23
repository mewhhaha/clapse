module Clapse.Tools.Selfhost
  ( writeSelfhostArtifacts
  ) where

import Clapse.Modules
  ( CompileDebugArtifact(..)
  , compileEntryModuleDebug
  )
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

writeSelfhostArtifacts :: FilePath -> FilePath -> IO (Either String ())
writeSelfhostArtifacts entryPath outDir = do
  result <- compileEntryModuleDebug entryPath
  case result of
    Left err ->
      pure (Left err)
    Right dbg -> do
      createDirectoryIfMissing True outDir
      writeFile (outDir </> "merged_module.txt") (show (debugMergedModule dbg))
      writeFile (outDir </> "type_info.txt") (show (debugTypeInfo dbg))
      writeFile (outDir </> "type_info_error.txt") (show (debugTypeInfoError dbg))
      writeFile (outDir </> "lowered_ir.txt") (show (debugLowered dbg))
      writeFile (outDir </> "collapsed_ir.txt") (show (debugCollapsed dbg))
      writeFile (outDir </> "exports.txt") (show (debugExports dbg))
      writeFile (outDir </> "wasm_stats.txt") (renderWasmStats (debugWasm dbg))
      pure (Right ())

renderWasmStats :: BS.ByteString -> String
renderWasmStats bytes =
  unlines
    [ "size_bytes=" <> show (BS.length bytes)
    , "prefix_hex=" <> concatMap hexByte (BS.unpack (BS.take 32 bytes))
    ]

hexByte :: Word8 -> String
hexByte w =
  let raw = showHex w ""
   in if length raw == 1 then '0' : raw else raw
