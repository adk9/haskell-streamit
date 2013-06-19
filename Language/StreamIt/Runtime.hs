module Language.StreamIt.Runtime
( run
 ) where

import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString as B
import Codec.Compression.GZip
import System.Environment
import System.Directory
import System.IO
import System.Posix.Files
import System.Posix.Process

import Language.StreamIt.Backend

runTbb :: L.ByteString -> IO ()
runTbb = undefined

-- | Runs a StreamIt executable at run-time
runStreamIt :: L.ByteString -> IO ()
runStreamIt bs = do
  (tf, th) <- openBinaryTempFile "." "streamit"
  L.hPut th (decompress bs)
  setFileMode tf ownerExecuteMode
  hClose th
  args <- getArgs
  pid <- forkProcess $ executeFile tf False args Nothing
  getProcessStatus True False pid
  removeFile tf


run :: Target -> L.ByteString -> IO ()
run target bs = case target of
  StreamIt -> runStreamIt bs
  TBB -> runTbb bs
