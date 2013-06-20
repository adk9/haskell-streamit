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
import Language.StreamIt.Compile

-- Default runtime method
run :: Target -> Embedding -> IO ()
run t e = case t of
  StreamIt -> case e of 
    EmbBinary bs -> runStrEmbedded bs
    DynLink bs -> runStrDynLink bs
    DynLoad bs -> runStrDynLoad bs
  TBB -> case e of
    EmbBinary bs -> runTBBEmbedded bs
    DynLink bs -> runTBBDynLink bs
    DynLoad bs -> runTBBDynLoad bs

-- | Runs a StreamIt executable embedded as a lazy bytestring
runStrEmbedded :: L.ByteString -> IO ()
runStrEmbedded bs = do
  (tf, th) <- openBinaryTempFile "." "streamit"
  L.hPut th (decompress bs)
  setFileMode tf ownerExecuteMode
  hClose th
  args <- getArgs
  pid <- forkProcess $ executeFile tf False args Nothing
  getProcessStatus True False pid
  removeFile tf

-- | Runs a StreamIt executable embedded as a lazy bytestring
runStrDynLink :: L.ByteString -> IO ()
runStrDynLink s = undefined
  
-- | Runs a StreamIt executable embedded as a lazy bytestring
runStrDynLoad :: L.ByteString -> IO ()
runStrDynLoad s = undefined
  
-- TBB runtime functions
runTBBEmbedded :: L.ByteString -> IO ()
runTBBEmbedded = undefined

runTBBDynLink :: L.ByteString -> IO ()
runTBBDynLink = undefined

runTBBDynLoad :: L.ByteString -> IO ()
runTBBDynLoad = undefined
