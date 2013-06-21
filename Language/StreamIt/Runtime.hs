module Language.StreamIt.Runtime
( run
 ) where

import Control.Monad (void)
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as L
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String
import System.Environment
import System.Directory
import System.IO
import System.Posix.DynamicLinker
import System.Posix.Files
import System.Posix.Process

import Language.StreamIt.Backend
import Language.StreamIt.Compile

type MainFn = CInt -> Ptr CString -> IO CInt
foreign import ccall smain :: MainFn
foreign import ccall "dynamic"
  smainptr :: FunPtr MainFn -> MainFn

-- Default runtime method
run :: Target -> Embedding -> IO ()
run t e = case t of
  StreamIt -> case e of 
    EmbBinary bs -> runStrEmbedded bs
    DynLink s -> runStrDynLink s
    DynLoad s -> runStrDynLoad s
  TBB -> case e of
    EmbBinary bs -> runTBBEmbedded bs
    DynLink s -> runTBBDynLink s
    DynLoad s -> runTBBDynLoad s

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
runStrDynLink :: String -> IO ()
runStrDynLink _ = do
  args <- getArgs
  cs <- mapM newCString args >>= newArray
  pid <- forkProcess $ void $ smain (fromIntegral $ length args) cs
  getProcessStatus True False pid
  return ()

-- | Runs a StreamIt executable embedded as a lazy bytestring
runStrDynLoad :: String -> IO ()
runStrDynLoad s = do
  args <- getArgs
  cs <- mapM newCString args >>= newArray
  dl <- dlopen s [RTLD_LAZY]
  smain <- dlsym dl "smain"
  pid <- forkProcess $ void $ smainptr smain (fromIntegral $ length args) cs
  dlclose dl
  getProcessStatus True False pid
  return ()

-- TBB runtime functions
runTBBEmbedded :: L.ByteString -> IO ()
runTBBEmbedded = undefined

runTBBDynLink :: String -> IO ()
runTBBDynLink = undefined

runTBBDynLoad :: String -> IO ()
runTBBDynLoad = undefined
