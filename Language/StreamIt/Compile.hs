module Language.StreamIt.Compile
 ( compileStrc,
   runStreamIt
 ) where

-- import qualified Data.ByteString as B
import Control.Monad.State
import Control.Exception
import qualified Data.ByteString.Lazy as L
import Codec.Compression.GZip
import System.Cmd
import System.Exit
import System.Directory
import System.IO
import System.IO.Temp
import System.Posix.Files
import System.Posix.Process

-- | Takes a StreamIt file, compiles it and returns the resulting executable as a ByteString
compileStrc :: FilePath -> IO (L.ByteString)
compileStrc file = do
  withTempDirectory "." "streamit." $ \tdir -> do
    bracket getCurrentDirectory setCurrentDirectory
      (\_ -> do
          setCurrentDirectory tdir
          exitCode <- rawSystem "strc" ["../" ++ file]
          when (exitCode /= ExitSuccess) $
            fail "strc faied."
          bs <- L.readFile "a.out"
          return $ compress bs)

-- | Runs a StreamIt executable (stored as ByteString) at runtime
runStreamIt :: L.ByteString -> IO ()
runStreamIt bs = do
  (tf, th) <- openBinaryTempFile "." "streamit"
  L.hPut th (decompress bs)
  setFileMode tf ownerExecuteMode
  hClose th
  pid <- forkProcess $ executeFile tf False [] Nothing
  getProcessStatus True False pid
  removeFile tf
