module Language.StreamIt.Compile
 ( compileStrc,
   runStreamIt
 ) where

-- import qualified Data.ByteString as B
import Control.Monad.State
import Control.Exception
import qualified Data.ByteString as B
import System.Cmd
import System.Exit
import System.Directory
import System.IO
import System.IO.Temp
import System.Posix.Files
import System.Posix.Process

compileStrc :: FilePath -> IO (B.ByteString)
compileStrc file = do
  withTempDirectory "." "streamit." $ \tdir -> do
    bracket getCurrentDirectory setCurrentDirectory
      (\_ -> do
          setCurrentDirectory tdir
          exitCode <- rawSystem "strc" ["../" ++ file]
          when (exitCode /= ExitSuccess) $
            fail "strc failed."
          B.readFile "a.out")

runStreamIt :: B.ByteString -> IO ()
runStreamIt bs = do
  (tf, th) <- openBinaryTempFile "." "streamit"
  B.hPut th bs
  setFileMode tf ownerExecuteMode
  hClose th
  pid <- forkProcess $ executeFile tf False [] Nothing
  getProcessStatus True False pid
  removeFile tf
