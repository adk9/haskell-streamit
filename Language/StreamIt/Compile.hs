module Language.StreamIt.Compile
 ( Embedding (..),
   compile,
 ) where

import Data.ByteString.Lazy (ByteString)
import Data.Word
import Data.Typeable
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as DT (replace, pack, unpack)
import Control.Exception
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Trans (MonadIO(..))
import Codec.Compression.GZip
import System.Cmd
import System.Exit
import System.Directory
import System.IO.Temp
import System.Posix.Files
import Language.Haskell.TH hiding (Exp)
import Language.Haskell.TH.Syntax hiding (Exp)
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Lift.Extras

import Language.StreamIt.Backend
import Language.StreamIt.Core
import Language.StreamIt.Graph

$(deriveLiftAbstract ''Word8 'fromInteger 'toInteger)
$(deriveLiftAbstract ''ByteString 'L.pack 'L.unpack)

data Embedding = EmbBinary L.ByteString
               | DynLink String
               | DynLoad String

-- | Compiles the given StreamIt file (using strc) to an executable and
--   returns the directory in which the file was generated.
callStrc :: FilePath -> IO (FilePath) 
callStrc file = do
  fp <- createTempDirectory "." "streamit.d"
  bracket getCurrentDirectory setCurrentDirectory
    (\_ -> do
        setCurrentDirectory fp
        exitCode <- rawSystem "strc" ["../" ++ file]
        S.when (exitCode /= ExitSuccess) $
          fail "strc failed.")
  return fp

-- | Compile a generated TBB C++ file (using g++) and returns the directory
--   in which the file was generated
callTbb file = do
  fp <- createTempDirectory "." "streamit.d"
  bracket getCurrentDirectory setCurrentDirectory
    (\_ -> do
        setCurrentDirectory fp
        exitCode <- rawSystem "g++" ["-O2 -DNDEBUG", "../" ++ file, "-ltbb -lrt"]
        S.when (exitCode /= ExitSuccess) $
          fail "g++ failed.")
  return fp

generateSharedLib :: FilePath -> IO String
generateSharedLib tdir = do
  bracket getCurrentDirectory setCurrentDirectory
    (\_ -> do
        setCurrentDirectory tdir
        txt <- readFile "combined_threads.cpp"
        writeFile "combined_threads.cpp" $ 
          DT.unpack (DT.replace (DT.pack "int main(int argc") (DT.pack "extern \"C\" int smain(int argc") (DT.pack txt))
        exist <- fileExist "combined_threads.o"
        when exist $ removeFile "combined_threads.o"
        exitCode <- rawSystem "g++" ["-O3 -fPIC -I$STREAMIT_HOME/library/cluster", "-c -o combined_threads.o" , "combined_threads.cpp"]
        when (exitCode /= ExitSuccess) $
          fail "g++ failed."
        exitCode <- rawSystem "g++" ["-O3 -fPIC -shared -Wl,-soname,libaout.so -o ../libaout.so", "combined_threads.o", "-L$STREAMIT_HOME/library/cluster", "-lpthread -lcluster -lstdc++"]
        when (exitCode /= ExitSuccess) $
          fail "g++ failed.")
  return "libaout.so"

-- Compile and embed the resulting executable as a lazy bytestring
compileEmbed :: (Elt a, Elt b, Typeable a, Typeable b) => Target -> StreamIt a b () -> Q THS.Exp
compileEmbed target s = do
  f <- liftIO $ codeGen target s
  dir <- case target of
    StreamIt -> liftIO $ callStrc f
    TBB -> liftIO $ callTbb f
  bs <- liftIO $ L.readFile $ dir ++ "/a.out"
  liftIO $ (ignoringIOErrors . removeDirectory) dir
  [| EmbBinary (L.pack $(lift (L.unpack $ compress bs)))|]

instance MonadIO Q where
  liftIO = runIO

-- Compile as a shared library and dynamically link to it
compileDynLink :: (Elt a, Elt b, Typeable a, Typeable b) => Target -> StreamIt a b () -> Q THS.Exp
compileDynLink target s = do
  f <- liftIO $ codeGen target s
  dir <- case target of
    StreamIt -> liftIO $ callStrc f
    TBB -> liftIO $ callTbb f
  shlib <- liftIO $ generateSharedLib dir
  liftIO $ (ignoringIOErrors . removeDirectory) dir
  -- dynamically link libaout.so
  [| DynLink $(lift shlib)|]

-- Compile as a shared library and load it using `dlsym` at runtime
compileDynLoad :: (Elt a, Elt b, Typeable a, Typeable b) => Target -> StreamIt a b () -> Q THS.Exp
compileDynLoad target s = do
  f <- liftIO $ codeGen target s
  dir <- case target of
    StreamIt -> liftIO $ callStrc f
    TBB -> liftIO $ callTbb f
  shlib <- liftIO $ generateSharedLib dir
  liftIO $ (ignoringIOErrors . removeDirectory) dir  
  [| DynLoad $(lift shlib)|]

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` (\e -> const (return ()) (e :: IOError))

-- Default compilation method
compile :: (Elt a, Elt b, Typeable a, Typeable b) => Target -> StreamIt a b () -> Q THS.Exp
compile = compileDynLoad
