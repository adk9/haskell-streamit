module Language.StreamIt.Compile
 ( compile,
 ) where

import Data.ByteString.Lazy (ByteString)
import Data.Word
import Data.Typeable
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString as B
import Control.Exception
import qualified Control.Monad.State as S
import Control.Monad.Trans (MonadIO(..))
import Codec.Compression.GZip
import System.Cmd
import System.Exit
import System.Directory
import System.IO.Temp
import Language.Haskell.TH hiding (Exp)
import Language.Haskell.TH.Syntax hiding (Exp)
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Lift.Extras

import Language.StreamIt.Backend
import Language.StreamIt.Core
import Language.StreamIt.Graph

$(deriveLiftAbstract ''Word8 'fromInteger 'toInteger)
$(deriveLiftAbstract ''ByteString 'L.pack 'L.unpack)

-- | Compile the given StreamIt file (using strc) to a resulting executable
callStrc :: FilePath -> IO (L.ByteString)
callStrc file = do
  withTempDirectory "." "streamit." $ \tdir -> do
    bracket getCurrentDirectory setCurrentDirectory
      (\_ -> do
          setCurrentDirectory tdir
          exitCode <- rawSystem "strc" ["../" ++ file]
          S.when (exitCode /= ExitSuccess) $
            fail "strc failed."
          bs <- L.readFile "a.out"
          return $ compress bs)

-- | Compile a generated TBB C++ file (using g++)
callTbb :: FilePath -> IO (L.ByteString)
callTbb file = do
  withTempDirectory "." "tbb." $ \tdir -> do
    bracket getCurrentDirectory setCurrentDirectory
      (\_ -> do
          setCurrentDirectory tdir
          exitCode <- rawSystem "g++" ["-O2 -DNDEBUG", "../" ++ file, "-ltbb -lrt"]
          S.when (exitCode /= ExitSuccess) $
            fail "g++ failed."
          bs <- L.readFile "a.out"
          return $ compress bs)

compile :: (Elt a, Elt b, Typeable a, Typeable b) => Target -> StreamIt a b () -> Q THS.Exp
compile target s = do
  f <- liftIO $ codeGen target s
  bs <- case target of
    StreamIt -> liftIO $ callStrc f
    TBB -> liftIO $ callTbb f
  [|(L.pack $(lift (L.unpack bs)))|]

instance MonadIO Q where
  liftIO = runIO
