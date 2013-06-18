module Language.StreamIt
  (
    module Language.StreamIt.Core,
    module Language.StreamIt.Filter,
    module Language.StreamIt.Graph,
    module Language.StreamIt.Backend,
    module Language.StreamIt.Compile,
    -- * StreamIt Code Generation
    compile
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Typeable
import Control.Monad.Trans (MonadIO(..))
import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph
import Language.StreamIt.Backend
import Language.StreamIt.Compile
import Language.Haskell.TH hiding (Exp)
import Language.Haskell.TH.Syntax hiding (Exp)
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Lift.Extras

$(deriveLiftAbstract ''Word8 'fromInteger 'toInteger)
$(deriveLiftAbstract ''ByteString 'L.pack 'L.unpack)

compile :: (Elt a, Elt b, Typeable a, Typeable b) => Target -> StreamIt a b () -> Q THS.Exp
compile target s = do
  f <- liftIO $ codeGen target s
  bs <- case target of
    StreamIt -> liftIO $ callStrc f
    TBB -> liftIO $ callTbb f
  [|(L.pack $(lift (L.unpack bs)))|]

instance MonadIO Q where
  liftIO = runIO
