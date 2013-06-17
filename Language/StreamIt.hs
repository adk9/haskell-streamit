module Language.StreamIt
  (
  -- * Types
    Exp
  , Var
  , Elt
  , NumE
  , Void
  , Const (..)
  , Array (..)
  -- * Expressions
  -- ** Constants
  , true
  , false
  , constant
  , zero
  , (.++)
  , (.--)
  , (+=.)
  , (-=.)
  , (*=.)
  , (/=.)
  -- ** Variable Reference
  , ref
  , (!)
  -- ** Logical Operations
  , not_
  , (&&.)
  , (||.)
  , and_
  , or_
  -- ** Equality and Comparison
  , (==.)
  , (/==.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  -- ** Arithmetic Operations
  , mod_
  , cond
  -- * Statements
  , Filter
  -- ** Variable Declarations
  , var
  , float
  , float'
  , int
  , int'
  , bool
  , bool'
  , array
  -- ** Variable Assignment
  , (<==)
  -- ** Conditional Execution
  , ifelse
  , if_
  -- ** Iteration
  , for_
  , while_
  -- * StreamIt Statements
  , StreamIt
  -- ** StreamIt Flow-rate Primitives
  , Rate (..)
  , push
  , peek
  , pop
  -- ** StreamIt I/O
  , println
  -- ** StreamIt Work Functions
  , work
  , init'
  -- ** StreamIt Aggregate Filters
  , add
  , add1
  , add2
  , add3
  , pipeline
  , pipeline_
  , splitjoin
  , splitjoin_
  , split
  , join
  , roundrobin
  , fileReader
  , fileWriter
  -- * StreamIt Code Generation
  , Target (..)
  , compile
  , runStreamIt
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
  bs <- liftIO $ callTbb f
  [|(L.pack $(lift (L.unpack bs)))|]

instance MonadIO Q where
  liftIO = runIO
