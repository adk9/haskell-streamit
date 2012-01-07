module Language.StreamIt
  (
  -- * Types
    E
  , V
  , AllE
  , NumE
  , Name
  -- * Expressions
  -- ** Constants
  , true
  , false
  , constant
  , zero
  , incr
  , decr
  -- ** Variable Reference
  , ref
  -- ** Logical Operations
  , not_
  , (&&.)
  , (||.)
  , and_
  , or_
  , (-->)
  -- ** Equality and Comparison
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  -- ** Arithmetic Operations
  , mod_
  -- * Statements
  , Filter
  -- ** Variable Declarations
  , var
  , input
  , float
  , float'
  , int
  , int'
  , bool
  , bool'
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
  , add'
  , pipeline
  , pipeline_
  , splitjoin
  , splitjoin_
  , split
  , join
  , roundrobin
  -- * StreamIt Code Generation
  , compileStreamIt
  , genStreamIt
  , runStreamIt
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Word
import Control.Monad.Trans (MonadIO(..))
import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph
import Language.StreamIt.Code
import Language.StreamIt.Compile
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Syntax hiding (Name)
import Language.Haskell.TH.Lift.Extras

$(deriveLiftAbstract ''Word8 'fromInteger 'toInteger)
$(deriveLiftAbstract ''ByteString 'L.pack 'L.unpack)

genStreamIt :: TypeSig -> Name -> StreamIt () -> IO (FilePath)
genStreamIt ty name s = do
  st <- liftIO $ execStream s
  fp <- code ty name st
  putStrLn $ "Generated file " ++ fp ++ "."
  return fp

compileStreamIt :: TypeSig -> Name -> StreamIt () -> Q Exp
compileStreamIt ty name s = do
  f <- liftIO $ genStreamIt ty name s
  bs <- liftIO $ compileStrc f  
  [|(L.pack $(lift (L.unpack bs)))|]

instance MonadIO Q where
  liftIO = runIO
