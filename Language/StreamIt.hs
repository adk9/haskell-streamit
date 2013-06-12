module Language.StreamIt
  (
  -- * Types
    Exp
  , Var
  , Elt
  , NumE
  , Void
  , Const (..)
  , Array
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
  , (!)
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
  , add'
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
  , generate
  , genTBB
  , compileTBB
  , genStreamIt
  , compileStreamIt
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
import Language.Haskell.TH hiding (Name, Exp)
import Language.Haskell.TH.Syntax hiding (Name, Exp)
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Lift.Extras

$(deriveLiftAbstract ''Word8 'fromInteger 'toInteger)
$(deriveLiftAbstract ''ByteString 'L.pack 'L.unpack)

generate :: (Elt a, Elt b, Typeable a, Typeable b) => 
            Target -> Name -> StreamIt a b () -> IO (FilePath)
generate tgt name s = do
  st <- liftIO $ execStream s
  fp <- code tgt (showStreamItType s) name st
  putStrLn $ "Generated file " ++ fp ++ "."
  return fp

genTBB :: (Elt a, Elt b, Typeable a, Typeable b) => Name -> StreamIt a b () -> IO (FilePath)
genTBB name s = generate TBB name s

compileTBB :: (Elt a, Elt b, Typeable a, Typeable b) => Name -> StreamIt a b () -> Q THS.Exp
compileTBB name s = do
  f <- liftIO $ generate TBB name s
  bs <- liftIO $ callTbb f
  [|(L.pack $(lift (L.unpack bs)))|]

genStreamIt :: (Elt a, Elt b, Typeable a, Typeable b) => Name -> StreamIt a b () -> IO (FilePath)
genStreamIt name s = generate StreamIt name s

compileStreamIt :: (Elt a, Elt b, Typeable a, Typeable b) => Name -> StreamIt a b () -> Q THS.Exp
compileStreamIt name s = do
  f <- liftIO $ generate StreamIt name s
  bs <- liftIO $ callStrc f
  [|(L.pack $(lift (L.unpack bs)))|]

instance MonadIO Q where
  liftIO = runIO
