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
  , (*.)
  , (/.)
  , div_
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
  , push
  , peek
  , pop
  , pop'
  , println
  , work
  , init'
  -- ** Variable Assignment
  , Assign (..)
  -- ** Conditional Execution
  , ifelse
  , if_
  , case_
  , (==>)
  , StreamIt
  -- * Stream Graph Constructs
  , add
  , add'
  , pipeline
  -- * Code Generation
  , compileStreamIt
  , genStreamIt
  , runStreamIt
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
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
$(deriveLiftAbstract ''ByteString 'B.pack 'B.unpack)

genStreamIt :: TypeSig -> Name -> StreamIt () -> IO (FilePath)
genStreamIt ty name s = do
  fp <- code ty name $ snd (evalStream 0 s)
  putStrLn $ "Generate file " ++ fp ++ "."
  return fp

compileStreamIt :: TypeSig -> Name -> StreamIt () -> Q Exp
compileStreamIt ty name s = do
  f <- liftIO $ genStreamIt ty name s
  bs <- liftIO $ compileStrc f  
  [|(B.pack $(lift (B.unpack bs)))|]

instance MonadIO Q where
  liftIO = runIO
