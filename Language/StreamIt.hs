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
  , float
  , float'
  , int
  , int'
  , bool
  , bool'
  , push
  , peek
  , pop
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
  -- * Code Generation
  , filter'
  -- * General Analysis
  , analyze
  ) where

import Language.StreamIt.Core
import Language.StreamIt.Filter
import qualified Language.StreamIt.Code as C

filter' :: String -> Name -> Filter () -> IO ()
filter' ty name filt = analyze (C.code ty name) filt

-- | Generic program analysis.
analyze :: (Statement -> IO a) -> Filter () -> IO a
analyze f program = f stmt
  where
  (_, stmt) = evalStmt 0 program
