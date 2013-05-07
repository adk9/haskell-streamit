module Language.StreamIt.Backend
  ( Target (..)
  , code
  ) where

import Language.StreamIt.Backend.StreamIt
import Language.StreamIt.Backend.TBB
import Language.StreamIt.Core
import Language.StreamIt.Graph

-- | Code generation targets.
data Target
  = StreamIt
  | TBB
  deriving Eq

-- | Generate target code.
code :: Target -> String -> Name -> StatementS -> IO (FilePath)
code target ty name node = f ty name node
  where
  f = case target of
    StreamIt -> codeStreamIt
    TBB -> codeTBB
