module Language.StreamIt.Backend
  ( Target (..)
  , codeGen
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
codeGen :: (Elt a, Elt b, Typeable a, Typeable b) => Target -> StreamIt a b () -> IO (FilePath)
codeGen target s = do
  st <- liftIO $ execStream s
  fp <- f st
  putStrLn $ "Generated file " ++ fp ++ "."
  return fp
  where
    f = case target of
      StreamIt -> codeStreamIt
      TBB -> codeTBB
