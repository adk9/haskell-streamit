module Language.StreamIt.Graph
  ( StreamIt (..)
  , StreamNode (..)
  , evalStream
  , FilterInfo
  , streamFilters
  , add
  , pipeline
  ) where

import Data.List
import Control.Monad

import Language.StreamIt.Core
import Language.StreamIt.Filter

data StreamNode where
  Chain    :: StreamNode -> StreamNode -> StreamNode
  Empty    :: StreamNode
  AddF     :: TypeSig -> Name -> Filter () -> StreamNode
  Pipeline :: TypeSig -> Name -> StreamNode -> StreamNode

type FilterInfo = (TypeSig, Name, Statement)

-- | The StreamIt monad holds the StreamIt graph.
data StreamIt a = StreamIt ((Int, StreamNode) -> (a, (Int, StreamNode)))

instance Monad StreamIt where
  return a = StreamIt $ \ s -> (a, s)
  (StreamIt f1) >>= f2 = StreamIt f3
    where
    f3 s1 = f4 s2
      where
      (a, s2) = f1 s1
      StreamIt f4 = f2 a

addNode :: StreamNode -> StreamIt ()
addNode n = StreamIt $ \ (id, node) -> ((), (id, Chain node n))

evalStream :: Int -> StreamIt () -> (Int, StreamNode)
evalStream id (StreamIt f) = snd $ f (id, Empty)

get :: StreamIt (Int, StreamNode)
get = StreamIt $ \ a -> (a, a)

put :: (Int, StreamNode) -> StreamIt ()
put s = StreamIt $ \ _ -> ((), s)

noob :: [FilterInfo] -> [FilterInfo]
noob a = nubBy (\xs ys -> name xs == name ys) a
  where name (_, n, _) = n

streamFilters :: StreamNode -> [FilterInfo]
streamFilters a = case a of
  AddF a b c     -> noob $ [(a, b, filterInfo c)]
  Pipeline _ _ c -> noob $ streamFilters c
  Chain a b      -> noob $ streamFilters a ++ streamFilters b
  Empty          -> []
  where
    filterInfo :: Filter () -> Statement
    filterInfo c = snd (evalStmt 0 c)
  
add :: TypeSig -> Name -> Filter () -> StreamIt ()
add ty name filt = addNode $ AddF ty name filt

pipeline :: TypeSig -> Name -> StreamIt () -> StreamIt ()
pipeline ty name n = do
  (id0, node) <- get
  let (id1, node1) = evalStream id0 n
  put (id1, node)
  addNode $ Pipeline ty name node1
