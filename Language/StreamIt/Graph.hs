module Language.StreamIt.Graph
  ( StreamIt (..)
  , StreamNode (..)
  , GraphInfo
  , evalStream
  , findDefs
  , add
  , add'
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
  AddN     :: TypeSig -> Name -> StreamIt () -> StreamNode
  Pipeline :: StreamNode -> StreamNode

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

type GraphInfo = (TypeSig, Name, StreamNode)

findDefs :: StreamNode -> ([FilterInfo], [GraphInfo])
findDefs a = case a of
  AddF a b c     -> ([(a, b, snd $ evalStmt 0 c)],[])
  AddN a b c     -> (fst fc, snd fc ++ [(a, b, snd $ evalStream 0 c)])
    where
      fc = findDefs (snd $ evalStream 0 c)
  Pipeline a     -> findDefs a
  Chain a b      -> (noob $ fst fa ++ fst fb, noob' $ snd fa ++ snd fb)
    where
      fa = findDefs a
      fb = findDefs b
  Empty          -> ([],[])
  where
    noob :: [FilterInfo] -> [FilterInfo]
    noob a = nubBy (\xs ys -> name xs == name ys) a
      where name (_, n, _) = n

    noob' :: [GraphInfo] -> [GraphInfo]
    noob' a = nubBy (\xs ys -> name xs == name ys) a
      where name (_, n, _) = n

add :: TypeSig -> Name -> Filter () -> StreamIt ()
add ty name filt = addNode $ AddF ty name filt

add' :: TypeSig -> Name -> StreamIt () -> StreamIt ()
add' ty name node = addNode $ AddN ty name node

pipeline :: StreamIt () -> StreamIt ()
pipeline n = do
  (id0, node) <- get
  let (id1, node1) = evalStream id0 n
  put (id1, node)
  addNode $ Pipeline node1
