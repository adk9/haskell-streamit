module Language.StreamIt.Graph
  ( StreamIt (..)
  , StreamNode (..)
  , GraphInfo
  , evalStream
  , findDefs
  , add
  , pipeline
  ) where

import Data.List
import Control.Monad

import Language.StreamIt.Core
import Language.StreamIt.Filter

data StreamNode where
  AddN     :: AddE a => TypeSig -> Name -> a -> StreamNode
  Pipeline :: StreamNode -> StreamNode
  Chain    :: StreamNode -> StreamNode -> StreamNode
  Empty    :: StreamNode

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

class AddE a where
  add :: TypeSig -> Name -> a -> StreamIt ()
  info :: TypeSig -> Name -> a -> ([FilterInfo], [GraphInfo])

instance AddE (Filter ()) where
  add  a b c = addNode $ AddN a b c
  info a b c = ([(a, b, snd $ evalStmt 0 c)],[])

instance AddE (StreamIt ()) where
  add  a b c = addNode $ AddN a b c
  info a b c = (fst fc, snd fc ++ [(a, b, snd $ evalStream 0 c)])
    where
      fc = findDefs (snd $ evalStream 0 c)

pipeline :: StreamIt () -> StreamIt ()
pipeline n = do
  (id0, node) <- get
  let (id1, node1) = evalStream id0 n
  put (id1, node)
  addNode $ Pipeline node1

type GraphInfo = (TypeSig, Name, StreamNode)

instance DeclE GraphInfo where
  noob a = nubBy (\xs ys -> name xs == name ys) a
    where name (_, n, _) = n

findDefs :: StreamNode -> ([FilterInfo], [GraphInfo])
findDefs a = case a of
  AddN a b c     -> info a b c
  Pipeline a     -> findDefs a
  Chain a b      -> (noob $ fst fa ++ fst fb, noob $ snd fa ++ snd fb)
    where
      fa = findDefs a
      fb = findDefs b
  Empty          -> ([],[])
