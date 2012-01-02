module Language.StreamIt.Graph
  ( StreamIt (..)
  , StatementS (..)
  , GraphInfo
  , evalStream
  , findDefs
  , add'
  , add
  , pipeline
  , splitjoin
  , split
  , join
  , roundrobin
  ) where

import Data.List
import Control.Monad hiding (join)

import Language.StreamIt.Core
import Language.StreamIt.Filter

data StatementS where
  DeclS     :: AllE a => V a -> StatementS
  AssignS   :: AllE a => V a -> E a -> StatementS
  BranchS   :: E Bool -> StatementS -> StatementS -> StatementS
  AddS      :: (AddE a, AllE b) => TypeSig -> Name -> a -> [E b] -> StatementS
  Pipeline  :: StatementS -> StatementS
  SplitJoin :: StatementS -> StatementS
  Split     :: Splitter -> StatementS
  Join      :: Joiner -> StatementS
  Chain     :: StatementS -> StatementS -> StatementS
  Empty     :: StatementS

instance Eq StatementS where (==) _ _ = True

data Splitter = RoundrobinS

instance Show Splitter where
  show sp = case sp of
    RoundrobinS -> "roundrobin()"

data Joiner = RoundrobinJ

instance Show Joiner where
  show jn = case jn of
    RoundrobinJ -> "roundrobin()"

-- | The StreamIt monad holds the StreamIt graph.
data StreamIt a = StreamIt ((Int, StatementS) -> (a, (Int, StatementS)))

instance Monad StreamIt where
  return a = StreamIt $ \ s -> (a, s)
  (StreamIt f1) >>= f2 = StreamIt f3
    where
    f3 s1 = f4 s2
      where
      (a, s2) = f1 s1
      StreamIt f4 = f2 a

addNode :: StatementS -> StreamIt ()
addNode n = StreamIt $ \ (id, node) -> ((), (id, Chain node n))

evalStream :: Int -> StreamIt () -> (Int, StatementS)
evalStream id (StreamIt f) = snd $ f (id, Empty)

get :: StreamIt (Int, StatementS)
get = StreamIt $ \ a -> (a, a)

put :: (Int, StatementS) -> StreamIt ()
put s = StreamIt $ \ _ -> ((), s)

class AddE a where
  add :: AllE b => TypeSig -> Name -> a -> [E b] -> StreamIt ()
  add' :: TypeSig -> Name -> a -> StreamIt ()
  info :: TypeSig -> Name -> a -> ([FilterInfo], [GraphInfo])

instance AddE (Filter ()) where
  add  a b c d = addNode $ AddS a b c d
  add' a b c = addNode $ AddS a b c ([]::[E Int])
  info a b c = ([(a, b, snd $ evalStmt 0 c)],[])

instance AddE (StreamIt ()) where
  add  a b c d = addNode $ AddS a b c d
  add' a b c = addNode $ AddS a b c ([]::[E Int])
  info a b c = (fst fc, snd fc ++ [(a, b, snd $ evalStream 0 c)])
    where
      fc = findDefs (snd $ evalStream 0 c)

instance CoreE (StreamIt) where
  var input name init = do
    (id, stmt) <- get
    put (id, Chain stmt $ DeclS (V input name init))
    return $ V input name init
  input _ name = var True name zero
  float name = var False name zero
  float' = var False
  int name = var False name zero
  int' = var False
  bool name = var False name zero
  bool' = var False
  a <== b = addNode $ AssignS a b
  ifelse cond onTrue onFalse = do
    (id0, node) <- get
    let (id1, node1) = evalStream id0 onTrue
        (id2, node2) = evalStream id1 onFalse
    put (id2, node)
    addNode $ BranchS cond node1 node2
  if_ cond stmt = ifelse cond stmt $ return ()

pipeline :: StreamIt () -> StreamIt ()
pipeline n = do
  (id0, node) <- get
  let (id1, node1) = evalStream id0 n
  put (id1, node)
  addNode $ Pipeline node1

splitjoin :: StreamIt () -> StreamIt ()
splitjoin n = do
  (id0, node) <- get
  let (id1, node1) = evalStream id0 n
  put (id1, node)
  addNode $ SplitJoin node1

class SplitterJoiner a where
  roundrobin :: a

instance SplitterJoiner Splitter where
  roundrobin = RoundrobinS

instance SplitterJoiner Joiner where
  roundrobin = RoundrobinJ

split :: Splitter -> StreamIt ()
split s = addNode $ Split s

join :: Joiner -> StreamIt ()
join j = addNode $ Join j

type GraphInfo = (TypeSig, Name, StatementS)

findDefs :: StatementS -> ([FilterInfo], [GraphInfo])
findDefs a = case a of
  DeclS _        -> ([],[])
  AssignS _ _    -> ([],[])
  BranchS _ a Empty -> ([],[])
  BranchS _ a b  -> (nub $ fst fa ++ fst fb, nub $ snd fa ++ snd fb)
    where
      fa = findDefs a
      fb = findDefs b
  AddS a b c _   -> info a b c
  Pipeline a     -> findDefs a
  SplitJoin a    -> findDefs a
  Split _        -> ([],[])
  Join _         -> ([],[])
  Chain a b      -> (nub $ fst fa ++ fst fb, nub $ snd fa ++ snd fb)
    where
      fa = findDefs a
      fb = findDefs b
  Empty          -> ([],[])
