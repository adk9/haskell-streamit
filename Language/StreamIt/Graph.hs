module Language.StreamIt.Graph
  ( StreamIt (..)
  , StatementS (..)
  , GraphInfo
  , evalStream
  , execStream
  , findDefs
  , add'
  , add
  , pipeline
  , pipeline_
  , splitjoin
  , splitjoin_  
  , split
  , join
  , roundrobin
  ) where

import Control.Monad hiding (join)
import qualified Control.Monad.State as S

import Language.StreamIt.Core
import Language.StreamIt.Filter

data StatementS where
  DeclS     :: AllE a => V a -> StatementS
  AssignS   :: AllE a => V a -> E a -> StatementS
  BranchS   :: E Bool -> StatementS -> StatementS -> StatementS
  AddS      :: (AddE a, AllE b) => TypeSig -> Name -> a -> [E b] -> StatementS
  Pipeline  :: Bool -> StatementS -> StatementS
  SplitJoin :: Bool -> StatementS -> StatementS
  Split     :: Splitter -> StatementS
  Join      :: Joiner -> StatementS
  Chain     :: StatementS -> StatementS -> StatementS
  Empty     :: StatementS

instance Eq StatementS where (==) _ _ = True

instance Show StatementS where
  show st = case st of
    DeclS _         -> "DeclS"
    AssignS _ _     -> "AssignS"
    BranchS _ _ _   -> "BranchS"
    AddS t n _ _    -> "AddS " ++ t ++ " " ++ n
    Pipeline _ _    -> "Pipeline"
    SplitJoin _ _   -> "SplitJoin"
    Split _         -> "Split"
    Join _          -> "Join"
    Chain _ _       -> "Chain"
    Empty           -> "Empty"

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

execStream:: StreamIt () -> StatementS 
execStream n = snd (evalStream 0 n)

get :: StreamIt (Int, StatementS)
get = StreamIt $ \ a -> (a, a)

put :: (Int, StatementS) -> StreamIt ()
put s = StreamIt $ \ _ -> ((), s)

class AddE a where
  add :: AllE b => TypeSig -> Name -> a -> [E b] -> StreamIt ()
  add' :: TypeSig -> Name -> a -> StreamIt ()
  info :: TypeSig -> Name -> a -> S.State ([FilterInfo], [GraphInfo]) ()

instance AddE (Filter ()) where
  add  a b c d = addNode $ AddS a b c d
  add' a b c = addNode $ AddS a b c ([]::[E Int])
  info a b c = do
    (f, g) <- S.get
    if (elem (a, b, execStmt c) f)
      then return ()
      else S.put ((a, b, execStmt c) : f, g)

instance AddE (StreamIt ()) where
  add  a b c d = addNode $ AddS a b c d
  add' a b c = addNode $ AddS a b c ([]::[E Int])
  info a b c = do
    (f, g) <- S.get
    if (elem (a, b, execStream c) g)
      then return ()
      else do
      S.put (f, (a, b, execStream c) : g)
      findDefs (execStream c)

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

pipelineT :: Bool -> StreamIt () -> StreamIt ()
pipelineT t n = do
  (id0, node) <- get
  let (id1, node1) = evalStream id0 n
  put (id1, node)
  addNode $ Pipeline t node1

pipeline :: StreamIt () -> StreamIt ()
pipeline n = pipelineT False n

pipeline_ :: StreamIt () -> StreamIt ()
pipeline_ n = pipelineT True n

splitjoinT :: Bool -> StreamIt () -> StreamIt ()
splitjoinT t n = do
  (id0, node) <- get
  let (id1, node1) = evalStream id0 n
  put (id1, node)
  addNode $ SplitJoin t node1

splitjoin :: StreamIt () -> StreamIt ()
splitjoin n = splitjoinT False n

splitjoin_ :: StreamIt () -> StreamIt ()
splitjoin_ n = splitjoinT True n

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

findDefs :: StatementS -> S.State ([FilterInfo], [GraphInfo]) ()
findDefs a = case a of
  DeclS _        -> return ()
  AssignS _ _    -> return ()
  BranchS _ a b  -> findDefs a >> findDefs b
  AddS a b c _   -> info a b c
  Pipeline _ a   -> findDefs a
  SplitJoin _ a  -> findDefs a
  Split _        -> return ()
  Join _         -> return ()
  Chain a b      -> findDefs a >> findDefs b
  Empty          -> return ()
