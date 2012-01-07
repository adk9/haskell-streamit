module Language.StreamIt.Graph
  ( StreamItT (..)
  , StreamIt
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

import Data.Unique
import Control.Monad hiding (join)
import Control.Monad.Trans
import qualified Control.Monad.State as S
import System.Mem.StableName

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

instance Eq (StatementS) where (==) _ _ = True

data Splitter = RoundrobinS

instance Show Splitter where
  show sp = case sp of
    RoundrobinS -> "roundrobin()"

data Joiner = RoundrobinJ

instance Show Joiner where
  show jn = case jn of
    RoundrobinJ -> "roundrobin()"

-- | The StreamIt monad holds the StreamIt graph.
newtype StreamItT m a = StreamItT {runStreamItT :: ((Int, StatementS) -> m (a, (Int, StatementS)))}

type StreamIt = StreamItT IO

instance Monad m => Monad (StreamItT m) where
  return a    = StreamItT $ \ s -> return (a, s)
  (>>=) sf f  = StreamItT $ \ s -> do (a1, s1) <- runStreamItT sf s
                                      (a2, s2) <- runStreamItT (f a1) s1
                                      return (a2, s2)

instance MonadTrans StreamItT where
  lift m = StreamItT $ \ s -> do
    a <- m
    return (a, s)

addNode :: Monad m => StatementS -> StreamItT m ()
addNode n = StreamItT $ \ (id, node) -> return ((), (id, Chain node n))

evalStream :: Monad m => Int -> StreamItT m () -> m (Int, StatementS)
evalStream id (StreamItT f) = do
  (_, x) <- f (id, Empty)
  return x

execStream:: Monad m => StreamItT m () -> m StatementS
execStream n = do
  (_, x) <- evalStream 0 n
  return x

get :: Monad m => StreamItT m (Int, StatementS)
get = StreamItT $ \ a -> return (a, a)

put :: Monad m => (Int, StatementS) -> StreamItT m ()
put s = StreamItT $ \ _ -> return ((), s)

class AddE a where
  add' :: AllE b => TypeSig -> a -> [E b] -> StreamIt ()
  add  :: TypeSig -> a -> StreamIt ()
  info :: TypeSig -> Name -> a -> S.StateT ([FilterInfo], [GraphInfo]) IO ()

instance AddE (Filter ()) where
  add' a b c = do
    n <- lift $ makeStableName b
    addNode $ AddS a ("filt" ++ show (hashStableName n)) b c
  add  a b = do
    n <- lift $ makeStableName b
    addNode $ AddS a ("filt" ++ show (hashStableName n)) b ([]::[E Int])
  info a b c = do
    (f, g) <- S.get
    cs <- liftIO $ execStmt c
    if (elem (a, b, cs) f)
      then return ()
      else S.put ((a, b, cs) : f, g)

instance AddE (StreamIt ()) where
  add' a b c = do
    n <- lift $ makeStableName b
    addNode $ AddS a ("filt" ++ show (hashStableName n)) b c
  add  a b = do
    n <- lift $ makeStableName b
    addNode $ AddS a ("filt" ++ show (hashStableName n)) b ([]::[E Int])
  info a b c = do
    (f, g) <- S.get
    cs <- liftIO $ execStream c
    if (elem (a, b, cs) g)
      then return ()
      else do
      S.put (f, (a, b, cs) : g)
      findDefs cs

instance CoreE (StreamIt) where
  var input init = do
    (id, stmt) <- get
    n <- lift newUnique
    put (id, Chain stmt $ DeclS (V input ("var" ++ show (hashUnique n)) init))
    return $ V input ("var" ++ show (hashUnique n)) init
  input _ = var True zero
  float = var False zero
  float' = var False
  int = var False zero
  int' = var False
  bool = var False zero
  bool' = var False
  a <== b = addNode $ AssignS a b
  ifelse cond onTrue onFalse = do
    (id0, node) <- get
    (id1, node1) <- lift $ evalStream id0 onTrue
    (id2, node2) <- lift $ evalStream id1 onFalse
    put (id2, node)
    addNode $ BranchS cond node1 node2
  if_ cond stmt = ifelse cond stmt $ return ()

pipelineT :: Bool -> StreamIt () -> StreamIt ()
pipelineT t n = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 n
  put (id1, node)
  addNode $ Pipeline t node1

pipeline :: StreamIt () -> StreamIt ()
pipeline n = pipelineT False n

pipeline_ :: StreamIt () -> StreamIt ()
pipeline_ n = pipelineT True n

splitjoinT :: Bool -> StreamIt () -> StreamIt ()
splitjoinT t n = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 n
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

findDefs :: StatementS -> S.StateT ([FilterInfo], [GraphInfo]) IO ()
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
