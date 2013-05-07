module Language.StreamIt.Graph
  ( StreamItT (..)
  , StreamIt
  , StatementS (..)
  , GraphInfo
  , evalStream
  , execStream
  , findDefs
  , showStreamItType
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

import Data.Char
import Data.Typeable
import Data.Unique
import Control.Monad.Trans
import qualified Control.Monad.State as S
import System.Mem.StableName

import Language.StreamIt.Core
import Language.StreamIt.Filter

data StatementS where
  DeclS     :: AllE a => V a -> StatementS
  AssignS   :: AllE a => V a -> E a -> StatementS
  BranchS   :: E Bool -> StatementS -> StatementS -> StatementS
  AddS      :: (AddE a, AllE b) => Name -> a -> [E b] -> StatementS
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
newtype StreamItT i o m a = StreamItT {runStreamItT :: ((Int, StatementS) -> m (a, (Int, StatementS)))}

type StreamIt a b = StreamItT a b IO

instance (Typeable a, Typeable b) => Typeable1 (StreamIt a b) where
  typeOf1 s = let
    tyCon = mkTyCon3 "Language" "StreamIt" "Graph.StreamIt"
    (a, b) = peel s

    peel :: StreamIt a b m -> (a, b)
    peel = undefined

    in mkTyConApp tyCon [typeOf a, typeOf b]

instance (AllE a, AllE b, Monad m) => Monad (StreamItT a b m) where
  return a    = StreamItT $ \ s -> return (a, s)
  (>>=) sf f  = StreamItT $ \ s -> do (a1, s1) <- runStreamItT sf s
                                      (a2, s2) <- runStreamItT (f a1) s1
                                      return (a2, s2)

instance (AllE a, AllE b) => MonadTrans (StreamItT a b) where
  lift m = StreamItT $ \ s -> do
    a <- m
    return (a, s)

instance (AllE a, AllE b, MonadIO m) => MonadIO (StreamItT a b m) where
	liftIO = lift . liftIO

addNode :: (AllE a, AllE b, Monad m) => StatementS -> StreamItT a b m ()
addNode n = StreamItT $ \ (id, node) -> return ((), (id, Chain node n))

evalStream :: (AllE a, AllE b, Monad m) => Int -> StreamItT a b m () -> m (Int, StatementS)
evalStream id (StreamItT f) = do
  (_, x) <- f (id, Empty)
  return x

execStream:: (AllE a, AllE b, Monad m) => StreamItT a b m () -> m StatementS
execStream n = do
  (_, x) <- evalStream 0 n
  return x

get :: (AllE a, AllE b, Monad m) => StreamItT a b m (Int, StatementS)
get = StreamItT $ \ a -> return (a, a)

put :: (AllE a, AllE b, Monad m) => (Int, StatementS) -> StreamItT a b m ()
put s = StreamItT $ \ _ -> return ((), s)

class AddE a where
  add' :: (AllE b, AllE c, AllE d) => a -> [E d] -> StreamIt b c ()
  add  :: (AllE b, AllE c) => a -> StreamIt b c ()
  info :: Name -> a -> S.StateT ([FilterInfo], [GraphInfo]) IO ()

instance (AllE a, AllE b, Typeable a, Typeable b) => AddE (Filter a b ()) where
  add' a b = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a b
  add  a = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a ([]::[E Int])
  info a b = do
    (f, g) <- S.get
    bs <- liftIO $ execStmt b
    if (elem (ty, a, bs) f)
      then return ()
      else S.put ((ty, a, bs) : f, g)
    where
      ty = showFilterType b

instance (AllE a, AllE b, Typeable a, Typeable b) => AddE (StreamIt a b ()) where
  add' a b = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a b
  add  a = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a ([]::[E Int])
  info a b = do
    (f, g) <- S.get
    bs <- liftIO $ execStream b
    if (elem (ty, a, bs) g)
      then return ()
      else do
      S.put (f, (ty, a, bs) : g)
      findDefs bs
    where
      ty = showStreamItType b

instance (AllE a, AllE b) => CoreE (StreamIt a b) where
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

pipelineT :: (AllE a, AllE b) => Bool -> StreamIt a b () -> StreamIt a b ()
pipelineT t n = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 n
  put (id1, node)
  addNode $ Pipeline t node1

pipeline :: (AllE a, AllE b) => StreamIt a b () -> StreamIt a b ()
pipeline n = pipelineT False n

pipeline_ :: (AllE a, AllE b) => StreamIt a b () -> StreamIt a b ()
pipeline_ n = pipelineT True n

splitjoinT :: (AllE a, AllE b) => Bool -> StreamIt a b () -> StreamIt a b ()
splitjoinT t n = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 n
  put (id1, node)
  addNode $ SplitJoin t node1

splitjoin :: (AllE a, AllE b) => StreamIt a b () -> StreamIt a b ()
splitjoin n = splitjoinT False n

splitjoin_ :: (AllE a, AllE b) => StreamIt a b () -> StreamIt a b ()
splitjoin_ n = splitjoinT True n

class SplitterJoiner a where
  roundrobin :: a

instance SplitterJoiner Splitter where
  roundrobin = RoundrobinS

instance SplitterJoiner Joiner where
  roundrobin = RoundrobinJ

split :: (AllE a, AllE b) => Splitter -> StreamIt a b ()
split s = addNode $ Split s

join :: (AllE a, AllE b) => Joiner -> StreamIt a b ()
join j = addNode $ Join j

type GraphInfo = (TypeSig, Name, StatementS)

findDefs :: StatementS -> S.StateT ([FilterInfo], [GraphInfo]) IO ()
findDefs a = case a of
  DeclS _        -> return ()
  AssignS _ _    -> return ()
  BranchS _ a b  -> findDefs a >> findDefs b
  AddS a b _     -> info a b
  Pipeline _ a   -> findDefs a
  SplitJoin _ a  -> findDefs a
  Split _        -> return ()
  Join _         -> return ()
  Chain a b      -> findDefs a >> findDefs b
  Empty          -> return ()

showStreamItType :: (Typeable a, Typeable b) => StreamIt a b () -> String 
showStreamItType s = map toLower $ (head $ tail t) ++ "->" ++ (head $ tail $ tail t)
  where
    t = words $ showTypeSig s
