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
  , fileReader
  , fileWriter
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
  DeclS     :: Elt a => Var a -> StatementS
  AssignS   :: Elt a => Var a -> Exp a -> StatementS
  BranchS   :: Exp Bool -> StatementS -> StatementS -> StatementS
  AddS      :: (AddE a, Elt b) => Name -> a -> [Exp b] -> StatementS
  Pipeline  :: Bool -> StatementS -> StatementS
  SplitJoin :: Bool -> StatementS -> StatementS
  Split     :: Splitter -> StatementS
  Join      :: Joiner -> StatementS
  Chain     :: StatementS -> StatementS -> StatementS
  File      :: Bool -> Const -> Name -> StatementS
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

-- | The StreamIt monad holds StreamIt statements.
newtype StreamItT i o m a = StreamItT {runStreamItT :: ((Int, StatementS) -> m (a, (Int, StatementS)))}

type StreamIt a b = StreamItT a b IO

instance (Typeable a, Typeable b) => Typeable1 (StreamIt a b) where
  typeOf1 s = let
    tyCon = mkTyCon3 "Language" "StreamIt" "Graph.StreamIt"
    (a, b) = peel s

    peel :: StreamIt a b m -> (a, b)
    peel = undefined

    in mkTyConApp tyCon [typeOf a, typeOf b]

instance (Elt a, Elt b, Monad m) => Monad (StreamItT a b m) where
  return a    = StreamItT $ \ s -> return (a, s)
  (>>=) sf f  = StreamItT $ \ s -> do (a1, s1) <- runStreamItT sf s
                                      (a2, s2) <- runStreamItT (f a1) s1
                                      return (a2, s2)

instance (Elt a, Elt b) => MonadTrans (StreamItT a b) where
  lift m = StreamItT $ \ s -> do
    a <- m
    return (a, s)

instance (Elt a, Elt b, MonadIO m) => MonadIO (StreamItT a b m) where
	liftIO = lift . liftIO

addNode :: (Elt a, Elt b, Monad m) => StatementS -> StreamItT a b m ()
addNode n = StreamItT $ \ (id, node) -> return ((), (id, Chain node n))

evalStream :: (Elt a, Elt b, Monad m) => Int -> StreamItT a b m () -> m (Int, StatementS)
evalStream id (StreamItT f) = do
  (_, x) <- f (id, Empty)
  return x

execStream:: (Elt a, Elt b, Monad m) => StreamItT a b m () -> m StatementS
execStream n = do
  (_, x) <- evalStream 0 n
  return x

get :: (Elt a, Elt b, Monad m) => StreamItT a b m (Int, StatementS)
get = StreamItT $ \ a -> return (a, a)

put :: (Elt a, Elt b, Monad m) => (Int, StatementS) -> StreamItT a b m ()
put s = StreamItT $ \ _ -> return ((), s)

{-|
    The 'AddE' class represents the add expressions in the StreamIt EDSL. It currently
    support two variants of the 'add' statement:
    1. add - which accepts a filter or a pipeline that accepts no stream parameters.
    2. add' - which is used for a filter/pipeline that accepts a list of stream parameters.

    @ add foo @ is equivalent to @ add' foo [] @ and thus, only provides sugar to make
    some of the examples look less verbose.
-}
class AddE a where
  add' :: (Elt b, Elt c, Elt d) => a -> [Exp d] -> StreamIt b c ()
  add  :: (Elt b, Elt c) => a -> StreamIt b c ()
  info :: Name -> a -> S.StateT ([FilterInfo], [GraphInfo]) IO ()

instance (Elt a, Elt b, Typeable a, Typeable b) => AddE (Filter a b ()) where
  add' a b = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a b
  add  a = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a ([]::[Exp Int])
  info a b = do
    (f, g) <- S.get
    bs <- liftIO $ execStmt b
    if (elem (ty, a, bs) f)
      then return ()
      else S.put ((ty, a, bs) : f, g)
    where
      ty = showFilterType b

instance (Elt a, Elt b, Typeable a, Typeable b) => AddE (StreamIt a b ()) where
  add' a b = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a b
  add  a = do
    n <- lift $ makeStableName a
    addNode $ AddS ("filt" ++ show (hashStableName n)) a ([]::[Exp Int])
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

instance (Elt a, Elt b) => CoreE (StreamIt a b) where
  var input init = do
    (id, stmt) <- get
    n <- lift newUnique
    put (id, Chain stmt $ DeclS (Var input ("var" ++ show (hashUnique n)) init))
    return $ Var input ("var" ++ show (hashUnique n)) init
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

{-|
    The 'pipeline' function is used to declare a composite stream consisting of
    multiple child streams. The variant 'pipeline_' is similar except that it
    does not gensym a new pipeline filter, and instead adds the pipeline definition
    inline in the generated code.
-}
pipelineT :: (Elt a, Elt b) => Bool -> StreamIt a b () -> StreamIt a b ()
pipelineT t n = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 n
  put (id1, node)
  addNode $ Pipeline t node1

pipeline :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
pipeline n = pipelineT False n

pipeline_ :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
pipeline_ n = pipelineT True n

{-|
    The two variants of 'splitjoin' are similar to those of the 'pipeline' statement
    described above.
-}
splitjoinT :: (Elt a, Elt b) => Bool -> StreamIt a b () -> StreamIt a b ()
splitjoinT t n = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 n
  put (id1, node)
  addNode $ SplitJoin t node1

splitjoin :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
splitjoin n = splitjoinT False n

splitjoin_ :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
splitjoin_ n = splitjoinT True n

class SplitterJoiner a where
  roundrobin :: a

instance SplitterJoiner Splitter where
  roundrobin = RoundrobinS

instance SplitterJoiner Joiner where
  roundrobin = RoundrobinJ

split :: (Elt a, Elt b) => Splitter -> StreamIt a b ()
split s = addNode $ Split s

join :: (Elt a, Elt b) => Joiner -> StreamIt a b ()
join j = addNode $ Join j

type GraphInfo = (TypeSig, Name, StatementS)

findDefs :: StatementS -> S.StateT ([FilterInfo], [GraphInfo]) IO ()
findDefs a = case a of
  BranchS _ a b  -> findDefs a >> findDefs b
  AddS a b _     -> info a b
  Pipeline _ a   -> findDefs a
  SplitJoin _ a  -> findDefs a
  Chain a b      -> findDefs a >> findDefs b
  _              -> return ()

showStreamItType :: (Typeable a, Typeable b) => StreamIt a b () -> String 
showStreamItType s = map toLower $ (head $ tail t) ++ "->" ++ (head $ tail $ tail t)
  where
    t = words $ showTypeSig s

fileReader :: (Elt a, Elt b, Elt c) => (c -> Const) -> Name -> StreamIt a b ()
fileReader ty name = addNode $ File False (ty zero) name

fileWriter :: (Elt a, Elt b, Elt c) => (c -> Const) -> Name -> StreamIt a b ()
fileWriter ty name = addNode $ File True (ty zero) name
