module Language.StreamIt.Graph
  ( StreamItT (..)
  , StreamIt
  , StatementS (..)
  , GraphInfo
  , evalStream
  , execStream
  , findDefs
  , add
  , add1
  , add2
  , add3
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

data FileMode = FileReader | FileWriter

data StatementS where
  DeclS     :: Elt a => Var a -> StatementS
  AssignS   :: Elt a => Var a -> Exp a -> StatementS
  BranchS   :: Exp Bool -> StatementS -> StatementS -> StatementS
  LoopS     :: StatementS -> Exp Bool -> StatementS -> StatementS -> StatementS
  AddS      :: AddE a => String -> a -> Maybe (Exp b) -> Maybe (Exp c) -> Maybe (Exp d) -> StatementS
  Pipeline  :: Maybe String -> StatementS -> StatementS
  SplitJoin :: Maybe String -> StatementS -> StatementS
  Split     :: Splitter -> StatementS
  Join      :: Joiner -> StatementS
  Chain     :: StatementS -> StatementS -> StatementS
  File      :: FileMode -> Const -> String -> StatementS
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

-- Returns the complete type (int->int) of the pipeline filter
instance (Typeable a, Typeable b, Typeable m) => Show (StreamIt a b m) where
  show s = map toLower $ (head $ tail t) ++ "->" ++ (head $ tail $ tail t)
    where
      t = words $ (show . typeOf) s

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

-- Helper gensym function
gensym :: AddE a => a -> String -> IO String
gensym obj template = do
  n <- lift $ makeStableName obj
  return (template ++ show (hashStableName n))

{-|
    The 'AddE' class represents add expressions in the StreamIt EDSL. It currently
    supports two variants of the 'add' statement:
    1. add  - accepts a filter or a pipeline that accepts no stream parameters.
    2. add' - used for a filter/pipeline that accepts a list of stream parameters.

    @ add foo @ is equivalent to @ add' foo [] @.
-}
class AddE a where
  add  :: (Elt b, Elt c) => a -> StreamIt b c ()
  add1 :: (Elt b, Elt c) => a -> (Exp d) -> StreamIt b c ()
  add2 :: (Elt b, Elt c) => a -> (Exp d, Exp e) -> StreamIt b c ()
  add3 :: (Elt b, Elt c) => a -> (Exp d, Exp e, Exp f) -> StreamIt b c ()
  info :: String -> a -> S.StateT ([FilterInfo], [GraphInfo]) IO ()

instance (Elt a, Elt b, Typeable a, Typeable b) => AddE (Filter a b ()) where
  add a = do
    name <- gensym a "filt"
    addNode $ AddS name a Nothing Nothing Nothing
  add1 a (b) = do
    name <- gensym a "filt"
    addNode $ AddS name a (Just b) Nothing Nothing
  add2 a (b,c) = do
    name <- gensym a "filt"
    addNode $ AddS name a (Just b) (Just c) Nothing
  add3 a (b,c,d) = do
    name <- gensym a "filt"
    addNode $ AddS name a (Just b) (Just c) (Just d)
  info a b = do
    (f, g) <- S.get
    bs <- liftIO $ execStmt b
    if (elem (ty, a, bs) f)
      then return ()
      else S.put ((ty, a, bs) : f, g)
    where
      ty = show b

instance (Elt a, Elt b, Typeable a, Typeable b) => AddE (StreamIt a b ()) where
  add a = do
    name <- gensym a "filt"
    addNode $ AddS name a Nothing Nothing Nothing
  add1 a (b) = do
    name <- gensym a "filt"
    addNode $ AddS name a (Just b) Nothing Nothing
  add2 a (b,c) = do
    name <- gensym a "filt"
    addNode $ AddS name a (Just b) (Just c) Nothing
  add3 a (b,c,d) = do
    name <- gensym a "filt"
    addNode $ AddS name a (Just b) (Just c) (Just d)  
  info a b = do
    (f, g) <- S.get
    bs <- liftIO $ execStream b
    if (elem (ty, a, bs) g)
      then return ()
      else do
      S.put (f, (ty, a, bs) : g)
      findDefs bs
    where
      ty = show b

instance (Elt a, Elt b) => CoreE (StreamIt a b) where
  var init = do
    (id, stmt) <- get
    n <- lift newUnique
    let sym = Var ("var" ++ show (hashUnique n)) init
    put (id, Chain stmt $ DeclS sym)
    return sym
  float = var zero
  float' = var
  int = var zero
  int' = var
  bool = var zero
  bool' = var
  array _ size = var (Array size zero)
  a <== b = addNode $ AssignS a b
  ifelse cond onTrue onFalse = do
    (id0, node) <- get
    (id1, node1) <- lift $ evalStream id0 onTrue
    (id2, node2) <- lift $ evalStream id1 onFalse
    put (id2, node)
    addNode $ BranchS cond node1 node2
  if_ cond stmt = ifelse cond stmt $ return ()
  for_ (init, cond, inc) body = do
    (id0, stmt) <- get
    (id1, stmt1) <- lift $ evalStream id0 body
    ini <- lift $ execStream init
    inc <- lift $ execStream inc
    put (id1, stmt)
    addNode $ LoopS ini cond inc stmt1
  while_ cond body = do
    (id0, stmt) <- get
    (id1, stmt1) <- lift $ evalStream id0 body
    put (id1, stmt)
    addNode $ LoopS Empty cond Empty stmt1

addPipeline :: (Elt a, Elt b) => Bool -> StreamIt a b () -> StreamIt a b ()
addPipeline t a = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 a
  put (id1, node)
  addNode $ Pipeline t node1

-- 'pipeline' declares a composite stream consisting of
-- multiple children streams.
pipeline :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
pipeline a = do
  name <- gensym a "pipe"
  addPipeline (Just name) a

-- Anonymous inline pipeline functions
pipeline_ :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
pipeline_ = addPipeline Nothing

addSplitjoin :: (Elt a, Elt b) => Bool -> StreamIt a b () -> StreamIt a b ()
addSplitjoin t a = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 a
  put (id1, node)
  addNode $ SplitJoin t node1

-- Split-join composite streams. 
splitjoin :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
splitjoin a = do
  name <- gensym a "splitjn"
  addSplitjoin (Just name) a

-- Anonymous, inline split-join streams. 
splitjoin_ :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
splitjoin_ = addSplitjoin Nothing

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

type GraphInfo = (String,     -- Type signature
                  String,     -- Name of the pipeline
                  StatementS)

findDefs :: StatementS -> S.StateT ([FilterInfo], [GraphInfo]) IO ()
findDefs a = case a of
  BranchS _ a b  -> findDefs a >> findDefs b
  AddS a b _ _ _ -> info a b
  Pipeline _ a   -> findDefs a
  SplitJoin _ a  -> findDefs a
  Chain a b      -> findDefs a >> findDefs b
  _              -> return ()

fileReader :: (Elt a, Elt b, Elt c) => (c -> Const) -> String -> StreamIt a b ()
fileReader ty name = addNode $ File FileReader (ty zero) name

fileWriter :: (Elt a, Elt b, Elt c) => (c -> Const) -> String -> StreamIt a b ()
fileWriter ty name = addNode $ File FileWriter (ty zero) name
