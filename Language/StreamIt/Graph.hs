module Language.StreamIt.Graph
  ( StreamItT (..)
  , StreamIt
  , StatementS (..)
  , FileMode (..)
  , GraphDecl
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
import Control.Monad.Trans
import qualified Control.Monad.State as S

import Language.StreamIt.Core
import Language.StreamIt.Filter

-- Type of file filter.
data FileMode = FileReader | FileWriter

data StatementS where
  DeclS     :: Elt a => Var a -> StatementS
  AssignS   :: Elt a => Var a -> Exp a -> StatementS
  BranchS   :: Exp Bool -> StatementS -> StatementS -> StatementS
  LoopS     :: StatementS -> Exp Bool -> StatementS -> StatementS -> StatementS
  AddS      :: (AddE a) => a -> String -> (Maybe (Exp b), Maybe (Exp c), Maybe (Exp d)) -> StatementS
  Pipeline  :: Bool -> StatementS -> StatementS
  SplitJoin :: Bool -> StatementS -> StatementS
  Split     :: Splitter -> StatementS
  Join      :: Joiner -> StatementS
  Chain     :: StatementS -> StatementS -> StatementS
  File      :: FileMode -> Const -> String -> StatementS
  Empty     :: StatementS

instance Eq (StatementS) where (==) _ _ = True

-- Type of splitters
data Splitter = RoundrobinS

instance Show Splitter where
  show sp = case sp of
    RoundrobinS -> "roundrobin()"

-- Type of joiners
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

-- Returns the complete type (int->int) of a pipeline filter
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

-- GraphDecl = (Type, Name, Arguments, AST node)
type GraphDecl = (String, String, String, StatementS)

instance (Elt a, Elt b) => CoreE (StreamIt a b) where
  var init = do
    (id, stmt) <- get
    sym <- lift $ gensym init 
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

----------------------------------------------------------------------------

--    The 'AddE' class represents add expressions in the StreamIt EDSL.
class AddE a where
  -- Add a primitive or composite stream to the stream graph.
  add  :: (Elt b, Elt c) => a -> StreamIt b c ()
  -- Add a primitive or composite stream accepting 1 argument.
  add1 :: (Elt b, Elt c, Elt d) => (Var d -> a) -> (Exp d) -> StreamIt b c ()
  -- Add a primitive or composite stream accepting 2 arguments.
  add2 :: (Elt b, Elt c, Elt d, Elt e) => (Var d -> Var e -> a) -> (Exp d, Exp e) -> StreamIt b c ()
  -- Add a primitive or composite stream accepting 3 arguments.  
  add3 :: (Elt b, Elt c, Elt d, Elt e, Elt f) => (Var d -> Var e -> Var f -> a) -> (Exp d, Exp e, Exp f) -> StreamIt b c ()
  -- Given an AST node, returns a list of filter and pipeline declarations
  -- in the body.
  info :: a -> String-> S.StateT ([FilterDecl], [GraphDecl]) IO ()

instance (Elt a, Elt b, Typeable a, Typeable b) => AddE (Filter a b ()) where
  add a = addNode $ AddS a "" (Nothing, Nothing, Nothing)
  add1 a (b) = do
    x <- lift $ genExpVar b
    addNode $ AddS (a x) (showVarDecl x) (Just b, Nothing, Nothing)
  add2 a (b,c) = do
    x <- lift $ genExpVar b 
    y <- lift $ genExpVar c
    addNode $ AddS (a x y) (showVarDecl x ++ "," ++ showVarDecl y) (Just b, Just c, Nothing)
  add3 a (b,c,d) = do
    x <- lift $ genExpVar b
    y <- lift $ genExpVar c
    z <- lift $ genExpVar d
    addNode $ AddS (a x y z) (showVarDecl x ++ "," ++ showVarDecl y ++ "," ++ showVarDecl z) (Just b, Just c, Just d)
  info b args = do
    (f, g) <- S.get
    bs <- liftIO $ execStmt b
    name <- liftIO $ newStableName bs "filt"
    if (elem (show b, name, args, bs) f)
      then return ()
      else S.put ((show b, name, args, bs) : f, g)

instance (Elt a, Elt b, Typeable a, Typeable b) => AddE (StreamIt a b ()) where
  add a = addNode $ AddS a "" (Nothing, Nothing, Nothing)
  add1 a (b) = do
    x <- lift $ genExpVar b
    addNode $ AddS (a x) (showVarDecl x) (Just b, Nothing, Nothing)
  add2 a (b,c) = do
    x <- lift $ genExpVar b 
    y <- lift $ genExpVar c
    addNode $ AddS (a x y) (showVarDecl x ++ "," ++ showVarDecl y) (Just b, Just c, Nothing)
  add3 a (b,c,d) = do
    x <- lift $ genExpVar b
    y <- lift $ genExpVar c
    z <- lift $ genExpVar d
    addNode $ AddS (a x y z) (showVarDecl x ++ "," ++ showVarDecl y ++ "," ++ showVarDecl z) (Just b, Just c, Just d)
  info b args = do
    (f, g) <- S.get
    bs <- liftIO $ execStream b
    name <- liftIO $ newStableName bs "filt"
    if (elem (show b, name, args, bs) g)
      then return ()
      else do
      S.put (f, (show b, name, args, bs) : g)
      findDefs bs

----------------------------------------------------------------------------

addPipeline :: (Elt a, Elt b) => Bool -> StreamIt a b () -> StreamIt a b ()
addPipeline named a = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 a
  put (id1, node)
  addNode $ Pipeline named node1

-- 'pipeline' declares a composite stream consisting of
-- multiple children streams.
pipeline :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
pipeline = addPipeline True

-- Anonymous, inline pipeline function.
pipeline_ :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
pipeline_ = addPipeline False

addSplitjoin :: (Elt a, Elt b) => Bool -> StreamIt a b () -> StreamIt a b ()
addSplitjoin named a = do
  (id0, node) <- get
  (id1, node1) <- lift $ evalStream id0 a
  put (id1, node)
  addNode $ SplitJoin named node1

-- Split-join composite streams. 
splitjoin :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
splitjoin = addSplitjoin True

-- An anonymous, inline split-join stream. 
splitjoin_ :: (Elt a, Elt b) => StreamIt a b () -> StreamIt a b ()
splitjoin_ = addSplitjoin False

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

-- Recurse down the root of the AST and return the filter and pipeline
-- definitions in the program.
findDefs :: StatementS -> S.StateT ([FilterDecl], [GraphDecl]) IO ()
findDefs st = case st of
  BranchS _ a b   -> findDefs a >> findDefs b
  AddS a args _   -> info a args
  Pipeline _ a    -> findDefs a
  SplitJoin _ a   -> findDefs a
  Chain a b       -> findDefs a >> findDefs b
  _               -> return ()

fileReader :: (Elt a, Elt b, Elt c) => (c -> Const) -> String -> StreamIt a b ()
fileReader ty fname = addNode $ File FileReader (ty zero) fname

fileWriter :: (Elt a, Elt b, Elt c) => (c -> Const) -> String -> StreamIt a b ()
fileWriter ty fname = addNode $ File FileWriter (ty zero) fname
