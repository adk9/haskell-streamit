module Language.StreamIt.Filter
  ( Statement (..)
  , FilterT (..)
  , Rate (..)
  , Filter
  , FilterDecl
  , evalStmt
  , execStmt
  , rate
  , push
  , peek
  , pop
  , println
  , work
  , init'
  ) where

import Data.Char
import Data.List
import Data.Typeable
import Control.Monad.Trans

import Language.StreamIt.Core

data Statement where
  Decl     :: Elt a => Var a -> Statement
  Assign   :: Elt a => Var a -> Exp a -> Statement
  Branch   :: Exp Bool -> Statement -> Statement -> Statement
  Loop     :: Statement -> Exp Bool -> Statement -> Statement -> Statement
  Sequence :: Statement -> Statement -> Statement
  Work     :: Rate -> Statement -> Statement
  Init     :: Statement -> Statement
  Push     :: Elt a => Exp a -> Statement
  Pop      :: Statement
  Println  :: Statement -> Statement
  Null     :: Statement

instance Eq (Statement) where (==) _ _ = True

-- | The Filter monad holds StreamIt filter statements.
newtype FilterT i o m a = FilterT {runFilterT :: ((Int, Statement) -> m (a, (Int, Statement)))}

type Filter a b = FilterT a b IO

instance (Typeable a, Typeable b) => Typeable1 (Filter a b) where
  typeOf1 s = let
    tyCon = mkTyCon3 "Language" "StreamIt" "Filter.Filter"
    (a, b) = peel s

    peel :: Filter a b m -> (a, b)
    peel = undefined

    in mkTyConApp tyCon [typeOf a, typeOf b]

instance (Monad m) => Monad (FilterT a b m) where
  return a    = FilterT $ \ s -> return (a, s)
  (>>=) sf f  = FilterT $ \ s -> do (a1, s1) <- runFilterT sf s
                                    (a2, s2) <- runFilterT (f a1) s1
                                    return (a2, s2)

instance MonadTrans (FilterT a b) where
  lift m = FilterT $ \ s -> do
    a <- m
    return (a, s)

instance (MonadIO m) => MonadIO (FilterT a b m) where
	liftIO = lift . liftIO

-- Returns the complete type (int->int) of a filter
instance (Typeable a, Typeable b, Typeable m) => Show (Filter a b m) where
  show s = map toLower $ (head $ tail t) ++ "->" ++ (head $ tail $ tail t)
    where
      t = words $ (show . typeOf) s

statement :: (Monad m) => Statement -> FilterT a b m ()
statement a = FilterT $ \ (id, statement) -> return ((), (id, Sequence statement a))

evalStmt :: (Monad m) => Int -> FilterT a b m () -> m (Int, Statement)
evalStmt id (FilterT f) = do
  (_, x) <- f (id, Null)
  return x

execStmt:: (Monad m) => FilterT a b m () -> m Statement 
execStmt f = do
  (_, x) <- evalStmt 0 f
  return x

get :: (Monad m) => FilterT a b m (Int, Statement)
get = FilterT $ \ a -> return (a, a)

put :: (Monad m) => (Int, Statement) -> FilterT a b m ()
put s = FilterT $ \ _ -> return ((), s)

-- FilterDecl = (Type, Name, Arguments, AST node)
type FilterDecl = (String, String, String, Statement)

instance CoreE (Filter a b) where
  var init = do
    (id, stmt) <- get
    sym <- lift $ gensym init
    put (id, Sequence stmt $ Decl sym)
    return sym
  float = var zero
  float' = var
  int = var zero
  int' = var
  bool = var zero
  bool' = var
  array _ size = var (Array size zero)
  a <== b = statement $ Assign a b
  ifelse cond onTrue onFalse = do
    (id0, stmt) <- get
    (id1, stmt1) <- lift $ evalStmt id0 onTrue
    (id2, stmt2) <- lift $ evalStmt id1 onFalse
    put (id2, stmt)
    statement $ Branch cond stmt1 stmt2
  if_ cond stmt = ifelse cond stmt $ return ()
  for_ (init, cond, inc) body = do
    (id0, stmt) <- get
    (id1, stmt1) <- lift $ evalStmt id0 body
    ini <- lift $ execStmt init
    inc <- lift $ execStmt inc
    put (id1, stmt)
    statement $ Loop ini cond inc stmt1
  while_ cond body = do
    (id0, stmt) <- get
    (id1, stmt1) <- lift $ evalStmt id0 body
    put (id1, stmt)
    statement $ Loop Null cond Null stmt1

-- | Push
push :: (Elt a, Elt b) => Exp b -> Filter a b ()
push a = statement $ Push a

-- | Peek
peek :: Elt a => Exp Int -> Exp a
-- RRN: Shouldn't the type be:
--  peek :: (Elt a, Elt b) => Exp Int -> Filter a b (Exp b)
peek = Peek

-- | Pop
pop :: forall a b . (Elt b) => Filter a b (Exp b)
pop = do 
  (id, stmt) <- get
  sym <- lift $ gensym zero
  put (id, Sequence stmt $
           Sequence (Decl sym) $
           Assign sym PopExp )
  return (Ref sym)

-- | Println
println :: (Elt a, Elt b) => Filter a b () -> Filter a b ()
-- RRN: I would expect:
-- println :: (Elt a) => Exp a -> Filter a b ()
println f = do
  s <- lift $ execStmt f
  statement $ Println s

-- | Rate declarations for work functions
data Rate = Rate {
  pushRate :: Exp Int,
  popRate :: Exp Int, 
  peekRate :: Exp Int
  }

showFlowRate :: Rate -> String
showFlowRate Rate {pushRate=a, popRate=b, peekRate=c} =
  intercalate " " $ zipWith showf ["push","pop","peek"] [a,b,c]
    where
      showf tag rate = case rate of
        Const 0 -> ""
        _       -> tag ++ " " ++ show rate

instance Show Rate where show = showFlowRate

rate :: Exp Int -> Exp Int -> Exp Int -> Rate
rate = Rate

-- | Initialization function
init' :: (Elt a, Elt b) => Filter a b () -> Filter a b ()
init' s = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 s
  put (id1, stmt)
  statement $ Init stmt1

-- | Work
work :: (Elt a, Elt b) => Rate -> Filter a b () -> Filter a b ()
work rate s = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 s
  put (id1, stmt)
  statement $ Work rate stmt1
