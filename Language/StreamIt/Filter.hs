module Language.StreamIt.Filter
  ( Statement (..)
  , FilterT (..)
  , Rate (..)
  , Filter
  , FilterInfo
  , evalStmt
  , execStmt
  , showFilterType
  , rate
  , push
  , peek
  , pop
  , incr
  , decr
  , println
  , work
  , init'
  , ifelse
  , if_
  , for_
  , while_
  ) where

import Data.Char
import Data.Typeable
import Data.Unique
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

instance (Elt a, Elt b, Monad m) => Monad (FilterT a b m) where
  return a    = FilterT $ \ s -> return (a, s)
  (>>=) sf f  = FilterT $ \ s -> do (a1, s1) <- runFilterT sf s
                                    (a2, s2) <- runFilterT (f a1) s1
                                    return (a2, s2)

instance (Elt a, Elt b) => MonadTrans (FilterT a b) where
  lift m = FilterT $ \ s -> do
    a <- m
    return (a, s)

instance (Elt a, Elt b, MonadIO m) => MonadIO (FilterT a b m) where
	liftIO = lift . liftIO

statement :: (Elt a, Elt b, Monad m) => Statement -> FilterT a b m ()
statement a = FilterT $ \ (id, statement) -> return ((), (id, Sequence statement a))

evalStmt :: (Elt a, Elt b, Monad m) => Int -> FilterT a b m () -> m (Int, Statement)
evalStmt id (FilterT f) = do
  (_, x) <- f (id, Null)
  return x

execStmt:: (Elt a, Elt b, Monad m) => FilterT a b m () -> m Statement 
execStmt f = do
  (_, x) <- evalStmt 0 f
  return x

get :: (Elt a, Elt b, Monad m) => FilterT a b m (Int, Statement)
get = FilterT $ \ a -> return (a, a)

put :: (Elt a, Elt b, Monad m) => (Int, Statement) -> FilterT a b m ()
put s = FilterT $ \ _ -> return ((), s)

type FilterInfo = (TypeSig, Name, Statement)

showFilterType :: (Typeable a, Typeable b) => Filter a b () -> String 
showFilterType s = map toLower $ (head $ tail t) ++ "->" ++ (head $ tail $ tail t)
  where
    t = words $ showTypeSig s

instance (Elt a, Elt b) => CoreE (Filter a b) where
  var input init = do
    (id, stmt) <- get
    n <- lift newUnique
    put (id, Sequence stmt $ Decl (Var input ("var" ++ show (hashUnique n)) init))
    return $ Var input ("var" ++ show (hashUnique n)) init
  input _ = var True zero
  float = var False zero
  float' = var False
  int = var False zero
  int' = var False
  bool = var False zero
  bool' = var False
  a <== b = statement $ Assign a b
  ifelse cond onTrue onFalse = do
    (id0, stmt) <- get
    (id1, stmt1) <- lift $ evalStmt id0 onTrue
    (id2, stmt2) <- lift $ evalStmt id1 onFalse
    put (id2, stmt)
    statement $ Branch cond stmt1 stmt2
  if_ cond stmt = ifelse cond stmt $ return ()

-- | Increments a Var Int.
incr :: (Elt a, Elt b) => Var Int -> Filter a b ()
incr a = a <== ref a + 1

-- | Decrements a Var Int.
decr :: (Elt a, Elt b) => Var Int -> Filter a b ()
decr a = a <== ref a - 1

-- | Push
push :: (Elt a, Elt b) => Exp b -> Filter a b ()
push a = statement $ Push a

-- | Peek
peek :: Elt a => Exp Int -> Exp a
-- RRN: Shouldn't the type be:
--  pop :: (Elt a, Elt b) => Exp Int -> Filter a b (Exp b)
peek = Peek

-- | Pop
pop :: (Elt a, Elt b) => Filter a b ()
-- RRN: Shouldn't the type be:
--  pop :: (Elt a, Elt b) => Filter a b (Exp b)
pop = statement $ Pop

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
  } deriving (Show)

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

-- | For loop.
for_ :: (Elt a, Elt b) => (Filter a b (), Exp Bool, Filter a b ()) -> Filter a b () -> Filter a b ()
for_ (init, cond, inc) body = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 body
  ini <- lift $ execStmt init
  inc <- lift $ execStmt inc
  put (id1, stmt)
  statement $ Loop ini cond inc stmt1

-- | While loop.
while_ :: (Elt a, Elt b) => Exp Bool -> Filter a b () -> Filter a b ()
while_ cond body = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 body
  put (id1, stmt)
  statement $ Loop Null cond Null stmt1
