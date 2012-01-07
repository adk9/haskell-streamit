module Language.StreamIt.Filter
  ( Statement (..)
  , FilterT (..)
  , Filter
  , FilterInfo
  , evalStmt
  , execStmt
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

import Data.Unique
import Control.Monad
import Control.Monad.Trans

import Language.StreamIt.Core

data Statement where
  Decl     :: AllE a => V a -> Statement
  Assign   :: AllE a => V a -> E a -> Statement
  Branch   :: E Bool -> Statement -> Statement -> Statement
  Loop     :: Statement -> E Bool -> Statement -> Statement -> Statement
  Sequence :: Statement -> Statement -> Statement
  Work     :: (E Int, E Int, E Int) -> Statement -> Statement
  Init     :: Statement -> Statement
  Push     :: AllE a => E a -> Statement
  Pop      :: Statement
  Println  :: Statement -> Statement
  Null     :: Statement

instance Eq (Statement) where (==) _ _ = True

-- | The Filter monad holds StreamIt filter statements.
newtype FilterT m a = FilterT {runFilterT :: ((Int, Statement) -> m (a, (Int, Statement)))}

type Filter = FilterT IO

instance Monad m => Monad (FilterT m) where
  return a    = FilterT $ \ s -> return (a, s)
  (>>=) sf f  = FilterT $ \ s -> do (a1, s1) <- runFilterT sf s
                                    (a2, s2) <- runFilterT (f a1) s1
                                    return (a2, s2)

instance MonadTrans FilterT where
  lift m = FilterT $ \ s -> do
    a <- m
    return (a, s)

statement :: Monad m => Statement -> FilterT m ()
statement a = FilterT $ \ (id, statement) -> return ((), (id, Sequence statement a))

evalStmt :: Monad m => Int -> FilterT m () -> m (Int, Statement)
evalStmt id (FilterT f) = do
  (_, x) <- f (id, Null)
  return x

execStmt:: Monad m => FilterT m () -> m Statement 
execStmt f = do
  (_, x) <- evalStmt 0 f
  return x

get :: Monad m => FilterT m (Int, Statement)
get = FilterT $ \ a -> return (a, a)

put :: Monad m => (Int, Statement) -> FilterT m ()
put s = FilterT $ \ _ -> return ((), s)

type FilterInfo = (TypeSig, Name, Statement)

instance CoreE (Filter) where
  var input init = do
    (id, stmt) <- get
    n <- lift newUnique
    put (id, Sequence stmt $ Decl (V input ("var" ++ show (hashUnique n)) init))
    return $ V input ("var" ++ show (hashUnique n)) init
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

-- | Increments a V Int.
incr :: V Int -> Filter ()
incr a = a <== ref a + 1

-- | Decrements a V Int.
decr :: V Int -> Filter ()
decr a = a <== ref a - 1

-- | Push
push :: AllE a => E a -> Filter ()
push a = statement $ Push a

-- | Peek
peek :: AllE a => E a -> E a
peek = Peek

-- | Pop
pop :: Filter ()
pop = statement $ Pop

-- | Println
println :: Filter () -> Filter ()
println f = do
  s <- lift $ execStmt f
  statement $ Println s

-- | Init
init' :: Filter () -> Filter ()
init' s = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 s
  put (id1, stmt)
  statement $ Init stmt1

-- | Work
work :: (E Int, E Int, E Int) -> Filter () -> Filter ()
work (push, pop, peek) s = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 s
  put (id1, stmt)
  statement $ Work (push, pop, peek) stmt1

-- | For loop.
for_ :: (Filter (), E Bool, Filter ()) -> Filter () -> Filter ()
for_ (init, cond, inc) body = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 body
  ini <- lift $ execStmt init
  inc <- lift $ execStmt inc
  put (id1, stmt)
  statement $ Loop ini cond inc stmt1

-- | While loop.
while_ :: E Bool -> Filter () -> Filter ()
while_ cond body = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 body
  put (id1, stmt)
  statement $ Loop Null cond Null stmt1
