module Language.StreamIt.Filter
  ( Statement (..)
  , FilterT (..)
  , Filter
  , FilterInfo
  , evalStmt
  , execStmt
  , showFilterType
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
newtype FilterT i o m a = FilterT {runFilterT :: ((Int, Statement) -> m (a, (Int, Statement)))}

type Filter a b = FilterT a b IO

instance (Typeable a, Typeable b) => Typeable1 (Filter a b) where
  typeOf1 s = let
    tyCon = mkTyCon "Language.StreamIt.Filter.Filter"
    (a, b) = peel s

    peel :: Filter a b m -> (a, b)
    peel = undefined

    in mkTyConApp tyCon [typeOf a, typeOf b]

instance (AllE a, AllE b, Monad m) => Monad (FilterT a b m) where
  return a    = FilterT $ \ s -> return (a, s)
  (>>=) sf f  = FilterT $ \ s -> do (a1, s1) <- runFilterT sf s
                                    (a2, s2) <- runFilterT (f a1) s1
                                    return (a2, s2)

instance (AllE a, AllE b) => MonadTrans (FilterT a b) where
  lift m = FilterT $ \ s -> do
    a <- m
    return (a, s)

instance (AllE a, AllE b, MonadIO m) => MonadIO (FilterT a b m) where
	liftIO = lift . liftIO

statement :: (AllE a, AllE b, Monad m) => Statement -> FilterT a b m ()
statement a = FilterT $ \ (id, statement) -> return ((), (id, Sequence statement a))

evalStmt :: (AllE a, AllE b, Monad m) => Int -> FilterT a b m () -> m (Int, Statement)
evalStmt id (FilterT f) = do
  (_, x) <- f (id, Null)
  return x

execStmt:: (AllE a, AllE b, Monad m) => FilterT a b m () -> m Statement 
execStmt f = do
  (_, x) <- evalStmt 0 f
  return x

get :: (AllE a, AllE b, Monad m) => FilterT a b m (Int, Statement)
get = FilterT $ \ a -> return (a, a)

put :: (AllE a, AllE b, Monad m) => (Int, Statement) -> FilterT a b m ()
put s = FilterT $ \ _ -> return ((), s)

type FilterInfo = (TypeSig, Name, Statement)

showFilterType :: (Typeable a, Typeable b) => Filter a b () -> String 
showFilterType s = map toLower $ (head $ tail t) ++ "->" ++ (head $ tail $ tail t)
  where
    t = words $ showTypeSig s

instance (AllE a, AllE b) => CoreE (Filter a b) where
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
incr :: (AllE a, AllE b) => V Int -> Filter a b ()
incr a = a <== ref a + 1

-- | Decrements a V Int.
decr :: (AllE a, AllE b) => V Int -> Filter a b ()
decr a = a <== ref a - 1

-- | Push
push :: (AllE a, AllE b) => E b -> Filter a b ()
push a = statement $ Push a

-- | Peek
peek :: AllE a => E a -> E a
peek = Peek

-- | Pop
pop :: (AllE a, AllE b) => Filter a b ()
pop = statement $ Pop

-- | Println
println :: (AllE a, AllE b) => Filter a b () -> Filter a b ()
println f = do
  s <- lift $ execStmt f
  statement $ Println s

-- | Init
init' :: (AllE a, AllE b) => Filter a b () -> Filter a b ()
init' s = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 s
  put (id1, stmt)
  statement $ Init stmt1

-- | Work
work :: (AllE a, AllE b) => (E Int, E Int, E Int) -> Filter a b () -> Filter a b ()
work (push, pop, peek) s = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 s
  put (id1, stmt)
  statement $ Work (push, pop, peek) stmt1

-- | For loop.
for_ :: (AllE a, AllE b) => (Filter a b (), E Bool, Filter a b ()) -> Filter a b () -> Filter a b ()
for_ (init, cond, inc) body = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 body
  ini <- lift $ execStmt init
  inc <- lift $ execStmt inc
  put (id1, stmt)
  statement $ Loop ini cond inc stmt1

-- | While loop.
while_ :: (AllE a, AllE b) => E Bool -> Filter a b () -> Filter a b ()
while_ cond body = do
  (id0, stmt) <- get
  (id1, stmt1) <- lift $ evalStmt id0 body
  put (id1, stmt)
  statement $ Loop Null cond Null stmt1
