module Language.StreamIt.Filter
  ( Statement (..)
  , evalStmt
  , Filter (..)
  , var
  , float
  , float'
  , int
  , int'
  , bool
  , bool'
  , push
  , peek
  , pop
  , println
  , work
  , init'
  , Assign (..)
  , ifelse
  , if_
  , case_
  , (==>)
  ) where

import Control.Monad
import Language.StreamIt.Core

infixr 0 <==, ==>

data Statement where
  Decl     :: AllE a => V a -> Maybe a -> Statement
  Assign   :: AllE a => V a -> E a -> Statement
  Branch   :: E Bool -> Statement -> Statement -> Statement
  Sequence :: Statement -> Statement -> Statement
  Work     :: (E Int, E Int, E Int) -> Statement -> Statement
  Init     :: Statement -> Statement
  Push     :: AllE a => E a -> Statement
  Pop      :: Statement
  Peek     :: V Int -> Statement
  Println  :: AllE a => E a -> Statement
  Null     :: Statement

-- | The Filter monad holds StreamIt statements.
data Filter a = Filter ((Int, Statement) -> (a, (Int, Statement)))

instance Monad Filter where
  return a = Filter $ \ s -> (a, s)
  (Filter f1) >>= f2 = Filter f3
    where
    f3 s1 = f4 s2
      where
      (a, s2) = f1 s1
      Filter f4 = f2 a

statement :: Statement -> Filter ()
statement a = Filter $ \ (id, statement) -> ((), (id, Sequence statement a))

evalStmt :: Int -> Filter () -> (Int, Statement)
evalStmt id (Filter f) = snd $ f (id, Null)

get :: Filter (Int, Statement)
get = Filter $ \ a -> (a, a)

put :: (Int, Statement) -> Filter ()
put s = Filter $ \ _ -> ((), s)

-- | Generic variable declaration.
var :: AllE a => Name -> a -> Filter (V a)
var name init = do
  (id, stmt) <- get
  put (id, Sequence stmt $ Decl (V name) (Just init))
  return $ V name

-- | Float variable declaration.
float :: Name -> Filter (V Float)
float name = do
  (id, stmt) <- get
  put (id, Sequence stmt $ Decl (V name::V Float) Nothing)
  return $ V name

float' :: Name -> Float -> Filter (V Float)
float' = var

-- | Int variable declaration.
int :: Name -> Filter (V Int)
int name = do
  (id, stmt) <- get
  put (id, Sequence stmt $ Decl (V name::V Int) Nothing)
  return $ V name

int' :: Name -> Int -> Filter (V Int)
int' = var

-- | Bool variable declaration.
bool :: Name -> Filter (V Bool)
bool name = do
  (id, stmt) <- get
  put (id, Sequence stmt $ Decl (V name::V Bool) Nothing)
  return $ V name

bool' :: Name -> Bool -> Filter (V Bool)
bool' = var

-- | Push
push :: AllE a => E a -> Filter ()
push a = statement $ Push a

-- | Peek
peek :: V Int -> Filter ()
peek a = statement $ Peek a

-- | Pop
pop :: Filter (V Int)
pop = do
  (id, stmt) <- get
  put (id, Sequence stmt $ Pop)
  return $ V "pop()"

-- | Println
println :: AllE a => E a -> Filter ()
println a = statement $ Println a

-- | Init
init' :: Filter () -> Filter ()
init' s = do
  (id0, stmt) <- get
  let (id1, stmt1) = evalStmt id0 s
  put (id1, stmt)
  statement $ Init stmt1

-- | Work
work :: (E Int, E Int, E Int) -> Filter () -> Filter ()
work (push, pop, peek) s = do
  (id0, stmt) <- get
  let (id1, stmt1) = evalStmt id0 s
  put (id1, stmt)
  statement $ Work (push, pop, peek) stmt1

class Assign a where (<==) :: V a -> E a -> Filter ()
instance AllE a => Assign a where a <== b = statement $ Assign a b

-- | Conditional if-else.
ifelse :: E Bool -> Filter () -> Filter () -> Filter ()
ifelse cond onTrue onFalse = do
  (id0, stmt) <- get
  let (id1, stmt1) = evalStmt id0 onTrue
      (id2, stmt2) = evalStmt id1 onFalse
  put (id2, stmt)
  statement $ Branch cond stmt1 stmt2

-- | Conditional if without the else.
if_ :: E Bool -> Filter () -> Filter ()
if_ cond stmt = ifelse cond stmt $ return ()

-- | Condition case statement.
case_ :: Case () -> Filter ()
case_ (Case f) = f $ return ()

data Case a = Case (Filter () -> Filter ())
instance Monad Case where
  return _ = Case id
  (>>=) = undefined
  (Case f1) >> (Case f2) = Case $ f1 . f2

(==>) :: E Bool -> Filter () -> Case ()
a ==> s = Case $ ifelse a s
