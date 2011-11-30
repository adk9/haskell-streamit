module Language.StreamIt
  (
  -- * Types
    E
  , V
  , AllE
  , NumE
  , Name
  -- * Expressions
  -- ** Constants
  , true
  , false
  , constant
  , zero
  -- ** Variable Reference
  , ref
  -- ** Logical Operations
  , not_
  , (&&.)
  , (||.)
  , and_
  , or_
  , (-->)
  -- ** Equality and Comparison
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  -- ** Arithmetic Operations
  , (*.)
  , (/.)
  , div_
  , mod_
  -- * Statements
  , Filter
  -- ** Variable Declarations
  , input
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
  , filter'
  -- ** Variable Assignment
  , Assign (..)
  -- ** Conditional Execution
  , ifelse
  , if_
  , case_
  , (==>)
  -- * General Analysis
  , analyze
  ) where

import Control.Monad

import Language.StreamIt.Core
import qualified Language.StreamIt.Code as C

--infixl 9 !, !.
infixl 7 *., /., `div_`, `mod_`
infix  4 ==., /=., <., <=., >., >=.
infixl 3 &&.
infixl 2 ||.
infixr 1 -->
infixr 0 <==, ==>

-- | True term.
true :: E Bool
true = Const True

-- | False term.
false :: E Bool
false = Const False

-- | Arbitrary constants.
constant :: AllE a => a -> E a
constant = Const

-- | Logical negation.
not_ :: E Bool -> E Bool
not_ = Not

-- | Logical AND.
(&&.) :: E Bool -> E Bool -> E Bool
(&&.) = And

-- | Logical OR.
(||.) :: E Bool -> E Bool -> E Bool
(||.) = Or

-- | The conjunction of a E Bool list.
and_ :: [E Bool] -> E Bool
and_ = foldl (&&.) true

-- | The disjunction of a E Bool list.
or_ :: [E Bool] -> E Bool
or_ = foldl (||.) false

-- | Logical implication.
(-->) :: E Bool -> E Bool -> E Bool 
a --> b = not_ a ||. b

-- | Equal.
(==.) :: AllE a => E a -> E a -> E Bool
(==.) = Eq

-- | Not equal.
(/=.) :: AllE a => E a -> E a -> E Bool
a /=. b = not_ (a ==. b)

-- | Less than.
(<.) :: NumE a => E a -> E a -> E Bool
(<.) = Lt

-- | Greater than.
(>.) :: NumE a => E a -> E a -> E Bool
(>.) = Gt

-- | Less than or equal.
(<=.) :: NumE a => E a -> E a -> E Bool
(<=.) = Le

-- | Greater than or equal.
(>=.) :: NumE a => E a -> E a -> E Bool
(>=.) = Ge

-- | Multiplication.
(*.) :: NumE a => E a -> a -> E a
(*.) = Mul

-- | Floating point division.
(/.) :: E Float -> Float -> E Float
_ /. 0 = error "divide by zero (/.)"
a /. b = Div a b

-- | Integer division.
div_ :: E Int -> Int -> E Int
div_ _ 0 = error "divide by zero (div_)"
div_ a b = Div a b

-- | Modulo.
mod_ :: E Int -> Int -> E Int
mod_ _ 0 = error "divide by zero (mod_)"
mod_ a b = Mod a b

-- | References a variable to be used in an expression ('E').
ref :: AllE a => V a -> E a
ref = Ref

get :: Filter (Int, Statement)
get = Filter $ \ a -> (a, a)

put :: (Int, Statement) -> Filter ()
put s = Filter $ \ _ -> ((), s)

input  :: AllE a => (Name -> Filter (V a)) -> Name -> E a
input _ n = ref $ V n

-- TODO: add input'

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

filter' :: String -> Name -> Filter () -> IO ()
filter' ty name filt = analyze (C.code ty name) filt

-- | Generic program analysis.
analyze :: (Statement -> IO a) -> Filter () -> IO a
analyze f program = f stmt
  where
  (_, stmt) = evalStmt 0 program
