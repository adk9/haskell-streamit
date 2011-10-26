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
  , any_
  , all_
  , (-->)
  -- ** Equality and Comparison
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  -- ** Min, Max, and Limiting
  , min_
  , minimum_
  , max_
  , maximum_
  , limit
  -- ** Arithmetic Operations
  , (*.)
  , (/.)
  , div_
  , mod_
  -- ** Conditional Operator
  , mux
  -- ** Lookups
  , linear
  -- * Statements
  , Stmt
  -- ** Variable Declarations
  , var
  , float
  , float'
  , push
  , peek
  , pop
  , filter'
  -- ** Variable Assignment
  , Assign (..)
  -- ** Conditional Execution
  , ifelse
  , if_
  , case_
  , (==>)
  -- ** Incrementing and decrementing.
  , incr
  , decr
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

-- | True iff the predicate is true for all elements.
all_ :: (a -> E Bool) -> [a] -> E Bool
all_ f a = and_ $ map f a

-- | True iff the predicate is true for any element.
any_ :: (a -> E Bool) -> [a] -> E Bool
any_ f a = or_ $ map f a

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

-- | Returns the minimum of two numbers.
min_ :: NumE a => E a -> E a -> E a
min_ a b = mux (a <=. b) a b

-- | Returns the minimum of a list of numbers.
minimum_ :: NumE a => [E a] -> E a
minimum_ = foldl1 min_

-- | Returns the maximum of two numbers.
max_ :: NumE a => E a -> E a -> E a
max_ a b = mux (a >=. b) a b

-- | Returns the maximum of a list of numbers.
maximum_ :: NumE a => [E a] -> E a
maximum_ = foldl1 max_

-- | Limits between min and max.
limit :: NumE a => E a -> E a -> E a -> E a
limit a b i = max_ min $ min_ max i
  where
  min = min_ a b
  max = max_ a b

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

-- | Linear interpolation and extrapolation of two points.
linear :: (Float, Float) -> (Float, Float) -> E Float -> E Float
linear (x1, y1) (x2, y2) a = a *. slope + constant inter
  where
  slope = (y2 - y1) / (x2 - x1)
  inter = y1 - slope * x1

-- | References a variable to be used in an expression ('E').
ref :: AllE a => V a -> E a
ref = Ref

-- | Conditional expression.
--
-- > mux test onTrue onFalse
mux :: AllE a => E Bool -> E a -> E a -> E a
mux = Mux

get :: Stmt (Int, Statement)
get = Stmt $ \ a -> (a, a)

put :: (Int, Statement) -> Stmt ()
put s = Stmt $ \ _ -> ((), s)

-- | Generic variable declaration.
var :: AllE a => Name -> a -> Stmt (V a)
var name init = do
  (id, stmt) <- get
  put (id, Sequence stmt $ Decl (V name) (Just init))
  return $ V name

-- | Float variable declaration.
float :: Name -> Stmt (V Float)
float name = do
  (id, stmt) <- get
  put (id, Sequence stmt $ Decl (V name::V Float) Nothing)
  return $ V name

float' :: Name -> Float -> Stmt (V Float)
float' = var

-- | Push
push :: AllE a => E a -> Stmt ()
push a = statement $ Push a

-- | Peek
peek :: V Int -> Stmt ()
peek a = statement $ Peek a

-- | Pop
pop :: Stmt ()
pop = statement Pop

-- | Increments an E Int.
incr :: V Int -> Stmt ()
incr a = a <== ref a + 1

-- | Decrements an E Int.
decr :: V Int -> Stmt ()
decr a = a <== ref a - 1

-- | The Stmt monad holds <strike>variable declarations</strike> and statements.
data Stmt a = Stmt ((Int, Statement) -> (a, (Int, Statement)))

instance Monad Stmt where
  return a = Stmt $ \ s -> (a, s)
  (Stmt f1) >>= f2 = Stmt f3
    where
    f3 s1 = f4 s2
      where
      (a, s2) = f1 s1
      Stmt f4 = f2 a

statement :: Statement -> Stmt ()
statement a = Stmt $ \ (id, statement) -> ((), (id, Sequence statement a))

evalStmt :: Int -> Stmt () -> (Int, Statement)
evalStmt id (Stmt f) = snd $ f (id, Null)

class Assign a where (<==) :: V a -> E a -> Stmt ()
instance AllE a => Assign a where a <== b = statement $ Assign a b

-- | Conditional if-else.
ifelse :: E Bool -> Stmt () -> Stmt () -> Stmt ()
ifelse cond onTrue onFalse = do
  (id0, stmt) <- get
  let (id1, stmt1) = evalStmt id0 onTrue
      (id2, stmt2) = evalStmt id1 onFalse
  put (id2, stmt)
  statement $ Branch cond stmt1 stmt2

-- | Conditional if without the else.
if_ :: E Bool -> Stmt () -> Stmt()
if_ cond stmt = ifelse cond stmt $ return ()

-- | Condition case statement.
case_ :: Case () -> Stmt ()
case_ (Case f) = f $ return ()

data Case a = Case (Stmt () -> Stmt ())
instance Monad Case where
  return _ = Case id
  (>>=) = undefined
  (Case f1) >> (Case f2) = Case $ f1 . f2

(==>) :: E Bool -> Stmt () -> Case ()
a ==> s = Case $ ifelse a s

filter' :: Name -> Stmt () -> IO ()
filter' name filt = analyze (C.code name) filt

-- | Generic program analysis.
analyze :: (Statement -> IO a) -> Stmt () -> IO a
analyze f program = f stmt
  where
  (_, stmt) = evalStmt 0 program
