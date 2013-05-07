module Language.StreamIt.Core
  ( E (..)
  , V (..)
  , CoreE (..)
  , TypeSig
  , Name
  , AllE (..)
  , NumE
  , Const (..)
  , Void
  , true
  , false
  , constant
  , ref
  , not_
  , (&&.)
  , (||.)
  , and_
  , or_
  , (-->)
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , mod_
  , showTypeSig
  ) where

import Data.Ratio
import Data.Typeable

--infixl 9 !, !.
infixl 7 `mod_`
infix  4 ==., /=., <., <=., >., >=.
infixl 3 &&.
infixl 2 ||.
infixr 1 -->
infixr 0 <==

type TypeSig = String
type Name = String

-- | A mutable variable.
data V a
  = V Bool Name a
  deriving Eq

instance Show (V a) where show (V _ n _) = n

class Eq a => AllE a where
  zero   :: a
  const' :: a -> Const

instance AllE Bool where
  zero = False
  const' = Bool

instance AllE Int where
  zero = 0
  const' = Int

instance AllE Float where
  zero = 0
  const' = Float

data Void
instance Show Void where show _ = "Void"
instance Eq Void where _ == _ = True
instance Ord Void where _ <= _ = True
instance Typeable Void where typeOf _ = mkTyConApp (mkTyCon3 "" "" "Void") []

instance AllE Void where
  zero = undefined
  const' = Void

class    AllE a => NumE a
instance NumE Int
instance NumE Float

-- | Generic variable declarations.
class Monad a => CoreE a where
  var :: AllE b  => Bool -> b -> a (V b)
  input :: AllE b => a (V b) -> a (V b)

  -- Float variable declarations.
  float :: a (V Float)
  float' :: Float -> a (V Float)
  -- Int variable declarations.
  int :: a (V Int)
  int' :: Int -> a (V Int)
  -- Bool variable declarations.
  bool :: a (V Bool)
  bool' :: Bool -> a (V Bool)
  -- Assignments.
  (<==) :: AllE b => V b -> E b -> a ()
  -- Conditional statements.
  ifelse :: E Bool -> a () -> a () -> a ()
  if_ :: E Bool -> a () -> a ()

-- | A logical, arithmetic, comparative, or conditional expression.
data E a where
  Ref   :: AllE a => V a -> E a
  Peek  :: AllE a => E a -> E a
  Const :: AllE a => a -> E a
  Add   :: NumE a => E a -> E a -> E a
  Sub   :: NumE a => E a -> E a -> E a
  Mul   :: NumE a => E a -> E a -> E a
  Div   :: NumE a => E a -> E a -> E a
  Mod   :: E Int -> Int -> E Int
  Not   :: E Bool -> E Bool
  And   :: E Bool -> E Bool -> E Bool
  Or    :: E Bool -> E Bool -> E Bool
  Eq    :: AllE a => E a -> E a -> E Bool
  Lt    :: NumE a => E a -> E a -> E Bool
  Gt    :: NumE a => E a -> E a -> E Bool
  Le    :: NumE a => E a -> E a -> E Bool
  Ge    :: NumE a => E a -> E a -> E Bool
  Mux   :: AllE a => E Bool -> E a -> E a -> E a

instance Show (E a) where show = undefined 
instance Eq   (E a) where (==) = undefined

instance (Num a, AllE a, NumE a) => Num (E a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate a = 0 - a
  abs a = Mux (Lt a 0) (negate a) a
  signum a = Mux (Eq a 0) 0 $ Mux (Lt a 0) (-1) 1
  fromInteger = Const . fromInteger

instance Fractional (E Int) where
  (/) = Div
  fromRational = undefined

instance Fractional (E Float) where
  (/) = Div
  recip a = 1 / a
  fromRational r = Const $ fromInteger (numerator r) / fromInteger (denominator r)

data Const
  = Bool   Bool
  | Int    Int
  | Float  Float
  | Void   Void
  deriving (Show, Eq, Ord)

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

-- | Modulo.
mod_ :: E Int -> Int -> E Int
mod_ _ 0 = error "divide by zero (mod_)"
mod_ a b = Mod a b

-- | References a variable to be used in an expression ('E').
ref :: AllE a => V a -> E a
ref = Ref

-- | Return the type signature of a Filter or StreamIt monad
showTypeSig :: Typeable a => a -> String
showTypeSig = show . typeOf
