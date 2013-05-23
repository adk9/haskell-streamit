module Language.StreamIt.Core
  ( Exp (..)
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
  (<==) :: AllE b => V b -> Exp b -> a ()
  -- Conditional statements.
  ifelse :: Exp Bool -> a () -> a () -> a ()
  if_ :: Exp Bool -> a () -> a ()

-- | A logical, arithmetic, comparative, or conditional expression.
data Exp a where
  Ref   :: AllE a => V a -> Exp a
  Peek  :: AllE a => Exp a -> Exp a
  Const :: AllE a => a -> Exp a
  Add   :: NumE a => Exp a -> Exp a -> Exp a
  Sub   :: NumE a => Exp a -> Exp a -> Exp a
  Mul   :: NumE a => Exp a -> Exp a -> Exp a
  Div   :: NumE a => Exp a -> Exp a -> Exp a
  Mod   :: Exp Int -> Int -> Exp Int
  Not   :: Exp Bool -> Exp Bool
  And   :: Exp Bool -> Exp Bool -> Exp Bool
  Or    :: Exp Bool -> Exp Bool -> Exp Bool
  Eq    :: AllE a => Exp a -> Exp a -> Exp Bool
  Lt    :: NumE a => Exp a -> Exp a -> Exp Bool
  Gt    :: NumE a => Exp a -> Exp a -> Exp Bool
  Le    :: NumE a => Exp a -> Exp a -> Exp Bool
  Ge    :: NumE a => Exp a -> Exp a -> Exp Bool
  Mux   :: AllE a => Exp Bool -> Exp a -> Exp a -> Exp a

instance Show   (Exp a) where show = undefined
instance Eq   (Exp a) where (==) = undefined

instance (Num a, AllE a, NumE a) => Num (Exp a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate a = 0 - a
  abs a = Mux (Lt a 0) (negate a) a
  signum a = Mux (Eq a 0) 0 $ Mux (Lt a 0) (-1) 1
  fromInteger = Const . fromInteger

instance Fractional (Exp Int) where
  (/) = Div
  fromRational = undefined

instance Fractional (Exp Float) where
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
true :: Exp Bool
true = Const True

-- | False term.
false :: Exp Bool
false = Const False

-- | Arbitrary constants.
constant :: AllE a => a -> Exp a
constant = Const

-- | Logical negation.
not_ :: Exp Bool -> Exp Bool
not_ = Not

-- | Logical AND.
(&&.) :: Exp Bool -> Exp Bool -> Exp Bool
(&&.) = And

-- | Logical OR.
(||.) :: Exp Bool -> Exp Bool -> Exp Bool
(||.) = Or

-- | The conjunction of a Exp Bool list.
and_ :: [Exp Bool] -> Exp Bool
and_ = foldl (&&.) true

-- | The disjunction of a Exp Bool list.
or_ :: [Exp Bool] -> Exp Bool
or_ = foldl (||.) false

-- | Logical implication.
(-->) :: Exp Bool -> Exp Bool -> Exp Bool 
a --> b = not_ a ||. b

-- | Equal.
(==.) :: AllE a => Exp a -> Exp a -> Exp Bool
(==.) = Eq

-- | Not equal.
(/=.) :: AllE a => Exp a -> Exp a -> Exp Bool
a /=. b = not_ (a ==. b)

-- | Less than.
(<.) :: NumE a => Exp a -> Exp a -> Exp Bool
(<.) = Lt

-- | Greater than.
(>.) :: NumE a => Exp a -> Exp a -> Exp Bool
(>.) = Gt

-- | Less than or equal.
(<=.) :: NumE a => Exp a -> Exp a -> Exp Bool
(<=.) = Le

-- | Greater than or equal.
(>=.) :: NumE a => Exp a -> Exp a -> Exp Bool
(>=.) = Ge

-- | Modulo.
mod_ :: Exp Int -> Int -> Exp Int
mod_ _ 0 = error "divide by zero (mod_)"
mod_ a b = Mod a b

-- | References a variable to be used in an expression.
ref :: AllE a => V a -> Exp a
ref = Ref

-- | Return the type signature of a Filter or StreamIt monad
showTypeSig :: Typeable a => a -> String
showTypeSig = show . typeOf
