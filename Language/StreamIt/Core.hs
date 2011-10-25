module Language.StreamIt.Core
  ( E (..)
  , V (..)
  , A (..)
  , Name
  , Path
  , PathName (..)
  , AllE (..)
  , NumE
  , Const (..)
  , Statement (..)
  , arrayLength
  ) where

import Data.List
import Data.Ratio

type Name = String

type Path = [Name]

-- | A mutable variable.
data V a
  = V Path a
  deriving Eq

-- | A mutable array.
data A a = A Path [a] deriving (Eq, Ord)

-- | Length of array.
arrayLength :: A a -> Int
arrayLength (A _ a) = length a

class    PathName a       where pathName :: a -> String
instance PathName Path    where pathName = intercalate "."
instance PathName (V a)   where
  pathName a = case a of
    V path _ -> pathName path
    -- VArray a _ -> pathName a
instance PathName (A a)   where pathName (A a _) = pathName a

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

class    AllE a => NumE a
instance NumE Int
instance NumE Float

-- | A logical, arithmetic, comparative, or conditional expression.
data E a where
  Ref   :: AllE a => V a -> E a
  Const :: AllE a => a -> E a
  Add   :: NumE a => E a -> E a -> E a
  Sub   :: NumE a => E a -> E a -> E a
  Mul   :: NumE a => E a -> a -> E a
  Div   :: NumE a => E a -> a -> E a
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
  (*) = error "general multiplication not supported, use (*.)"
  negate a = 0 - a
  abs a = Mux (Lt a 0) (negate a) a
  signum a = Mux (Eq a 0) 0 $ Mux (Lt a 0) (-1) 1
  fromInteger = Const . fromInteger

instance Fractional (E Float) where
  (/) = error "general division not supported, use (/.)"
  recip a = 1 / a
  fromRational r = Const $ fromInteger (numerator r) / fromInteger (denominator r)

data Statement where
  Assign   :: AllE a => V a -> E a -> Statement
  Branch   :: E Bool -> Statement -> Statement -> Statement
  Sequence :: Statement -> Statement -> Statement
  Push     :: AllE a => E a -> Statement
  Pop      :: Statement
  Peek     :: V Int -> Statement
  Null     :: Statement

data Const
  = Bool   Bool
  | Int    Int
  | Float  Float
  deriving (Show, Eq, Ord)
