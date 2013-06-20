module Language.StreamIt.Core
  ( Exp (..)
  , Var (..)
  , CoreE (..)
  , Elt (..)
  , NumE
  , Const (..)
  , Void
  , Array (..)
  , true
  , false
  , constant
  , ref
  , not_
  , (&&.)
  , (||.)
  , and_
  , or_
  , (==.)
  , (/==.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , (.++)
  , (.--)
  , (+=)
  , (-=)
  , (*=)
  , (/=.)
  , mod_
  , cond
  , fcall
  , showConstType
  , gensym
  , genExpVar
  , showVarDecl
  , newStableName
  , isScalar
  , (!)
  ) where

import Data.List
import Data.Ratio
import Data.Typeable
import Data.Unique
import System.Mem.StableName

infixl 9 !
infixl 7 `mod_`
infix  4 ==., /==., <., <=., >., >=.
infix  4 .++, .--, +=, -=, *=, /=.
infixl 3 &&.
infixl 2 ||.
infixr 0 <==

-- | A mutable variable, aka an LValue in C.  This includes both locations in arrays,
-- as well as scalar variables.
data Var a = Var {
  vname :: String,    -- name of the variable
  val   :: Elt a => a -- initial value
  }

instance Show (Var a) where
  show (Var n _) = n

class Eq a => Elt a where
  zero   :: a
  const' :: a -> Const

instance Elt Bool where
  zero = False
  const' = Bool

instance Elt Int where
  zero = 0
  const' = Int

instance Elt Float where
  zero = 0
  const' = Float

class    (Elt a, Num a, Ord a) => NumE a
instance NumE Int
instance NumE Float

data Void
instance Show Void where show _ = "Void"
instance Eq Void where _ == _ = True
instance Ord Void where _ <= _ = True
instance Typeable Void where typeOf _ = mkTyConApp (mkTyCon3 "" "" "Void") []

instance Elt Void where
  zero = undefined
  const' = Void

-- | For describing StreamIt array types.
data Array a = Array {
  bound :: Exp Int, -- array upper bound
  ele   :: a        -- array element type
  } deriving (Show, Eq)

instance (Elt a) => Elt (Array a) where
  zero = Array (constant 0) zero
  const' (Array b e) = ArrayT b (const' e)

-- | Generic variable declarations.
class Monad a => CoreE a where
  var    :: Elt b => b -> a (Var b)
  -- Float variable declarations.
  float  :: a (Var Float)
  float' :: Float -> a (Var Float)
  -- Int variable declarations.
  int    :: a (Var Int)
  int'   :: Int -> a (Var Int)
  -- Bool variable declarations.
  bool   :: a (Var Bool)
  bool'  :: Bool -> a (Var Bool)
  -- Array declarations.
  array  :: Elt b => a (Var b) -> Exp Int -> a (Var (Array b))
  -- Assignments.
  (<==)  :: Elt b => Var b -> Exp b -> a ()
  -- Conditional statements.
  ifelse :: Exp Bool -> a (b) -> a (b) -> a ()
  if_    :: Exp Bool -> a (b) -> a ()
  -- Loops
  for_   :: (a (), Exp Bool, a ()) -> a (b) -> a ()
  while_ :: Exp Bool -> a (b) -> a ()

-- Generate a new variable but do not add it to the AST
gensym :: Elt b => b -> IO (Var b)
gensym init = do
  n <- newUnique
  return $ Var ("var" ++ show (hashUnique n)) init

-- Generate a new variable for a given expression Exp b
genExpVar :: Elt b => Exp b -> IO (Var b)
genExpVar e = case e of
  Ref a -> return a
  _ -> gensym zero

-- Return a variable declaration given a Var b
showVarDecl :: Elt b => (Var b) -> String
showVarDecl v = showConstType (const' $ val v) ++ " " ++ vname v

-- Helper function to generate new or find existing names by
-- hashing the AST node pointer.
newStableName :: a -> String -> IO String
newStableName obj template = do
  n <- makeStableName obj
  return (template ++ show (hashStableName n))

-- | A logical, arithmetic, comparative, or conditional expression.
data Exp a where
  Ref   :: Elt a => Var a -> Exp a
  Peek  :: Elt a => Exp Int -> Exp a -- RRN: this has an effect so it should be in the
                                     -- Filter monad.

  PopExp :: Elt a => Exp a -- INTERNAL, not typesafe to expose.
           
  Fcall :: String -> Exp a -> Exp b
  Const :: Elt a => a -> Exp a
  Add   :: NumE a => Exp a -> Exp a -> Exp a
  Sub   :: NumE a => Exp a -> Exp a -> Exp a
  Mul   :: NumE a => Exp a -> Exp a -> Exp a
  Div   :: NumE a => Exp a -> Exp a -> Exp a
  Mod   :: Exp Int -> Int -> Exp Int
  Not   :: Exp Bool -> Exp Bool
  And   :: Exp Bool -> Exp Bool -> Exp Bool
  Or    :: Exp Bool -> Exp Bool -> Exp Bool
  Eq    :: Elt a => Exp a -> Exp a -> Exp Bool
  Lt    :: NumE a => Exp a -> Exp a -> Exp Bool
  Gt    :: NumE a => Exp a -> Exp a -> Exp Bool
  Le    :: NumE a => Exp a -> Exp a -> Exp Bool
  Ge    :: NumE a => Exp a -> Exp a -> Exp Bool
  Cond  :: Exp Bool -> Exp a -> Exp a -> Exp a

instance Show (Exp a) where
  show a = case a of
    Ref a     -> show a
    Peek a    -> "peek(" ++ show a ++ ")"
    Fcall f a -> f ++ "(" ++ show a ++ ")"
    Const a   -> show $ const' a
    Add a b   -> group [show a, "+", show b]
    Sub a b   -> group [show a, "-", show b]
    Mul a b   -> group [show a, "*", show b]
    Div a b   -> group [show a, "/", show b]
    Mod a b   -> group [show a, "%", show (const' b)]
    Not a     -> group ["!", show a]
    And a b   -> group [show a, "&&",  show b]
    Or  a b   -> group [show a, "||",  show b]
    Eq  a b   -> group [show a, "==",  show b]
    Lt  a b   -> group [show a, "<",   show b]
    Gt  a b   -> group [show a, ">",   show b]
    Le  a b   -> group [show a, "<=",  show b]
    Ge  a b   -> group [show a, ">=",  show b]
    Cond a b c -> group [show a, "?", show b, ":", show c] 
    where
      group :: [String] -> String
      group a = "(" ++ intercalate " " a ++ ")"

instance Eq a => Eq (Exp a) where
  a == b = evalExp a == evalExp b

instance NumE a => Num (Exp a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate a = 0 - a
  abs a = Cond (Lt a 0) (negate a) a
  signum a = Cond (Eq a 0) 0 $ Cond (Lt a 0) (-1) 1
  fromInteger = Const . fromInteger

instance Fractional (Exp Int) where
  (/) = Div
  fromRational = undefined

instance Fractional (Exp Float) where
  (/) = Div
  recip a = 1 / a
  fromRational r = Const $ fromInteger (numerator r) / fromInteger (denominator r)

evalExp :: Exp a -> a
evalExp e = case e of
  Ref a     -> val a -- RRN: Is this a safe assumption?  That the variable will have its initial value?
                     -- Seems like this function should return Maybe, and this should potentially be a Nothing case...
  Peek _    -> undefined
  Fcall _ _ -> undefined
  Const a   -> a
  Add a b   -> evalExp a + evalExp b
  Sub a b   -> evalExp a - evalExp b
  Mul a b   -> evalExp a * evalExp b
  Div _ _   -> undefined
  Mod a b   -> evalExp a `mod` b
  Not a     -> not $ evalExp a
  And a b   -> evalExp a &&  evalExp b
  Or  a b   -> evalExp a ||  evalExp b
  Eq  a b   -> evalExp a ==  evalExp b
  Lt  a b   -> evalExp a < evalExp b
  Gt  a b   -> evalExp a > evalExp b
  Le  a b   -> evalExp a <= evalExp b
  Ge  a b   -> evalExp a >= evalExp b
  Cond a b c -> if (evalExp a) then evalExp b else evalExp c

data Const
  = Bool   Bool
  | Int    Int
  | Float  Float
  | Void   Void
  | ArrayT (Exp Int) Const
  deriving (Eq)

isScalar :: Const -> Bool
isScalar c = case c of
  ArrayT _ _ -> False
  _        -> True

instance Show (Const) where
  show a = case a of
    Bool  True  -> "true"
    Bool  False -> "false"
    Int   a     -> show a
    Float a     -> show a
    Void _      -> ""
    ArrayT b e  -> "{" ++ (intercalate "," $ replicate (evalExp b) (show e)) ++ "}"

showConstType :: Const -> String
showConstType a = case a of
  Bool  _     -> "boolean"
  Int   _     -> "int"
  Float _     -> "float"
  Void _      -> "void"
  ArrayT b e  -> (showConstType e) ++ "[" ++ show b ++ "]"

-- | True term.
true :: Exp Bool
true = Const True

-- | False term.
false :: Exp Bool
false = Const False

-- | Arbitrary constants.
constant :: Elt a => a -> Exp a
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

boundsCheck :: Exp Int -> Array a -> Bool
boundsCheck idx arr = (x <= y)
  where
    x = evalExp idx
    y = evalExp (bound arr)

-- | Array Dereference:
(!) :: Elt a => Var (Array a) -> Exp Int -> Var a 
(Var name arr) ! idx =
  Var (name ++ "[" ++ show idx ++ "]") $ ele arr
--  ADK: First, need to fix evalExpr before we can
--       do bounds checking.
--  if (boundsCheck idx arr)
--  then Var (name ++ "[" ++ show idx ++ "]") $ ele arr
--  else error $ "invalid array index: " ++ show idx ++ " in " ++ show (Var name arr)

-- | The conjunction of a Exp Bool list.
and_ :: [Exp Bool] -> Exp Bool
and_ = foldl (&&.) true

-- | The disjunction of a Exp Bool list.
or_ :: [Exp Bool] -> Exp Bool
or_ = foldl (||.) false

-- | Equal.
(==.) :: Elt a => Exp a -> Exp a -> Exp Bool
(==.) = Eq

-- | Not equal.
(/==.) :: Elt a => Exp a -> Exp a -> Exp Bool
a /==. b = not_ (a ==. b)

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

-- | Expression level conditionals.
cond :: Elt a => Exp Bool -> Exp a -> Exp a -> Exp a
cond = Cond

-- | Modulo.
mod_ :: Exp Int -> Int -> Exp Int
mod_ _ 0 = error "divide by zero (mod_)"
mod_ a b = Mod a b

-- | References a variable to be used in an expression.
ref :: Elt a => Var a -> Exp a
ref = Ref

-- | Increments a Var Int.
(.++) :: CoreE a => Var Int -> a ()
(.++) a = a <== ref a + 1

-- | Decrements a Var Int.
(.--) :: CoreE a => Var Int -> a ()
(.--) a = a <== ref a - 1

-- | Sum assign a Var Int.
(+=) :: (NumE n, CoreE a) => Var n -> Exp n -> a ()
a += b = a <== ref a + b

-- | Subtract and assign a Var.
(-=) :: (NumE n, CoreE a) => Var n -> Exp n -> a ()
a -= b = a <== ref a - b

-- | Product assign a Var.
(*=) :: (NumE n, CoreE a) => Var n -> Exp n -> a ()
a *= b = a <== ref a * b

-- | Divide and assign a Var Int.
(/=.) :: CoreE a => Var Int -> Exp Int -> a ()
a /=. b = a <== ref a / b

-- | FFI. This takes an arbitrary function name and arguments, and
-- | splices it directly in the translated program.
fcall :: String -> Exp a -> Exp b
fcall = Fcall
