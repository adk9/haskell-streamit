{-# LANGUAGE TypeSynonymInstances, GADTs #-}

-- An example of how to wire StreamIt graphs with Arrows (if it is possible!)

import Language.StreamIt
import Control.Arrow
import Control.Category

-- | GADT Reifying the Arrow (and Category) type classes.
data StreamItArrow a b where
  Arr    :: (a -> b) -> StreamItArrow a b
  First  :: StreamItArrow b c -> StreamItArrow (b,d) (c,d)
  Second :: StreamItArrow b c -> StreamItArrow (d,b) (d,c)
  Cat    ::  StreamItArrow b c -> StreamItArrow a b -> StreamItArrow a c 
  
instance Arrow StreamItArrow where
  arr     = Arr
  first s = First s
  second  = Second

instance Category StreamItArrow where
  -- Control.Category.id :: Category cat => cat a a
  -- (Control.Category..) :: cat b c -> cat a b -> cat a c
  id = Arr Prelude.id
  ar1 . ar2 = Cat ar1 ar2

-- | We can only run arrow computations that operate over Exp's 
runIt :: (Elt a, Elt b) =>
         StreamItArrow (Exp a) (Exp b) -> StreamIt a b ()
runIt = error "foo"

----------------------------------------

intSource :: Filter Void Int ()
intSource = do
  x <- int
  init' $ do
    x <== 0
  work Rate {pushRate=1, popRate=0, peekRate=0} $ do
    (.++)x
    push(ref x)

intPrinter :: Filter Int Void ()
intPrinter = do
  work (Rate 0 1 0) $ do
    println $ pop

helloWorld :: StreamIt Void Void ()
helloWorld = pipeline $ do
  add intSource
--  arr (+1)
  let a :: StreamItArrow (Exp Int) (Exp Int)
      a = arr (+1)
      r :: StreamIt Int Int ()
      r = runIt a
  add r
  add intPrinter

-- fn     = StreamItArrow $ do
--     -- We are implicitly part of a pipeline, add one filter to it:
--     add $ do
--       init' (return ())
--       work Rate {pushRate=1, popRate=1, peekRate=0} $ do
--         x <- pop
--         push (fn x)
--     return ()
