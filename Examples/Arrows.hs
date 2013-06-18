{-# LANGUAGE TypeSynonymInstances, GADTs, ScopedTypeVariables #-}

-- An example of how to wire StreamIt graphs with Arrows (if it is possible!)

import Language.StreamIt
import Control.Arrow
import Control.Category
import Data.Typeable

-- | GADT Reifying the Arrow (and Category) type classes.
data StreamItArrow a b where
  Arr    :: (a -> b) -> StreamItArrow a b
  First  :: StreamItArrow b c -> StreamItArrow (b,d) (c,d)
  Second :: StreamItArrow b c -> StreamItArrow (d,b) (d,c)
  Cat    ::  StreamItArrow b c -> StreamItArrow a b -> StreamItArrow a c 
  
instance Arrow StreamItArrow where
  arr     = Arr
  first   = First 
  second  = Second

instance Category StreamItArrow where
  -- Control.Category.id :: Category cat => cat a a
  -- (Control.Category..) :: cat b c -> cat a b -> cat a c
  id = Arr Prelude.id
  ar1 . ar2 = Cat ar1 ar2

-- | We can only run arrow computations that operate over Exp's 
runIt :: forall inT ouT . (Elt inT, Elt ouT, Typeable ouT, Typeable inT) =>
         StreamItArrow (Exp inT) (Exp ouT) -> StreamIt inT ouT ()
runIt rep =
  case rep of
    Arr fn ->
      let filt :: Filter inT ouT ()
          filt = do
            work Rate {pushRate=1, popRate=1, peekRate=0} $ do
              pop
              push (fn (peek 0))
              return ()
      -- We are implicitly part of a pipeline, add one filter to it:
      in add filt

--    First arr ->   error "Finish FIRST"
    
  -- Ok, we can't really do first unless we can make tuples an (Elt), which perhaps
  -- would be possible by mapping onto struct types.

  -- Really, First should not be handled through a local translation, rather, we
  -- should zoom out to the surround context and see what the passed-through wire is
  -- used for.  It could become a teleporting message.  It could result in the
  -- insertion of a split/join at a larger scope.  A First and Second together can
  -- represent task parallelism on two different types of data.  It's not actually
  -- clear how to do that in streamit with no sum types and such a restrictive
  -- split/join.

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
  let a :: StreamItArrow (Exp Int) (Exp Int)
      a = arr (+1)
      r :: StreamIt Int Int ()
      r = runIt a
  add r
  add intPrinter
