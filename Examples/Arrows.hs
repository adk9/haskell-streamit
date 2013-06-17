{-# LANGUAGE TypeSynonymInstances #-}

-- An example of how to wire StreamIt graphs with Arrows (if it is possible!)

import Language.StreamIt
import Control.Arrow
import Control.Category

-- type StreamItArrow a b = StreamIt a b ()
newtype StreamItArrow a b = StreamItArrow { unArrow :: StreamIt a b () }

instance Arrow StreamItArrow where  
  arr fn = StreamItArrow $ do 
    -- We are implicitly part of a pipeline, add one filter to it:
    add $ do 
      init' (return ())
      work Rate {pushRate=1, popRate=1, peekRate=0} $ do
        x <- pop 
        push (fn x)
    return ()

instance Category StreamItArrow where
  -- Control.Category.id :: Category cat => cat a a
  -- (Control.Category..) :: cat b c -> cat a b -> cat a c
  
-- arr :: (Arrow a) => (b -> c) -> a b c
-- first :: (Arrow a) => a b c -> a (b, d) (c, d)
-- second :: (Arrow a) => a b c -> a (d, b) (d, c)
 
  -- > newtype SimpleFunc a b = SimpleFunc {
-- >     runF :: (a -> b)
-- > }
-- >
-- > instance Arrow SimpleFunc where
-- >     arr f = SimpleFunc f
-- >     first (SimpleFunc f) = SimpleFunc (mapFst f)
-- >                   where mapFst g (a,b) = (g a, b)
-- >     second (SimpleFunc f) = SimpleFunc (mapSnd f)
-- >                   where mapSnd g (a,b) = (a, g b)
-- >
-- > instance Category SimpleFunc where
-- >     (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
-- >     id = arr id


{-
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
  arr (+1)
  add intPrinter
-}
