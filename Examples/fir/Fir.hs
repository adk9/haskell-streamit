module Fir where

import Language.StreamIt

fir :: Var Int -> Var (Array Float) -> Filter Float Float ()
fir n weights = 
  work Rate{pushRate=1, popRate=1, peekRate=ref n} $ do 
    result <- float' 0
    for (0, ref n) $ \i -> do
      let i' = ref i
      result += ref (weights ! i') * peek i'
      -- Looks pretty similar to StreamIt code..
      -- But you can call Haskell functions in here!
      pop
      push (ref result)



for (lower,upper) fn = do 
  i <- int
  for_ (i <== lower, ref i <. upper, i += 1) $ fn i  
