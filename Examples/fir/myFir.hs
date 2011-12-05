module Main (main) where

import Language.StreamIt

myFir :: Filter ()
myFir = do
  result <- float "result"
  weights <- float "weights"

  result <== ref result + ref weights
  work (1, 0, 1) $ do
    moo <- int "moo"
    moo <== ref moo

  pop
  push $ ref result

main :: IO ()
main = filter' "float->float" "myFir" myFir
