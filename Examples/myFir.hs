module Main (main) where

import Language.StreamIt

myFir :: Filter ()
myFir = do
  result <- float "result"
  tmp <- int "tmp"

  result <== ref result
  work (1, 0, 0) $ do
    moo <- int "moo"
    moo <== ref moo

  pop
  push $ ref result

main :: IO ()
main = filter' "float->float" "myFir" myFir
