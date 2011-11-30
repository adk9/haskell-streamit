module Main (main) where

import Language.StreamIt

myFir :: E Int -> E Float -> Filter ()
myFir n weights = do
  result <- float "result"
  tmp <- int "tmp"

  result <== ref result + weights
  work (1, 0, n) $ do
    moo <- int "moo"
    moo <== ref moo

  pop
  push $ ref result

n :: E Int
n = input int "n"

weights :: E Float
weights = input float "weights"

main :: IO ()
main = filter' "float->float" "myFir" $ myFir n weights
