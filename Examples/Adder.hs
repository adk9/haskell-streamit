module Main (main) where

import Language.StreamIt

fir :: Stmt ()
fir = do
  result <- float "result"
  tmp <- int "tmp"

  result <== ref result
  pop
  push $ ref result

main :: IO ()
main = filter' "FIR" fir
