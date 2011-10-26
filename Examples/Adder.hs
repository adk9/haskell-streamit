module Main (main) where

import Language.StreamIt

fir :: Stmt ()
fir = do
  result <- float "result"
  tmp <- float "tmp"

  result <== ref result + ref tmp
  pop
  push $ ref result

main :: IO ()
main = filter' "FIR" fir
