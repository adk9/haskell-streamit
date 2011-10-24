module Main (main) where

import Language.StreamIt

fir :: Stmt ()
fir = do
  n <- input int "N"
  weights <- input float "weights"

  result <- float "result" 0
  tmp <- float "tmp" 42
  result <== ref result + ref tmp
  pop
  push $ ref result

main :: IO ()
main = filter' "FIR" $ fir
