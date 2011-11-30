module Main (main) where

import Control.Monad
import Language.StreamIt

intSource :: Filter ()
intSource = do
  x <- int "x"
  init' $ do
    x <== 0

  work (1, 0, 0) $ do
    push(ref x + 1)

intPrinter :: Filter ()
intPrinter = do
  work (0, 1, 0) $ do
    pop >>= (println . ref)

helloWorld :: StreamIt ()
helloWorld = pipeline "void->void" "helloWorld" $ do
  add intSource
  add intPrinter

main :: IO ()
main = runStreamIt helloWorld
