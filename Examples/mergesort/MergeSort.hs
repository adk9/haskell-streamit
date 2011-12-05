module Main (main) where

import Control.Monad
import Language.StreamIt

sortInput :: Filter ()
sortInput = do
  work (1, 0, 0) $ do
    x <- int' "x" 0
    push $ ref x

intPrinter :: Filter ()
intPrinter = do
  work (0, 1, 0) $ do
    println $ pop'

merger :: Filter ()
merger = do
  work (0, 1, 1) $ do
    index1 <- int' "index1" 0
    index2 <- int' "index2" 1
    push $ ref index2
    pop

merger2 :: Filter ()
merger2 = do
  work (1, 1, 0) $ do
    index1 <- int' "index1" 0
    push $ ref index1
    pop

sorter :: StreamIt ()
sorter = pipeline $ do
  add "int->int" "Merger" merger
  add "int->int" "Merger2" merger2

mergeSort :: StreamIt ()
mergeSort = pipeline $ do
  add "void->int" "SortInput" sortInput
  add' "int->int" "Sorter" sorter
  add "int->void" "IntPrinter" intPrinter

main :: IO ()
main = do
  genStreamIt "void->void" "MergeSort" mergeSort
  return ()
