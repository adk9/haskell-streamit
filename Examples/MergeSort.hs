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

sorter :: StreamIt ()
sorter = pipeline "int->int" "Sorter" $ do
  add "int->int" "Merger" merger

mergeSort :: StreamIt ()
mergeSort = pipeline "void->void" "MergeSort" $ do
  add "void->int" "SortInput" sortInput
  add' "Sorter" sorter
  add "int->void" "IntPrinter" intPrinter

main :: IO ()
main = runStreamIt "MergeSort.str" mergeSort
