module Main (main) where

import Control.Monad
import Language.StreamIt

sortInput :: Filter ()
sortInput = do
  n <- input int "N"
  work (ref n, 0, 0) $ do
    i <- int "i"
    for_ (i <== 0, ref i <. ref n, incr i) $ do
      push $ ref n - ref i

intPrinter :: Filter ()
intPrinter = do
  work (0, 1, 0) $ do
    println $ pop'

merger :: Filter ()
merger = do
  n <- input int "N"
  work (ref n, ref n, 0) $ do
    index1 <- int' "index1" 0
    index2 <- int' "index2" 1

    while_ ((ref index1 <. ref n) &&. (ref index2 <. ref N)) $ do
      val1 <- int "val1"
      val2 <- int "val2"
      val1 <== peek $ ref index1
      val2 <== peek $ ref index2
      ifelse (ref val1 <=. ref val2)
        (do
            push $ ref val1
            index1 <== ref index1 + 2)
        (do
            push $ ref val2
            index2 <== ref index2 + 2)
        
    leftover <- int "leftover"
    ifelse (ref index1 <. ref n)
      (leftover <== ref index1)
      (leftover <== ref index2)
      i <- int "i"
      for_ (i <== ref leftover, ref i <. ref n, i <== ref i + 2) $ do
        push $ peek i

    for_ (i <== 0, ref i <. ref n, incr i) $ pop

sorter :: StreamIt ()
sorter = pipeline $ do
  n <- input int "N"
  if (ref n >. 2) $ do
    addFilter splitjoin $ do
      split roundrobin
        add "int->int"  "Sorter" sorter [ref n /. 2]
        add "int->int"  "Sorter" sorter [ref n /. 2]
        join roundrobin

    add "int->int" "Merger" merger [ref n]
    
mergeSort :: StreamIt ()
mergeSort = pipeline $ do
  numInputs <- int' "NUM_INPUTS" 16
  mult <- int' "MULT" 4

  add "void->int" "SortInput"  sortInput [ref numInputs /. ref mult]
  add "int->int"  "Sorter"     sorter [ref numInputs]
  add "int->void" "IntPrinter" intPrinter

main :: IO ()
main = do
  genStreamIt "void->void" "MergeSort" mergeSort
  return ()
