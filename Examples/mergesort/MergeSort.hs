module Main (main) where

import Language.StreamIt

sortInput :: Filter ()
sortInput = do
  n <- input int
  work (ref n, 0, 0) $ do
    i <- int
    for_  (i <== 0, ref i <. ref n, incr i) $ do
      push $ ref n - ref i

intPrinter :: Filter ()
intPrinter = do
  work (0, 1, 0) $ do
    println $ pop

merger :: Filter ()
merger = do
  n <- input int
  work (ref n, ref n, 0) $ do
    index1 <- int' 0
    index2 <- int' 1

    while_ ((ref index1 <. ref n) &&. (ref index2 <. ref n)) $ do
      val1 <- int
      val2 <- int
      val1 <== peek (ref index1)
      val2 <== peek (ref index2)
      ifelse (ref val1 <=. ref val2)
        (do
            push $ ref val1
            index1 <== ref index1 + 2)
        (do
            push $ ref val2
            index2 <== ref index2 + 2)
        
    leftover <- int
    ifelse (ref index1 <. ref n)
      (leftover <== ref index1)
      (leftover <== ref index2)
    
    i <- int
    for_ (i <== ref leftover, ref i <. ref n, i <== ref i + 2)
      (push $ peek (ref i))

    for_ (i <== 0, ref i <. ref n, incr i) pop

sorter :: StreamIt ()
sorter = pipeline $ do
  n <- input int
  if_ (ref n >. 2) (splitjoin_ $ do
                       split roundrobin
                       add' "int->int" sorter [ref n / 2]
                       add' "int->int" sorter [ref n / 2]
                       join roundrobin)
  add' "int->int" merger [ref n]

mergeSort :: StreamIt ()
mergeSort = pipeline $ do
  numInputs <- int' 16
  mult <- int' 4

  add' "void->int" sortInput [ref numInputs / ref mult]
  add' "int->int"  sorter [ref numInputs]
  add "int->void"  intPrinter

main :: IO ()
main = do
  genStreamIt "void->void" "MergeSort" mergeSort
  return ()
