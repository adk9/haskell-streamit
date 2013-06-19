module Examples.Mergesort.MergeSort (mergeSort) where

import Language.StreamIt

sortInput :: Var Int -> Filter Void Int ()
sortInput n = do
  work Rate {pushRate=ref n, popRate=0, peekRate=0} $ do
    i <- int
    for_  (i <== 0, ref i <. ref n, (.++)i) $ do
      push $ ref n - ref i

intPrinter :: Filter Int Void ()
intPrinter = do
  work (Rate 0 1 0) $ do
    println =<< pop

merger :: Var Int -> Filter Int Int ()
merger n = do
  work Rate {pushRate=ref n, popRate=ref n, peekRate=0} $ do
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

    for_ (i <== 0, ref i <. ref n, (.++)i) pop

sorter :: Var Int -> StreamIt Int Int ()
sorter n = pipeline $ do
  if_ (ref n >. 2) (splitjoin_ $ do
                       split roundrobin
                       add1 sorter (ref n / 2)
                       add1 sorter (ref n / 2)
                       join roundrobin)
  add1 merger (ref n)

mergeSort :: StreamIt Void Void ()
mergeSort = pipeline $ do
  numInputs <- int' 16
  mult <- int' 4

  add1 sortInput (ref numInputs / ref mult)
  add1 sorter (ref numInputs)
  add  intPrinter
