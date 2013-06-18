module Examples.Vectadd.VectAdd (vectAdd) where

import Language.StreamIt

vectAddKernel :: Filter Int Int ()
vectAddKernel = do
  work Rate {pushRate=0, popRate=2, peekRate=0} $ do
    t1 <- int
    t2 <- int
--    t1 <== pop
--    t2 <== pop
    push(ref t1 + ref t2)

vectSource :: Var Int -> Var (Array Int) -> Filter Void Int ()
vectSource n z = do
  idx <- int
  init' $ do
    idx <== 0

  work Rate {pushRate=1, popRate=0, peekRate=0} $ do
    push(ref (z ! ref idx))
    (.++)idx
    if_ (ref idx >. ref n)
      (idx <== 0)

vectPrinter :: Filter Int Void ()
vectPrinter = do
  work (Rate 0 1 0) $ do
    println $ pop

vectAdd :: StreamIt Void Void ()
vectAdd = pipeline $ do
  n <- int' 10
  arr <- array int 20
  i <- int
  for_ (i <== 0, ref i <. ref n, (.++)i) $ do
    arr!(2*ref i) <== 2*ref i
    arr!(2*(ref i)+1) <== 2*(ref i)+1
  
  -- Use a single source
  add2 vectSource (2*(ref n), ref arr)

  add vectAddKernel
  add vectPrinter
