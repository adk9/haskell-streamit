module Examples.Hello.HelloWorld (helloWorld) where

import Language.StreamIt

intSource :: Filter Void Int ()
intSource = do
  x <- int
  init' $ do
    x <== 0

  work Rate {pushRate=1, popRate=0, peekRate=0} $ do
    (.++)x
    push(ref x)

intPrinter :: Filter Int Void ()
intPrinter = do
  work (Rate 0 1 0) $ do
    x <- pop 
    println x
    return ()

helloWorld :: StreamIt Void Void ()
helloWorld = pipeline $ do
  add intSource
  add intPrinter
