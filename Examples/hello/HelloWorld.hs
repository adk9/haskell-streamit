module HelloWorld (helloWorld) where

import Language.StreamIt

intSource :: Filter Void Int ()
intSource = do
  x <- int
  init' $ do
    x <== 0

  work (1, 0, 0) $ do
    incr x
    push(ref x)

intPrinter :: Filter Int Void ()
intPrinter = do
  work (0, 1, 0) $ do
    println $ pop

helloWorld :: StreamIt Void Void ()
helloWorld = pipeline $ do
  add intSource
  add intPrinter
