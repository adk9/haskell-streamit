module HelloWorld (helloWorld) where

import Language.StreamIt

intSource :: Filter ()
intSource = do
  x <- int
  init' $ do
    x <== 0

  work (1, 0, 0) $ do
    incr x
    push(ref x)

intPrinter :: Filter ()
intPrinter = do
  work (0, 1, 0) $ do
    println $ pop

helloWorld :: StreamIt ()
helloWorld = pipeline $ do
  add "void->int" intSource
  add "int->void" intPrinter
