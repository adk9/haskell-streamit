module Examples.File.FileTest (fileTest) where

import Language.StreamIt

fileSource :: Filter Void Float ()
fileSource = do
  x <- float
  init' $ do
    x <== 0

  work Rate {pushRate=1, popRate=0, peekRate=0} $ do
    x <== ref x + 2
    push(ref x)

fileTest :: StreamIt Void Void ()
fileTest = pipeline $ do
  add fileSource
  fileWriter Float "float.test"
