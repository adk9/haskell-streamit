module FilePrinter (filePrinter) where

import Language.StreamIt

fileSink :: Filter Float Void ()
fileSink = do
  work Rate {pushRate=0, popRate=1, peekRate=0} $ do
    println $ pop

filePrinter :: StreamIt Void Void ()
filePrinter = pipeline $ do
  fileReader Float "float.test"
  add fileSink
