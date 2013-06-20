{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.File.FilePrinter (filePrinter)
import Examples.File.FileTest (fileTest)

main = do
  run StreamIt $(compile StreamIt fileTest)
  run StreamIt $(compile StreamIt filePrinter)