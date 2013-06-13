{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.File.FilePrinter (filePrinter)
import Examples.File.FileTest (fileTest)

main = do
  runStreamIt $(compileStreamIt "FileTest" fileTest)
  runStreamIt $(compileStreamIt "FilePrinter" filePrinter)