{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import FileTest (fileTest)
import FilePrinter (filePrinter)
import Language.StreamIt

main = do
  runStreamIt $(compileStreamIt "FileTest" fileTest)
  runStreamIt $(compileStreamIt "FilePrinter" filePrinter)