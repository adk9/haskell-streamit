{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import HelloWorld (helloWorld)
import Language.StreamIt

main = runStreamIt $(compileStreamIt "void->void" "HelloWorld" helloWorld)
