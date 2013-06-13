{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.Hello.HelloWorld (helloWorld)

main = runStreamIt $(compileStreamIt "HelloWorld" helloWorld)
