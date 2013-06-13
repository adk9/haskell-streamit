{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.Hello.HelloWorld (helloWorld)

main = genTBB "HelloWorld" helloWorld
