{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.Vectadd.VectAdd (vectAdd)

main = runStreamIt $(compileStreamIt "VectAdd" vectAdd)
