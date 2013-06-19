{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.Vectadd.VectAdd (vectAdd)

main = run StreamIt $(compile StreamIt vectAdd)
