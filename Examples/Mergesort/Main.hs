{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.Mergesort.MergeSort (mergeSort)

-- main :: IO ()
-- main = do
--   genStreamIt mergeSort
--   return ()

main = run StreamIt $(compile StreamIt mergeSort)
