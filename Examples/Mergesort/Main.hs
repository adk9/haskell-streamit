{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.Mergesort.MergeSort (mergeSort)

-- main :: IO ()
-- main = do
--   genStreamIt "MergeSort" mergeSort
--   return ()

main = runStreamIt $(compileStreamIt "MergeSort" mergeSort)
