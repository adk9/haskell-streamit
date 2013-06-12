{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import MergeSort (mergeSort)
import Language.StreamIt

-- main :: IO ()
-- main = do
--   genStreamIt "MergeSort" mergeSort
--   return ()

main = runStreamIt $(compileStreamIt "MergeSort" mergeSort)
