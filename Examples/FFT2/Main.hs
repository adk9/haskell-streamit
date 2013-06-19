{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.FFT2.FFT2 (fft2)

main = runStreamIt $(compile StreamIt fft2)
