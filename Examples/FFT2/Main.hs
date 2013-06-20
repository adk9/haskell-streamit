{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.StreamIt
import Examples.FFT2.FFT2 (fft2)

main = run StreamIt $(compile StreamIt fft2)
