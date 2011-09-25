#!/usr/bin/env runhaskell

import Distribution.Simple

main :: IO () 
main = do putStrLn$ "Running Setup.hs ..."
	  defaultMain

