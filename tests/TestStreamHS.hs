module Main where

import Language.StreamIt
import Examples.StreamIt

import Test.HUnit (Assertion)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

testHello :: Assertion
testHello = do
  run StreamIt $(compile StreamIt helloWorld)
  return ()
            
testMergeSort :: Assertion
testMergeSort = do
  run StreamIt $(compile StreamIt mergeSort)
  return ()

testFile :: Assertion
testFile = do
  run StreamIt $(compile StreamIt fileTest)
  run StreamIt $(compile StreamIt filePrinter)
  return ()
  
tests :: [Test]
tests = [
  testGroup "Basic examples" [
       testCase "Hello"     (testHello)
     , testCase "File"      (testFile)
     , testCase "MergeSort" (testMergeSort)
     ]
  ]

main :: IO ()
main = defaultMain tests
