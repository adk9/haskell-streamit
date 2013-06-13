module Main where

import Language.StreamIt
import Examples.StreamIt

import Test.HUnit (Assertion)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

testHello :: Assertion
testHello = do
  runStreamIt $(compileStreamIt "HelloWorld" helloWorld)
  return ()
            
testMergeSort :: Assertion
testMergeSort = do
  runStreamIt $(compileStreamIt "MergeSort" mergeSort)
  return ()

testFile :: Assertion
testFile = do
  runStreamIt $(compileStreamIt "FileTest" fileTest)
  runStreamIt $(compileStreamIt "FilePrinter" filePrinter)
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
