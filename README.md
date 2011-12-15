haskell-streamit
================

**haskell-streamit** is a StreamIt EDSL in Haskell. It generates a StreamIt source program from an equivalent Haskell program written using StreamIt abstractions exposed by the EDSL. This is a very rough prototype and many of the StreamIt features are lacking.

Installation
------------

    cabal install

Example
-------

The following program defines a simple Hello World StreamIt program that increments and prints an integer stream.

    module HelloWorld (helloWorld) where

    import Control.Monad
    import Language.StreamIt

    intSource :: Filter ()
    intSource = do
      x <- int "x"
      init' $ do
        x <== 0

      work (1, 0, 0) $ do
        incr x
        push(ref x)

    intPrinter :: Filter ()
    intPrinter = do
      work (0, 1, 0) $ do
        println $ pop

    helloWorld :: StreamIt ()
    helloWorld = pipeline $ do
      add "void->int" "IntSource" intSource
      add "int->void" "IntPrinter" intPrinter

Documentation
-------------

Refer to the generated Haddock documentation [here](http://adk9.github.com/haskell-streamit/).

Contact
-------

Abhishek Kulkarni, adkulkar@indiana.edu