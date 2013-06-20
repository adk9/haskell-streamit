haskell-streamit
================

**haskell-streamit** is a StreamIt EDSL in Haskell. It generates a StreamIt source program from an equivalent Haskell program written using StreamIt abstractions exposed by the EDSL. This is a very rough prototype and many of the StreamIt features are lacking.

Installation
------------

    cabal install

Example
-------

The following program defines a simple Hello World StreamIt program that increments and prints an integer stream.

    module Examples.Hello.HelloWorld (helloWorld) where
    
    import Language.StreamIt
    
    intSource :: Filter Void Int ()
    intSource = do
      x <- int
      init' $ do
        x <== 0
    
      work Rate {pushRate=1, popRate=0, peekRate=0} $ do
        (.++)x
        push(ref x)
    
    intPrinter :: Filter Int Void ()
    intPrinter = do
      work (Rate 0 1 0) $ do
        println $ pop
    
    helloWorld :: StreamIt Void Void ()
    helloWorld = pipeline $ do
      add intSource
      add intPrinter

Look in the `Examples` directory for other examples and StreamIt constructs
supported by our EDSL.

Documentation
-------------

Refer to the generated Haddock documentation [here](http://adk9.github.com/haskell-streamit/).

Contact
-------

Abhishek Kulkarni, adkulkar@indiana.edu
