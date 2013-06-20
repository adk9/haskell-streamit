module Examples.FFT2.FFT2 (fft2) where

import Language.StreamIt

fftTestSource :: Var Int -> Filter Void Float ()
fftTestSource n = do
  work Rate {pushRate=2*ref n, popRate=0, peekRate=0} $ do
    i <- int
    push(0.0)
    push(0.0)
    push(1.0)
    push(0.0)
    for_ (i <== 0, ref i <. 2*((ref n)-2), (.++)i) $ do
      push(0.0)

floatPrinter :: Filter Float Void ()
floatPrinter = do
  work (Rate 0 1 0) $ do
    println =<< pop

combineDFT :: Var Int -> Filter Float Float ()
combineDFT n = do
  -- coefficients, real and imaginary interleaved
  w <- array float (ref n)
  init' $ do
    wn_r <- float
    wn_r <== fcall "cos" (2 * 3.141592654 / ref n)
    wn_i <- float
    wn_i <== fcall "sin" (-2 * 3.141592654 / ref n)
    real <- float' 1
    imag <- float' 0
    next_real <- float
    next_imag <- float

    i <- int
    for_ (i <== 0, ref i <. ref n, i += 2) $ do
      w!(ref i) <== ref real
      w!((ref i) + 1) <== ref imag
      next_real <== (ref real * ref wn_r - ref imag * ref wn_i)
      next_imag <== (ref real * ref wn_i + ref imag * ref wn_r)
      real <== ref next_real
      imag <== ref next_imag

  work Rate {pushRate=2*ref n, popRate=2*ref n, peekRate=0} $ do
    i <- int
    results <- array float (2*ref n)
    for_ (i <== 0, ref i <. ref n, i += 2) $ do
      -- this is a temporary work-around since there seems to be
      -- a bug in field prop that does not propagate nWay into the
      -- array references.  --BFT 9/10/02
	    
      --int tempN <== nWay
      --Fixed --jasperln
            
      -- removed nWay, just using n --sitij 9/26/03

      i_plus_1 <- int
      i_plus_1 <== ref i+1

      y0_r <- float
      y0_r <== peek (ref i)

      y0_i <- float
      y0_i <== peek (ref i_plus_1)
            
      y1_r <- float
      y1_r <== peek (ref n + ref i)
      y1_i <- float
      y1_i <== peek (ref n + ref i_plus_1)

      -- load into temps to make sure it doesn't got loaded
      -- separately for each load
      weight_real <- float
      weight_real <== ref (w!(ref i))
      weight_imag <- float
      weight_imag <== ref (w!(ref i_plus_1))

      y1w_r <- float
      y1w_r <== ref y1_r * ref weight_real - ref y1_i * ref weight_imag
      y1w_i <- float
      y1w_i <== ref y1_r * ref weight_imag + ref y1_i * ref weight_real

      results!(ref i) <== ref y0_r + ref y1w_r
      results!((ref i) + 1) <== ref y0_i + ref y1w_i
	    
      results!(ref n + ref i) <== ref y0_r - ref y1w_r
      results!(ref n + ((ref i) + 1)) <== ref y0_i - ref y1w_i

    for_ (i <== 0, ref i <. 2 * ref n, (.++)i) $ do
      pop
      push (ref (results!ref i))

fftReorderSimple :: Var Int -> Filter Float Float ()
fftReorderSimple n = do
  totalData <- int
  init' $ do
    totalData <== 2*ref n
  
  work Rate {pushRate=2*ref n, popRate=2*ref n, peekRate=0} $ do
    i <- int
    
    for_ (i <== 0, ref i <. ref totalData, i += 4) $ do
      push $ peek (ref i)
      push $ peek (ref i + 1)

    for_ (i <== 2, ref i <. ref totalData, i += 4) $ do
      push $ peek (ref i)
      push $ peek (ref i + 1)
    
    for_ (i <== 0, ref i <. ref n, (.++)i)
      (pop >> pop)

fftReorder :: Var Int -> StreamIt Float Float ()
fftReorder n = pipeline $ do
  i <- int
  for_ (i <== 1, ref i <. (ref n / 2), i *= 2)
    (add1 fftReorderSimple (ref n / ref i))

fftKernel1 :: Var Int -> StreamIt Float Float ()
fftKernel1 n = pipeline $ do
  splitjoin_ $ do
  split $ roundrobin [2]
  add1 fftKernel1 (ref n / 2)
  add1 fftKernel1 (ref n / 2)
  join $ roundrobin [ref n]

fftKernel2 :: Var Int -> StreamIt Float Float ()
fftKernel2 n = splitjoin $ do
  i <- int
  split $ roundrobin [2*ref n]
  for_ (i <== 0, ref i <. 2, (.++)i)
    (pipeline_ $ do
        add1 fftReorder (ref n)
        j <- int
        for_ (j <== 2, ref j <=. ref n, j *= 2)
          (add1 combineDFT (ref j)))
  join $ roundrobin [2*ref n]

fft2 :: StreamIt Void Void ()
fft2 = pipeline $ do
  add1 fftTestSource (64)
  add1 fftKernel2 (64)
  add  floatPrinter
