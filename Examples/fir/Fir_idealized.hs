

import qualified Language.StreamIt as S
-- Functional layer:
-- import qualified Language.StreamIt.Fun as F
import qualified Prelude as P
import Prelude ((*),($), error, Int, Float, Num)

{-

  The cannonical streamit fir filter:

     float->float filter FIR (int N, float[N] weights) {     
             work push 1 pop 1 peek N
             {
                     float result = 0;
                     for (int i = 0; i < N; i++) {
                             result += weights[i] * peek(i);
                     }
                     pop();
                     push(result);
             }
     }

  An example of what we might like to write, ideally:

    loop strm =
      let window = take n strm in
      sum (zipWith (*) weights window)
       `scons`
      loop (tail strm)

  But that can't quite work, because we won't have a way to capture the loop and turn
  it into a for-loop.

-}

--------------------------------------------------------------------------------
-- Here's the version of Filter we can do:

-- | The take is the source of the loops, other array ops get fused with that.
fir :: Int -> SArray Float -> S.Filter Float Float ()
fir n weights =
  filter $ \ strm loopK -> 
    let window = take n strm in
    sum (zipWith (*) weights window) <:>
     loopK (tail strm)

--------------------------------------------------------------------------------
-- Types:

-- | An abstract handle on the stream.
data Stream a = Stream Int
  -- ^ Right now it's nothing but a cursor that starts at zero.

-- | A StreamIt (staged) array. 
data SArray a =
  SArray
  {
-- FINISHME:
-- 
--  Here we use a push/pull array hybrid where the consumer chooses which
--  representation to use.
-- 
--  (This combines push-arrays with the generalized-stream-fusion style multi-rep
--  bundling.)
  }
  
--------------------------------------------------------------------------------

take :: Int -> Stream a -> SArray a
take = error "take"

scons :: S.Exp a -> Stream a -> Stream a
scons = error "scons"

-- Shorthand:
x <:> y = scons x y

head :: Stream a -> S.Exp a
head = error "head"

tail :: Stream a -> Stream a
tail = error "tail"

filter :: (Stream a -> (Stream a -> Stream b) -> Stream b) -> S.Filter a b ()
filter = error "filter"

sum :: Num a => SArray a -> S.Exp a
-- sum :: Num a => SArray a -> a
sum = error "sum"

zipWith :: (a -> b -> c) -> SArray a -> SArray b -> SArray c
zipWith = error "zipwith"
