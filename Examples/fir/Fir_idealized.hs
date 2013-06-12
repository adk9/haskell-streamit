{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

import qualified Language.StreamIt as S
import Language.StreamIt -- (Exp, Filter, Var)
-- Functional layer:
-- import qualified Language.StreamIt.Fun as F
import qualified Prelude as P
import Prelude ((+),(*),($), (.), error, return,
                Int, Float, Num)

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
fir :: Int -> SArray i o Float -> S.Filter Float Float ()
fir n weights =
  filter $ \ strm loopK -> 
    let window = take (S.constant n) strm in
    sum (zipWith (*) weights window) <:>
     loopK (tail strm)

--------------------------------------------------------------------------------
-- Types:

-- | An abstract handle on the stream.
data Stream a = Stream Int
  -- ^ Right now it's nothing but a cursor that starts at zero.

-- | A StreamIt (staged) array.
-- 
--  Here we use a push/pull array hybrid where the consumer chooses which
--  representation to use.
-- 
--  (This combines push-arrays with the generalized-stream-fusion style multi-rep
--  bundling.)
data SArray inT ouT a =
  SArray
  { pushrep :: PushArray inT ouT a
  , pullrep :: PullArray a 
  }  

data PushArray inT ouT a = PushArray ((Exp Int -> Exp a -> Filter inT ouT ()) -> Filter inT ouT ())
type PullArray a = (Exp Int -> Exp a)

-- | Turn a manifest array bound to a variable into an array that can be used with
-- the combinators.
namedArr :: Var (S.Array a) -> SArray i o a
namedArr vr =
  SArray
  { pushrep = loopPusher (constant 0) (\i -> vr  ! ref i)
  , pullrep = (vr S.!) 
  }
  
--------------------------------------------------------------------------------

take :: Elt a => Exp Int -> Stream a -> SArray i o a
take n (Stream cursor) =
  SArray
  { pullrep = peeker
  , pushrep = loopPusher n (peeker . ref)
  }
  where peeker ix = peek (S.constant cursor + ix)

-- | Capture the common pattern of sequentially pushing all elements of the array
-- with a for loop.
loopPusher :: Exp Int -> (Var Int -> Exp a) -> PushArray i o a 
loopPusher len body = 
  PushArray $ \ rcvr -> do
           -- Here we loop through and do all the peeks, pushing them to the consumer:
           ix <- int
           for_ (ix <== 0, ref ix <. len, ix <== ref ix + 2) $ do 
              rcvr (ref ix) (body ix)
           return ()


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

sum :: Num a => SArray i o a -> S.Exp a
-- sum :: Num a => SArray a -> a
sum = error "sum"

zipWith :: (Exp a -> Exp b -> Exp c) -> SArray i o a -> SArray i o b -> SArray i o c
zipWith fn SArray{pushrep=push1, pullrep=pull1}
           SArray{pushrep=push2, pullrep=pull2} =
  SArray
  { pullrep = \ ix -> fn (pull1 ix) (pull2 ix)
  , pushrep = error "FINSHME"                      
    -- When pushing, zipWith prefers not to generate TWO loops.  The sizes should match
    -- so one loop should suffice.
    --
    -- Currently there's no way to know which rep each array "prefers" so we arbitrarily
    -- pull the first and let the second push.                      
  }
