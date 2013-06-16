{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, CPP, NamedFieldPuns #-}

import qualified Language.StreamIt as S
import Control.Monad.State.Strict
import Language.StreamIt -- (Exp, Filter, Var)
import Language.StreamIt.Core
-- Functional layer:
-- import qualified Language.StreamIt.Fun as F
import qualified Prelude as P
import Control.Monad (mapM_, forM_, return)
import Prelude ((+),(-),(*),($), (.), (++),error, return, show,
                Int, Float, Num, Bool, Show, Read, Ord, Eq)
import Debug.Trace

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
-- MAJOR QUESTIONS / TODOS:

-- Can sum,zipWith,fold stay pure?  They would need to create a datatype capturing
-- all the relevant info.

--------------------------------------------------------------------------------
-- Here's the version of Filter we can do:

-- | The take is the source of the loops, other array ops get fused with that.
fir :: Int -> SArray i o Float -> S.Filter Float Float ()
fir n weights =
-- n <- input int
  funFilter $ \ strm loopK -> 
    let window = take (S.constant n) strm in
    -- sum (zipWith (*) weights window) <:>
    --  loopK (tail strm)
    
--    sum (zipWith (*) weights window) <:>
    loopK (tail strm)
--    error "FINISHME - fir"
    
example :: S.StreamIt Float Float ()
example = pipeline$ do
  weights <- array float 10
  -- FIXME: This argument passing method won't work for this:
  -- Passing Var's around is generally unsafe because it can break lexical scope:
  add (fir 10 (namedArr weights))
  

--------------------------------------------------------------------------------
-- Types:

type FunFilterM a b = StateT () (Filter a b)

-- | An abstract handle on the stream.
data Stream a = Stream [Exp a] Int
  -- ^ It contains a cursor that starts at zero as well as a list of elements scons'd
  --   on the front, with the head of the list being the most recently added.
  deriving (Show, Eq)

-- | A StreamIt (staged) array.
-- 
--  Here we use a push/pull array hybrid where the consumer chooses which
--  representation to use.
-- 
--  This combines push-arrays with the generalized-stream-fusion style multi-rep
--  bundling.)  The benefit of push arrays is that they will provide safety while
--  avoiding bounds-checks, at least if consumed by a reduction instead of used to
--  populate an array.
data SArray inT ouT a =
  SArray
  { pushrep :: PushArray inT ouT a
  , pullrep :: PullArray a
  , arrlen  :: Exp Int  
  }  

data PushArray inT ouT a =
     PushArray ((Exp Int -> Exp a -> Filter inT ouT ()) -> Filter inT ouT ())
type PullArray a = (Exp Int -> Exp a)

-- | Turn a manifest array bound to a variable into an array that can be used with
-- the combinators.
namedArr :: Elt a => Var (S.Array a) -> SArray i o a
namedArr vr =
  SArray
  { pushrep = loopPusher (constant 0) (\i -> ref (vr  ! ref i))
  , pullrep = ref . (vr S.!)

  , arrlen  = bound (val vr) -- error "FIX LENGTH" 
       -- FIXME ^ Watch out for duplicating side-effecting expressions!!
  }
  
--------------------------------------------------------------------------------

-- FIXME: take into account the added elements!!!
take :: forall i o a . Elt a => Exp Int -> Stream a -> SArray i o a
take n (Stream ls cursor) =
  SArray
  { pullrep = peeker
   -- TODO: Opitimize for the case where we know the indices we're pushing to at the Haskell level...
              --  We can avoid the generation of StreamIt conditionals below...
  , pushrep = loopPusher n (peeker . ref)
  , arrlen  = n
  }
  where
    peeker ix = mkConds 0 ls ix $
                \n -> peek (S.constant cursor + n)

    -- If the user adds to the front of a stream, and then takes a window from it, we
    -- end up with a pile of conditionals to check whether we hit the 
    mkConds :: Int -> [Exp a] -> Exp Int -> (Exp Int -> Exp a) -> Exp a
    mkConds offset []      requestedIx conseq = conseq (requestedIx `minus` offset)
    mkConds offset (hd:tl) requestedIx conseq =
      cond (requestedIx ==. constant offset)
           hd
           (mkConds (offset+1) tl requestedIx conseq )

    minus x 0 = x
    minus x y = x - constant y 

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

-- | This replaces pop.
tail :: Stream a -> Stream a
tail (Stream [] cursor) = Stream [] (cursor+1)
tail (Stream ls cursor) = Stream (P.tail ls) cursor  

-- | Create a complete StreamIt filter, but build the body using the functional
-- stream abstraction.
funFilter :: forall a b . (Elt a, Elt b) =>
             (Stream a -> (Stream a -> FunFilterM a b (Stream b)) -> FunFilterM a b (Stream b)) ->
             S.Filter a b ()
funFilter kern = do
  let fm = kern initStrm cont 

  (strmOut,()) <- runStateT fm ()
                  -- Uh, wrong place for effects?  Need to capture emitted code somehow?
                  -- Possibly use evalStmt here...
  trace ("GOT STRMOUT " ++ show strmOut) $ return ()
  case strmOut of
    -- All the push/pop actions a user can take end up in this output stream:
    Stream ls cursor -> do
      init' $ return ()
      let peeks = 0 -- How do we know the high-water-mark of all PEEKS?  (e.g. takes)
      work Rate { pushRate= constant (P.length ls),
                  popRate = constant cursor,
                  peekRate= peeks } $ do
        -- Then we generate a solid block of pushes and pops:
        mapM_ push ls
        if cursor P.< 5 then
          mapM_ (\_ -> pop) [1..cursor]
         else do
          ix <- int
          for_ (ix <== 0, ref ix <. constant cursor, ix += 1) $ pop
        return ()
 where
   initStrm = Stream [] 0
   
   cont (Stream [] popCount) =     
     -- FIXME: record popCount
     return (Stream [] 0) -- Fresh Stream 'b' to receive pushed elements...
     
   cont (Stream ls _) =
     error "funFilter: cannot push elements back onto input stream; continuation arg must be a tail of the input stream."

zipWith :: (Exp a -> Exp b -> Exp c) -> SArray i o a -> SArray i o b -> SArray i o c
zipWith fn SArray{pushrep=push1, pullrep=pull1, arrlen=len1 }
           SArray{pushrep=push2, pullrep=pull2, arrlen=len2 } =
  SArray
  { arrlen  = len1 -- FIXME: check that lengths are equal.
  , pullrep = \ ix -> fn (pull1 ix) (pull2 ix)
  , pushrep = error "FINSHME - zipWith pushrep"                      
    -- When pushing, zipWith prefers not to generate TWO loops.  The sizes should match
    -- so one loop should suffice.
    --
    -- Currently there's no way to know which rep each array "prefers" so we arbitrarily
    -- pull the first and let the second push.                      
  }

scons :: S.Exp a -> Stream a -> Stream a
scons hd (Stream ls cursor) = Stream (hd:ls) cursor

#if 0
--------------------OPTION 1: Pure SArray ops--------------------
    
sum :: (Num a, NumE a) => SArray i o a -> S.Exp a
sum = fold (+) 0 

-- | Fold is a *consumer* of arrays.
fold :: (Exp a -> Exp b -> Exp a) -> a -> SArray i o b -> S.Exp a
fold fn init arr =   
  error "Finishme - SArray fold"

-- Shorthand:
x <:> y = scons x y

#else
--------------------OPTION 2: Monadic SArray ops--------------------

sum :: (Num a, NumE a) => SArray i o a -> S.Filter i o a
sum = fold (+) 0 

fold :: (Exp a -> Exp b -> Exp a) -> a -> SArray i o b -> S.Filter i o a
fold fn init arr@SArray{arrlen} = do 
  ix <- int
  sum <- int
  sum <== 0 
  for_ (ix <== 0, ref ix <. arrlen, ix <== ref ix + 2) $ do
    return ()
  return (error "FINISH FOLD")

-- Shorthand:
(<:>) :: FunFilterM i o (Exp a) -> 
         FunFilterM i o (Stream a) ->
         FunFilterM i o (Stream a)
x <:> y = do
  x' <- x
  y' <- y
  return (scons x' y')

#endif
