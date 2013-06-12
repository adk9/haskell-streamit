module Fir where

import Language.StreamIt

fir N weights = filt (1,1,N)
        (do
                result <- float 0; -- new, initlzd variable

                for (0,N) $ \i -> do
                        t <- peek i
                        result += weights!i * t
                        -- Looks pretty similar to StreamIt code..
                        -- But can call Haskell functions in here!
                        pop
                        push result
        )

