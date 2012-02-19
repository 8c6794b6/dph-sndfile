{-# LANGUAGE ParallelArrays, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fvectorise #-}
module Test.Vectorised where

import Data.Array.Parallel (PArray, toPArrayP)
import Data.Array.Parallel.Prelude.Double ((+), (*), (/))
import qualified Data.Array.Parallel.Prelude.Double as D
import qualified Data.Array.Parallel.Prelude.Int as I

import Prelude (Double, Int)
import qualified Prelude as P

gensinePA :: Double -> Int -> Int -> PArray Double
gensinePA freq len rate = toPArrayP (gensine freq len rate)
{-# NOINLINE gensinePA #-}

gensinesPA :: Double -> Int -> Int -> PArray Double
gensinesPA freq len rate = toPArrayP (gensines freq len rate)
{-# NOINLINE gensinesPA #-}

gensine :: Double -> Int -> Int -> [:Double:]
gensine freq len rate =
  [: D.sin (2 * pi * D.fromInt t * freq / D.fromInt rate)
  | t <- I.enumFromToP 0 (len I.- 1) :]

gensines :: Double -> Int -> Int -> [:Double:]
gensines freq len rate =
  let f t x = D.sin (2 * pi * D.fromInt t * x / D.fromInt rate)
      g t = f t freq + f t (freq*1.5) + f t (freq*2) + f t (freq*5) +
            f t (freq*6) + f t (freq*8) + f t (freq*9) + f t (freq*12)
  in  [:0.125 * g t | t <- I.enumFromToP 0 (len I.- 1):]

pi :: Double
pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286
{-# INLINE pi #-}
