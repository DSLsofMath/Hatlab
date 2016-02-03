-- | Simple automatic differentiation, with only first order
-- derivatives
module Hatlab.AD where

import Hatlab.Plot

-- | |D f f'|, f is the value and f' is the value of the derivative
data D a = D a a
  deriving Show


idD :: Num a => a -> D a
idD x = D x 1

constD :: Num a => a -> D a
constD c = D c 0

chainRule :: Num a => (a -> a) -> (a -> a) -> D a -> D a
chainRule f f' (D g g') = D (f g) (f' g * g')

instance Num a => Num (D a) where
  fromInteger = constD . fromInteger

  D f f' + D g g' = D (f + g) (f' + g')
  D f f' * D g g' = D (f * g) (f * g' + f' * g)

  negate = chainRule negate (const (-1))

  signum = chainRule signum (const 0)
  abs    = chainRule abs signum

sqr :: Num a => a -> a
sqr x = x * x

instance Fractional a => Fractional (D a) where
  fromRational = constD . fromRational
  recip = chainRule recip (negate . recip . sqr)

instance Floating a => Floating (D a) where
  pi = constD pi

  sqrt = chainRule sqrt (recip . (2 *) . sqrt)

  exp = chainRule exp exp
  log = chainRule log recip

  sin = chainRule sin cos
  cos = chainRule cos (negate . sin)

  asin = chainRule asin (recip . sqrt . (1 -) . sqr)
  acos = chainRule acos (negate . recip . sqrt . (1 -) . sqr)
  atan = chainRule atan (recip . (+ 1) . sqr)

  sinh = chainRule sinh cosh
  cosh = chainRule cosh sinh

  asinh = chainRule asinh (recip . sqrt . (+ 1) . sqr)
  acosh = chainRule acosh (recip . sqrt . (\x -> x - 1) . sqr)
  atanh = chainRule atanh (recip . (1 -) . sqr)

fun :: Num a => (D a -> D a) -> a -> D a
fun f = f . constD

data ADPlot = AD (D Double -> D Double) String

instance Plottable ADPlot where
  plot fs = undefined
