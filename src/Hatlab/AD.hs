-- | Simple automatic differentiation, with only first order
-- derivatives
module Hatlab.AD where


-- | |D f f'|, f is the value and f' is the value of the derivative
data D a = D a a
  deriving Show


idD x = D x 1
constD c = D c 0

chainRule f f' (D g g') = D (f g) (f' g * g')

instance Num a => Num (D a) where
  fromInteger = constD . fromInteger

  D f f' + D g g' = D (f + g) (f' + g')
  D f f' * D g g' = D (f * g) (f * g' + f' * g)

  negate = chainRule negate (const (-1))

  signum = chainRule signum (const 0)
  abs    = chainRule abs signum

sqr x = x * x

instance Fractional a => Fractional (D a) where
  fromRational = constD . fromRational
  recip = chainRule recip (negate . recip . sqr)

instance Floating a => Floating (D a) where
  pi = constD pi

  sqrt = chainRule sqrt undefined

  exp = chainRule exp exp

  sin = chainRule sin cos
  cos = chainRule cos (negate . sin)

  asin = chainRule asin (recip . sqrt . (1 -) . sqr)
  acos = chainRule acos (negate . recip . sqrt . (1 - ) . sqr)
