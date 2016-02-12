-- |
module Hatlab.SimplifyUniplate where

import Data.Generics.Uniplate.Data

import Hatlab.Derivatives hiding (simplify, exprAt)
import qualified Hatlab.Derivatives as D

simplify :: Expression -> Expression
simplify = transform f
  where
    f (Add a b)
      | isZero a = b
      | isZero b = a
    f (Sub a b)
      | isZero a = Negate b
      | isZero b = a
    f (Mul a b)
      | isZero a = 0
      | isZero b = 0
    f (Div a b)
      | isZero a = 0
    f (Pow a b)
      | isOne a  = 1
      | isZero a = 1
      | isZero b = 1
      | isOne b  = a
    f (Negate (V a)) = V (negate a)
    f (Negate (Negate e)) = e
    f (Negate e)
      | isZero e = 0
    f e = e

exprAt e v = transform f e
  where f X = v
        f e = e
