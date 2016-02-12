module Main where

import Hatlab.Derivatives
import qualified Hatlab.SimplifyUniplate as U
import Test.QuickCheck

instance Arbitrary Expression where
    arbitrary = sized arb
        where
            arb :: Int -> Gen Expression
            arb 0 = oneof [fmap V arbitrary, return X]
            arb n = oneof [pure Add <*> (arb n') <*> (arb n'),
                           pure Mul <*> (arb n') <*> (arb n'),
                           pure Sub <*> (arb n') <*> (arb n'),
                           pure Div <*> (arb n') <*> (arb n'),
                           pure Pow <*> (arb n') <*> (arb n'),
                           pure Sin <*> (arb (n-1)),
                           pure Cos <*> (arb (n-1)),
                           pure Sqrt <*> (arb (n-1)),
                           pure Ln <*> (arb (n-1)),
                           arb 0,
                           arb 0
                          ]
                          where
                            n' = n `div` 2

prop_simplify e x = evalFun e x == evalFun (simplify e) x

prop_exprAtEq = \e x -> exprAt e x == U.exprAt e x

main = quickCheck prop_exprAtEq
