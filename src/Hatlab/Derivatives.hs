module Hatlab.Derivatives where

import Hatlab.Expression

derivative :: Expression -> Expression
derivative (V d)     = 0
derivative X         = 1
derivative (Add a b) = simplify $ (derivative a) + (derivative b)
derivative (Sub a b) = simplify $ (derivative a) - (derivative b)
derivative (Negate a)= simplify $ negate (derivative a)
derivative (Mul a b) = simplify $ (a * (derivative b)) + ((derivative a) * b)
derivative (Div a b) = simplify $ ((b * (derivative a)) - (a * (derivative b))) / (b * b)
derivative (Pow f g) = simplify $ (f ** g) * (derivative g) * (log f) + ((f ** (g - 1)) * g * (derivative f))
derivative (Sin e)   = simplify $ (cos e) * (derivative e)
derivative (Cos e)   = simplify $ negate (sin e) * derivative e
derivative (Sqrt e)  = simplify $ derivative (e ** 0.5)
derivative (Exp e)   = simplify $ Exp e * derivative e
derivative (Ln e)    = simplify $ (derivative e) / e
derivative (Tan e)   = simplify $ (derivative e) * (1 + (tan e)*(tan e))
derivative (ASin e)  = simplify $ (derivative e) / (sqrt (1-e*e))
derivative (ACos e)  = simplify $ (derivative e) / (-1*(sqrt (1-e*e)))
derivative (ATan e)  = simplify $ (derivative e) / (e*e+1)
derivative (Sinh e)  = simplify $ (derivative e) * (cosh e)
derivative (Cosh e)  = simplify $ (derivative e) * (sinh e)
derivative (Tanh e)  = simplify $ (derivative e) / (cosh e)
derivative (ASinh e) = simplify $ (derivative e) / (sqrt (e*e+1))
derivative (ACosh e) = simplify $ (derivative e) / (sqrt (e*e-1))
derivative (ATanh e) = simplify $ (derivative e) / (1 - e*e)
