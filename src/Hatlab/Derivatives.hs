{-# LANGUAGE FlexibleInstances #-}
module Hatlab.Derivatives where

import Control.Monad (guard)

import Hatlab.Plot

data Expression = Add      Expression Expression
                | Mul      Expression Expression
                | Sub      Expression Expression
                | Div      Expression Expression
                | V        Double
                | Pow      Expression Expression
                | Sin      Expression
                | Cos      Expression
                | Tan      Expression
                | ASin     Expression
                | ACos     Expression
                | ATan     Expression
                | Sqrt     Expression
                | Ln       Expression
                | Tanh     Expression
                | Sinh     Expression
                | Cosh     Expression
                | ATanh    Expression
                | ACosh    Expression
                | ASinh    Expression
                | X

x       = X
v d     = V d
e .+. f = Add e f
e .-. f = Sub e f
e .*. f = Mul e f
e ./. f = Div e f
cose e  = Cos e
sine e  = Sin e
tane e  = Tan e
atane e = ATan e
acose e = ACos e
asine e = ASin e
sinhe e = Sinh e
coshe e = Cosh e
tanhe e = Tanh e
asinhe e = ASinh e
acoshe e = ACosh e
atanhe e = ATanh e
e .^. f = Pow e f
sqrte e = Sqrt e
ln e    = Ln e

instance Num Expression where

    (+) = (.+.)
    (*) = (.*.)
    (-) = (.-.)

    negate e = 0 .-. e
    abs e    = sqrte (e * e)
    signum e = e ./. (abs e)

    fromInteger = v . fromInteger

instance Fractional Expression where

    (/)     = (./.)

    fromRational = v . fromRational

instance Floating Expression where
    
   pi  = V pi 
   exp = (**) (V (exp 1))
   log = ln
   sin = sine
   cos = cose
   tan = tane
   asin = asine
   acos = acose
   atan = atane
   sinh = sinhe
   cosh = coshe
   tanh = tanhe
   asinh = asinhe
   acosh = acoshe
   atanh = atanhe
   sqrt = sqrte
   (**) = (.^.)


derivative :: Expression -> Expression
derivative (V d)     = 0
derivative X         = 1
derivative (Add a b) = simplify $ (derivative a) + (derivative b)
derivative (Sub a b) = simplify $ (derivative a) - (derivative b)
derivative (Mul a b) = simplify $ (a * (derivative b)) + ((derivative a) * b)
derivative (Div a b) = simplify $ ((b .*. (derivative a)) - (a * (derivative b))) / (b * b)
derivative (Pow f g) = simplify $ (f .^. g) * (derivative g) * (ln f) + ((f .^. (g - 1)) * g * (derivative f))
derivative (Sin e)   = simplify $ (derivative e) * (cos e)
derivative (Cos e)   = simplify $ 0 - (sin e)
derivative (Sqrt e)  = simplify $ derivative (e .^. 0.5)
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

evalFun :: Expression -> Double -> Maybe Double
evalFun (V d) x     = Just d
evalFun X x         = Just x
evalFun (Add a b) x = (+) <$> evalFun a x <*> (evalFun b x)
evalFun (Mul a b) x = (*) <$> evalFun a x <*> (evalFun b x)
evalFun (Sub a b) x = (-) <$> evalFun a x <*> (evalFun b x)
evalFun (Cos a)   x = cos <$> evalFun a x
evalFun (Sin a)   x = sin <$> evalFun a x
evalFun (Tan a)   x = tan <$> evalFun a x
evalFun (ATan a) x  = atan <$> evalFun a x
evalFun (Tanh a) x  = tanh <$> evalFun a x
evalFun (Sinh a) x  = sinh <$> evalFun a x
evalFun (Cosh a) x  = cosh <$> evalFun a x
evalFun (ASinh a) x = asinh <$> evalFun a x

evalFun (ATanh a) x = do
  a' <- evalFun a x
  guard (abs a' < 1)
  return (atanh a')

evalFun (ACosh a) x = do
  a' <- evalFun a x
  guard (a' >= 1)
  return (acosh a')

evalFun (Div a b) x = do
  a' <- evalFun a x
  b' <- evalFun b x
  guard (b' /= 0)
  return (a' / b')

evalFun (Pow a b) x = do
  a' <- evalFun a x
  guard (a' >= 0)
  b' <- evalFun b x
  return (a' ** b')

evalFun (Sqrt a)  x = do
  a' <- evalFun a x
  guard (a' >= 0)
  return (sqrt a')

evalFun (Ln a)    x = do
  a' <- evalFun a x
  guard (a' >= 0)
  return (log a')

evalFun (ACos a)  x = do
  a' <- evalFun a x
  guard (abs a' < 1)
  return (acos a')

evalFun (ASin a)  x = do
  a' <- evalFun a x
  guard (abs a' < 1)
  return (asin a')

showexp :: Expression -> String
showexp X         = "x"
showexp (V d)     = show d
showexp (Add a b) = (showexp a) ++ " + " ++ (showexp b)
showexp (Sub a b) = (showexp a) ++ " - " ++ (showexp' b)
    where
        showexp' e@(Sub _ _) = paren (showexp e)
        showexp' e@(Add _ _) = paren (showexp e)
        showexp' e           = showexp e
showexp (Mul a b) = (showexp' a) ++ " * " ++ (showexp' b)
    where
        showexp' e@(Sub _ _) = paren (showexp e)
        showexp' e@(Add _ _) = paren (showexp e)
        showexp' e           = showexp e
showexp (Div a b) = (showexp' a) ++ " / " ++ (showexp' b)
    where
        showexp' e@(Sub _ _) = paren (showexp e)
        showexp' e@(Add _ _) = paren (showexp e)
        showexp' e@(Mul _ _) = paren (showexp e)
        showexp' e           = showexp e
showexp (Pow a b) = (showexp' a) ++ " \\^ " ++ (showexp' b)
    where
        showexp' X       = "x"
        showexp' (V d)   = show d
        showexp' (Cos e) = showexp (Cos e)
        showexp' (Sin e) = showexp (Sin e)
        showexp' (Ln e)  = showexp (Ln e)
        showexp' e       = paren (showexp e)
showexp (Sin e)   = "sin(" ++ (showexp e)++")"
showexp (Cos e)   = "cos(" ++ (showexp e)++")"
showexp (Ln e)    = "ln(" ++ (showexp e)++")"
showexp (Sqrt e)  = "sqrt(" ++ (showexp e)++")"
showexp (Tan e)   = "tan("++(showexp e)++")"
showexp (ACos e)  = "acos("++(showexp e)++")"
showexp (ASin e)  = "asin("++(showexp e)++")"
showexp (ATan e)  = "atan("++(showexp e)++")"
showexp (Tanh e)  = "tanh("++(showexp e)++")"
showexp (Cosh e)  = "cosh("++(showexp e)++")"
showexp (Sinh e)  = "sinh("++(showexp e)++")"
showexp (ASinh e) = "asinh("++(showexp e)++")"
showexp (ACosh e) = "acosh("++(showexp e)++")"
showexp (ATanh e) = "atanh("++(showexp e)++")"

paren :: String -> String
paren s = "("++s++")"

simplify :: Expression -> Expression
simplify X                     = X
simplify (V d)                 = V d
simplify (Add a b)
    | isZero (simplify b)      = simplify a
    | isZero (simplify a)      = simplify b
    | otherwise                = Add (simplify a) (simplify b)
simplify (Sub a b)
    | isZero (simplify b)      = a
    | otherwise                = Sub (simplify a) (simplify b)
simplify (Mul a b)
    | isZero (simplify a)      = 0
    | isZero (simplify b)      = 0
    | isOne (simplify a)       = (simplify b)
    | isOne (simplify b)       = (simplify a)
    | otherwise                = Mul (simplify a) (simplify b)
simplify (Div a b)
    | isZero (simplify a)      = 0
    | otherwise                = Div (simplify a) (simplify b)
simplify (Pow a b)
    | isOne (simplify a)       = 1
    | isZero (simplify a)      = 1
    | isZero (simplify b)      = 1
    | isOne (simplify b)       = a
simplify e                     = e

isOne :: Expression -> Bool
isOne X         = False
isOne (V d)     = d == 1
isOne (Add a b) = ((isOne a) && (isZero b)) || ((isOne b) && (isZero a))
isOne (Sub a b) = (isZero b) && (isOne a)
isOne (Mul a b) = (isOne a) && (isOne b)
isOne (Div a b) = (isOne a) && (isOne b)
isOne (Pow a b) = (isOne a) || (isZero b)
isOne (Cos a)   = (isZero a)
isOne e         = False

isZero :: Expression -> Bool
isZero X         = False
isZero (V d)     = d == 0
isZero (Add a b) = (isZero a) && (isZero b)
isZero (Sub a b) = (isOne a) && (isOne b)
isZero (Mul a b) = (isZero a) || (isZero b)
isZero (Div a b) = isZero a
isZero (Pow a b) = isZero a
isZero (Ln a)    = isOne a
isZero (Sin a)   = isZero a
isZero e         = False

instance Show Expression where
    show = showexp

instance Plottable Expression where
    plot [] = return ()
    plot es = do clear
                 plotCmd [headers es]
                 plotCmd (map ((++"e\n") . p) es)

        where
            headers (e : es) = "plot [-2:2] "
                               ++ concatMap (\x -> x++", ")
                                            (header "'-' " e : map (header "'' ") es)
            header str e = str ++ " w l lw 2 t " ++ show (show e)

            resolution = 4000

            p e = concatMap show_ $
                  map (\(x, Just y) -> (x, y)) $
                  filter (\(x, y) -> isJust y) $
                  map (\x -> (x, evalFun e x)) (lspace resolution (-2.0, 2.0))

            isJust Nothing = False
            isJust _       = True

            show_ (x, y) = show x ++ " " ++ show y ++ "\n"
