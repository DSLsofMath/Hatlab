{-# LANGUAGE FlexibleInstances #-}
module Hatlab.Derivatives where

import Control.Monad (guard)

import Hatlab.Plot

data Expression = Add      Expression Expression
                | Mul      Expression Expression
                | Sub      Expression Expression
                | Div      Expression Expression
                | Pow      Expression Expression
                | Negate   Expression
                | Sin      Expression
                | Cos      Expression
                | Tan      Expression
                | ASin     Expression
                | ACos     Expression
                | ATan     Expression
                | Sqrt     Expression
                | Ln       Expression
                | Exp      Expression
                | Tanh     Expression
                | Sinh     Expression
                | Cosh     Expression
                | ATanh    Expression
                | ACosh    Expression
                | ASinh    Expression
                | V        Double
                | X

x = X

instance Num Expression where

    (+) = Add
    (*) = Mul
    (-) = Sub

    negate = Negate
    abs e    = sqrt (e * e)
    signum e = e / (abs e)

    fromInteger = V . fromInteger

instance Fractional Expression where

    (/)     = Div

    fromRational = V . fromRational

instance Floating Expression where

    pi  = V pi
    exp = Exp
    log = Ln
    sin = Sin
    cos = Cos
    tan = Tan
    asin = ASin
    acos = ACos
    atan = ATan
    sinh = Sinh
    cosh = Cosh
    tanh = Tanh
    asinh = ASinh
    acosh = ACosh
    atanh = ATanh
    sqrt = Sqrt
    (**) = Pow

exprAt :: Expression -> Expression -> Expression
exprAt (V d) v     = V d
exprAt X     v     = v
exprAt (Add e f) v = Add      (exprAt e v) (exprAt f v)
exprAt (Sub e f) v = Sub      (exprAt e v) (exprAt f v)
exprAt (Negate e)v = Negate   (exprAt e v)
exprAt (Mul e f) v = Mul      (exprAt e v) (exprAt f v)
exprAt (Div e f) v = Div      (exprAt e v) (exprAt f v)
exprAt (Pow e f) v = Pow      (exprAt e v) (exprAt f v)
exprAt (Exp e)   v = Exp      (exprAt e v)
exprAt (Sin e)   v = Sin      (exprAt e v)
exprAt (Cos e)   v = Cos      (exprAt e v)
exprAt (Tan e)   v = Tan      (exprAt e v)
exprAt (ASin e)  v = ASin     (exprAt e v)
exprAt (ACos e)  v = ACos     (exprAt e v)
exprAt (ATan e)  v = ATan     (exprAt e v)
exprAt (Tanh e)  v = Tanh     (exprAt e v)
exprAt (Sinh e)  v = Sinh     (exprAt e v)
exprAt (Cosh e)  v = Cosh     (exprAt e v)
exprAt (ACosh e) v = ACosh    (exprAt e v)
exprAt (ASinh e) v = ASinh    (exprAt e v)
exprAt (ATanh e) v = ATanh    (exprAt e v)
exprAt (Ln e)    v = Ln       (exprAt e v)


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

evalFun :: Expression -> Double -> Maybe Double
evalFun (V d) x     = Just d
evalFun X x         = Just x
evalFun (Add a b) x = (+) <$> evalFun a x <*> evalFun b x
evalFun (Mul a b) x = (*) <$> evalFun a x <*> evalFun b x
evalFun (Exp e)   x = exp <$> evalFun e x
evalFun (Sub a b) x = (-) <$> evalFun a x <*> evalFun b x
evalFun (Negate a)x = negate <$> evalFun a x
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
showexp (Negate e)= "-(" ++ (showexp e)++")"
showexp (Sin e)   = "sin(" ++ (showexp e)++")"
showexp (Cos e)   = "cos(" ++ (showexp e)++")"
showexp (Exp e)   = "exp(" ++ showexp e ++ ")"
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

type SimplExpr = Expression

simplify :: Expression -> SimplExpr
simplify X                     = X
simplify (V d)                 = V d
simplify (Add a b)
    | isZero (simplify b)      = simplify a
    | isZero (simplify a)      = simplify b
    | otherwise                = Add (simplify a) (simplify b)
simplify (Sub a b)
    | isZero (simplify b)      = a
    | isZero (simplify a)      = Negate (simplify b)
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
simplify (Negate e)
  | isZero e                   = 0
simplify (Negate (Negate e))   = simplify e
simplify e                     = e

isOne :: SimplExpr -> Bool
isOne (V d)     =  d == 1
isOne (Add a b) = (isOne a  &&  isZero b) || (isOne b  &&  isZero a)
isOne (Sub a b) =  isOne a  &&  isZero b
isOne (Mul a b) =  isOne a  &&  isOne b
isOne (Div a b) =  isOne a  &&  isOne b
isOne (Pow a b) =  isOne a  ||  isZero b
isOne (Exp a)   =  isZero a
isOne (Cos a)   =  isZero a
isOne e         =  False

isZero :: SimplExpr -> Bool
isZero (V d)     =  d == 0
isZero (Add a b) =  isZero a  &&  isZero b
isZero (Sub a b) =  isOne a   &&  isOne b
isZero (Mul a b) =  isZero a  ||  isZero b
isZero (Div a b) =  isZero a
isZero (Pow a b) =  isZero a
isZero (Ln a)    =  isOne a
isZero (Sin a)   =  isZero a
isZero e         =  False

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
