{-# LANGUAGE FlexibleInstances #-}
module Hatlab.Derivatives where

import Hatlab.Plot

data Expression = Add Expression Expression
                | Mul Expression Expression
                | Sub Expression Expression
                | Div Expression Expression
                | X
                | V Double
x       = X
v d     = V d
e .+. f = Add e f
e .-. f = Sub e f
e .*. f = Mul e f
e ./. f = Div e f

derivative :: Expression -> Expression
derivative (V d)     = V 0
derivative X         = V 1
derivative (Add a b) = simplify $ (derivative a) .+. (derivative b)
derivative (Sub a b) = simplify $ (derivative a) .-. (derivative b)
derivative (Mul a b) = simplify $ (a .*. (derivative b)) .+. ((derivative a) .*. b)
derivative (Div a b) = simplify $ ((b .*. (derivative a)) .-. (a .*. (derivative b))) ./. (b .*. b)

evalFun :: Expression -> Double -> Double
evalFun (V d) x     = d 
evalFun X x         = x
evalFun (Add a b) x = (evalFun a x) + (evalFun b x)
evalFun (Mul a b) x = (evalFun a x) * (evalFun b x)
evalFun (Sub a b) x = (evalFun a x) - (evalFun b x)
evalFun (Div a b) x = (evalFun a x) / (evalFun b x)

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

paren :: String -> String
paren s = "("++s++")"

simplify :: Expression -> Expression
simplify X                     = X
simplify (V d)                 = V d
simplify (Add a b) 
    | isZero (simplify b)      = simplify a
    | otherwise                = Add (simplify a) (simplify b)
simplify (Sub a b)
    | isZero (simplify b)      = a
    | otherwise                = Sub (simplify a) (simplify b)
simplify (Mul a b)
    | isZero (simplify a)      = V 0
    | isZero (simplify b)      = V 0
    | isOne (simplify a)       = (simplify b)
    | isOne (simplify b)       = (simplify a)
    | otherwise                = Mul (simplify a) (simplify b)
simplify (Div a b)
    | isZero (simplify a)      = V 0
    | otherwise                = Div (simplify a) (simplify b)

isOne :: Expression -> Bool
isOne X         = False
isOne (V d)     = d == 1
isOne (Add a b) = ((isOne a) && (isZero b)) || ((isOne b) && (isZero a))
isOne (Sub a b) = (isZero b) && (isOne a)
isOne (Mul a b) = (isOne a) && (isOne b)
isOne (Div a b) = (isOne a) && (isOne b)

isZero :: Expression -> Bool
isZero X         = False
isZero (V d)     = d == 0
isZero (Add a b) = (isZero a) && (isZero b)
isZero (Sub a b) = (isOne a) && (isOne b)
isZero (Mul a b) = (isZero a) || (isZero b)
isZero (Div a b) = isZero a

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

            p e = concatMap show_ $ map (\x -> (x, evalFun e x)) (lspace resolution (-2.0, 2.0))

            show_ (x, y) = show x ++ " " ++ show y ++ "\n"
