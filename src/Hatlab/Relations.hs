{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}
module Hatlab.Relations where

import Hatlab.Plot

type Algebra f a = f a -> a

data Deep f = In (f (Deep f))

fold :: (Functor f) => Algebra f a -> Deep f -> a
fold g (In x) = g (fmap (fold g) x)

data Relation a
  = Relation (Double -> Double -> Bool) String
  | Intersect a a
  | Union     a a
  | Minus     a a
  | Complement a
  deriving Functor

r f s     = In (Relation f s)
r ./\. r' = In (Intersect r r')
r .\/. r' = In (Union     r r')
r .\.  r' = In (Minus     r r')
c r       = In (Complement r)

label :: Relation String -> String
label (Relation _ s)     = s
label (Intersect r1 r2)  = "(intersection " ++ r1 ++ " " ++ r2 ++ ")"
label (Union r1 r2)      = "(union " ++ r1 ++ " " ++ r2 ++ ")"
label (Minus r1 r2)      = "(" ++ r1 ++ " minus " ++ r2 ++ ")"
label (Complement r)     = "(complement " ++ r ++ ")"

filter_fun :: Relation ((Double, Double) -> Bool) -> (Double, Double) -> Bool
filter_fun (Union     r1 r2) p   = (r1 p) || (r2 p)
filter_fun (Intersect r1 r2) p   = (r1 p) && (r2 p)
filter_fun (Minus     r1 r2) p   = (r1 p) && (not (r2 p))
filter_fun (Relation r _) (x, y) = r x y
filter_fun (Complement r)    p   = not (r p)

instance Plottable (Deep Relation) where
    plot [] = return ()
    plot rs = do clear
                 plotCmd ["set size ratio -1\n"]
                 plotCmd [headers rs]
                 plotCmd (map ((++"e\n") . p) rs)

        where
            headers (r : rs) = "plot [-2:2] "
                               ++ concatMap (\x -> x++", ")
                                            (header "'-' " r : map (header "'' ") rs)
            header str r = str ++ " w p pt 7 t " ++ show (fold label r)

            resolution = 400

            p r = concatMap show_ $
                  filter (fold filter_fun r) $
                  [(x, y) | x <- lspace resolution (-2.0, 2.0)
                          , y <- lspace resolution (-2.0, 2.0)]

            show_ (x, y) = show x ++ " " ++ show y ++ "\n"

instance Show (Deep Relation) where
    show = fold label
