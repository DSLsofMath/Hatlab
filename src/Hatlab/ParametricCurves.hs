{-# LANGUAGE FlexibleInstances #-}
module Hatlab.ParametricCurves where

import Hatlab.Plot

data Parametric = Par {x_t :: Double -> Double, y_t :: Double -> Double, interval :: (Double, Double), label :: String}

polarCurve :: (Double -> Double) -> (Double, Double) -> String -> Parametric
polarCurve r_fun interval name = Par (\theta -> (cos theta)*(r_fun theta)) (\theta -> (sin theta)*(r_fun theta)) interval name

instance Plottable Parametric where
    plot [] = return ()
    plot rs = do clear
                 plotCmd ["set size ratio -1\n"] 
                 plotCmd [headers rs]
                 plotCmd (map ((++"e\n") . p) rs)

        where
            headers (r : rs) = "plot ["++(show min_v)++":"++(show max_v)++"] "
                               ++ concat (map (\x -> x++", ") (header "'-' " r : map (header "'' ") rs))
            header str r = str ++ " w l lw 2 t " ++ (show (label r))

            resolution = 4000

            p r = concatMap show_ $ ps r

            ps r = [(x_t r t, y_t r t) | t <- lspace resolution (interval r)]

            show_ (x, y) = show x ++ " " ++ show y ++ "\n"

            min_v :: Double
            min_v = minimum ((map fst (concatMap ps rs))++(map snd (concatMap ps rs)))

            max_v :: Double
            max_v = maximum ((map fst (concatMap ps rs))++(map snd (concatMap ps rs)))
