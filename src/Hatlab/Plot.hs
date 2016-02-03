{-# LANGUAGE ScopedTypeVariables #-} -- for exception handling
module Hatlab.Plot where

import Control.Concurrent
import System.Process
import System.IO
import System.IO.Unsafe
import Control.Exception
import Data.List

import qualified Data.Vector.Storable as V

-- Low-level things to start and connect to gnuplot ----------------------------

tryRunGnuplot :: IO (Handle, Handle, Handle, ProcessHandle)
tryRunGnuplot =
  let run path = runInteractiveProcess path [] Nothing Nothing
  in catch (run "gnuplot")
       (\(e :: SomeException) ->
         catch (run "/usr/bin/gnuplot")
           (\(e :: SomeException) -> run "/usr/local/bin/gnuplot"))

gnuplotHandle :: Handle
gnuplotHandle = unsafePerformIO $
           do (inp,_out,_err,pid) <- tryRunGnuplot
              hSetBinaryMode inp False
              hSetBuffering inp LineBuffering
              hPutStrLn inp "set multiplot\nplot 1\nclear"
              return inp

plotCmd :: [String] -> IO ()
plotCmd strs = do
                 -- For debugging, remove comment in next line to see gnuplot cmds in terminal
                 -- mapM_ putStrLn strs
                 mapM_ (hPutStrLn gnuplotHandle) strs
                 return ()

-- Downscaled version of Hatlab plotting, to avoid all Chebyshev things.
-- Still vectors, but functions do not take vector arguments (to avoid having to install hmatrix).

class Plottable a where
   plot :: [a] -> IO ()

data BasicPlot = Fun  (Double -> Double) String
               | Pts  (V.Vector Double)  (Double -> Double) String
               | Pts2 (V.Vector Double)  (V.Vector Double) String

clear = plotCmd ["clear"]

instance Plottable BasicPlot where
  plot [] = return ()
  plot fs = do
    clear
    plotCmd [mkPlot (-1,1) lbl fs]
    plotCmd (map ((++ "e") . p) fs)
    where
      lbl (Fun _ lab)    = " w l lw 2 t " ++ show lab
      lbl (Pts _ _ lab)  = " w p pt 7 t " ++ show lab
      lbl (Pts2 _ _ lab) = " w p pt 7 t " ++ show lab

      p :: BasicPlot -> String
      p (Fun f _) = plotPts xs (V.map f xs)
        where xs = linspace 1001 (-1,1)      -- Plot interval "hard-coded"
      p (Pts xs f lab) = plotPts xs (V.map f xs)
      p (Pts2 xs ys lab) = plotPts xs ys


mkPlot :: (Double, Double) -- limits
       -> (a -> String) -- plottable thing to label
       -> [a]
       -> String
mkPlot _ _ [] = ""
mkPlot (a, b) lbl (f:fs) =
  concat
    (["plot [",
      show a,
      ":",
      show b,
      "] "
     ] ++
     (intersperse ", "
      (("'-' " ++ lbl f) :
       map (("'' " ++) . lbl) fs)))

plotPts :: V.Vector Double -> V.Vector Double -> String
plotPts xs ys = V.ifoldr g "\n" xs
  where g i x ack = show x ++ " " ++ show (ys V.! i) ++ "\n" ++ ack

lspace :: Int -> (Double, Double) -> [Double]
lspace n (a, b) = itr n (\x -> x+h) a
  where
    h = (b-a) / fromIntegral (n-1)
    itr n = (take n .) . iterate

linspace :: Int -> (Double, Double) -> V.Vector Double
linspace n (a,b) = V.generate n (\k -> a + fromIntegral (k-1) * h)
  where h = (b-a)/fromIntegral (n-1)
