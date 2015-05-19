module Plotting where

import Benchmarks hiding (main)
import Graphics.EasyPlot
import Data.Tree
import System.Environment
import Control.SpaceProbe.Search
import Control.Applicative
import Control.SpaceProbe.Probe
import Control.Monad
import qualified Data.Map as M
import Data.List (group, foldl', sort)
import System.Random
import System.IO.Unsafe

takeEvery k xs =                                                                
  case drop k xs of                                                             
    [] -> []                                                                    
    (y:ys) -> y : takeEvery k ys                                                
                                                                                
fmt ([a, b], _) = (a, b)                                                        
                  
softexp x = if x < 5 then exp x else x

ys = minimize (softexp . rosenbrock) $ hypercube 2 (-5) 5                                   

zs = minimize bukin $ (,) <$> uniform (-15) (-5) <*> uniform (-3) (3)

qs = minimize schafferN4 $ centeredSquare 100

xs = minimize table $ centeredSquare 10

us = minimize ackley $ perturbedSquare 5

xs1 = minimize (const 1) $ intDistribution (\x -> 1 - exp (-0.01 * fromIntegral x)) 0 100000 :: [(Int, Float)]

xs2 = minimize (const 1) $ uniformInt 0 100000

xs3 :: [(Int, Float)]
xs3 = minimize (const 1) $ normalInt 10 5

xs4 :: [(Int, Float)]
xs4 = minimize (const 1) $ exponentialInt 10

counts :: [(Int, Float)] -> [(Float, Float)]
counts = map (\xs -> (fromIntegral $ head xs, fromIntegral $ length xs)) . group . sort . filter (<50) . map fst 

pss :: [Double]
pss = map fst $ minimize (const 1) $ normal 15 5

rs :: [Float]
rs = map fst $ minimize (const 1) $ exponential 5

ps :: [((Double, Double), Float)]
ps = minimize (const 1) $ (,) <$> normal 15 5 <*> normal 5 2 

histogram :: Float -> Float -> Int -> [Float] -> [(Float, Float)]
histogram a b numBins xs = map (\(a, b) -> (fromBin a, fromIntegral b)) . 
                           M.toAscList $ 
                           foldl' (\m x -> M.insertWith (+) (bin x) 1 m)
                           M.empty xs
  where bin x     = round $ fromIntegral numBins * (x - a) / (b - a)  
        fromBin k = a + (fromIntegral k / fromIntegral numBins) * (b - a)

rand_ :: IO Float
rand_ = randomRIO (0, 100)
{-# NOINLINE rand_ #-}

plot_ :: Int -> IO Bool                                                                        
plot_ n = plot (JPEG $ "plot.jpg") $
            counts $  
            take n $ 
            xs3
    
maitn = runBenchmark bukin ((,) <$> uniform (-15) (-5) <*> uniform (-3) 3) 0.292529 >>= print
    
maimn = join $ (fmap $ ( mapM_ print . lowestYet)) $ evaluateForusecs 5000000 $ zs                                                                            
searchTreeToTree (SearchTree n xs) = Data.Tree.Node n $ map searchTreeToTree xs 
                                                                                
fmtTree t k = drawTree $ fmap show $ cut k $ searchTreeToTree $ t               
                                                                                
cut 0 (Data.Tree.Node n xs) = Data.Tree.Node n []                               
cut k (Data.Tree.Node n xs) = Data.Tree.Node n $ map (cut $ k - 1) xs           
                                                                                
layer 0 (Data.Tree.Node n _) = [n]                                              
layer k (Data.Tree.Node _ xs) = concatMap (layer $ k - 1) xs    



