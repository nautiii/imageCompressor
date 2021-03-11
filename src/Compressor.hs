--
-- EPITECH PROJECT, 2019
-- FUN_imageCompressor_2019
-- File description:
-- Compressor.hs
--

module Compressor where

import System.IO
import System.Random

import Data.Maybe
import Data.List

import Image
import Exit

computeDist :: Color -> Color -> Float
computeDist (r1, g1, b1) (r2, g2, b2) = sqrt $ fromIntegral ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

listDist :: Pixel -> Cluster -> [Float]
listDist pix [] = []
listDist (Pixel (t, p, r, g, b)) ((a, _) : xs) = (computeDist (r, g, b) a) : (listDist (Pixel (t, p, r, g, b)) xs)

fillCenter :: Pixel -> Int -> Cluster -> Cluster
fillCenter pix 0 ((a, b) : xs) = (a, pix : b) : xs
fillCenter pix i (x : xs) = x : fillCenter pix (i - 1) xs

fillClusters :: Image -> Cluster -> Cluster
fillClusters [] color = color
fillClusters (x : xs) color = fillClusters xs fill
    where fill = fillCenter x (fromMaybe 0 (elemIndex (minimum list) list)) color
          list = listDist x color

sumRGB :: Image -> Int -> Int
sumRGB [] _ = 0
sumRGB ((Pixel (_, _, r ,g ,b)) : xs) i = ([r, g, b] !! i) + (sumRGB xs i)

average :: Cluster -> Cluster
average [] = []
average ((x, []) : xs) = (x, []) : average xs
average (((r, g, b), e) : xs) = ((averageC 0, averageC 1, averageC 2), []) : average xs
    where div = (realToFrac (length e))
          averageC = \index -> fromIntegral (round ((realToFrac (sumRGB e index)) / div)) :: Int

continueAlgo :: Cluster -> Cluster -> Float -> Bool
continueAlgo [] [] _ = False
continueAlgo ((a, b) : xs) ((c, d) : ys) e | computeDist a c <= e = continueAlgo xs ys e
                                           | otherwise = True

showImage :: Image -> IO ()
showImage [] = return ()
showImage (pixel : xs) = do
    print pixel
    showImage xs

showCluster :: Cluster -> IO ()
showCluster [] = return ()
showCluster ((a, b) : xs) = do
    putStrLn "--"
    print a
    putStrLn "-"
    showImage $ reverse b
    showCluster xs

runAlgo :: Image -> Cluster -> Float -> IO ()
runAlgo pix centers e = do
    let clusters = fillClusters pix centers
    let new = average clusters
    if (continueAlgo new clusters e)
        then runAlgo pix new e
    else
        showCluster clusters

getRands :: (Int, Int) -> Int -> StdGen -> [Int]
getRands range n = take n . randomRs range

generateCentroids :: [Int] -> Cluster
generateCentroids [] = []
generateCentroids (a : b : c : next) = ((a, b, c), []) : generateCentroids next

compress :: Image -> Int -> Float -> StdGen -> IO ()
compress image n e g = runAlgo image (generateCentroids $ getRands (0, 255 :: Int) (n * 3) g) e
