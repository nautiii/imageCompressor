-- 
-- EPITECH PROJECT, 2019
-- FUN_imageCompressor_2019
-- File description:
-- Main.hs
--

module Main where

import Image
import Compressor
import Exit

import System.Environment
import System.Random

main :: IO ()
main = do
    av <- getArgs
    seed <- newStdGen
    ok <- checkArgs av
    image <- parseImage av ok
    if image == []
        then failure
    else
        compress image (read (av !! 0) :: Int) (read (av !! 1) :: Float) seed
    success
