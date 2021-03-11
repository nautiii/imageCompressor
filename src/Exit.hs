--
-- EPITECH PROJECT, 2019
-- FUN_imageCompressor_2019
-- File description:
-- Exit.hs
--

module Exit where

import System.Exit

success :: IO ()
success = exitWith ExitSuccess

failure :: IO ()
failure = do
    putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"
    exitWith $ ExitFailure 84
