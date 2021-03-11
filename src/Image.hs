--
-- EPITECH PROJECT, 2019
-- FUN_imageCompressor_2019
-- File description:
-- Image.hs
--

{-# LANGUAGE ScopedTypeVariables #-}

module Image where

import Exit

import System.IO

import Data.Maybe
import Data.Char

import Text.Read
import Control.Exception

data Pixel = WrongPixel | Pixel (Int, Int, Int, Int, Int) deriving Eq

instance Show Pixel where
    show (Pixel (x, y, r, g, b)) = show (x, y) ++ ' ' : show (r, g, b)

type Color = (Int, Int, Int)
type Image = [Pixel]
type Cluster = [(Color, Image)]

delSpace :: String -> String
delSpace xs = [x | x <- xs, not (x `elem` " ")]

clearNonNum :: String -> String
clearNonNum [] = []
clearNonNum (x : xs) | isDigit x == False && x /= '-' = ' ' : clearNonNum xs
                     | otherwise = x : clearNonNum xs

clearArray :: [String] -> [String]
clearArray x = map clearNonNum x

arrToPixel :: [String] -> Pixel
arrToPixel (x : y : r : g : b : _) | notNumbs == True || wrongRGBVal == True = WrongPixel
                                   | otherwise = Pixel ((read x :: Int), (read y :: Int), (read r :: Int), (read g :: Int), (read b :: Int))
    where col = [read r :: Int, read g :: Int, read b :: Int]
          mcol = [readMaybe x :: Maybe Int, readMaybe y :: Maybe Int, readMaybe r :: Maybe Int, readMaybe g :: Maybe Int, readMaybe b :: Maybe Int]
          notNumbs = (mcol !! 0 == Nothing || mcol !! 1 == Nothing || mcol !! 2 == Nothing || mcol !! 3 == Nothing || mcol !! 4 == Nothing)
          wrongRGBVal = ((col !! 0) < 0 || (col !! 0) > 255 || (col !! 1) < 0 || (col !! 1) > 255 || (col !! 2) < 0 || (col !! 2) > 255)

serializer :: String -> Pixel
serializer x | length (words x) /= 5 = WrongPixel
             | otherwise = arrToPixel (words x)

parseArray :: [String] -> Image
parseArray x = map serializer x

checkContent :: Image -> IO Image
checkContent image | WrongPixel `elem` image = return []
                   | otherwise = return image

getContent :: String -> IO Image
getContent file = do
    str <- readFile file
    return $ parseArray $ clearArray $ words $ delSpace str

openImage :: String -> IO Bool
openImage file = handle (\(e :: IOException) -> return False) $ do
    readFile file
    return True

checkArgs :: [String] -> IO Bool
checkArgs av | length av /= 3 = return False
checkArgs (a : b : c : _) | areNumbers == False || (head a) == '-' || (head b) == '-' = return False
                          | otherwise = openImage c
    where areNumbers = ((readMaybe a :: Maybe Int) /= Nothing && (readMaybe b :: Maybe Double) /= Nothing)

parseImage :: [String] -> Bool -> IO Image
parseImage _ ok | ok == False = return []
parseImage (_ : _ : file : _) _ = do
    content <- getContent file
    checkContent content
