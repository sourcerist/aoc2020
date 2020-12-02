{-# LANGUAGE ScopedTypeVariables #-}

module Ex1 where

import Control.Monad (guard)
import Data.Maybe (listToMaybe)

getForN n cnt lst = do
    s <- subsOfCnt cnt lst 
    guard $ sum s == n
    return s

subsOfCnt :: Int -> [a] -> [[a]]
subsOfCnt _ [] = []
subsOfCnt 1 xs = fmap pure xs
subsOfCnt cnt (x:xs) = (fmap (x:) $ subsOfCnt (cnt-1) xs) ++ subsOfCnt cnt xs

runN cnt path = do
    content <- readFile path
    let (nums :: [Integer]) = fmap read . lines $ content
    case listToMaybe (getForN 2020 cnt nums) of
        Just xs -> putStrLn . concat $ ["product ", show xs, " = ", (show . product) xs]
        Nothing -> putStrLn "no matches found"