{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Ex10 where

import Control.Monad.RWS
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')
import qualified Data.Set as Set

jolts [] = do
    tell [3]
    modify (+3)
    return ()
jolts (x:xs) = do
    curr <- get
    tell [x - curr]
    put x
    jolts xs

run path = do
    contents <- readFile path
    let nums = sort . fmap read . lines $ contents
    (_,log) <- execRWST (jolts nums) () 0
    let ones = length . filter (==1) $ log
    let threes = length . filter (==3) $ log
    print (ones, threes, ones * threes)

subLists :: [a] -> [[a]]
subLists [] = []
subLists (x:xs) = (x:xs) : subLists xs
    
possiblePaths :: [Integer] -> Integer
possiblePaths [_] = 1
possiblePaths (x:xs) = sum . fmap possiblePaths . takeWhile f . subLists $ xs where
    f [] = False
    f (y:_) = y - x <= 3

buildMap :: [Integer] -> Map Integer Integer
buildMap xs = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty lst where
    s = Set.fromList xs
    lst = do
        x <- xs
        y <- [1..3]
        guard $ (x + y) `Set.member` s
        return (x + y)

runAlt path = do
    contents <- readFile path
    let nums :: [Integer] = sort . fmap read . lines $ contents
    --print $ possiblePaths (0 : nums <> [last nums + 3])
    let m = buildMap (0 : nums <> [last nums + 3])
    print $ Map.toList m 
    --print $  product . fmap snd . Map.toList $ m
    return ()
