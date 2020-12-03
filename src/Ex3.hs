module Ex3 where

import Debug.Trace
import Data.Char

isTree over down i xs = 
    case i*over `mod` down of
        0 -> xs !! ((i*over `div` down) `mod` length xs) == '#'
        _ -> False 

treeCount over down = length . filter (==True) . zipWith (isTree over down) [0..] . fmap (filter (not . isSpace))

run rds path = do
    content <- readFile path 
    let totals = fmap (uncurry treeCount) rds <*> [lines content]
    putStrLn $ "Tree counts: " <> show totals
    putStrLn $ "Product: " <> (show . product) totals
