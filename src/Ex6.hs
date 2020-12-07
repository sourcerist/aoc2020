module Ex6 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split
import Data.Char (isAlpha)

answerGroups = Set.fromList .  splitOn "" 

run path = do
    contents <- readFile path
    let s = fmap (Set.size . Set.fromList . concat) . splitOn [""] . fmap (filter isAlpha) . lines $ contents
    print $ sum s

runAlt path = do
    contents <- readFile path
    let s = fmap (Set.size . foldl1 Set.intersection . fmap Set.fromList) . splitOn [""] . fmap (filter isAlpha) . lines $ contents
    print $ sum s