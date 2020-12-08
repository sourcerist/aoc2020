module Ex6 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split ( splitOn )
import Data.Char (isAlpha)
import Data.List (foldl1')

answerGroups = Set.fromList .  splitOn "" 

run path = do
    contents <- readFile path
    let s = fmap (Set.size . Set.fromList . concat) . splitOn [""] . lines $ contents
    print $ sum s

runAlt path = print =<< sum . fmap (Set.size . foldl1' Set.intersection . fmap Set.fromList) . splitOn [""] . lines <$> readFile path
