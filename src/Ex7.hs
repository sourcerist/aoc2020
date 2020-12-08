{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Ex7 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec.String (Parser)
import Text.Parsec
import Data.Functor (($>))
import Data.Foldable
import Data.Maybe (maybeToList)
import Debug.Trace (trace)

data BagRequirementEntry = BagRequirementEntry
    { bagColor :: String
    , containBags :: [(Integer, String)] }
    deriving (Eq, Ord, Show)

number :: Parser Integer
number = read <$> many1 digit

-- >>> parse bagRequirementEntry "" "dotted black bags contain no other bags."
-- Right (BagRequirementEntry {bagColor = "dotted black", containBags = fromList []})

bagRequirementEntry :: Parser BagRequirementEntry
bagRequirementEntry = do
    bagColor <- anyChar `manyTill` try (string " bags contain ")
    containBags <- (try (string "no other bags") $> []) 
               <|> (containsClause `sepBy1` char ',')
    char '.'
    newline
    return BagRequirementEntry {..} where
        containsClause = do
            optional space
            cnt <- number
            space
            bag <- anyChar `manyTill` try (string " bag")
            optional (char 's')
            return (cnt, bag)

bagRequirementEntries = do
    entries <- many bagRequirementEntry
    eof
    return entries

buildReverseLookup :: [BagRequirementEntry] -> Map String (Set String)
buildReverseLookup = foldl' insertOrAdd Map.empty where
    insertOrAdd m BagRequirementEntry{..} = foldl' (insertOrAdd' bagColor) m (fmap snd containBags) 
    insertOrAdd' c m x = Map.insertWith Set.union x (Set.singleton c) m

findAll :: String -> Map String (Set String) -> Set String
findAll k m = 
    case Map.lookup k m of 
        Just s -> Set.union s (Set.unions . Set.map (`findAll` m) $ s)
        Nothing -> error $ "Invalid value: " <> k 
    
buildLookup :: [BagRequirementEntry] -> Map String BagRequirementEntry
buildLookup = Map.fromList . fmap (\x -> (bagColor x, x))

run path = do
    content <- readFile path
    entries <- case parse bagRequirementEntries "" content of
                 Right xs -> return xs
                 Left e -> fail (show e)
    let m = buildReverseLookup entries
    print . length . findAll "shiny gold" $ m

countAll :: String -> Map String BagRequirementEntry -> Integer
countAll k m = 
    case Map.lookup k m of
        Just BagRequirementEntry{..} -> trace (show x) x where x = 1 + (sum . fmap (\(cnt, k') -> cnt * countAll k' m)) containBags
        Nothing -> error $ "Invalid value: " <> k

runAlt path = do
    content <- readFile path
    entries <- case parse bagRequirementEntries "" content of
                 Right xs -> return xs
                 Left e -> fail (show e)
    --mapM_ print entries
    let m = Map.fromList . fmap (\x -> (bagColor x, x)) $ entries
    print $ countAll "shiny gold" m -1

