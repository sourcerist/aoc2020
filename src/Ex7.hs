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
    , containBags :: Set (Integer, String) }
    deriving (Eq, Ord, Show)



number :: Parser Integer
number = read <$> many1 digit

-- >>> parse bagRequirementEntry "" "dotted black bags contain no other bags."
-- Right (BagRequirementEntry {bagColor = "dotted black", containBags = fromList []})

bagRequirementEntry :: Parser BagRequirementEntry
bagRequirementEntry = do
    bagColor <- anyChar `manyTill` try (string " bags contain ")
    containBags <- (try (string "no other bags") $> Set.empty) 
               <|> (Set.fromList <$> containsClause `sepBy1` char ',')
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
    insertOrAdd m BagRequirementEntry{..} = Set.foldl' (insertOrAdd' bagColor) m (Set.map snd containBags) --Map.insertWith Set.union bagColor (Set.map snd containBags) m
    insertOrAdd' c m x = Map.insertWith Set.union x (Set.singleton c) m
    

buildLookup :: [BagRequirementEntry] -> Map String (Set String)
buildLookup = foldl' insertOrAdd Map.empty where
    insertOrAdd m BagRequirementEntry{..} = Map.insertWith Set.union bagColor (Set.map snd containBags) m

findAll :: String -> Map String (Set String) -> Set String
findAll k m = 
    case Map.lookup k m of 
        Just s -> Set.union s (Set.unions . Set.map (`findAll` m) $ s)
        Nothing -> Set.empty 

run path = do
    content <- readFile path
    entries <- case parse bagRequirementEntries "" content of
                 Right xs -> return xs
                 Left e -> fail (show e)
    let m = buildReverseLookup entries
    print . length . findAll "shiny gold" $ m

