{-# LANGUAGE FlexibleContexts #-}
module Ex5 where

import Text.Parsec.String (Parser)
import Text.Parsec
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

encodedBinary :: Char -> Char -> Parser Integer
encodedBinary zeroChar oneChar = calc <$> many encoded where
    encoded = (char zeroChar $> 0) <|> (char oneChar $> 1)
    calc = sum . zipWith (\x y -> y * 2^x) [0..] . reverse

encodedLine = (,) <$> encodedBinary 'F' 'B' <*> encodedBinary 'L' 'R'

checksum x y = 8 * x + y

run path = do
    content <- readFile path    
    entries <- case parse (encodedLine `sepBy` newline) "" content of
               Right xs -> return xs
               Left e -> fail (show e)
    let checksums = fmap (uncurry checksum) entries
    print $ maximum checksums

allPossible = do
    x <- [0..127]
    y <- [0..7]
    return $ checksum x y

findMissing (x:y:xs) = if (y-x) == 2 then Right $ (x+y) `div` 2 else findMissing (y:xs)
findMissing _ = Left "no more remaining"

runAlt path = do
    content <- readFile path    
    sums <- case parse (encodedLine `sepBy` newline) "" content of
            Right xs -> return xs
            Left e -> fail (show e)
    let missing = (findMissing . sort . fmap (uncurry checksum)) sums
    print missing