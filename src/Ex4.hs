module Ex4 where

import Text.Parsec.String (Parser)
import Text.Parsec
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (guard)

spaceChars = " \r\n\t"

kvPair :: Parser (String, String)
kvPair = do
    k <- many1 (noneOf (':' : spaceChars))
    char ':'
    v <- many1 (noneOf spaceChars)
    choice [newline, char ' ']
    return (k, v)

passportInfo :: Parser (Map String String)
passportInfo = Map.fromList <$> many kvPair

passports :: Parser [Map String String]
passports = passportInfo `sepBy` newline <* eof

requiredFields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid m = requiredFields `Set.intersection` (Set.fromList . Map.keys) m == requiredFields 

isValidAlt m = (length . filter (==Right ()) . fmap test . Map.toList) m == 7 where
    test (k, v) = parse (requiredField k) "" v

run = runInternal isValid

runAlt = runInternal isValidAlt

runInternal f path = do
    content <- readFile path
    xs <- case parse passports "" content of
            Right xs -> return xs
            Left e -> fail (show e)
    let validPassports = filter f xs           
    putStrLn $ "Valid count: " <> (show . length) validPassports

number :: Parser Integer
number = read <$> many1 digit

numberBetween low high = do
    x <- number
    guard $ x >= low && x <= high
    return ()

requiredField "byr" = numberBetween 1920 2002 
requiredField "iyr" = numberBetween 2010 2020
requiredField "eyr" = numberBetween 2020 2030
requiredField "hgt" = choice . fmap try $ [numberBetween 150 193 <* string "cm", numberBetween 59 76 <* string "in"]
requiredField "hcl" = do
    char '#' 
    count 6 . oneOf $ (['0'..'9'] <> ['a'..'f'])
    eof
    return ()
requiredField "ecl" = do
    choice . fmap (try . string) $ ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    return ()
requiredField "pid" = do
    count 9 digit 
    eof
    return ()
requiredField _ = parserFail "invalid field name"
