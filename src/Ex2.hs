{-# LANGUAGE RecordWildCards #-}

module Ex2 where

import Text.Parsec.String (Parser)
import Text.Parsec
    ( char, digit, noneOf, oneOf, many1, many, parse )

number :: Parser Integer
number = read <$> many1 digit

data InputLine = InputLine
    { compareChar :: Char
    , minAppearances :: Integer
    , maxAppearances :: Integer
    , password :: String }
    deriving (Show)

spaceChars = " \r\n\t"

inputLine = do
    minAppearances <- number
    char '-'
    maxAppearances <- number
    whitespace
    compareChar <- noneOf spaceChars
    char ':'
    whitespace
    password <- many1 (noneOf spaceChars)
    return InputLine{..}

whitespace = many $ oneOf spaceChars

input :: Parser [InputLine]
input = many (inputLine <* whitespace)

isValid InputLine{..} = cnt >= minAppearances && cnt <= maxAppearances where 
    cnt = (fromIntegral . length . filter (==compareChar)) password

isValidAlt InputLine{..} = length xs == 1 where
    xs = filter (\x -> x == parm1 || x == parm2) (zip [1..] password)
    parm1 = (minAppearances, compareChar)
    parm2 = (maxAppearances, compareChar)

run = runInternal isValid

runAlt = runInternal isValidAlt

runInternal f path = do
    content <- readFile path
    inputLines <- case parse input "" content of
                    Right x -> return x
                    Left e -> fail (show e)
    let validCount = (length . filter f) inputLines           
    putStrLn $ "Valid count: " <> show validCount


