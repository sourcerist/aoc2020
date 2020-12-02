{-# LANGUAGE RecordWildCards #-}

module Ex2 where

import Text.Parsec.String (Parser)
import Text.Parsec
import Control.Monad (void)

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

-- >>> parse input "" $ "1-7 j: jkjlhjh \r\n 17-19 z: zzzvzzzzzzzzzzzzzzzg"
-- Right [InputLine {compareChar = 'j', minAppearances = 1, maxAppearances = 7, password = "jkjlhjh"},InputLine {compareChar = 'z', minAppearances = 17, maxAppearances = 19, password = "zzzvzzzzzzzzzzzzzzzg"}]

-- >>> isValid $ InputLine {compareChar = 'j', minAppearances = 1, maxAppearances = 7, password = "jkjlhjh"}
-- True

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


