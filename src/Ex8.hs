{-# LANGUAGE RecordWildCards #-}
module Ex8 where

import Data.Vector
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec.String (Parser)
import Text.Parsec
    (many, eof, optional, newline, parse, sepBy1,  char, digit, spaces, string, choice, many1, (<|>), try )
import Data.Functor (($>))
import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Except
import Prelude hiding (init, length)

data Instruction
    = Jump Int
    | Accumulate Int
    | Nop Int
    deriving (Eq, Ord, Show)

data ProgramOutput 
    = Invalid 
    | InfiniteLoop Int
    | Terminated Int
    deriving (Eq, Ord, Show)
    
number :: Parser Int
number = read <$> many1 digit

instruction :: Parser Instruction
instruction = choice [try jump, try noop, accumulate] <* newline where
    instrFor str = do
        string str
        spaces
        sign <- (char '+' $> 1) <|> (char '-' $> -1)
        (*sign) <$> number
    jump = Jump <$> instrFor "jmp" 
    noop = Jump 1 <$ instrFor "nop"
    accumulate = Accumulate <$> instrFor "acc"

data WalkState = WalkState
    { visited :: Set Int
    , currentPos :: Int
    , acc :: Int }
    deriving (Eq, Ord, Show)

type AppState = State WalkState

execInstr :: WalkState -> Instruction -> WalkState
execInstr WalkState{..} (Jump n) = WalkState (Set.insert currentPos visited) c acc where c = currentPos + fromIntegral n
execInstr WalkState{..} (Accumulate n) = WalkState (Set.insert currentPos visited) c (acc + fromIntegral n) where c = currentPos + 1


walkUntilRepeat :: Vector Instruction -> AppState ProgramOutput
walkUntilRepeat instructions = do
    s@WalkState{..} <- get
    if currentPos < 0 then 
        return Invalid
    else if currentPos > length instructions then 
        return $ Terminated acc
    else if currentPos `Set.member` visited then 
        return $ InfiniteLoop acc
    else do
        put $ execInstr s (instructions ! currentPos) 
        walkUntilRepeat instructions

parseInstructions str = 
    case parse (many instruction <* eof) "" str of
        Right xs -> return xs
        Left e -> fail (show e)

run path = do
    contents <- readFile path
    instructions <- fromList <$> parseInstructions contents 
    let (r,_) = runState (walkUntilRepeat instructions) (WalkState Set.empty 0 0)
    print r

    
    
    --if currentPos < minIndex instructions then fail "Position cannot be less than " <> show minIndex instructions
    --else if currentPos > maxIndex instructions then fail "Position cannot be greater than " <> show maxIndex instructions
    --else 


