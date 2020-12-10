{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ex9 where

import Data.Sequence (Seq)
import Data.Sequence
import Control.Monad.State
import Prelude hiding (drop, filter, lookup, splitAt)
import qualified Prelude as P
import Data.Maybe (listToMaybe)
import Data.Foldable (toList)

type BufferStateT = StateT Buffer

newtype Buffer = Buffer 
    { buffer :: Seq Integer }
    deriving (Eq, Ord, Show)

newtype CipherText = CipherText 
    { cipherText :: [Integer] }
    deriving (Eq, Ord, Show)

verifyOrNone :: MonadState Buffer m => Integer -> m (Maybe Integer)
verifyOrNone i = do
    Buffer{..} <- get
    put $ Buffer (drop 1 buffer >< singleton i)
    return . listToMaybe $ do
        x <- toList buffer
        y <- toList buffer
        guard $ x /= y
        guard $ x + y == i
        return i

firstFault :: MonadState Buffer m => [Integer] -> m (Maybe Integer)
firstFault [] = return Nothing
firstFault (x:xs) = do
    y <- verifyOrNone x
    case y of
        Just _ -> firstFault xs
        Nothing -> return $ Just x

firstFaultForSize bufSize nums = do
    let b = Buffer . fromList . P.take bufSize $ nums
    let xs = P.drop bufSize nums
    (firstBad, _) <- runStateT (firstFault xs) b
    case firstBad of
        Nothing -> fail "no invalid value found"
        Just n -> return n

run :: Int -> FilePath -> IO ()
run bufSize path = do
    contents <- readFile path
    let nums = (fmap read . lines) contents
    print <$> firstFaultForSize bufSize nums
    return ()

findContiguousFor :: MonadState Buffer m => Integer -> [Integer] -> m (Maybe Integer)
findContiguousFor n lst = do
    Buffer{..} <- get
    case compare (sum buffer) n of
        EQ -> return . Just $ minimum buffer + maximum buffer
        GT -> do
            put $ Buffer (drop 1 buffer)
            findContiguousFor n lst
        LT -> case lst of
                [] -> return Nothing
                (x:xs) -> do
                    put $ Buffer (buffer >< singleton x)
                    findContiguousFor n xs

runAlt bufSize path = do
    contents <- readFile path
    let nums = (fmap read . lines) contents
    target <- firstFaultForSize bufSize nums
    (r, _) <- runStateT (findContiguousFor target nums) (Buffer empty)
    print r
    return ()

