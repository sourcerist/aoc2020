module Main where

import System.Environment
import qualified Ex1
import qualified Ex2

main :: IO ()
main = do
    args <- getArgs
    run args


run ["ex1", path] = Ex1.runN 2 path
run ["ex1b", path] = Ex1.runN 3 path
run ["ex2", path] = Ex2.run path
run ["ex2b", path] = Ex2.runAlt path