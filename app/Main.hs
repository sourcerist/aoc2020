module Main where

import System.Environment
import qualified Ex1
import qualified Ex2
import qualified Ex3
import qualified Ex4
import qualified Ex5
import qualified Ex6
import qualified Ex7

main :: IO ()
main = do
    args <- getArgs
    run args


run ["ex1", path] = Ex1.runN 2 path
run ["ex1b", path] = Ex1.runN 3 path
run ["ex2", path] = Ex2.run path
run ["ex2b", path] = Ex2.runAlt path
run ["ex3", path] = Ex3.run [(3,1)] path
run ["ex3b", path] = Ex3.run [(1,1), (3,1), (5,1), (7,1), (1,2)] path
run ["ex3c", path] = Ex3.run [(1,2)] path
run ["ex4", path] = Ex4.run path
run ["ex4b", path] = Ex4.runAlt path
run ["ex5", path] = Ex5.run path
run ["ex5b", path] = Ex5.runAlt path
run ["ex6", path] = Ex6.run path
run ["ex6b", path] = Ex6.runAlt path
run ["ex7", path] = Ex7.run path
run ["ex7b", path] = Ex7.runAlt path