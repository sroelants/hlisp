module Main where

import Environment (prelude)
import Evaluator
import Parser

interpret = eval prelude . parse

main :: IO ()
main = putStrLn "lol"
