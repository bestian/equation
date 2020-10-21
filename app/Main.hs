module Main where

import Lib

main :: IO ()
main = do print $ "Enter an Equation"
          x <- getLine
          let eq = parseEq x
          print eq
          print $ solve eq
