{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( Equation (Q, L), solve, parseEq,
      splitBy, sp, r, rm, rm1
    ) where

import Text.Regex.Posix

data Equation = Q Float Float Float | L Float Float deriving (Show, Read)

solve :: Equation -> [Float]
solve (Q a b c) = [x1, x2]
    where x1 = (- b + sqrt d) / (2 * a)
          x2 = (- b - sqrt d) / (2 * a)
          d = b ** 2 - 4 * a * c

solve (L a b) = [x]
    where x = - b / a

splitBy :: Char -> [Char] -> [[Char]]
splitBy c [] = []
splitBy c (x:xs) = sp c [] (x:xs)

sp :: Char -> [Char] -> [Char] -> [[Char]]
sp c ks [] = ks : []
sp c ks (x:xs) | x == c    = ks : sp c [] xs
               | otherwise = sp c (ks ++ [x]) xs

r :: [String] -> Int -> Float
r list k = (read (list !! k) :: Float)

rm :: [Char] -> [Char] -> [Char]
rm _ [] = []
rm [] xs = xs
rm (a:as) (x:xs) | a == x    = rm1 as xs (x:xs)
                 | otherwise = x : rm (a:as) xs

rm1  :: [Char] -> [Char] -> [Char] -> [Char]
rm1 [] [] os = os
rm1 _ [] os = os
rm1 [] xs os = xs
rm1 (a:as) (x:xs) os | a == x    = rm1 as xs os
                     | otherwise = os


parseEq a@(x:xs) | a =~ "^[0-9]*x ?[+-] ?[0-9]+ ?= ?0$" :: Bool  = L (r list 0) (r list 1)
                 | a =~ "^[0-9]*x.+2 ?[+-] ?[0-9]*x ?[+-] ?[0-9]+ ?= ?0$" :: Bool  = Q (r list 0) (r list 2) (r list 3)
                 | a =~ "^[0-9]*x.+2 ?[+-] ?[0-9]+ ?= ?0$" :: Bool  = Q (r list 0) 0 (r list 2)
                 | otherwise       = L 1 0
                 where list = getAllTextMatches (a1 =~ "-? ?[0-9]+" :: AllTextMatches [] String)
                       a1 | a =~ "^x" :: Bool = "1" ++ a
                          | otherwise         = a


{-
parseEq [] = L 1 0
parseEq a@(x:xs) | length (list) == 2  = L (r list 0) (r list 1)
                 | length (list) == 3  = Q (r list 0) (r list 1) (r list 2)
                 | otherwise           = L 1 0
    where list = splitBy '+' pretty
          pretty = (filter (/= ' ') . filter (/= '=') . filter (/= '0') . filter (/= 'x')) a1
          -- a1 = a
          a1 = rm "^2" a
-}