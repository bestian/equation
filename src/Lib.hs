{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( Equation (Q, L), solve, parseEq,
      splitBy, sp, r
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


parseEq a@(x:xs) | a =~ "^[0-9]*x ?[+-] ?[0-9]+ ?= ?[0-9]+$" :: Bool  = L (r list 0) ((r list 1) - (r list 2))
                 | a =~ "^[0-9]*x.+2 ?[+-] ?[0-9]+x ?[+-] ?[0-9]+ ?= ?[0-9]+$" :: Bool  = Q (r list 0) (r list 2) ((r list 3) - (r list 4))
                 | a =~ "^[0-9]*x.+2 ?[+] ?x ?[+-] ?[0-9]+ ?= ?[0-9]+$" :: Bool  = Q (r list 0) 1 ((r list 2) - (r list 3))
                 | a =~ "^[0-9]*x.+2 ?[-] ?x ?[+-] ?[0-9]+ ?= ?[0-9]+$" :: Bool  = Q (r list 0) (-1) ((r list 2) - (r list 3))
                 | a =~ "^[0-9]*x.+2 ?[+-] ?[0-9]+ ?= ?[0-9]+$" :: Bool  = Q (r list 0) 0 ((r list 2) - (r list 3))
                 | a =~ "^[0-9]*x.+2 ?= ?[0-9]+$" :: Bool  = Q (r list 0) 0 (-(r list 2))
                 | a =~ "^[0-9]*x.+2 ?[+] ?x ?= ?[0-9]+$" :: Bool  = Q (r list 0) 1 (-(r list 2))
                 | a =~ "^[0-9]*x.+2 ?[-] ?x ?= ?[0-9]+$" :: Bool  = Q (r list 0) (-1) (-(r list 2))
                 | otherwise       = L 1 0
                 where list = getAllTextMatches (a1 =~ "-? ?[0-9]+" :: AllTextMatches [] String)
                       a1 | a =~ "^x" :: Bool = "1" ++ a
                          | otherwise         = a
