module Lib.Top where

import Text.Pretty.Simple (pPrint)
import Prelude hiding (lookup)

{-
Given a list of things that we can order (meaning that we need a Ord predicate),
write the elements in a binary Tree like structure such that:
1. all elements smaller than the root are put on the left
2. all elements greater than the root are put on the right
-}

data Tree where
  Empty :: Tree
  Branch :: Tree -> Int -> Tree -> Tree
  deriving (Show)

t0 = Empty
t1 = Branch t0 2 t0
t2 = foldr add Empty [5, 2, 4, 10, 9, 19, 3, 15, 11, 14, 17]

add :: Int -> Tree -> Tree
add i Empty = Branch Empty i Empty
add i (Branch l j r)
  | i < j = Branch (add i l) j r
  | i > j = Branch l j (add i r)
  | otherwise = Branch l j r

lookup :: Int -> Tree -> Bool
lookup i Empty = False
lookup i (Branch l j r)
  | i == j = True
  | i > j = lookup i r
  | i < j = lookup i l
main :: IO ()
main = do
  pPrint "Hello, People!"
  pPrint t0
  pPrint t1
  pPrint t2

  pPrint (lookup 100 t2)
  pPrint (lookup 14 t2)
