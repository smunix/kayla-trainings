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

ls0 = [5, 2, 4, 10, 9, 19, 3, 15, 11, 14, 17]
t0 = Empty
t1 = Branch t0 2 t0
t2 = foldr add Empty ls0

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

lookupStep :: Int -> Int -> Tree -> Int
lookupStep i k Empty = k
lookupStep i k (Branch l j r)
  | i == j = k
  | i > j = lookupStep i (k + 1) r
  | i < j = lookupStep i (k + 1) l

count :: Tree -> Int
count Empty = 0
count (Branch l j r) = count l + count r + 1

flatten :: Tree -> [Int]
flatten Empty = []
flatten (Branch l j r) = flatten l <> [j] <> flatten r

mkTree :: [Int] -> Tree
mkTree ls = foldr add Empty ls

order :: [Int] -> [Int]
order = flatten . mkTree

data Exp where
  Number :: Int -> Exp
  Plus :: Exp -> Exp -> Exp
  Sub :: Exp -> Exp -> Exp
  Mul :: Exp -> Exp -> Exp
  deriving (Show)

e5 = Number 5
ePlus = Plus e5 (Number 3)
eMul = Mul (Number 3) ePlus

value :: Exp -> Int
value (Number i) = i
value (Plus i k) = value i + value k
value (Sub i k) = value i - value k
value (Mul i k) = value i * value k

change :: Int -> Int -> Exp -> Exp
change x y (Number e)
  | x == e = Number y
  | otherwise = Number e
change x y (Plus a b) = Plus (change x y a) (change x y b)
change x y (Sub a b) = Sub (change x y a) (change x y b)
change x y (Mul a b) = Mul (change x y a) (change x y b)

main :: IO ()
main = do
  pPrint "Hello, People!"

  pPrint t0
  pPrint t1
  pPrint t2

  pPrint (lookup 100 t2)
  pPrint (lookup 14 t2)
  pPrint (lookupStep 3 0 t2)
  pPrint (count Empty)
  pPrint (count t2)
  pPrint (count t1)

  pPrint (t2, ls0)
  pPrint (flatten t2)

  pPrint (order [5, 10, 7, 12, 31, 1, 20, 4])
  pPrint e5
  pPrint ePlus
  pPrint eMul
  pPrint (value eMul)
  pPrint (change 3 5 eMul)
  pPrint (value (change 3 5 eMul))
