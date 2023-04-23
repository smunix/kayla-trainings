module Lib.Top where

import Text.Pretty.Simple (pPrint)

{-
Given a list of things that we can order (meaning that we need a Ord predicate),
write the elements in a binary Tree like structure such that:
1. all elements smaller than the root are put on the left
2. all elements greater than the root are put on the right
-}

data Tree where
  MkNode :: Tree -> Int -> Tree -> Tree
  MkEmpty :: Tree
  deriving (Show)

t0 :: Tree
t0 = MkNode (MkNode MkEmpty 5 MkEmpty) 6 (MkNode MkEmpty 8 MkEmpty)

t10 = insert 10 t0

t6 = insert 6 t10

insert :: Int -> Tree -> Tree
insert x MkEmpty = MkNode MkEmpty x MkEmpty
insert x (MkNode left y right)
  | x < y = MkNode (insert x left) y right
  | x > y = MkNode left y (insert x right)
  | x == y = MkNode left y right

main :: IO ()
main = do
  pPrint "Hello, People!"
  pPrint MkEmpty
  pPrint [MkEmpty, MkEmpty, MkEmpty]
  pPrint (MkNode MkEmpty 6 MkEmpty)
  pPrint t0
  pPrint t10
  pPrint t6

  pPrint (foldr insert t6 [5, 7, 3, 55, 6, 4, 2, 0, 19, 7, 8, 19])
