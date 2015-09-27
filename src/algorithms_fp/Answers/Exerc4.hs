module Exerc4 where

--
--  exercise 1
--

-- If we assume n lists of length m
--
-- concat1(n,m) : O(m.n)
-- concat2(n,m) : O(m.n^2)
--

--
-- exercise 2
--
comp   f g l = [f x | x <- map g l, x>10]
comp'  f g l = [f (g x) | x <- l, g x > 10]
comp'' f g l = [f gx | x <-l, let gx=g x, gx>10]

--
-- exercise 3
--

split x []                 = ([],[])
split x (y:ys) | y <= x    = (y:ys1,ys2)
               | otherwise = (ys1,y:ys2)
               where (ys1,ys2) = split x ys

split' x l = splitTR x l ([],[])
    where splitTR x []     (ys1,ys2) = (reverse ys1,reverse ys2)
          splitTR x (y:ys) (ys1,ys2) 
              | y <= x               = splitTR x ys (y:ys1,ys2)
              | otherwise            = splitTR x ys (ys1,y:ys2)

--
-- exercise 5
--

data BinTree a = Empty
               | NodeBT a (BinTree a) (BinTree a)
    deriving Show

bt = (NodeBT 5 (NodeBT 8 (NodeBT 3 Empty Empty) (NodeBT 1 Empty Empty))
               (NodeBT 6 Empty (NodeBT 4 Empty Empty)))
bt'= (NodeBT 4 (NodeBT 6 (NodeBT 2 Empty Empty) (NodeBT 1 Empty Empty))
               (NodeBT 6 Empty (NodeBT 5 Empty Empty)))
bt''=(NodeBT 5 (NodeBT 8 Empty (NodeBT 1 Empty Empty))
               (NodeBT 6 Empty (NodeBT 4 Empty Empty)))

sameStruct Empty Empty        = True
sameStruct (NodeBT _ lf   rt)
           (NodeBT _ lf' rt') = sameStruct lf lf' && sameStruct rt rt'
sameStruct _ _                = False

--
--  exercise 6
--

inorder Empty          = []
inorder (NodeBT a l r) = inorder l ++ [a] ++ inorder r

size tr                = length (inorder tr)

size' Empty z          = z
size' (NodeBT a l r) z = 1 + size' l (size' r z)

{- examples of evaluations and results
? comp (+1) (*2) [1..10]
[13, 15, 17, 19, 21]
? comp' (+1) (*2) [1..10]
[13, 15, 17, 19, 21]
? comp'' (+1) (*2) [1..10]
[13, 15, 17, 19, 21]
? split 5 [1..10]
([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])
? split' 5 [1..10]
([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])
? sameStruct bt bt'
True
? sameStruct bt bt''
False

-}
