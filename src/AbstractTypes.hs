module AbstractTypes
  (Set, nil, set) where

import Data.List(nub, sort)
import Test.QuickCheck


-- Set
type Set a = [a]

nil :: Set a
nil = []

insert :: a -> Set a -> Set a
insert x xs = x:xs

set :: [a] -> Set a
set xs = xs

element :: Eq a => a -> Set a -> Bool
x `element` xs = x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
xs `equal` ys = xs `subset` ys && ys `subset` xs
  where
      xs `subset` ys = and [ x `elem` ys | x <- xs]

invariant :: Ord a => Set a -> Bool
invariant xs = and [ x < y | (x,y) <- zip xs (tail xs) ]

insertOrder :: Ord a => a -> Set a -> Set a
insertOrder x [] = [x]
insertOrder x (y:ys) | x < y = x : y : ys
                | x == y = y : ys
                | x > y = y : insert x ys

setOrder :: Ord a => [a] -> Set a
setOrder xs = nub (sort xs)


