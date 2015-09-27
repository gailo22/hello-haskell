module Exerc6 where

-- 
-- examples of solutions to exercises of chapter 6
--


---
--- Exercise 1
---
isort::(Ord a) => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

insert::(Ord a) => a -> [a] -> [a]
insert k []       = [k]
insert k l@(x:xs) | k <= x    = k:l
                  | otherwise = x:(insert k xs)

ans6_1 = isort [4,2,5,6,10,3,7]


---
--- Exercise 2
---

data Person = P (String,Int) deriving Show

instance Eq Person
    where P(_,age1) == P(_,age2) = age1 == age2
instance Ord Person
    where P(_,age1) <= P(_,age2) = age1 <= age2

-- which can be called as follows
-- isort [P ("Bob",50),P("Mary",18),P("Ted",48),P("Alice",23)]

--
-- Exercise 3
-- 
-- First version of Bubble Sort (a).
bsort::(Ord a) => [a] -> [a]
bsort []  = []
bsort [x] = [x]
bsort xs  = let (xs',last) = bubble(xs) in bsort xs'++[last]
    where
    -- bubble down and return the last element separately
    bubble [last]   = ([],last)
    bubble (x:y:xs)
        | x <= y    = let (xs',last) = bubble(y:xs) in (x:xs',last)
        | otherwise = let (xs',last) = bubble(x:xs) in (y:xs',last)
    
-- Second version that ends if nothing has been swapped (b).
bsort'::(Ord a) => [a] -> [a]
bsort' []  = []
bsort' [x] = [x]
bsort' xs  = let (xs',last,changed) = bubble(xs)
                 in if changed then bsort xs'++[last]
                    else xs'++[last]
    where
    -- bubble down, return the last element and indicate change
    bubble [last]   = ([],last,False)
    bubble (x:y:xs)
        | x <= y    = let (xs',last,changed) = bubble(y:xs)
                          in (x:xs',last,changed)
        | otherwise = let (xs',last,_) = bubble(x:xs)
                          in (y:xs',last,True)
--
-- Exercise 5
-- Author : Jean-Francois Gagne

-- First version  (corresponds to the first version given in 6.3.3)
-- not very efficient because of the use of append function and
-- going throught the list three times (two for list comprehension and one
-- for the length function).
qsortlimit::(Ord a) => [a] -> Int -> [a]
qsortlimit [] _  = []
qsortlimit list@(pivot:rest) limit
    | length list < limit = isort list
    | otherwise           = qsortlimit lower limit ++ [pivot] ++
                            qsortlimit upper limit
    where lower = [x | x <- rest, x <= pivot]
          upper = [x | x <- rest, x >  pivot]

-- Second version without append, but still going three times throught the list.
-- This version is not a big improvment of the first because the use of append
-- is the same thing as going through half the list and since having gotten
-- rid of the append is not bad, we still go through the list three times.
qsortlimit'::(Ord a) => [a] -> Int -> [a]
qsortlimit' xs limit = qs xs []
   where qs::(Ord a) => [a] -> [a] -> [a]
         qs [] s = s
         qs list@(pivot:rest) s
             | length list< limit = isorts list s
             -- As s is already sorted, the performance of iSorts will not
             -- be affected because all elements will be added in front of s.
             | otherwise           = qs lower (pivot : qs upper s)
             where lower = [x | x <- rest, x <= pivot]
                   upper = [x | x <- rest, x >  pivot]


-- Third version with only one pass in the list and no append
qsortlimit''::(Ord a) => [a] -> Int -> [a]
qsortlimit'' xs limit = qs xs []
   where qs::(Ord a) => [a] -> [a] -> [a]
         qs [] s = s
         qs list@(pivot:rest) s
             | length list < limit = isorts list s
             -- As s is already sorted, the performance of iSorts will not
             -- be affected because all elements will be added in front of s
             | otherwise           = qs lower (pivot : qs upper s)
             where (lower,upper) = split rest pivot [] [] 
                   split::(Ord a) => [a] -> a -> [a] -> [a] -> ([a], [a])
                   split []     _     lower1 upper1
                       = (lower1, upper1)
                   split (x:xs) pivot lower1 upper1
                       = if x<pivot
                         then split xs pivot (x:lower1) upper1
                         else split xs pivot lower1 (x:upper1)

-- Insersion sort in front of a sorted list
-- needed by the last two versions of qsortlimit.

isorts ::(Ord a) => [a] -> [a] -> [a]
isorts [] s     = s
isorts (x:xs) s = isorts xs (insert x s)


-- list of numbers to sort
-- example used by Knuth in Art of Computer Programming, Vol 3

klist = [503, 87, 512, 61,908,170,897,275,653,426,154,509,612,677,765,703]

--
-- End of file Exerc6.hs
--



