module Computation where

import           Data.Char
import           Prelude   hiding ((++), (.))
-- import qualified Prelude as P

sum1 :: [Int] -> Int
sum1 []     = 0
sum1 (x:xs) = x + sum1 xs

product1 :: [Int] -> Int
product1 []     = 1
product1 (x:xs) = x * product1 xs

isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 0

isEven :: Int -> Bool
isEven = not . isOdd

sumSqOdd :: [Int] -> Int
sumSqOdd xs = sum [ x*x | x <- xs, isOdd x ]

sumSqOddRec :: [Int] -> Int
sumSqOddRec []                 = 0
sumSqOddRec (x:xs) | isOdd x   = x*x + sumSqOddRec xs
                   | otherwise = sumSqOddRec xs

max :: Int -> Int -> Int
max x y | x >= y = x
        | y >= x = y

max3 :: Int -> Int -> Int -> Int
max3 x y z | x >= y && x >= z = x
           | y >= x && y >= z = y
           | z >= x && z >= y = z

max' :: Int -> Int -> Int
max' x y = if x >= y then x else y

max3' :: Int -> Int -> Int -> Int
max3' x y z = if x >= y && x >= z then x
              else if y >= x && y >= z then y
              else z

-- better
max'' :: Int -> Int -> Int
x `max''` y | x >= y    = x
            | otherwise = y

max3'' :: Int -> Int -> Int -> Int
max3'' x y z = x `max''` y `max''` z

-- append
(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- [1..3]  == enumFromTo 1 3
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' m n | m > n  = []
                | m <= n = m : enumFromTo' (m+1) n

-- [1..] == enumFrom [1..]
enumFrom' :: Int -> [Int]
enumFrom' m = m : enumFrom' (m+1)

-- factorial
factorial :: Int -> Int
factorial n = product [1..n]

factorialRec :: Int -> Int
factorialRec n = fact 1 n
  where
    fact :: Int -> Int -> Int
    fact m n | m > n  = 1
             | m <= n = m * fact (m+1) n

-- zip
zip' :: [a] -> [b] -> [(a,b)]
zip' [] ys         = []
zip' xs []         = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- need to be the same lenght
zipHarsh :: [a] -> [b] -> [(a,b)]
zipHarsh [] [] = []
zipHarsh (x:xs) (y:ys) = (x,y) : zipHarsh xs ys

-- search
search :: Eq a => [a] -> a -> [Int]
search xs y = [ i | (i, x) <- zip [0..] xs, x == y ]

serachRec :: Eq a => [a] -> a -> [Int]
serachRec xs y = srch xs y 0
  where
      srch :: Eq a => [a] -> a -> Int -> [Int]
      srch [] y i       = []
      srch (x:xs) y i
        | x == y        = i : srch xs y (i+1)
        | otherwise     = srch xs y (i+1)

-- select, take and drop
selectComp :: [a] -> Int -> a -- (!!)
selectComp xs i = the [ x | (j,x) <- zip [0..] xs, j == i ]
  where
      the [x] = x

takeComp :: Int -> [a] -> [a]
takeComp i xs = [ x | (j,x) <- zip [0..] xs, j < i ]

dropComp :: Int -> [a] -> [a]
dropComp i xs = [ x | (j,x) <- zip [0..] xs, j >= i ]

-- select, take and drop (recursion)
(!) :: [a] -> Int -> a
(x:xs) ! 0     = x
(x:xs) ! i     = xs ! (i-1)

take' :: Int -> [a] -> [a]
take' 0 xs         = []
take' i []         = []
take' i (x:xs)     = x : take' (i-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs         = xs
drop' i []         = []
drop' i (x:xs)     = drop' (i-1) xs

-- Squares
squares :: [Int] -> [Int]
squares xs = [ x*x | x <- xs ]

squaresRec :: [Int] -> [Int]
squaresRec []     = []
squaresRec (x:xs) = x*x : squaresRec xs

-- Ords
ords :: [Char] -> [Int]
ords []     = []
ords (x:xs) = ord x : ords xs

-- Map
map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs ]

mapRec' :: (a -> b) -> [a] -> [b]
mapRec' f []     = []
mapRec' f (x:xs) = f x : mapRec' f xs

squaresMap :: [Int] -> [Int]
squaresMap xs = map sqr xs
  where
      sqr x = x*x

-- Filter
positives :: [Int] -> [Int]
positives xs = [ x | x <- xs, x > 0 ]

positivesRec :: [Int] -> [Int]
positivesRec []                 = []
positivesRec (x:xs) | x > 0     = x: positivesRec xs
                    | otherwise = positivesRec xs

digits :: [Char] -> [Char]
digits xs = [ x | x <- xs, isDigit x ]

digitsRec :: [Char] -> [Char]
digitsRec []                 = []
digitsRec (x:xs) | isDigit x = x : digitsRec xs
                 | otherwise = digitsRec xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [ x | x <- xs, p x ]

filterRec' :: (a -> Bool) -> [a] -> [a]
filterRec' p []                 = []
filterRec' p (x:xs) | p x       = x : filterRec' p xs
                    | otherwise = filterRec' p xs

digitsFilter :: [Char] -> [Char]
digitsFilter xs = filter isDigit xs

-- Fold
sum''' :: [Int] -> Int
sum''' []     = 0
sum''' (x:xs) = x + sum''' xs

product''' :: [Int] -> Int
product''' []     = 1
product''' (x:xs) = x * product''' xs

concat''' :: [[a]] -> [a]
concat''' []       = []
concat''' (xs:xss) = xs ++ concat''' xss

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f a []     = a
foldr' f a (x:xs) = f x (foldr' f a xs)

sumFold :: [Int] -> Int
sumFold xs = foldr (+) 0 xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' f a []     = a
foldl' f a (x:xs) = foldl' f (f a x) xs


-- Currying
add :: Int -> (Int -> Int)
(add x) y = x + y

-- lambda
f :: [Int] -> Int
f xs = foldr (+) 0
         (map (\x -> x * x)
           (filter (\x -> x > 0) xs))

-- Composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)



