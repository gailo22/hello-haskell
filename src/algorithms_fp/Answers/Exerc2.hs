module Exerc2 where

-- 
-- examples of solutions to exercises of chapter 2
--

import Array
--
-- Exercise 2
--
fact 0             = 1
fact 1             = 1
fact n | n < 0     = -1
       | otherwise = n * fact (n-1)
--
-- Exercise 4
--
f l = reverse (f' l [])
    where f' [] r     = r
          f' (x:xs) r = (2*x):(f' xs r)
--
-- Exercise 6
--
average :: [Float]->Float
average [] = error "average of []"
average xs = sum xs / fromIntegral(length xs)

average'::[Float]->Float
average' [] = error "average' of []"
average' xs = av xs 0 0
    where av [] s n     = s/n
          av (x:xs) s n = av xs (s+x) (n+1)

middle []                   = error "middle of []"
middle xs | odd (length xs) = xs !! (length xs `div` 2)
          | otherwise       = error "middle of even length list"

--
-- Exercise 7
--
ex7_a1 = [(x,y) | x<-[1..2], y<-[2..5],(x+y)/=4]
-- => [(1, 2), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)]

ex7_a2 = [x | x<-[1..10], (x `mod` 2) == 0]
-- => [2, 4, 6, 8, 10]

ex7_b1 = [ x | x<-[1..15] , x/=9]
ex7_b2 = [x*(-1)^x|x<-[2..11]]

--
-- Exercise 8
--
neg x = length [y|y<-x,y <0]
rep n = [i|i<-[1..n],k<-[1..i]]

--
-- Exercise 9
--

string2int s = s2i s 0  -- with explicit recursion using an accumulator
    where s2i [] v        = v
          s2i (d:ds) v
              | isDigit d = s2i ds (v*10+digitToInt d)
              | otherwise = error ("string2int: illegal digit=" ++ [d])

string2int' s = foldl f 0 s     -- with a left fold
    where f v d | isDigit d = v*10+digitToInt d
                | otherwise = error ("string2int: illegal digit=" ++ [d])
                         

--
-- Exercise 10
--
ex10_1 :: [Int]
ex10_1 = map fst [(1,2),(3,8),(0,6),(3,1)]

ex10_2 :: (Int,Int)
ex10_2 = (foldr f 0 l,foldl f 0 l)
    where l = [6,9,8,3,10]
          f x y = (x+y) `div` 2

ex10_3 :: [Int]
ex10_3 = foldr (++) [] [[1,2,3],[4,5,6],[],[7]]

--
-- Exercise 11
--
compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f (g x) -- the same as (.)

--
-- Exercise 12
--

ex12_1 = array (1,4) [(1,11),(2,20),(3,36),(4,47)]
ex12_2 = array (1,14) (zip [1..14] [ x | x<-[1..15] , x/=9])
ex12_3 = array (1,10) [(x-1,x*(-1)^x)|x<-[2..11]] -- see ex7_2b


--
-- Exercise 13
--

ex13_1 = array ((1,1),(3,3))
         [((1,1),2),((1,2),3),((1,3),4),
          ((2,1),5),((2,2),6),((2,3),7),
          ((3,1),8),((3,2),9),((3,3),10)]

ex13_2 = array ((1,1),(3,3))
         [(ij,v)|(ij,v)<-zip [(i,j)|i<-[1..3],j<-[1..3]]
                             [2..10]]

transpose3x3 a = array ((1,1),(3,3)) [((i,j),a!(j,i))|i<-[1..3],j<-[1..3]]
 
transposeNxN a = array ((ly,lx),(hy,hx)) 
                 [ ((j,i),a!(i,j)) | i <-[lx..hx],j<-[ly..hy] ]
              where
                 ((lx,ly),(hx,hy)) = bounds a
                 


--
-- Exercise 14
--

cube :: Num a => a -> a
cube x = x*x*x

maxi :: Ord a => a -> a -> a
maxi x y | x>= x     = x
         | otherwise = y

sumAtoB :: (Num a, Enum a) => a -> a -> a
sumAtoB a b = sum [a..b]


--
-- Exercise 15
--

-- (!) :: Ix a => Array a b -> a -> b
-- bounds :: Ix a => Array a b -> (a,a)
-- indices :: Ix a => Array a b -> [a]
-- elems :: Ix a => Array a b -> [b]
