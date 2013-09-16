doubleMe :: Int -> Int
doubleMe x = x + x

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]

average :: [Int] -> Int
average ns = sum ns `div` length ns


sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs


qsort :: Ord a => [a] -> [a]                      
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]


data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)  
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 


data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)

add'     :: Int -> (Int -> Int)
add' x y = x + y

max3       :: Int -> Int -> Int -> Int
max3 x y z = max x (max y z)

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

-- Proof
prop_append_assoc :: [Int] -> [Int] -> [Int] -> Bool
prop_append_assoc xs ys zs = 
	(xs +++ ys) +++ zs == xs +++ (ys +++ zs)

prop_append_ident :: [Int] -> Bool
prop_append_ident xs = 
	xs +++ [] == xs && xs == [] +++ xs

prop_append_cons :: Int -> [Int] -> Bool
prop_append_cons x xs =
	[x] +++ xs == x : xs

factorialRec :: Int -> Int
factorialRec n = fact 1 n
    where
    fact :: Int -> Int -> Int
    fact m n | m > n = 1
             | m <= n = m * fact (m+1) n

zip' :: [a] -> [b] -> [(a, b)]
zip' [] ys         = []
zip' xs []         = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys


search :: Eq a => [a] -> a -> [Int]
search xs y = [ i | (i,x) <- zip [0..] xs, x == y ]

searchRec :: Eq a => [a] -> a -> [Int]
searchRec  xs y = srch xs y 0
  where
  srch [] y i                 = []
  srch (x:xs) y i | x == y    = i : srch xs y (i+1)
                  | otherwise = srch xs y (i+1)

(!!!) :: [a] -> Int -> a
xs !!! i = the [ x | (j,x) <- zip [0..] xs, j == i ]
  where
  the [x] = x

(!!!!) :: [a] -> Int -> a
(x:_) !!!! 0     = x
(_:xs) !!!! i    = xs !!!! (i-1)

take' :: Int -> [a] -> [a]
take' 0 _              = []
take' _ []             = []
take' i (x:xs) | i > 0 = x : take (i-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs             = xs
drop' _ []             = []
drop' i (_:xs) | i > 0 = drop (i-1) xs


map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f []     = []
map'' f (x:xs) = f x : map'' f xs

positives :: [Int] -> [Int]
positives xs = [ x | x <- xs, x > 0 ]

positivesRec :: [Int] -> [Int]
positivesRec []                 = []
positivesRec (x:xs) | x > 0     = x : positivesRec xs
                    | otherwise = positivesRec xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [ x | x <- xs, p x]


filterRec' :: (a -> Bool) -> [a] -> [a]
filterRec' p []                 = []
filterRec' p (x:xs) | p x       = x : filterRec' p xs
                    | otherwise = filterRec' p xs

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f a []     = a
foldr' f a (x:xs) = f x (foldr' f a xs)

sum'' :: [Int] -> Int
sum'' xs = foldr' add 0 xs
  where
  add x y = x + y

sum''' :: [Int] -> Int
sum''' = foldr' add 0
  where
  add x y = x + y




