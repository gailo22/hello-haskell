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
sum' [] = 0
sum' (x:xs) = x + sum' xs


qsort :: Ord a => [a] -> [a]                      
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]