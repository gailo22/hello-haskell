module Algorithms where

import Prelude hiding ((+), (-), (*))

sort :: (Ord a) => [a] -> [a]
sort []    = []
sort(x:xs) = sort before ++ [x] ++ sort after
  where
      before = filter (<= x) xs
      after  = filter (> x)  xs

data Expr1 = Lit Integer
           | Add Expr1 Expr1
           | Sub Expr1 Expr1
           | Mul Expr1 Expr1
           | Var String Expr1
  deriving Eq

class Num1 a where
    fromInteger :: Integer -> a
    (+)         :: a -> a -> a
    (-)         :: a -> a -> a
    (*)         :: a -> a -> a

{- 

instance Num1 Expr1 where 
  fromInteger n = Lit n 
  e1 + e2 = Add e1 e2 
  e1 - e2 = Sub e1 e2 
  e1 * e2 = Mul e1 e2

run :: Expr1 -> Integer 
run (Lit n)   = n 
run (Add a b) = run a + run b 
run (Sub a b) = run a - run b 
run (Mul a b) = run a * run b


f :: Expr1 -> Expr1 
f x = x + 1

-}