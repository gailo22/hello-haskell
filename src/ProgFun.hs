module ProgFun where

-- Exercise 1: Pascal’s Triangle
next xs = zipWith (+) ([0] ++ xs) (xs ++ [0])
pascal = iterate next [1]

-- Exercise 2: Parentheses Balancing
balance = 1

-- Exercise 3: Counting Change
countChange = 1



data List a = Nil | Cons a (List a) 
  deriving (Show, Eq)

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _   = False

head :: List a -> a
head Nil        = error "Nil.head"
head (Cons h t) = h

tail :: List a -> List a
tail Nil        = error "Nil.tail"
tail (Cons h t) = t 

-- instance Show (List a) where
--  show a = "List a"

foreach :: (a -> IO ()) -> List a -> IO ()
foreach f Nil        = print "Nil"
foreach f (Cons h t) = do
                          f h
                          foreach f t


list = Cons 1 (Cons 2 Nil)

h = ProgFun.head list
t = ProgFun.tail list

main = do 
  print (isEmpty Nil)
  print (isEmpty list)
  print h
  print t
  foreach print list
