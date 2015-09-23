module IntroToHaskell where

import Data.Array

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show


shoe :: Thing
shoe = Shoe


listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]


isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

-- main = print (isSmall Cabbage)


isSmall2 :: Thing -> Bool
isSmall2 Ship       = False
isSmall2 King       = False
isSmall2 _          = True

-- main = print (isSmall2 Cabbage)



data FailableDouble = Failure
                    | OK Double
  deriving Show

a = Failure
b = OK 3.4

-- OK :: Double -> FailableDouble

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- main = print (safeDiv 2 0, safeDiv 3 4)


failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- main = print (failureToZero Failure, failureToZero (OK 3.4))

-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 30 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- main = print (getAge brent)
type Type11 = String
type Type12 = Integer
type Type21 = Double
type Type31 = Person
type Type32 = Thing
type Type33 = [Char]

data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4

-- Recursive Data Types
data IntList = Empty | Cons Int IntList
  deriving Show

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x xs) = x * intListProd xs

-- main = print (intListProd (Cons 3 (Cons 2 (Cons 4 Empty))))

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

-- main = print tree

-- Map
addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)

myIntList = Cons 2 (Cons (-3) (Cons 5 Empty))

-- main = print (addOneToAll myIntList)

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

myIntList1 = Cons 2 (Cons (-3) (Cons 5 Empty))

-- main = print (absAll myIntList1)


squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

myIntList2 = Cons 2 (Cons (-3) (Cons 5 Empty))

-- main = print (squareAll myIntList2)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)


addOne x = x + 1
square x = x * x

addOneToAllF xs = mapIntList addOne xs

absAllF xs = mapIntList abs xs

squareAllF xs   = mapIntList square xs

myIntList3 = Cons 2 (Cons (-3) (Cons 5 Empty))

-- main = print (absAllF myIntList3)


-- Filter
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs) 
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs
  
myIntList4 = Cons 2 (Cons (-3) (Cons 5 Empty))

-- main = print (keepOnlyEven myIntList4)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
  | p x       = Cons x (filterIntList p xs)
  | otherwise = filterIntList p xs

myIntList5 = Cons 2 (Cons (-3) (Cons 5 Empty))

-- main = print (filterIntList even myIntList5)

-- Fold
-- Polymorphic data types
data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs
  
myList = C 2 (C (-3) (C 5 E))

-- main = print (filterList even myList)

mapList :: (t -> t) -> List t -> List t 
mapList f (C x xs) = C (f x) (mapList f xs)
mapList f E        = E

myList1 = C 2 (C (-3) (C 5 E))

double x = 2 * x


instance Show (List a) where
  show a = "List a"
  
-- main = print (mapList double myList1)


doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + head (tail xs)

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2


data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNEL :: [a] -> Maybe (NonEmptyList a)
listToNEL []     = Nothing
listToNEL (x:xs) = Just (NEL x xs)

headNEL :: NonEmptyList a -> a
headNEL (NEL x _) = x

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ xs) = xs


-- Function composition
comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)


-- Folds
sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

fold :: (a -> b -> b) -> b -> [a] -> b
fold f z []     = z
fold f z (x:xs) = f x (fold f z xs)

sum''     = fold (+) 0
product'' = fold (*) 1
length''  = fold (\_ s -> 1 + s) 0

myList3 = [1,2,3,4,5]

-- main = print (sum' myList3, product' myList3, length' myList3)

-- Prelude 'foldr'
{- 
length :: [a] -> Int
sum :: Num a => [a] -> a
product :: Num a => [a] -> a
and :: [Bool] -> Bool
or :: [Bool] -> Bool
any :: (a -> Bool) -> [a] -> Bool
all :: (a -> Bool) -> [a] -> Bool
-}

-- Type Classes
data Foo = F Int | G Char

{- 
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- default implementation
  x /= y = not (x == y)
-}

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2

  (G c1) == (G c2) = c1 == c2

  _ == _ = False
  foo1 /= foo2 = not (foo1 == foo2)
  
-- automatically generate instances
data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

class Listable a where
  toList :: a -> [Int]
  
instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

-- main = print (toList True, toList (7::Int))


-- Lazy evaluation
repeat' :: a -> [a]
repeat' x = x : repeat' x

take' :: Int -> [a] -> [a]
take' n _      | n <= 0 =  []
take' _ []              =  []
take' n (x:xs)          =  x : take' (n-1) xs


knapsack :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack [3,4,5,8,10] [2,3,4,5,9] 20

main = print example