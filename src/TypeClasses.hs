module TypeClasses where

{- 
--import Data.List
import Data.Char
import Prelude hiding (Eq, Ord, Show, Bool, (==), True, False)

data Set a = MkSet [a]

nil :: Set a
nil = MkSet []

insert :: a -> Set a -> Set a
insert x (MkSet xs) = MkSet (x:xs)

set :: [a] -> Set a
set xs = MkSet xs


elem :: Eq a => a -> [a] -> Bool

-- comprehension
elem x ys = or [ x == y | y <- ys ]

-- recursion
elemRec x [] = False
elemRec x (y:ys) = x == y || elemRec x ys

-- higher-order
elemHigh x ys = foldr (||) False (map (x ==) ys)

{-
class Eq a where
    (==) :: a -> a -> Bool

instance Eq Int where
    (==) = eqInt
-}

instance Eq Char where
    x == y = ord x == ord y

instance (Eq a, Eq b) => Eq (a,b) where
    (u,v) == (x,y) = (u == y) && (v == y)

instance Eq a => Eq [a] where
    [] == [] = True
    [] == y:ys = False
    x:xs == [] = False
    x:xs == y:ys = (x == y) && (xs == ys)
    

data EqDict a = EqD (a -> a -> Bool)

eq :: EqDict a -> a -> a -> Bool
eq (EqDict f) = f

elem :: EqD a -> a -> [a] -> Bool

-- comprehension
elem d x ys = or [ eq d x y | y <- ys ]

-- recursion
elem d x [] = False
elem d x (y:ys) = eq d x y || elem x ys

-- higher-order
elem d x ys = foldr (||) False (map (eq d x) ys)


-- from standard haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    -- minimum definition: (==)
    x /= y = not (x == y)

class (Eq a) => Ord a where
    (<)  :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>)  :: a -> a -> Bool
    (>=) :: a -> a -> Bool

    -- minimum definition: (<=)
    x < y = x <= y && x /= y
    x > y = y < x
    x >= y = y <= x

class Show a where
    show :: a -> String

-- instance of Boolean
instance Eq Bool where
    False == False = True
    False == True = False
    True  == False = False
    True  == True = True

instance Ord Bool where
    False <= False = True
    False <= True = True
    True <= False = False
    True <= True = True
    
instance Show Bool where
    show False = "False"
    show True  = "True"

-- instance of Pair
instance (Eq a, Eq b) => Eq (a,b) where
    (x,y) == (x',y') = x == x' && y == y'

instance (Ord a, Ord b) => Ord (a,b) where
    (x,y) <= (x',y') = x < x' || (x == x' && y <= y')

instance (Show a, Show b) => Show (a,b) where
    show (a,b) = "(" ++ show x ++ "," ++ show y ++ ")"

-- instance of List
instance Eq a => Eq [a] where
    [] == []     = True
    [] == y:ys   = False
    x:xs == []   = False
    x:xs == y:ys = x == y && xs == ys

instance Ord a => Ord [a] where
    []   <= ys   = True
    x:xs <= []   = False
    x:xs <= y:ys = x < y || (x == y && xs <= ys)

instance Show a => Show [a] where
    show []     = "[]"
    show (x:xs) = "[" ++ showSep x xs ++ "]"
      where
          showSep x []     = show x
          showSep x (y:ys) = show x ++ "," showSep y ys

data Bool = False | True
  deriving (Eq, Ord, Show)

data Pair a b = MkPair a b
  deriving (Eq, Ord, Show)

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Ord a => Eq (Set a) where
    s == t = s `equal` t


-}