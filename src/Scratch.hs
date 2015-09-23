module Scratch where

import GHC.Base
import GHC.Integer
import GHC.Int
import GHC.Types
import System.IO

import Prelude () -- hide everything

class Num a where
    (+), (*) :: a -> a -> a

instance Num Integer where
    (+) = plusInteger
    (*) = timesInteger

instance Num Int where
    (+) = plusInt
    (*) = timesInt

x = 1
y = 2

z = x + y :: Integer
z2 = x * y :: Integer

type IntPair = (Int,Int)
 
f :: IntPair -> Int
f (l,r) = l + r

class Eq1 a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool


data Maybe1 a = Nothing1 | Just1 a

instance Eq1 (Maybe1 a) where
    Nothing1 == Nothing1 = True
    Just1 x  == Just1 y  = True
    _ == _               = False

    Nothing1 /= Just1 a  = True
    Just1 a  /= Nothing1 = True
    _ /= _       = False

class YesNo a where
    yesno :: a -> Bool

instance YesNo (Maybe1 a) where
    yesno Nothing1  = False
    yesno (Just1 a) = True


main = do putStrLn "Hello, what is your name?"
          name <- getLine
          putStrLn ("Hello, " ++ name ++ "!")
          print $ f (1,2)
