module AlgebraicType where

--import qualified Prelude as P
import           Data.List hiding ((++))
import           Prelude   hiding (not, (&&), (++), (||))

-- Algebraic data types
data Bool' = False' | True'
data Season = Winter | Spring | Summer | Fall
data Shape = Circle Float | Rectangle Float Float
data List a = Nil | Cons a (List a)
data Nat = Zero | Succ Nat
data Exp = Lit Int | Add Exp Exp | Mul Exp Exp
data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
data Maybe a = Nothing | Just a
data Pair a b = Pair a b
data Sum a b = Left a | Right b

not :: Bool -> Bool
not False = True
not True  = False

(&&) :: Bool -> Bool -> Bool
False && q = False
True  && q = q

(||) :: Bool -> Bool -> Bool
False || q = q
True  || q = True

next :: Season -> Season
next Winter = Spring
next Spring = Summer
next Summer = Fall
next Fall   = Winter

eqSeason :: Season -> Season -> Bool
eqSeason Winter Winter = True
eqSeason Spring Spring = True
eqSeason Summer Summer = True
eqSeason Fall Fall     = True
eqSeason x y           = False

showSeason :: Season -> String
showSeason Winter = "Winter"
showSeason Spring = "Spring"
showSeason Summer = "Summer"
showSeason Fall   = "Fall"

type Radius = Float
type Width  = Float
type Height = Float

data Shape' = Circle' Radius
            | Rect Width Height

area :: Shape' -> Float
area (Circle' r) = pi * r^2
area (Rect w h)  = w * h

radius :: Shape' -> Float
radius (Circle' r) = r

width :: Shape' -> Float
width (Rect w h) = w

height :: Shape' -> Float
height (Rect w h) = h

-- List
data List' a = Nil'
             | Cons' a (List' a)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- Expression Tree
data Exp' = Lit' Int
          | Add' Exp' Exp'
          | Mul' Exp' Exp'

evalExp :: Exp -> Int
evalExp (Lit n)   = n
evalExp (Add e f) = evalExp e + evalExp f
evalExp (Mul e f) = evalExp e * evalExp f

showExp :: Exp -> String
showExp (Lit n)   = show n
showExp (Add e f) = par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f) = par (showExp e ++ "*" ++ showExp f)

par :: String -> String
par s = "(" ++ s ++ ")"


-- Propositions
type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env   = [(Name, Bool)]


showProp :: Prop -> String
showProp (Var x)   = x
showProp F         = "F"
showProp T         = "T"
showProp (Not p)   = par ("~" ++ showProp p)
showProp (p :|: q) = par (showProp p ++ "|" ++ showProp q)
showProp (p :&: q) = par (showProp p ++ "&" ++ showProp q)

names :: Prop -> Names
names (Var x)   = [x]
names F         = []
names T         = []
names (Not p)   = names p
names (p :|: q) = nub (names p ++ names q)
names (p :&: q) = nub (names p ++ names q)

eval :: Env -> Prop -> Bool
eval e (Var x)   = lookUp e x
eval e F         = False
eval e T         = True
eval e (Not p)   = not (eval e p)
eval e (p :|: q) = eval e p || eval e q
eval e (p :&: q) = eval e p && eval e q

lookUp :: Eq a => [(a, b)] -> a -> b
lookUp xys x = the [ y | (x', y) <- xys, x == x' ]
  where
      the [x] = x

envs :: Names -> [Env]
envs []     = [[]]
envs (x:xs) = [ (x, False):e | e <- envs xs ] ++
              [ (x, True):e  | e <- envs xs ]

envs' :: Names -> [Env]
envs' []     = [[]]
envs' (x:xs) = [ (x,b):e | b <- bs, e <- envs' xs ]
   where
       bs = [False, True]

satisfiable :: Prop -> Bool
satisfiable p = or [ eval e p | e <- envs (names p) ]

