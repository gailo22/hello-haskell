module Natural(Nat) where

import Test.QuickCheck

data Nat = MkNat Integer

invariant :: Nat -> Bool
invariant (MkNat x) = x >= 0

instance Eq Nat where
    MkNat x == MkNat y = x == y

instance Ord Nat where
    MkNat x <= MkNat y = x <= y

instance Show Nat where
    show (MkNat x) = show x


