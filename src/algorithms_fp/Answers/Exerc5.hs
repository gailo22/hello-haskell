module Exerc5 where

-- 
-- examples of solutions to exercises of chapter 5
--


---
--- Exercise 1
---

dist (x,y) = sqrt (x^2+y^2)

data Point = Pt (Float,Float)

instance Eq Point
  where (Pt p1) == (Pt p2) = (dist p1 == dist p2)

instance Ord Point
  where (Pt p1) <= (Pt p2) = (dist p1 <= dist p2)

--
--  Exercise 2
--

-- the changes are in the following:

enPQ    :: (a -> a -> Bool) -> a -> PQueue a -> PQueue a

type PQueue a           = [a]

enPQ smaller x []       = [x]
enPQ smaller x r@(e:r') 
       | x `smaller` e  = x:r
       | otherwise      = e:(enPQ smaller x r')

--
-- Exercise 3
--
inside s x = or [x'==x|x'<-s]

included s1 s2 = and [inside s2 x|x<-s1]

inter s1 s2 = [x|x<-s1, inside s2 x]

union s1 s2 = s1++[x|x<-s2, not (inside s1 x)]

--
-- Exercises 7 and 8
--  need access to the internals of Heap.hs which are repeated here
----------------------------------------------------------------------
--module Heap(Heap,emptyHeap,heapEmpty,findHeap,insHeap,delHeap)
--    where 

emptyHeap:: (Ord a) => Heap a
heapEmpty:: (Ord a) => Heap a -> Bool
findHeap :: (Ord a) => Heap a -> a
insHeap  :: (Ord a) => a -> Heap a -> Heap a
delHeap  :: (Ord a) => Heap a -> Heap a

-- IMPLEMENTATION with Leftist Heap
-- adapted from C. Okasaki Purely Functional Data Structures p 197

data (Ord a) => Heap a = EmptyHP
                       | HP a Int (Heap a) (Heap a)
    deriving Show

emptyHeap = EmptyHP

heapEmpty EmptyHP = True
heapEmpty _       = False
                    
findHeap EmptyHP      = error "findHeap:empty heap"
findHeap (HP x _ a b) = x

insHeap x h = merge (HP x 1 EmptyHP EmptyHP) h

delHeap EmptyHP      = error "delHeap:empty heap"
delHeap (HP x _ a b) = merge a b

-- auxiliary functions

rank :: (Ord a) => Heap a -> Int
rank EmptyHP      = 0
rank (HP _ r _ _) = r
                    
makeHP :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeHP x a b | rank a >= rank b = HP x (rank b + 1) a b
             | otherwise        = HP x (rank a + 1) b a

merge ::(Ord a) =>  Heap a -> Heap a -> Heap a
merge h EmptyHP = h
merge EmptyHP h = h
merge h1@(HP x _ a1 b1) h2@(HP y _ a2 b2)
      | x <= y    = makeHP x a1 (merge b1 h2)
      | otherwise = makeHP y a2 (merge h1 b2)

-- end of internals of Heap.hs
----------------------------------------------------------------------
listToHeap xs = foldr insHeap emptyHeap xs

listToHeap' []  = EmptyHP
listToHeap' [x] = HP x 1 EmptyHP EmptyHP
listToHeap' xs  = merge (listToHeap' xs1) (listToHeap' xs2)
    where xs1 = take n xs
          xs2 = drop n xs
          n = length xs `div` 2

---
--- Exercise 8
---

-- Replacing the value of the first parameter in the definition of merge 
-- because h  = h1 = HP x 1 EmptyHP EmptyHP
--         a1 = b1 = EmptyHP

insHeap' :: (Ord a) => a -> Heap a -> Heap a
insHeap' x EmptyHP = HP x 1 EmptyHP EmptyHP
-- second case of merge can never occur
insHeap' x h2@(HP y _ a2 b2)
--     | x <= y    = makeHP x EmptyHP (merge EmptyHP h2)
       | x <= y    = makeHP x EmptyHP h2
--     | otherwise = makeHP y a2 (merge (HP x 1 EmptyHP EmptyHP) b2)
--         which by definition of insHeap' can be rewritten as
       | otherwise = makeHP y a2 (insHeap' x b2)

---
--- Exercise 9
---

data (Ord a,Show a) => AVLTree a = EmptyAVL
                                 | NodeAVL a Int (AVLTree a) 
                                                 (AVLTree a)
    deriving Show

emptyAVL = EmptyAVL

height EmptyAVL            = 0
height (NodeAVL _ h lf rt) = h

rotateLeft,rotateRight :: (Ord a,Show a)=> AVLTree a -> AVLTree a
rotateLeft EmptyAVL   = EmptyAVL
rotateLeft (NodeAVL v _ (NodeAVL lv _ lflf lfrt)
                         rt)  
                      = NodeAVL lv h lflf
                                (NodeAVL v h' lfrt rt)
                         where h' = max (height lfrt) (height rt) + 1
                               h  = max h' (height lflf) + 1

rotateRight EmptyAVL  = EmptyAVL
rotateRight (NodeAVL v _ lf
                       (NodeAVL rv _ rtlf rtrt)) 
                      = NodeAVL rv h (NodeAVL v h' lf rtlf)
                                rtrt
                         where h' = max (height rtlf) (height lf) + 1
                               h  = max h' (height rtrt) + 1

dRotLeftRight , dRotRightLeft 
    :: (Ord a,Show a) => AVLTree a -> AVLTree a
dRotRightLeft (NodeAVL v _ lf
                         (NodeAVL rtv _ (NodeAVL rtlv _ rtlflf rtlfrt)
                                     rtrt))
    = NodeAVL rtlv h (NodeAVL v h' lf rtlflf)
                     (NodeAVL rtv h'' rtlfrt rtrt)
                         where h''= max (height rtlfrt) (height rtrt) + 1
                               h' = max (height rtlflf) (height lf) + 1
                               h  = max h' h'' + 1

dRotLeftRight (NodeAVL v _ (NodeAVL lfv _ lflf
                                          (NodeAVL lfrv _ lfrtlf
                                                         lfrtrt))
                           rt)
    = NodeAVL lfrv h (NodeAVL lfv h' lflf lfrtlf)
                     (NodeAVL v h'' lfrtrt rt)
                         where h''= max (height lfrtrt) (height rt) + 1
                               h' = max (height lflf) (height lfrtlf) + 1
                               h  = max h' h'' + 1

addAVL i EmptyAVL= NodeAVL i 1 EmptyAVL EmptyAVL
addAVL i (NodeAVL v _ lf rt) 
    | i < v     = let newlf@(NodeAVL newlfv h _ _)  = addAVL i lf
                      h'                            = height rt 
                  in if ((h - h') == 2)
                     then if i < newlfv
                          then rotateLeft (NodeAVL v 0 newlf rt)
                          else dRotLeftRight (NodeAVL v 0 newlf rt)
                     else (NodeAVL v (max h h' + 1) newlf rt)
    | otherwise = let newrt@(NodeAVL newrtv h _ _)  = addAVL i rt
                      h'                            = height lf
                  in if ((h - h') == 2)
                     then if i > newrtv
                          then rotateRight (NodeAVL v 0 lf newrt)
                          else dRotRightLeft (NodeAVL v 0 lf newrt)
                   else (NodeAVL v (max h h' + 1) lf newrt)

{- examples of calls and results

? foldr addAVL emptyAVL [7,6..1]
NodeAVL 4 3 (NodeAVL 2 2 (NodeAVL 1 1 EmptyAVL EmptyAVL) (NodeAVL 3 1 EmptyAVL EmptyAVL)) (NodeAVL 6 2 (NodeAVL 5 1 EmptyAVL EmptyAVL) (NodeAVL 7 1 EmptyAVL EmptyAVL))

-}

--
--  Exercise 10
--

--The deletion of a node is a complex process, possibly involving
--O(log n) rotations. For an information description of the algorithm,
--see:

--R.L. Kruse, Data Structures and Program Design, Prentice Hall, 1984,
--pp. 365-367.


