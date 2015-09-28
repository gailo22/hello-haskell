-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module SimpleCatenableDeque (module CatenableDeque,SimpleCatDeque) where
  import Prelude hiding (head,tail,last,init,(++))
  import CatenableDeque

  data SimpleCatDeque d a = 
      Shallow (d a) 
    | Deep (d a) (SimpleCatDeque d (d a)) (d a)

  tooSmall d = isEmpty d || isEmpty (tail d)

  dappendL d1 d2 = if isEmpty d1 then d2 else cons (head d1) d2
  dappendR d1 d2 = if isEmpty d2 then d1 else snoc d1 (head d2)

  instance Deque d => Deque (SimpleCatDeque d) where
    empty = Shallow empty
    isEmpty (Shallow d) = isEmpty d
    isEmpty _ = False

    cons x (Shallow d) = Shallow (cons x d)
    cons x (Deep f m r) = Deep (cons x f) m r

    head (Shallow d) = head d
    head (Deep f m r) = head f

    tail (Shallow d) = Shallow (tail d)
    tail (Deep f m r)
        | not (tooSmall f') = Deep f' m r
        | isEmpty m = Shallow (dappendL f' r)
        | otherwise = Deep (dappendL f' (head m)) (tail m) r
      where f' = tail f

    snoc (Shallow d) x = Shallow (snoc d x)
    snoc (Deep f m r) x = Deep f m (snoc r x)

    last (Shallow d) = last d
    last (Deep f m r) = last r

    init (Shallow d) = Shallow (init d)
    init (Deep f m r)
        | not (tooSmall r') = Deep f m r'
        | isEmpty m = Shallow (dappendR f r')
        | otherwise = Deep f (init m) (dappendR (last m) r')
      where r' = init r

  instance Deque d => CatenableDeque (SimpleCatDeque d) where
    (Shallow d1) ++ (Shallow d2)
        | tooSmall d1 = Shallow (dappendL d1 d2)
        | tooSmall d2 = Shallow (dappendR d1 d2)
        | otherwise = Deep d1 empty d2
    (Shallow d) ++ (Deep f m r)
        | tooSmall d = Deep (dappendL d f) m r
        | otherwise = Deep d (cons f m) r
    (Deep f m r) ++ (Shallow d)
        | tooSmall d = Deep f m (dappendR r d)
        | otherwise = Deep f (snoc m r) d
    (Deep f1 m1 r1) ++ (Deep f2 m2 r2) = Deep f1 (snoc m1 r1 ++ cons f2 m2) r2
