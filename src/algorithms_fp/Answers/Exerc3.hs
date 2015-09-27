module Exerc3 where

import Prelude hiding (sum)

--
--  exercise 1
--
-- (f $! a) b            -- a only
-- (f a) $! b            -- b only
-- (f $! a) $! b         -- both a and b

--
--  exercise 2
--

-- time counting version

powerT k = if (k==0)
           then 1
           else if (k `mod` 2) == 0
                then 1 + (powerT (k `div` 2))
                else 1 + (powerT (k `div` 2))

-- recurrence relation solution (for k>0):

powerTsol k = truncate(logBase 2 k) + 2

--
--  exercise 3
--

-- head   : a. O(1),  b. O(n),  c. O(mn)  
-- length : a. O(n),  b. O(n),  c. O(mn)
-- sum    : a. O(mn), b. O(mn), c. O(mn)

--
--  exercise 4
--

-- original version
prodsum x = prod x + sum x

prod 0 = 1
prod n = n * prod (n - 1)

sum 0 = 0
sum n = n + sum (n - 1)

-- tail recursive version (a)

prodsumTR x = prodTR x 1 + sumTR x 0
          
prodTR 0 r = r
prodTR n r = prodTR (n-1) (n*r)

sumTR 0 r = r
sumTR n r = sumTR (n-1) (n+r)

-- one pass - tuple version (b)

prodsumTP x = f+t
    where (f,t) = prodsum' x
          prodsum' 0 = (1,0)
          prodsum' n = (n*f',n+t')
              where (f',t') = prodsum' (n-1)

-- one pass - no tuple (c)

prodsumNTP x = prodsumNTP' x 1 0
    where prodsumNTP' 0 f t = f+t
          prodsumNTP' n f t = prodsumNTP' (n-1) (n*f) (n+t)

