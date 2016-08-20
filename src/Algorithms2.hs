---------------------------------------------------------------------------------------
-- Module      : Algorithms2
-- Description : Random Algorithms in Haskell
--               The  Algorithms  are  all  implemented  by  taking  advantage  of  the
--               Haskell(s) support  for Lazy  evaluation and infinite  lists. Wherever
--               possible (based  on my level  of knowledge of Haskell)  these features
--               have been  harnessed inorder to  get the optimal performance  for Time
--               and Space.
-- Copyright   :
-- Maintainer  : Singamsetty.Sampath@gmail.com
-- License     : MIT
---------------------------------------------------------------------------------------
module Algorithms2
(
change,
minFree
)

where

import qualified Data.List as L

---------------------------------------------------------------------------------------
{-|
   Coin Change Problem
   Coin Change is the problem of finding the number of ways of making changes
   for a particular amount of cents, η using a given set of denominations like
   d₁, d₂, d₃, ... ,dₙ. It can be solved using Dynamic Programming
   Available resources are infinite set of coins S = {S₁,S₂,S₃,...,Sₙ}.
   For a given integer A.
   The key idea to solve this problem is to consider that either a coin will
   be present in the solution or not
   if S                = solution
   sum                 = sum to be produced by the given coins
   list [1 .. m]       = list of m given coins
   list [m]            = denomination of the mᵗʰ coin
   S(sum, list[1...m]) = S(sum, list[1...m-1]) + S(sum - list[m], list[1...m])
-}

change :: [Int] -> Int -> [[Int]]
change [] 0 = [[]]
change [] _ = []
change c@(coin : coins) value
    | value < coin = change coins value
    | otherwise    = map (coin : ) $ change c (value - coin) ++ change coins value

-- λ> changes [1,2,3] 5
-- [[1,1,1,1,1],[1,1,1,1,2],[1,1,1,2,3],[1,1,2,2],[1,2,2,3]]

-- if we want only the minimum set of coins then use the head
-- head $ changes [25,10,5,1] 63 = [25,25,10,1,1,1]
---------------------------------------------------------------------------------------


{-|

 Smallest free Id
 Using divide and conquer
 keep all elements xᵢ ≤ ⌊n/2⌋ as a sublist A'
 and put the rest of elements as a sublist A''
 if length(A') == ⌊n/2⌋ then the first half of numbers are full
 indicating that the minimum free number is in A''. Otherwise
 its in A'

 minfree(A) = search(A, 0, |A| - 1)

 search(A, l, u) = l                   if A = []
                 = search(A'', m+1, u) if |A'| = m-l+1
                 = search(A', l, m)    otherwise

              where m = ⌊(l+u)/2⌋
                    A' = { ∀ x ∈ A ∧ x ≤ m }
                    A'' = { ∀ x ∈ A ∧ x ≻ m }

-}

minFree :: [Int] -> Int
minFree xs = binSearch xs 0 (length xs - 1)

-- Using TCO, the total cost is O(n)
binSearch :: [Int] -> Int -> Int -> Int
binSearch xs l u
    | null xs                 = l
    | length ys == mid - l + 1 = binSearch zs (mid + 1) u
    | otherwise               = binSearch xs l u
    where
    mid = (l + u) `div` 2
    (ys, zs) = L.partition (<= mid) xs
---------------------------------------------------------------------------------------
