{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------------
-- |
-- Module      : Algorithms4
-- Description : Random Algorithms in Haskell
--               The  Algorithms  are  all  implemented  by  taking  advantage  of  the
--               Haskell(s) support  for Lazy  evaluation and infinite  lists. Wherever
--               possible (based  on my level  of knowledge of Haskell)  these features
--               have been  harnessed inorder to  get the optimal performance  for Time
--               and Space.
-- Copyright   :
-- Maintainer  : Singamsetty.Sampath@gmail.com
-- License     : MIT
--
-- 1. An implementation of the Coin change problem.
-- 2. An implementation of the Water Fall problem.
-- 3. An implementation of the String Edit Distance Algorithm
--
-- /Warning/:
--
-----------------------------------------------------------------------------------
module Algorithms4 where

import qualified Data.Array as A
import qualified Data.List as L

{-
It is a classic problem from computer science to determine the number of ways in
which coins can  be combined to form  a particular target; as  an example, there
are 31  ways to  form 40  cents from an  unlimited supply  of pennies  (1 cent),
nickels (5  cents), dimes (10 cents)  and quarters (25 cents),  ranging from the
3-coin set (5 10 25) to the 40-coin set consisting only of pennies.

The solution is usually stated in recursive form:  if c is the first coin in the
set of coins cs and n is the target, the solution is the number of ways to reach
the target after removing c from cs plus the number of ways to reach n − c using
all the coins in  cs. The algorithm to make a list of  the coins, instead of the
count, is the same, but keeping track of the list of coins instead of the count.

case - 1

If unlimited supply  of coins of each denomination are  available and also there
is no constraint of using only the minimum number of coins, then we can have the
below solution

Consider that the coin will be either a part of the solution or not
Sol(sum, list[1..m]) =
Sol(sum, list[1..m-1]) + Sol(sum - list[m], list[1..m])
where Sol is Solution
      sum is Sum to be produced using the given coins
      list[1..m] is List of m given coins
      list[m] is denomination of the m-th coin

If a coin is excluded from Sol, then it becomes a sub problem of finding the sum
with one  coin less  as represented  by Sol(sum, list[1..m-1]).  If the  coin is
included, then it  becomes sub-problem of finding the (sum  - coin denomination)
with the given list of coins as represented by Sol(sum - list[m], list[1..m-1])

λ> change [1,2,3] 4
[[1,1,1,1],[1,1,2],[1,3],[2,2]]
λ> change [4,10,25] 41
[[4,4,4,4,25]]

λ> head . change denominations $ 51
[25,25,1]
λ> head . change denominations $ 432
[100,100,100,100,25,5,1,1]
-}
type Coins  = Integer
type Amount = Integer

denominations :: [Coins]
denominations = [100, 25, 15, 10, 5, 1]


change :: [Coins] -> Amount -> [[Integer]]
change [] 0 = [[]]
change [] _ = []
change coins@(c : cs) amount
   | c > amount = change cs amount
   | otherwise  = map (c :) (change coins (amount - c))
                  ++
                  change cs amount

-- Given a list of coins [c₁, c₂, c₃,...,cₙ] of various denominations and an amount
-- a, find  the list [a₁, a₂,  a₃,...,aₙ] of integers  such that the value  c₁•a₁ +
-- c₂•a₂ + ... + cₙ•aₙ = a, with a minimum number of coins
-- first try using a Brute force method
-- using the coin denominations of 1,5,10,15,25,100
-----------------------------------------------------------------------------------
-- testing the output with main
coinChange :: IO ()
coinChange = do
             putStrLn $ ("change for 17 cents: " ++) . show . head . change denominations $ 17
             putStrLn $ ("change for 41 cents: " ++) . show . head . change denominations $ 41
             putStrLn $ ("change for 30 cents: " ++) . show . head . change denominations $ 30
-----------------------------------------------------------------------------------
{-
2. Water Fall problem

If we have walls of variable heights  represented by an array of integers, where
the values at each index of the array is the height of the wall. For example the
array A = [2,5,1,2,3,4,7,7,6] as represented in the below figure.

                              _7___7_
                             |   |   |_6_
          _5_                |   |   |   |
         |   |            _4_|   |   |   |
         |   |        _3_|   |   |   |   |
      _2_|   |    _2_|   |   |   |   |   |
     |   |   |_1_|   |   |   |   |   |   |
     |   |   |   |   |   |   |   |   |   |
   -----------------------------------------

Now  if it  rains, how  much water  is going  to be  accumulated in  the puddles
between the  walls? No puddles  are formed  at the edges  of the wall,  water is
considered to simply run  off the edges. We can count the  volume in each square
blovk of 1 X 1.  In that way we will be left with a  puddle between the column 1
and column 6 and the volume is 10.

Water will stay within  the walls in a square or a rectangle  only if there is a
wall on either  side of the area. If hᵢ  is the height at the index  i and leftᵢ
denote the highest point to the left  of the index i and similarly rightᵢ denote
the highest point to the right of the index i.

An area will be filled only when hᵢ ≤ min(leftᵢ, rightᵢ)

The height of water level at  each index water_levelᵢ can be expressed
using the equality, water_levelᵢ = min(leftᵢ, rightᵢ).

For left side
At index 0, left₀ = h₀
            leftᵢ₊₁ = max(hᵢ₊₁, leftᵢ)

For right side
At index 0, rightₙ = hₙ
            rightᵢ₋₁ = max(hᵢ, leftᵢ)

The above recursive function can be expressed using scan function in haskell
-}
-- Heights of the pillars given
heights :: [Integer]
heights = [2,5,1,2,3,4,7,7,6]

maxLeftHeights :: [Integer] -> [Integer]
maxLeftHeights = scanl1 max

maxRightHeights :: [Integer] -> [Integer]
maxRightHeights = scanr1 max

minfilledHeights :: [Integer] -> [Integer] -> [Integer]
minfilledHeights = zipWith min

-- The above list of heights of water columns include the height of the pillar
-- remove the height of the pillar from each value in the list
waterFilledHeights :: [Integer] -> [Integer] -> [Integer]
waterFilledHeights = zipWith (-)

-- maximum water fall in the puddle
maxWaterFall :: [Integer] -> Integer
maxWaterFall h = sum $ waterFilledHeights (minfilledHeights (maxLeftHeights h) (maxRightHeights h)) h

-- get the maximum water filled by the puddle
waterVolume :: [Integer] -> IO ()
waterVolume hs = putStrLn $ "The maximumvolume of water between the heights "
                          ++ show hs
                          ++ " is " ++ show (maxWaterFall hs)

-- λ> waterVolume heights
-- The maximumvolume of water between the heights [2,5,1,2,3,4,7,7,6] is 10
-----------------------------------------------------------------------------------
{-
         3. STRING EDIT DISTANCE ALGORITHM

    String Edit  Distance is  a measure of  checking how  different or
    similar two strings  are. The difference between 2  strings can be
    given  by  either Hamming  distance  or  levenstein distance.  But
    Hamming distance  only works  for string of  equal length,  so its
    better to use  the Levenstein's distance. It's  the minimum number
    of steps required  to go from one string to  the other, where each
    step involves the following:

    1. Remove a character FALTER -> ALTER  -> (Remove Char F)
    2. Add a character    ALTER  -> FALTER -> (Add Char F)
    3. Update a character BILL   -> MILL   -> (Update Char B with Char M)

    For example if we want to transform string "ab" into "abc", we need to
    insert a character "c".  Hence it can be done in  1 step. Similarly if
    we need to transform "abc" into "ab", the number of operations is 1 as
    character 'c' needs to be deleted. Edit distance is just the number of
    operations required for transforming one string into another.

    Antoher example  for str1 =  "INTENTION" and str2 =  "EXECUTION". Here
    the minimum edit distance is 5.

    r - remove | u - update | a - add

    r u u   a u
    | | |   | |
    I N T E _ N T I O N
    |       | |
    _ E X E C U T I O N

    Algorithm:
    ______________________________________________
    (1)

    For two strings s1 & s2, the edit distance is defined using a recurrence
    relation as follows:
    d('', '') = 0               -- '' = empty string
    d(s, '')  = d('', s) = |s|  -- i.e. length of s
    d(s1 + c1, s2 + c2)
      = min( d(s1, s2)      + if c1 = c2 then 0 else 1 fi,
             d(s1 + c1, s2) + 1,
             d(s1, s2 + c2) + 1 )
    ______________________________________________
    (2)

    Or explicitly as below for 2 strings s1 and s2
    Base Conditions:
      d(i, 0) = i
      d(0, j) = j
    Termination Condition:
      d(x, y) is the distance
    Recurrence Relation:
      for each i = 1..y
          for each j = 1..x
              d(i, j)= Min { d(i-1, j) + 1
                             d(i, j-1) + 1
                             d(i-1, j-1) + δ(s1[i-1], s2[j-1])
                           }

                       δ(a, b) = 0 if a == b
                               = 1 otherwise
    ______________________________________________
    (3)

    Or it can be specified as follows For 2 strings x and y, if dᵢⱼ is
    the distance  between their suffixes of  lengths i and j  then the
    following relation can be used.

    dᵢ₀ = i       ∀ 0 ≤ i ≤ |x|
    d₀ⱼ  = j       ∀ 0 ≤ j ≤ |y|
    dᵢⱼ  = dᵢ₋₁,ⱼ₋₁  if xᵢ ≠ yⱼ

    dᵢⱼ  = minimim { dᵢ₋₁   + 1 // delete
                    dᵢ,ⱼ₋₁   + 1 // insert
                    dᵢ₋₁,ⱼ₋₁ + 1 // modify
                   }

-}
-- Hamming distance between the strings
hamming :: String -> String -> Int
hamming x y
   | length x == length y = sum $ map (\(a, b) -> if a == b then 1 else 0) $ zip x y
   | otherwise = error "Inequal Strings cannot be compared"

-- levenstein's distance between two strings
edDistance1 :: String -> String -> Int
edDistance1 "" "" = 0
edDistance1 "" s2 = length s2
edDistance1 s1 "" = length s1
edDistance1 s1 s2
   | last s1 == last s2 = edDistance1 (init s1) (init s2)
   | otherwise = minimum [edDistance1 (init s1) s2 + 1,
                          edDistance1 s1 (init s2) + 1,
                          edDistance1 (init s1) (init s2) + 1
                         ]

-- the above version is too slow and the following can boost speed
edDistance2 :: String -> String -> Int
edDistance2 s1 s2
   | length s1 > length s2 = edDistance2 s2 s1
   | length s1 < length s2 = edDistance2 s1 (take (length s2 - d) s2)
                             where
                             d = length s2 - length s1
edDistance2 "" "" = 0
edDistance2 s1 s2
   | last s1 == last s2 = edDistance2 (init s1) (init s2)
   | otherwise = minimum [edDistance2 (init s1) s2 + 1,
                          edDistance2 s1 (init s2) + 1,
                          edDistance2 (init s1) (init s2) + 1
                         ]

-- another naive implementation of the string edit distance
lavensteinNaive :: String -> String -> Int
lavensteinNaive x y = d (length x) (length y)
                where
                d i 0 = i
                d 0 j = j
                d i j
                   | x !! (i - 1) == y !! (j - 1) = d (i - 1) (j - 1)
                   | otherwise = minimum [ d (i - 1) j + 1
                                         , d i (j - 1) + 1
                                         , d (i - 1) (j - 1) + 1
                                         ]

-- test runs
-- λ> lavenstein  "Shakespeare" "shake spear"
-- 3
-- (3.14 secs, 1,250,457,792 bytes)
-- λ> edDistance1 "Shakespeare" "shake spear"
-- 3
-- (3.01 secs, 1,112,005,984 bytes)
-- λ> edDistance2 "Shakespeare" "shake spear"
-- 7
-- (0.02 secs, 8,786,296 bytes)
--
-- The lavenstein distance defined above has an exponential running time
-- The runtime can be reduced by using memization instead of recalculating.
lavensteinMemoize :: String -> String -> Int
lavensteinMemoize x y = d a b
                  where
                  (a, b) = (length x, length y)
                  d i 0 = i
                  d 0 j = j
                  d i j
                     | x !! (i - 1) == y !! (j - 1) = ds A.! (i - 1, j - 1)
                     | otherwise = minimum [ ds A.! (i - 1, j) + 1
                                           , ds A.! (i, j - 1) + 1
                                           , ds A.! (i - 1, j - 1) + 1
                                           ]
                  ds = A.listArray pairs [d i j | (i, j) <- A.range pairs]
                  pairs = ((0, 0), (a, b))

-- test runs
-- λ> lavensteinMemoize "Shakespeare" "shake spear"
-- 3
-- (0.00 secs, 1,033,936 bytes)

-- because lists are not a good fit for random access using (!!) operator, they
-- might cause serious performace issues when run on longer strings.
-- for better performace we can convert the lists to arrays and indexing them.
lavensteinIdx :: String -> String -> Int
lavensteinIdx x y = d a b
              where
              (a, b) = (length x, length y)
              p = A.listArray (1, a) x
              q = A.listArray (1, b) y
              d i 0 = i
              d 0 j = j
              d i j
                 | p A.! i == q A.! j = ds A.! (i - 1, j - 1)
                 | otherwise = minimum [ ds A.! (i - 1, j) + 1
                                       , ds A.! (i, j - 1) + 1
                                       , ds A.! (i - 1, j - 1) + 1
                                       ]
              ds = A.listArray pairs [d i j | (i, j) <- A.range pairs]
              pairs = ((0, 0), (a, b))

-- test runs
-- λ> lavensteinIdx "Pneumonoultramicroscopicsilicovolcanoconiosis" "Pseudopseudohypoparathyroidism"
-- 35
-- (0.01 secs, 6,243,768 bytes)
-- λ> lavensteinMemoize  "Pneumonoultramicroscopicsilicovolcanoconiosis" "Pseudopseudohypoparathyroidism"
-- 35
-- (0.01 secs, 4,179,008 bytes)
-----------------------------------------------------------------------------------

-- A  final implementation  of the  above algorithm  using specific  data
-- types  for providing  a more  generalized  view of  the algorithm.  In
-- particular here we  are going to capture the list  of actions required
-- to transform one  string to another, along with the  distance. We will
-- define  and use  a cost  function which  would specify  how much  each
-- possible action is worth.
--
-- four possible edit actions
data EditAction = None
                | Add
                | Delete
                | Modify  deriving (Eq, Show)

type EditDistance = Int

costFunction :: EditAction -> EditDistance
costFunction None = 0
costFunction _ = 1

lavenstein :: (Eq a) => (EditAction -> EditDistance) -> [a] -> [a] -> [EditAction]
lavenstein costFunction x y = snd $ d a b
  where (a, b) = (length x, length y)
        p = A.listArray (1, a) x
        q = A.listArray (1, b) y
        d 0 0 = (0, [])
        d i 0 = loop (i - 1) 0 Delete
        d 0 j = loop 0 (j - 1) Add
        d i j
           | p A.! == q A.! = loop (i - 1) (j - 1) None
           | otherwise     = minOf [ loop (i - 1) j Delete
                                   , loop i (j - 1) Add
                                   , loop (i - 1) (j - 1) Modify
                                   ]
        minOf = L.minimumBy (comparing fst)
        loop s t action = let (score, actions) = ds A.! (s, t) in
                          (score + costFunction action, action : actions)
        ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]
        bounds = ((0, 0), (a, b))                                                        
