---------------------------------------------------------------------------------------
-- Module      : Algorithms1
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
module Algorithms1
(
uglynum
)
where


---------------------------------------------------------------------------------------
{--|
                            Number Puzzle

        Find the 1500ₜₕ number  which contains factor 2, 3 or  5.  The first 3
        numbers are obviously 2, 3 and 5.   For example, the 25ₜₕ number is 60
        which can  be split as  60 = 2².3¹.5¹. The  first 10 such  numbers are
        2,3,4,5,6,8,9,10,12,15. If we consider 1 = 2⁰.3⁰.5⁰ then 1 can also be
        included as a valid number and is the first such number.

        This  problem is  a version  of  somewhat popular  Ugly Numbers  which
        defines as: Ugly numbers are positive numbers whose prime factors only
        include 2, 3,  5. For example, 1, 2, 3,  4, 5, 6, 8, 9, 10,  12 is the
        sequence of the first 10 ugly numbers.

        Αn algorithm  for this can  be designed  as follows:

        Start from base number  1 and multiply it with 2, 3  and 5 to generate
        the rest of the  numbers. Now the problem turns out  to be finding the
        candidate number in order. A handy  solution to use in this case would
        be a Queue or a Pririty Queue without repetition (like a Set).

        The idea is  to Push 1 (2⁰.3⁰.5⁰)  as the only element  into the Queue
        and in  each iteration and  element with  the lowest value  is Popped,
        multiplied with 2, 3 and 5 to get  the next 3 elements. Push the 3 new
        elements back into the Queue in the  same order. If the new element is
        already present  in the Queue, that  element will be dropped.  The new
        element may also be smaller than  the elements already in the Queue in
        which case it has to be pushed to the correct position.

        [1][] → 1 → 1*2 1*3 1*5 → [2][3][5]
        [2][3][5] → 2 → 2*2 2*3 2*5 → [3][4][5][6][10]
        [3][4][5][6][10] → 3 → 3*2 3*3 3*5 → [4][5][6][9][10][15]

        and so on

        Here are the steps for the above operations

        ⒈ Initialize Queue with 1 as the only element
        ⒉ Pop 1 and Push new elements 2, 3, 5 back
        ⒊ Pop 2 and Push back 4, 6, 10 to for a bigger list

        If X is the infinite series consisiting  of only factors of 2, 3 or 5,
        then we can have the following mathematical relationship.

        X = {1} ∪ {2x: ∀x ∈ X} ∪ {3x: ∀x ∈ X} ∪ {5x: ∀x ∈ X}

        The Union (∪)  here will store all  the elements as sorted  as well as
        keep them unique to each other.

        If X = {x₁, x₂, x₃,...}, Y = {y₁, y₂, y₃,...}
           X' = {x₂, x₃, ...}, Y' = {y₂, y₃,...} then we have the following

                     X             : Y = ∅
                     Y             : X = ∅
           X ∪ Y =   {x₁, X′ ∪ Y}  : x₁ ≺ y₁
                     {x₁, X′ ∪ Y'} : x₁ = y₁
                     {y₁, X ∪ Y'}  : x₁ ≻ y₁

--}

-- Solution approach - 1
-- The Time complexity is O(n)
-- Taking advantage of the infinite lazy evaluation of Haskell

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x < y     = x : merge xs (y : ys)
    | x == y     = x : merge xs ys
    | otherwise = y : merge (x : xs) ys

ns :: [Integer]
ns = 1 : merge (map (*2) ns) (merge (map (*3) ns) (map (*5) ns))

uglynum :: Int -> Integer
uglynum n = ns !! (n - 1)

-- λ> uglynum 25
-- 60
-- λ> uglynum 1500
-- 859963392
--
---------------------------------------------------------------------------------------

{--|
                Approach 2

The above  approach although is  fast produces many  duplicate numbers
and they are finally dropped while  examining the Queue. Also it scans
the whole queue  linearly giving a Time complexity  of O(|Queue|). The
alternate approach to  implementing the above solution  using 3 Queues
instead of a single Queue.

Three Queue can be initialized as follows

Q₂ = {2}
Q₃ = {3}
Q₅ = {5}

Each  time a  smallest element  x from  the queues  Q₂, Q₃  and Q₅  is
Dequeued or POPPED, the following tests can be performed.

① If x  comes from Q₂, then 2x,  3x and 5x will be  Enqueued or PUSHED
back into the Queues Q₂, Q₃ and Q₅ respectively.

② If x  comes from Q₃, then only  3x will be Enqueued to Q₃  and 5x to
Q₅. 2x need not ne Enqueue into Q₂ because 2x is already present in Q₃

③ If x comes from Q₅, then only  5x will Enqueued into Q₅. There is no
need of Enqueing 2x and 3x into  Q₂ and Q₃ as they are already present
in the Queues.

Repeated Enqueing will be done untill the nₜₕ element is found

--}
--
-- Solution Approach 2
--

{--|
    Edit Distance between two strings

        Shortest sequence  of simple  editing operations to  transform one
    string into another string. The same analysis used here can be applied
    to get the difference between two lists.

        We  suppose that  there are  five  basic editing  operations on  a
    string.  We  can change one  character into another, copy  a character
    without modifying it,  delete or insert a character  and delete (kill)
    to the end of  the string. We also assume that  each operation has the
    same cost, except a copy which is free.

        To turn a string "fish" into "chips", we can either kill the whole
    string, then insert  characters one-by-one at a total cost  of six. An
    optimal solution will copy only as much of the string as possible.

    insert char 'c'
    change 'f' to 'h'
    copy 'i'
    insert 'p'
    copy 's'
    finally - delete string "h"
--}

-- define a data type for Edit
data Edit = Change Char
          | Copy
          | Delete
          | Insert Char
          | Kill
          deriving (Eq, Show)

-- For Edit distance, the problem is to find the lowest cost
-- sequence of edits to take from one string to another.
--
-- To transform a non-empty string to empty, Kill it
-- To transform an empty string to a string, Insert each char.
--
-- In the more geeric case of 2 non-empty string, we have a
-- choice as follows:
-- Which one to use first ???
-- Copy / Delete / Insert / Change
-- If the first characters are equal, use Copy
-- otherwise no obvious choice and hence all possibilities
-- need to be checked.
transform :: String -> String -> [Edit]
transform [] [] = []
transform _ []  = [Kill]
transform [] ys = map Insert ys
transform (x : xs) (y : ys)
    | x == y = Copy : transform xs ys
    | otherwise = optimum [ Delete : transform xs (y : ys)
                          , Insert y : transform (x : xs) ys
                          , Change y : transform xs ys
                          ]

optimum :: [[Edit]] -> [Edit]
optimum [x] = x
optimum (x : xs)
    | cost x <= cost z = x
    | otherwise       = z
      where
      z = optimum xs

-- The cost is given by charging one for every operation except copy,
-- which is equivalent to 'leave unchanged'.
cost :: [Edit] -> Int
cost = length . filter (/= Copy)

