-----------------------------------------------------------------------------
-- |
-- Module      : Algorithms3
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
-- An implementation of Fast versions of caluculating the Nth Fibonacci
-- number based on a couple of algorithms.
-- The Fibonacci numbers or the Fibonacci sequences are represented as
-- 1, 1, 2, 3, 5, 8, 13, 21, 34, 55...
-- or starting from 0 as
-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55...
-- By definition, the first two numbers in the Fibonacci sequence are
-- either 1 and 1, or 0 and 1, depending on the chosen starting point of
-- the sequence, and each subsequent number is the sum of the previous two
--
-- /Warning/:
--
-----------------------------------------------------------------------------
module Algorithms3 where


{-|
   In Mathematical form the sequence of the Fibonacci numbers are
   represented by the following recurrence relation
   Fₙ = Fₙ₋₁ + Fₙ₋₂
   with the seed values as
   F₀ = 0, F₁ = 1 or
   F₁ = 1, F₂ = 1

   The same sequence can be extended to negative index n using
   the re-arranged recurrence relation as follows

   Fₙ₋₂ = Fₙ - Fₙ₋₁ which yields the follogin negafibonacci series

   F₋ₙ = (-1)ⁿ⁺¹Fₙ

   Because of the recurrence relation and repeated calculations
   the problem quite quickly fills the stack with traditional
   dynamic programming and recurrence programming models.

   Recursive Solution (Extremely Slow):
   The normal recursive algorithm is extremely slow.
   Running time is Exponential and
   It's Space compleximity is O(n) and uses O(φⁿ) operations.
   where φ is the Golden Ratio defined as

   φ = (√5 + 1)/2

   Dynamic Programming (Slower):
   If we already computed the previous two values, like for instance
   Fₖ₋₂ and Fₖ₋₁ then we can add these two to get the next number Fₖ.
   Next we add Fₖ₋₁ and Fₖ to get Fₖ₊₁. This will be repeated until,
   k == n.
   This takes O(n) Operations and the Space Complexity = O(1)

   A Fast Approach using Matrices.
   As a first fast method we will use the Matrix Approach to calculate
   the Nₜₕ Fibonacci number. Using exponentiation by squaring, we can
   derive a mathematical equation which can be representated in a Matrix
   equation format.
   Exponentiation with Squaring ：https://en.wikipedia.org/wiki/Exponentiation_by_squaring

   F₀  = 0             or F(0) = 0
   F₁  = 1             or F(1) = 1
   Fₖ₊₁ = Fₖ + Fₖ₋₁     or F(k+1) = F(k) F(k-1)

   if Matrix Α = [1 1]
                 [1 0]

   then A¹ = [1 1] = [F₂ F₁]
             [1 0]   [F₁ F₀]

   If the formula is TRUE, then
   Aᴷ⁺¹ = Aᴷ.A¹ =   [1 1][Fₖ₊₁  Fₖ] = [Fₖ₊₁ + Fₖ  Fₖ + Fₖ₋₁] = [Fₖ₊₂  Fₖ₊₁]
                    [1 0][Fₖ  Fₖ₋₁]   [Fₖ₊₁         Fₖ    ]   [Fₖ₊₁   Fₖ ]

   [1 1]ᴷ = [Fₖ₊₁  Fₖ]  or  [F(k+1)  F(k) ]
   [1 0]    [Fₖ  Fₖ₋₁]      [F(k)   F(k-1)]

   The algorithm takes O(1) space complexity
                       O(log n) Operations
-}

-- Matrix Approach
-- By the power of Matrices
-- define a 2 X 2 Matrix data type
data Matrix a = Matrix a a a a deriving (Eq)

instance (Show m) => Show (Matrix m) where
    show (Matrix a b c d) = "[[" ++ show a ++ ", " ++ show b ++ "]\n [" ++ show c ++ ", " ++ show d ++ "]]"


-- Matrix as an instance of a Functor
instance Functor Matrix where
    fmap f (Matrix a b c d) = Matrix (f a) (f b) (f c) (f d)

-- Matrix as an instance of the Num class
instance Num a => Num (Matrix a) where
    (Matrix x1 x2 x3 x4) + (Matrix y1 y2 y3 y4) = Matrix (x1+y1) (x2+y2) (x3+y3) (x4+y4)
    (Matrix x1 x2 x3 x4) - (Matrix y1 y2 y3 y4) = Matrix (x1-y1) (x2-y2) (x3-y3) (x4-y4)
    (Matrix x1 x2 x3 x4) * (Matrix y1 y2 y3 y4) =
            Matrix (x1*y1 + x2*y3) (x1*y2 + x2*y4) (x3*y1 + x4*y3) (x3*y2 + x4*y4)
    fromInteger x                               = Matrix (fromInteger x) 0 0 (fromInteger x)
    abs                                         = fmap abs
    negate                                      = fmap negate
    signum                                      = fmap signum

-- Calculate  Matrix to the power of a value n
matrixPowerOf :: (Num a) => Matrix a -> Int -> Matrix a
matrixPowerOf mat n = mat ^ n

-- For fibonacci sequence, the default startup matrix is Matrix 1 1 1 0
matrixPower :: (Num a) => Int -> Matrix a
matrixPower = matrixPowerOf (Matrix 1 1 1 0)

-- Get a particular value from the Matrix
-- In this case the calue of intrest is at (1, 2)
getFromMatrix :: (Num a) => Matrix a -> a
getFromMatrix (Matrix _ x _ _) = x

-- Calculate the fibonacci number
fibonacci :: Int -> Integer
fibonacci n
    | n <= 0 = 1
    | otherwise = getFromMatrix $ matrixPower n

-- Get the Fibonacci sequence
fibsequence :: Int -> [Integer]
fibsequence n = fmap fibonacci [0 .. n]

-- λ> fibonacci 10
-- 55
-- λ> fibsequence 10
-- [1,1,1,2,3,5,8,13,21,34,55]

{-|
   Fast Doubling Algorithm for Fibonacci series (Fastest)

   Asymptotic Complexity = O(log n)

   Proof:

   From the previous matrix format, we have the below matrix equation

   Aᴷ =    [1 1]ᴷ = [fₖ₊₁  fₖ]  or  [f(k+1)  f(k) ]
           [1 0]    [fₖ  fₖ₋₁]      [f(k)   f(k-1)]

   Taking determinant on both sides of the above identity will give
   Cassini's Identity,

   (-1)ᴷ = fₖ₋₁.fₖ₊₁ - (fₖ)² or f(k-1).f(k+1) - (f(k))²

   Also we have the following matematical identities which will be used later
   Aᴷ = A      if K == 1
   Aᴷ = A.(A ᴾ)² if K is ODD, where P = (k-1)/2
   Aᴷ = (Aᴾ)²   if K is EVEN, where P = K/2

   A²ᴷ = Aᴷ.Aᴷ
   hence the following follows
   A²ᴷ = [f(2k+1)   f(2k) ] = [f(k+1)  f(k) ] X [f(k+1)  f(k) ]
         [f(2k)    f(2k-1)]   [f(k)   f(k-1)]   [f(k)   f(k-1)]

                            = [{f(k+1)}² + {f(k)}²      f(k)f(k+1) + f(k-1)f(k)]
                              [f(k)f(k+1) + f(k-1)f(k)     {f(k)}² + {f(k-1)}² ]

   Equating the LHS & RHS, we have the following equations
   f(2k+1) = {f(k+1)}² + {f(k)}²
   f(2k)   = f(k)f(k+1) + f(k-1)f(k)
   f(2k-1) = {f(k)}² + {f(k-1)}²

   since f(k+1) = f(k) + f(k-1)
         f(k)f(k+1) + f(k-1)f(k) = f(k){f(k-1) + f(k+1)}
                                 = f(k){f(k+1) -f(k) + f(k+1)}
                                 = f(k){2f(k+1) -f(k)}

   The final equations useful for fast fibonacci calculation are
   f(2k+1) = {f(k)}² + {f(k+1)}²
   f(2k)   = f(k){2f(k+1) - f(k)}
-}

fib :: Integer -> (Integer, Integer)
fib 0 = (0, 1)
fib n
    | even n = (f_2n, f_2n_plus_1)
    | odd n = (f_2n_plus_1, f_2n + f_2n_plus_1)
    where
    (f_n, f_n_plus_1) = fib (n `div` 2)
    f_n_sq = f_n * f_n
    f_n_plus_1_sq = f_n_plus_1 * f_n_plus_1
    f_2n = 2 * f_n * f_n_plus_1 - f_n_sq
    f_2n_plus_1 = f_n_sq + f_n_plus_1_sq

fastfib :: Integer -> Integer
fastfib num
    | num >= 0 = fst $ fib num
    | otherwise = 1
