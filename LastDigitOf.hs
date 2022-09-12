module LastDigitOf where

import Data.List()
import Data.Text (splitOn)

-- <func append> -> <empty unit> -> <base elem> -> <exponential> -> <res>
fastContAppendOp :: Integral a => (b -> b -> b) -> b -> b -> a -> b
fastContAppendOp _ e _ 0 = e
fastContAppendOp f e a k
    | mod k 2 == 1 = f a (fastContAppendOp f e a (k - 1))
    | otherwise = fastContAppendOp f e (f a a) (div k 2)

-- Optimized specified FCOODCMWDBAE algorithm on Monoid <N+, *> with mod operation
-- <base> -> <exp> -> <moder> -> <res>
fastExpModOpti :: Integral a => a -> a -> a -> a
fastExpModOpti a k m = fastContAppendOp (\a b -> mod (a * b) m) 1 a k

-- Problem 1193 Specified algorithm
-- <A> -> <M> -> <N> -> <res>
passwordAtNthDay :: Int -> Int -> Int -> Int
passwordAtNthDay a m n = mod (fastExpModOpti a n m) m

splitAndExtInt :: String -> [Int]
splitAndExtInt s = map read $ splitOn " " s

lastDigit :: Integer -> Integer -> Integer
lastDigit a b = fastExpModOpti a b 10
