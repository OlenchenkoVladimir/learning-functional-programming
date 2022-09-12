{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module LastDigitOf where
import Data.List (unfoldr)


lastDigitOf :: Integer -> Integer -> Integer
lastDigitOf 0 0 = 1
lastDigitOf 0 1 = 0
lastDigitOf x y = last $ digs $ x ^ y


digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

digs' :: Integer -> [Integer]
digs' = unfoldr (\b -> if b == 0
        then Nothing
        else Just (b `mod` 10,
                  b `div` 10))
diggg :: Integral b => b -> [b]
diggg x = scanl mod x []
