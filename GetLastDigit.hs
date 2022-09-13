module GetLastDigit (getLastDigit) where

{- Return the last digit of a number -}
getLastDigit :: Integer -> Integer
getLastDigit num
    | num < 0 = ((-1) * num) `rem` 10
    | otherwise = num `rem` 10