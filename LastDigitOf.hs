module LastDigitOf where
    
lastDigit :: Integer -- ^ 
  -> Integer -- ^ 
  -> Integer
lastDigit a b = ((a `rem` 10) ^ ((b - 1) `rem` 4 + 1)) `rem` 10