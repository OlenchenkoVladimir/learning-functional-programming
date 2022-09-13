module LastDigit (lastDigit) where
-- Last digit of a huge number

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit as = getLastDigit $ numLardg as

numLardg :: Integral b => [b] -> b
numLardg  = foldr (^) 1

getLastDigit :: Integer -> Integer
getLastDigit num
    | num < 0 = ((-1) * num) `rem` 10
    | otherwise = num `rem` 10

{-
Test Results:
Execution Timed Out
STDERR
Execution Timed Out (12000 ms)
Why did my code time out?
Our servers are configured to only allow a certain amount of time 
for your code to execute. 
In rare cases the server may be taking on too much work 
and simply wasn't able to run your code efficiently enough. 
Most of the time though this issue is 
caused by inefficient algorithms. 
If you see this error multiple times you should 
try to optimize your code further.
-}


{-

For a given list [x1, x2, x3, ..., xn] compute the last 
(decimal) digit of x1 ^ (x2 ^ (x3 ^ (... ^ xn))).

E. g., with the input [3, 4, 2], your code should return 1 
because 3 ^ (4 ^ 2) = 3 ^ 16 = 43046721.

Beware: powers grow incredibly fast. For example, 
9 ^ (9 ^ 9) has more than 369 millions of digits. 
lastDigit has to deal with such numbers efficiently.

Corner cases: we assume that 0 ^ 0 = 1 and that 
lastDigit of an empty list equals to 1.

This kata generalizes Last digit of a large number; 
you may find useful to solve it beforehand.

-}
