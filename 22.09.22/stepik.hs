module Haskell04Intro where
import Data.Tuple

{-
Какому известному вам библиотечному оператору,
конструктору или функции эквивалентно выражение uncurry (flip const)?
-}
test1 = if snd (1,2) == uncurry (flip const) (1,2) then putStrLn "snd == uncurry (flip const)"
                                                   else putStrLn "Error"
{-
В модуле Data.Tuple стандартной библиотеки 
определена функция swap :: (a,b) -> (b,a), 
переставляющая местами элементы пары:
-}                                                   
test2 = if swap (1,2) == uncurry (flip (,)) (1,2) then putStrLn "answer: uncurry (flip (,))" 
                                                  else putStrLn "Error"
{-
Определите функцию вычисляющую двойной факториал,
то есть произведение натуральных чисел, 
не превосходящих заданного числа и имеющих ту же четность. 
Например: 7!!= 7 * 5 * 3 * 1.
-}
-- remind yourself factorial:
factorial n = if n >= 1 then n * factorial (n - 1) else 1 

doubleFact :: Integer -> Integer
doubleFact n = if n >= 1 then n * doubleFact (n - 2) else 1 

-- Реализуйте функцию, находящую элементы следующей рекуррентной последовательности
-- Implement a function that finds the of the elements following recurrent sequence.
-- b_0 = 1; b_1 = 2 ; b_2 = 3; b_{k+3} = b_{k+2} - 2 * b_{k+1} + 3 * b_{k}.

--seqP = [(k+2) - 2*(k+1) + k]
--seqS = 1 : 2 : 3 : map (\a -> (a+2)-(2*(a+1))+3*a) [4..]

--f = \a -> \b -> a + b
--fibonachi = 1 : scanl1 f [1..]

-- Hint:
fibn n = fibs !! n
 where
 fibs = 0 : 1 : map f [2..]
 f n = fibs !! (n-1) + fibs !! (n-2)
 
 
-- fibn' n | n > 0 = fibs !! n
        -- | n < 0 = fibs' !! n'
             -- where
             -- fibs = 0 : 1 : map f [2..]
             -- f n = fibs !! (n-1) + fibs !! (n-2)
             -- fibs' = 0 : (-1) : map f' [(-2), (-3)..]
             -- n' = (-n)
             -- f' n'  = fibs' !! (n' + 2) - fibs' !! (n' + 1)
 
seqN n = fibs !! n
 where
 fibs = 1 : 2 : 3 : map f [3..]
 f n = fibs !! (n-1) - 2 * fibs !! (n-2) + 3 * fibs !! (n-3)

 
seqB :: Integer -> Integer
seqB m = fibs !! n
 where
 n = fromInteger m :: Int
 fibs = 1 : 2 : 3 : map f [3..]
 f n = fibs !! (n-1) - 2 * fibs !! (n-2) + 3 * fibs !! (n-3)
-- ==========================================================
-- -1 - (-2) - (-3) = -1 + 2 = 1 - 
-- fibonacci :: Integer -> Integer
-- fibonacci 0 = 0
-- fibonacci 1 = 1
-- fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
-- fibonacci (-1) = 1
-- fibonacci (-2) = -1
-- fibonacci n | n < 0 = fibonacci (n + 2) - fibonacci(n + 1)

posFibs = 0 : 1 : zipWith (+) posFibs (tail posFibs)
negFibs = 0 : 1 : zipWith (-) negFibs (tail negFibs)

fibonacci n | (n >= 0)  = posFibs !!   n
            | otherwise = negFibs !! (-n)
-- ==========================================================
 
seqB' :: Integer -> Integer
seqB' n = go n (1,2,3)
  where
    go n (zero, one, two) | n == 0 = zero
                          | otherwise  = go (n - 1) (one, two, two - 2 * one + 3 * zero)
