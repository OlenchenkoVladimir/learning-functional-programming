module RichardBird where
-- Pearls of Functional Algorithm
-- Design
-- Наименьшее отсутствующее число
-- Решение с использованием массива

-- Определим спецификацию задачи с помощью функции minfree:
-- An array-based solution
-- The problem can be specified as a function minfree, defined by
-- minfree :: [Nat] → Nat
-- minfree xs = head ([0 .. ] \\ xs)
-- The expression us \\ vs denotes the list of those elements of us that remain
-- after removing any elements in vs:
-- (\\) :: Eq a ⇒ [a] → [a] → [a]
-- us \\ vs = filter (∈ vs) us
-- minfree    :: [Nat] -> Nat
-- minfree xs =  head ([0..] \\ xs)

-- infixl 2

-- (\\)     :: Eq a => [a] -> [a] -> [a]
-- us \\ vs =  filter (not vs) us

-- The following program for checklist makes use of the
-- library Data.Array.ST:
checklist xs = runSTArray (do
                {a ← newArray (0, n) False;
                sequence [writeArray a x True | x ← xs, x ≤ n];
                return a})
                where n = length xs
                
-- A divide and conquer solution
minfree xs = minfrom 0 (length xs, xs)
minfrom a (n, xs) | n == 0 = a
                  | m == b − a = minfrom b (n − m, vs)
                  | otherwise  = minfrom a (m, us)
                    where (us, vs) = partition (< b) xs
                          b        = a + 1 + n div 2
                          m        = length us