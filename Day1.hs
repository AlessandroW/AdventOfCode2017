-- DAY 1
-- The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.

-- For example:

--     1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
--     1111 produces 4 because each digit (all 1) matches the next.
--     1234 produces 0 because no digit matches the next.
--     91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

-- Convert an Integer to a List of digits
toDigits:: Integer -> [Integer]
toDigits x = if div x 10 == 0 then (mod x 10) : []
                     else (toDigits(div x 10)) ++ [(mod x 10)]

-- Removes the nth element of a list
removeNth:: Int -> [Integer] -> [Integer]
removeNth n list = fst (splitAt n list) ++ tail (snd (splitAt n list))
                          
-- Removes the nth number if the next number doesn't match
reduce::  Int -> [Integer] ->[Integer]
reduce n list = if (n < ((length list)-1)) then
                  if (list !! n == list !! (n+1)) then reduce (n+1) list
                  else reduce n (removeNth n list)
                else list

-- Compares the first and last element of a list (circular condition)
compareEnd:: [Integer] -> Bool
compareEnd list = ((length list == 1) || ((list !! 0) /= (list !! ((length list) - 1))))

-- Removes the digits of a list which don't fulfill the initial condition
removeSingles:: [Integer] -> [Integer]
removeSingles list = if (compareEnd list) then (init (reduce 0 list)) else (reduce 0 list)

-- Summates the Digits of a list
sumList :: [Integer] -> Integer
sumList list = if (length list) > 1 then (list !! 0) + (sumList (tail list))
               else
                 if (length list) == 1 then (list !! 0)
                 else 0

-- Solves the given captcha
solveCaptcha :: Integer -> Integer
solveCaptcha n = sumList(removeSingles(toDigits(n)))

-- Returns the next length/2 th elemnt
getNextElement :: [Integer]-> Int -> Integer
getNextElement list n = list !! (mod (n + (div (length list) 2)) (length list))

evaluateElement :: [Integer]-> Int -> Integer
evaluateElement list n = if (list !! n) == (getNextElement list n) then (list !! n)
                         else 0
  
evaluateList :: [Integer]-> Int -> [Integer]
evaluateList input n = if ((length input) == 0 || (length input) == n) then []
                      else (evaluateElement input n) : (evaluateList input (n+1))

solveSecondPuzzle :: Integer -> Integer
solveSecondPuzzle number = sum(evaluateList (toDigits number) 0)
