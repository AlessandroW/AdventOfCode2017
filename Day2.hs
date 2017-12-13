-- Day 2 / Advent of Code 2017

-- see http://adventofcode.com/2017/day/2

-- Refactoring: Read from StdIn, Replace "\t" with " "
import Data.List

input :: String -- Part I
input = "5 1 9 5\n7 5 3\n2 4 6 8"

input2 :: [[Integer]] -- Part II
input2 = [[5,9,2,8],[9,4, 7, 3],[3,8,6,5]]

main :: IO()
main = print (sumMatrix (map words (lines input)))

-- Converts a String to an Integer
makeInteger :: String -> Integer
makeInteger string = read string :: Integer

-- Summates the results of each row
sumMatrix :: [[String]]->Integer
sumMatrix matrix = if (length matrix)==1 then sumRow(map makeInteger (matrix !! 0))
                   else (sumRow(map makeInteger (matrix !! 0)) + sumMatrix (tail matrix))

-- Calculates the Difference of max and min
sumRow :: [Integer]-> Integer
sumRow list = (maximum list) - (minimum list)


-- Finds the only two numbers where one evenly divides the other, divides them, and adds up
-- e.g.[5,9,2,8] = []
compareOthers :: [Integer] -> Integer
compareOthers list = sum (map checkDivision (filter (\x -> length x == 2) (subsequences  list)))

-- Checks if digits evenly divide each other
checkDivision :: [Integer] -> Integer
checkDivision list =  if (list !! 0 ) == (list !! 1) then 1
                      else (divide list) + (divide (reverse list))

divide :: [Integer] -> Integer
divide list = if ((mod (list !! 0) (list !! 1)) == 0) && (length list) == 2 then div (list !! 0) (list !! 1)
              else 0
 
solveSecond :: [[Integer]] -> Integer
solveSecond matrix = sum(map compareOthers matrix)


