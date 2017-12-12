-- Day 2 / Advent of Code 2017

-- For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.

-- see http://adventofcode.com/2017/day/2

-- Refactoring: Read from StdIn, Replace "\t" with " "

input = "5 1 9 5\n7 5 3\n2 4 6 8"
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

