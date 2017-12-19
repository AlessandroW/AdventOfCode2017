import Data.List

main :: IO()
main = do
  print "Solution fst puzzle"
  print (sum (map compareSortedList (map qsort (map words (lines test)))))
  print "Solution snd puzzle"
  print (sum (map compareSortedList (map qsort (map sortString (map words (lines test))))))

qsort :: [String] -> [String]
qsort []= []
qsort (x:xs) = qsort small ++ mid ++ qsort large
  where
    small = [y | y<-xs, y<x]
    mid   = [y | y<-xs, y==x] ++ [x]
    large = [y | y<-xs, y>x]

qsort' :: [Char] -> [Char]
qsort' []= []
qsort' (x:xs) = qsort' small ++ mid ++ qsort' large
  where
    small = [y | y<-xs, y<x]
    mid   = [y | y<-xs, y==x] ++ [x]
    large = [y | y<-xs, y>x]

sortString :: [String] -> [String]
sortString list = map qsort' list


-- countDuplicates :: [String] -> Integer -> Integer
-- countDuplicates sum map 

-- compare :: [String] -> Integer
-- compare list = do
--   let cmp = tail list
-- variant of map that passes each element's index as a second argument to f
compareSortedList :: [String] -> Integer
compareSortedList list = if length list < 2
                   then 1
                   else if((list !! 0) == (list !! 1))
                        then 0
                        else (compareSortedList (tail list))
  
test :: String
test = "aaa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa"
