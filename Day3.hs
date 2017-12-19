main = do
  print "Enter Number"
  input <- getLine
  let x = read input ::Integer
  print (solve x)

getRing :: Integer-> Integer -> Integer
getRing x n = if ((n^2) < x || (mod n 2 == 0)) then getRing x (n+1) else n

getHeight :: Integer -> Integer
getHeight x = div (getRing x 1) 2

getWidth :: Integer -> Integer
getWidth x = abs((getRelativeMid x) - x)
  
solve :: Integer -> Integer
solve n = (getHeight n) + (getWidth n)

-- Problem: Get correct width!
getRelativeMid :: Integer -> Integer
getRelativeMid x = (getCorner x 3) - (div (getRing x 1) 2)

  
getCorner :: Integer -> Integer -> Integer
getCorner x s = do -- Number Ring Side
  let n = getRing x 1
  let corner = ((n^2) - ((s)*(n-1))) 
  if x  <= corner then corner
    else getCorner x (s-1)

getDistance :: Integer -> Integer
getDistance x = (getWidth x) + (getHeight x)

-- Part II

-- returns the bottom right corner (biggest number of a ring)
getCornerBelow :: Integer -> Integer
getCornerBelow x = ((getRing x 1) - 2)^2

-- returns the nth corner (0-3)
getNthCorner :: Integer -> Integer -> Integer
getNthCorner x n = do
  let width = getRing x 1
  if (n < 0) || (n >3) then -1
  else width^2 - n*(width-1)

getCornerNumber :: Integer -> Integer
getCornerNumber x = do 
  let ring = getRing x 1
  div (ring^2 - (getCorner x 3)) (ring -1)

-- returns the index below in the rectangle
getBelow :: Integer -> Integer
getBelow x = do
  let width = (getCorner x 3) - x
  let cornerBelow = getNthCorner (getCornerBelow x) (getCornerNumber x)
  if x <= 1 then 0
  else 
    if (x-1) == (getCornerBelow x) then getBelow (x+1)
    else 
        if (width < 2) then cornerBelow
        else cornerBelow - (width -1)
  
-- returns the next number for the given list
getNext :: [Integer] -> Integer
getNext f = do
  let _x = length f
  let x = toInteger (_x+1)
  let corner = getCorner x 3
  let _below = fromIntegral ((getBelow x) -1)
  let fBelow = below f x
  let fBefore = f !! (_x-1)
  if (x-1) == (maxRing (x-1)) 
    then (fBefore + (below f (x+1))) -- first of a new ring, special case
    else if x == (maxRing (x)) -- last of a new ring, special case
         then fBefore + fBelow + (f !! (_below +1)) 
         else if x == corner
              then fBefore + fBelow
              else if x == ((getCornerBelow x)+2) -- index special case second of new ring
                   then fBelow + fBefore + (f !! (_x -2)) + (f!! (_below +1)) 
                   else if x == (corner -1) -- before corner
                        then if x == (maxRing (x) -1)
                             then fBelow + fBefore + (f!! (_below -1)) + (f!! (_below +1)) 
                             else fBelow + fBefore + (f!! (_below -1)) 
                        else if x == ((getCorner (x-1) 3)+1) -- first after corner
                             then fBelow + fBefore + (f !! (_x -2)) + (f!! (_below +1)) 
                             else fBelow + fBefore + (f !! (_below -1)) + (f!! (_below +1)) 
  
-- hard cording first cases for avoiding more cases
start :: [Integer]  
start = [1,1,2,4,5,10,11,23]
  
-- returns the nth number in the series
get :: [Integer] -> Int -> Integer
get f n = do
  if n < length f then f !! (n-1)
  else get (f ++ ((getNext f) : [])) n

-- returns the number below in the rectangle
below :: [Integer] -> Integer -> Integer
below field index = do
  let i = fromIntegral ((getBelow index) - 1)
  if i >= length field  then 0
  else field !! i 

maxRing :: Integer -> Integer
maxRing x = ((getRing x 1)^2)

-- returns the first value bigger than n
solveSecond :: Integer -> Int -> Integer
solveSecond n index = do
  let solution = get start index
  if solution > n then solution
  else solveSecond n (index +1)
