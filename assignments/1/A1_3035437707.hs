

-- Question 1 

solve :: Int -> Int -> [Int]
solve n m = createList (n `div` m) (n `rem` m) m 
  where 
    createList :: Int -> Int -> Int -> [Int]
    createList _ 0 0       = []
    createList value rem m = if rem == 0 then value : createList value 0 (m-1) else value + 1 : createList value (rem-1) (m-1)

-- Question 2

ismember :: Eq a =>  a -> [a] -> Bool
ismember _ [] = False
ismember a (x:xs) = if a == x then True else ismember a xs

-- Question 3

checkHappyDigits :: [Int] -> Int -> Int
checkHappyDigits xs n = count xs n 0
  where 
    count ::  [Int] -> Int -> Int -> Int
    count [] _ result = result
    count (x:xs) n result = if compare x n 0 then count xs n result  else count xs n (result+1)
      where
        compare :: Int -> Int -> Int -> Bool
        compare 0 n h = if n < h then True else False
        compare x n h = if x `mod` 10 == 4 || x `mod` 10 == 7 then compare (x `div` 10) n (h+1) else compare (x `div` 10) n h

-- Question 4

-- Common method to caluclate number of BST trees with n keys using facotiral 
-- fromIntegral converts back Integers which were created using toInteger

numBST :: Int -> Int
numBST n = fromIntegral ( (calculate n) `mod` (10^9+7))
  where
    calculate :: Int -> Integer
    calculate n = ((factorial (toInteger (n*2))) `div` (multiply n))
      where 
        multiply :: Int -> Integer
        multiply n = factorial (toInteger (n+1)) * factorial (toInteger (n))

factorial :: Integer -> Integer
factorial n
 | n < 0     = error "Invalid Input"
 | n < 2     = 1
 | otherwise = factorial (n-1) * (n)

-- Question 5

-- Sorting using quick sort but in reverse
-- returning (x,x) accordingly

pointIn :: [Int] -> Int -> [Int]
pointIn xs k = if checkValidInput xs k then pointIn' (quickSort xs) k else [-1]
  where
    pointIn' :: [Int] -> Int -> [Int]
    pointIn' (x:_) 0 =  [x+1,x+1]
    pointIn' (x:xs) k = if k > 1 then pointIn' xs (k-1) else [x,x]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x : xs) = quickSort larger ++ [x] ++ quickSort smaller
  where
    smaller = filter (<= x) xs
    larger = filter (> x) xs

checkValidInput :: [Int] -> Int -> Bool
checkValidInput [] _ = True
checkValidInput (x:xs) k = if x >= 1 && k >= 0 then checkValidInput xs k else False

-- Question 6

-- List comprehension with even odd

isTwinPaired :: [Int] -> Bool
isTwinPaired xs = check [ x | x <- xs, even x] 0 && check [ x | x <- xs, odd x] 1
  where
    check :: [Int] -> Int -> Bool
    check [] _ = True
    check (_:[]) _ = True
    check (x:y:xs) k = if k == 1 then if x >= y then check (y:xs) k else False else if x <= y then check (y:xs) k else False

-- Question 7

-- Similar trick to N Queens
-- check Diagnol ( x - y == x' - y' with recursion) & L Shape ( checks 1 row, 2 columns ) looks for conflict

chess :: Int -> Int
chess n = if checkValidInput n then checkKnight [1 .. n] [] 1 else error "Please enter in range [8, 15)"
  where
    checkValidInput :: Int -> Bool
    checkValidInput n = if n >= 8 && n < 15 then True else False
    checkKnight ::  [Int] -> [(Int, Int)] -> Int -> Int
    checkKnight [] _ _ = 1
    checkKnight  xs list index = sum ( dfs  xs list index)
      where
        dfs ::  [Int] -> [(Int, Int)]  -> Int -> [Int]
        dfs  xs list index = map (\(a, b) -> checkKnight  ([ o | o <- xs, o /= b ]) ((a, b) : list) (index + 1)) ( check xs list index )
          where
            check ::  [Int] -> [(Int, Int)] -> Int -> [(Int, Int)]
            check  xs list index = [ (index, x) | x <- xs  , not (checkDiagonal (index, x) list), not (checkColNRow (index, x) list), not (checkLShape (index, x) list)]
              where
                checkColNRow ::    (Int, Int) -> [(Int, Int)] -> Bool
                checkColNRow _ [] = False
                checkColNRow  (x', y') ((x, y) : xys) = if x' /= x || y' /= y then (checkColNRow  (x', y') xys)  else True
                checkDiagonal ::    (Int, Int) -> [(Int, Int)] -> Bool
                checkDiagonal _ [] = False
                checkDiagonal  (x', y') ((x, y) : xys) = if abs (x' - x) /= abs (y' - y) then (checkDiagonal  (x', y') xys)  else True
                checkLShape :: (Int, Int) -> [(Int, Int)] -> Bool
                checkLShape _ [] = False
                checkLShape (x, y) [(x', y')] = if abs (y - y') /= 2 then False else True
                checkLShape (x, y) ((x', y'):(x'', y''): xys) = if abs (y - y') /= 2 && abs (y - y'') /= 1 then False else True