module Kodable (
    kodable,
    charPosition
    ) where

import Common 
import Solvable
import Play
import Solution

import Control.Monad.State
import Control.Monad

import Data.List
import Data.Char

import Text.Printf
import System.IO

import System.Directory

kodable :: IO ()
kodable = do
            putStr "Select File: "
            allFiles <- getMaps
            putAvailableMaps allFiles
            putStrLn ""
            filename <- getMapFile
            case filename `elem` allFiles of 
                False -> do
                            putStrLn "Invalid File."
                            kodable
                True -> putStr ""
            file <- readFile filename
            let map = lines file
            printMap map
            case solvable map (charPosition 0 '@' map) (charPosition 0 't' map) of 
                True -> putStrLn "This map is SOLVABLE!"
                False -> putStrLn "This map is NOT SOLVABLE. I suggest you to load a new map."
            options map
            return ()
            where 
                options :: [String] -> IO() 
                options map = do   
                            putStrLn "" 
                            putStrLn "Please Choose:"
                            putStrLn "play          -> Play without function."
                            putStrLn "play _ _ _    -> Play with function."
                            putStrLn "solvable      -> Checks if the map is solvable."
                            putStrLn "bonus         -> Checks if player -> (all bonus) reachable AND (all bonus) -> target reachable."
                            putStrLn "solution      -> Get an optimal solution to the loaded map."
                            putStrLn "load          -> Load new map."
                            putStrLn "quit          -> To Quit the game."
                            putStr "Your choice: "
                            choice <- getLine
                            putStrLn ""
                            case choice of
                                "solvable" ->do case (solvable map (charPosition 0 '@' map) (charPosition 0 't' map)) of
                                                    True -> do
                                                                putStrLn "This map is solvable!"
                                                            
                                                    False -> do
                                                                putStrLn "This map is not solvable."
                                                options map
                                "bonus" -> do
                                                case length (bonusNotReachable map (charPosition 0 '@' map))  == 0 of
                                                                True -> putStrLn "All bonus reachable!"
                                                                False -> putStrLn ((show (length (bonusNotReachable map (charPosition 0 '@' map)))) ++ " bonus not reachable.")
                                                case length (bonusToTargetNotReachable map (charPosition 0 't' map))  == 0 of
                                                                True -> putStrLn "Target reachable from all bonuses!"
                                                                False -> putStrLn ("Target not reachable from " ++ (show (length (bonusToTargetNotReachable map (charPosition 0 't' map)))) ++ " bonus.")
                                                options map
                                "quit"  -> return ()
                                "load"  -> kodable
                                "play"  -> do
                                                play map []
                                                options map
                                "solution" -> do
                                                putStr "Solution without Function and Loops: "
                                                putStrLn (printList (solution map (charPosition 0 '@' map) ((length (bonusReachable map (charPosition 0 '@' map))) - (length (bonusToTargetNotReachable map (charPosition 0 't' map))))  ))
                                                putStr "Compressed: "
                                                putStr (printList (compressSolution (solution map (charPosition 0 '@' map) ((length (bonusReachable map (charPosition 0 '@' map))) - (length (bonusToTargetNotReachable map (charPosition 0 't' map))))  )))
                                                putStrLn ""
                                                options map
                                                
                                str     -> do
                                                playFunc map str
                                                options map
                

putAvailableMaps :: [String] -> IO()
putAvailableMaps [] = return ()
putAvailableMaps (t:ts) = do    putStr (t ++ " ")
                                putAvailableMaps ts

getMaps :: IO [String]
getMaps = do 
            all <- getDirectoryContents "./"
            let filtered = filter (isSuffixOf "-2.txt") all
            return filtered


-- explore :: IO ()
-- explore = do 
--             filename <- getMapFile
--             file <- readFile filename
--             let linesOfFile = lines file
--             let (x, y) = charPosition 0 '@' linesOfFile
--             printMap linesOfFile
--             putStrLn "Valid Directions: Left, Right, Up, Down"
--             explore' linesOfFile (x, y)
--             where 
--                 explore' map position = do 
--                     putStr "Direction: "
--                     direction <- getLine
--                     case length direction of 
--                         0 -> putPosition position
--                         n -> do 
--                                 let newPosition = movePlayer map position direction 'n'
--                                 putPosition newPosition
--                                 printMap (updateMap (replaceDash map (charPosition 0 '@' map)) newPosition)
--                                 explore' map newPosition

-- getMapFile :: IO String
-- getMapFile = putStr "Please load a map: " >> getLine

-- outputMap :: String -> IO ()
-- outputMap fileContents = mapM_ putStrLn $ lines fileContents

-- printMap :: [String] -> IO ()
-- printMap []    = return ()
-- printMap (m:ms) = do putStrLn m
--                      printMap ms
                     

-- putPosition :: (Int, Int) -> IO()
-- putPosition (x, y) =   do   putStr "("
--                             putStr (show x)
--                             putStr ","
--                             putStr (show (y `div` 2))
--                             putStrLn ")"

-- updateMap :: [String] -> (Int, Int) -> [String]
-- updateMap map (x,y) = helperX map 0 (x,y)
--     where 
--         helperX :: [String] -> Int -> (Int, Int) -> [String]
--         helperX (m: ms) count (x,y) = if count == x then ((helperY m 0 y): ms) else (m:(helperX ms (count + 1) (x,y)))
--         helperY :: String -> Int -> Int -> String
--         helperY (x: xs) count y = if count == y then ('@':xs) else (x:(helperY xs (count+1) y))

-- replaceDash :: [String] -> (Int, Int) -> [String]
-- replaceDash map (x,y) = helper map (x,y)
--     where   helper :: [String] -> (Int, Int) -> [String]
--             helper map (x, y) = helperX map 0 (x, y)
--                 where   helperX :: [String] -> Int -> (Int, Int) -> [String]
--                         helperX (m: ms) count (x,y) = if count == x then ((helperY m 0 y): ms) else (m:(helperX ms (count + 1) (x,y)))
--                         helperY :: String -> Int -> Int -> String
--                         helperY (x: xs) count y = if count == y then ('-':xs) else (x:(helperY xs (count+1) y))

-- charPosition :: Int -> Char -> [String] -> (Int, Int)
-- charPosition x c [] = (-1,-1)
-- charPosition x c (rx: rxs) =  if (charPositionHelp 0 c rx) > -1 then (x, charPositionHelp 0 c rx) else charPosition (x+1) c rxs
--     where   charPositionHelp :: Int -> Char -> String -> Int
--             charPositionHelp y c [] = -1
--             charPositionHelp y c (r : rs) =  if r == c then y else  charPositionHelp (y+1) c rs 

-- playerPos :: Int -> [String] -> (Int, Int)
-- playerPos x [] = (-1,-1)
-- playerPos x (rx: rxs) =  if (playerPosHelp 0 rx) > -1 then (x, playerPosHelp 0 rx) else playerPos (x+1) rxs
--     where   playerPosHelp :: Int -> String -> Int
--             playerPosHelp y [] = -1
--             playerPosHelp y (r : rs) =  if r == '@' then y else  playerPosHelp (y+1) rs 

-- left :: [String] -> Char -> (Int, Int) -> (Int, Int)
-- left map c (x, y) = if y - 2 >= 0 then 
--                                     if (map !! x) !! (y-2) /= '-' then (if (map !! x) !! (y-2) == '*' then (x, y)
--                                                                         else (if c == ((map !! x) !! (y-2)) then (x, y-2)
--                                                                                 else left map c (x, y-2) ))
--                                     else left map c (x, y-2)
--                                 else (x, y) 

-- -- please calculate board dimensions
-- right :: [String] -> Char -> (Int, Int) -> (Int, Int)
-- right map c (x, y) = if y + 2 <= (length (map !! 0) -1) then 
--                                     if (map !! x) !! (y+2) /= '-' then (if (map !! x) !! (y+2) == '*' then (x, y)
--                                                                         else (if c == ((map !! x) !! (y+2)) then (x, y+2)
--                                                                                 else right map c (x, y+2) ))
--                                     else right map c (x, y+2)
--                                 else (x, y)

-- up :: [String] -> Char -> (Int, Int) -> (Int, Int)
-- up map c (x, y) = if x - 1 >= 0 then 
--                                     if (map !! (x-1)) !! y /= '-' then (if (map !! (x-1)) !! y == '*' then (x, y)
--                                                                         else (if c == ((map !! (x-1)) !! y) then (x-1, y)
--                                                                                 else up map c (x-1, y) ))
--                                     else up map c (x-1, y)
--                                 else (x, y)

-- -- please calculate board dimensions
-- down :: [String] -> Char -> (Int, Int) -> (Int, Int)
-- down map c (x, y) = if x + 1 <= ((length map) -1) then 
--                                     if (map !! (x+1)) !! y /= '-' then (if (map !! (x+1)) !! y == '*' then (x, y)
--                                                                         else (if c == ((map !! (x+1)) !! y) then (x+1, y)
--                                                                                 else down map c (x+1, y) ))
--                                     else down map c (x+1, y)
--                                 else (x, y)

-- movePlayer :: [String] -> (Int, Int) -> String -> Char -> (Int, Int)
-- movePlayer map position move nextMove
--     | move == "L" = left map nextMove position
--     | move == "R" = right map nextMove position
--     | move == "D" = down map nextMove position
--     | move == "U" = up map nextMove position
--     | otherwise = position

-- updateConditional :: String -> String
-- updateConditional m =
--             if (take 1 (drop 5 m)) `elem` ["p","o","y"] then 
--                 ( if take (length (drop 8 m) - 1) (drop 8 m) == "Right" then ('R' : (take 1 (drop 5 m)))
--                 else if take (length (drop 8 m) - 1) (drop 8 m) == "Left" then ('L' : (take 1 (drop 5 m)))
--                 else if take (length (drop 8 m) - 1) (drop 8 m) == "Up" then ('U' : (take 1 (drop 5 m)))
--                 else if take (length (drop 8 m) - 1) (drop 8 m) == "Down" then ('D' : (take 1 (drop 5 m)))
--                 else  "Invalid" )
--             else "Invalid"

-- extractMoves :: String -> [String]
-- extractMoves move = helperExtract move ""
--     where
--         helperExtract :: String -> String -> [String]
--         helperExtract [] str = [str]
--         helperExtract (m:move) str = if m /= ',' then (helperExtract move (str ++ [m])) else (str : helperExtract move "")

-- expandLoop :: String -> [String] -> [String]
-- expandLoop moves funcMoves = expandList (loopHelper moves (read (take 1 (drop 5 moves))::Int)) funcMoves
--     where 
--         loopHelper :: String -> Int -> [String]
--         loopHelper moves 0 = []
--         loopHelper moves count = if count > 5 || length (extractMoves (take (length (drop 8 moves)-1) (drop 8 moves))) > 2 
--                                     then ["Invalid"]
--                                  else (extractMoves (take (length (drop 8 moves)-1) (drop 8 moves))) ++ (loopHelper moves (count-1))

-- expandList :: [String] -> [String] -> [String] 
-- expandList [] funcMoves = []
-- expandList (m:moves) funcMoves
--             | take 8 m == "Function" = expandList ( funcMoves ++ moves ) funcMoves
--             | take 4 m == "Loop" = (expandLoop m funcMoves) ++ expandList moves funcMoves
--             | take 4 m == "Cond" = ((updateConditional m) : expandList moves funcMoves)
--             | m == "Left" = ("Ln" : (expandList moves funcMoves))
--             | m == "Right" = ("Rn" : (expandList moves funcMoves))
--             | m == "Up" = ("Un" : (expandList moves funcMoves))
--             | m == "Down" = ("Dn" : (expandList moves funcMoves))
--             | otherwise = ["Invalid"]

-- checkValidInput :: [String] -> Bool
-- checkValidInput [] = True
-- checkValidInput (m:moves) = if m == "Invalid" then False else checkValidInput moves 

-- findBonus :: [String] -> (Int, Int) -> (Int, Int) -> [String]
-- findBonus map (cx, cy) (nx, ny) =   if cx == nx then
--                                         (if cy > ny then (
--                                             if (helperBonusY map (cx, cy) (nx, ny)) /= (-1,-1) 
--                                                 then (replaceDash map (helperBonusY map (cx, cy) (nx, ny)))
--                                             else map )
--                                         else if cy < ny then ( 
--                                             if (helperBonusY map (nx, ny) (cx, cy)) /= (-1,-1) 
--                                                 then (replaceDash map (helperBonusY map (nx, ny) (cx, cy)))
--                                             else map )
--                                         else map)
--                                     else if cy == ny then 
--                                         (if cx > nx then ( 
--                                             if (helperBonusX map (cx, cy) (nx, ny)) /= (-1,-1) 
--                                                 then (replaceDash map (helperBonusX map (cx, cy) (nx, ny)))
--                                             else map )
--                                         else if cx < nx then ( 
--                                             if (helperBonusX map (nx, ny) (cx, cy)) /= (-1,-1) 
--                                                 then (replaceDash map (helperBonusX map (nx, ny) (cx, cy)))
--                                             else map )
--                                         else map)
--                                     else map
--         where -- point x y and max x y
--             helperBonusY :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
--             helperBonusY map (px, py) (mx, my) = if py > my then
--                                                     if ((map !! px) !! (py-1)) == 'b' then (px, py-1) else helperBonusY map (px,py-1) (mx, my)
--                                                 else (-1, -1)
--             helperBonusX :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
--             helperBonusX map (px, py) (mx, my) = if px > mx then
--                                                     if ((map !! (px-1)) !! py) == 'b' then (px-1, py) else helperBonusX map (px-1,py) (mx, my)
--                                                 else (-1, -1)

-- printMaps :: [(Int, Int)] -> [String] -> IO() 
-- printMaps [] map = return ()
-- printMaps (l:[]) map = printMap (updateMap (replaceDash map (charPosition 0 '@' map)) l)
-- printMaps (l:n:list) map = do
--                              printMap (updateMap (replaceDash map (charPosition 0 '@' map)) l)
--                              putStrLn ""
--                              let bonusMap = findBonus map l n
--                              case bonusMap == map of 
--                                  True -> putStr ""
--                                  False -> putStrLn "Bonus Collected!"
--                              printMaps (n:list) (findBonus map l n)

-- putPositions :: [(Int, Int)] -> IO()
-- putPositions [] = return ()
-- putPositions (l:list) = do
--                              putPosition l
--                              putPositions list 

-- play :: [String] -> [String] -> IO()
-- play map funcMoves = do 
--         putStrLn "Valid Directions: Left, Right, Up, Down"
--         putStrLn "Valid Moves: Cond{block}{Direction}, Loop{iterations}{Direction, Direction}, Function"
--         putStr "First Move: "
--         move <- getLine
--         moves <- playHelp [move]
--         let newList = expandList moves funcMoves
--         case checkValidInput newList of 
--             True -> do
--                     let list = (implementMove map (charPosition 0 '@' map) (expandList moves funcMoves))
--                     printMaps list map
--                     case (list !! (length list - 1)) == (charPosition 0 't' map) of 
--                         True -> putStrLn "Congratulations! Map succesfully completed."
--                         False -> do 
--                                  putStrLn "Sorry! You did not reach the target."
--             False -> putStrLn "Invalid Input"
--         where
--             playHelp :: [String] -> IO [String]
--             playHelp moveList = do  
--                             putStr "Next Move: "
--                             move <- getLine
--                             case length move of 
--                                 0 -> return moveList
--                                 m -> playHelp (moveList ++ [move])

--             implementMove :: [String] -> (Int, Int) -> [String] -> [(Int, Int)]
--             implementMove map (x,y) [] = [(x,y)]
--             implementMove map (x,y) (current:[]) = ((movePlayer map (x,y) (take 1 current) 'n') : (implementMove map (movePlayer map (x,y) (take 1 current) 'n') []))
--             implementMove map (x,y) (current:next:moves) = ((movePlayer map (x,y) (take 1 current) ((tail next) !! 0)) : (implementMove map (movePlayer map (x,y) (take 1 current) ((tail next) !! 0)) (next:moves)))

-- extractMovesFunc :: String -> [String]
-- extractMovesFunc move = helperExtract move ""
--     where
--         helperExtract :: String -> String -> [String]
--         helperExtract [] str = [str]
--         helperExtract (m:move) str = if m /= ' ' then (helperExtract move (str ++ [m])) else (str : helperExtract move "")

-- playFunc :: [String] -> String -> IO()
-- playFunc map line = do 
--                         case (take 5 line) == "play " of
--                             True -> do 
--                                     let funcMoves = (extractMovesFunc (drop 5 line))
--                                     case length funcMoves == 3 && (substring "Loop" line) of
--                                         True -> do 
--                                                     play map funcMoves
--                                         False -> putStrLn "Only 3 moves allowed & Loops are not allowed in function defination!"      
--                             False -> putStrLn "Invalid Choice"
















----------------------------------------------------------------------------------------------------------------------------


-- play :: [String] -> IO()
-- play map = do 
--         putStrLn "First Move:"
--         move <- getLine
--         moves <- playHelp [move]
--         let newList = expandList moves
--         case checkValidInput newList of 
--             True -> do
--                     let newPosition = (implementMove map (playerPos 0 map) (expandList moves))
--                     putPosition newPosition
--                     printMap (updateMap (replaceDash map) newPosition)
--             False -> putStrLn "Invalid Input"
--         --putPosition (implementMove map (playerPos 0 map) (expandList moves))
--         where 
--             playHelp :: [String] -> IO [String]
--             playHelp moveList = do  
--                             putStrLn "Next Move: "
--                             move <- getLine
--                             case length move of 
--                                 0 -> return moveList
--                                 m -> playHelp (moveList ++ [move])

--             implementMove :: [String] -> (Int, Int) -> [String] -> (Int, Int)
--             implementMove map (x,y) [] = (x,y)
--             implementMove map (x,y) (current:[]) = implementMove map (movePlayer map (x,y) (take 1 current) 'n') []
--             implementMove map (x,y) (current:next:moves) = implementMove map (movePlayer map (x,y) (take 1 current) ((tail next) !! 0)) (next:moves)

-- -- putStr "Direction: "
-- -- direction <- getLine
-- putPosition (playerPos 0 map)
-- let newPosition = loop map (playerPos 0 map) 2 "Right" "Up"
-- putPosition newPosition
-- printMap (updateMap (replaceDash map) newPosition)


-- parseMoves :: [String] -> [String]
-- parseMoves (m :moves) = parseMove m 
--     where 
--         checkSize  


-- convertMove :: [String] -> (Int, Int) -> String -> (Int, Int)
-- convertMove map (x,y) nextMove 

-- play :: String -> String -> String ->  IO()
-- play m1 m2 m3 = do putStr "Hello"
                    

-- kodable :: IO ()
-- kodable = do 
--             filename <- getMapFile
--             file <- readFile filename
--             let map = lines file
--             printMap map
--             -- putStr "Direction: "
--             -- direction <- getLine
--             putPosition (playerPos 0 map)
--             let newPosition = loop map (playerPos 0 map) 2 "Right" "Up"
--             putPosition newPosition
--             printMap (updateMap (replaceDash map) newPosition)

            -- putPosition (movePlayer linesOfFile (x, y) direction 'n')

-- playerPos :: Int -> [String] -> (Int, Int)
-- playerPos x [] = (-1,-1)
-- playerPos x (rx: rxs) =  if (playerPosHelp 0 rx) > -1 then (x, playerPosHelp 0 rx) else playerPos (x+1) rxs
--     where   playerPosHelp :: Int -> String -> Int
--             playerPosHelp y [] = -1
--             playerPosHelp y (r : s : rs) =  if r == '@' then y else if length rs > 2 then playerPosHelp (y+1) rs else -1


-- changePos :: [String] -> (Int, Int) -> [String]
-- changePos map (x',y') = 
--     where   removePlayer :: [String] -> (Int, Int) -> [String]
--         removePlayer map (x', y') = 
--                         let (x, y) = playerPos 0 map
--                         (map !! x) !! y = '-'
--                         (map !! x') !! y' = '@'
--                         return map

-- left :: [String] -> String -> (Int, Int) -> [String]
-- left map type (x, y) = if x - 2 /= '-' then left (changePos map (x-2,y)) type (x-2,y) else changePos map (x-2,y)) (x-2,y)

-- type is given by the moves inputed by the player
-- (x, y) current position


-- data Cell = At | Dash | Star | Pink | Orange | Yellow | Bonus | Target
--   deriving Eq

-- instance Show Cell where
--   show At = "@"
--   show Dash = "-"
--   show Star = "*"
--   show Pink = "p"
--   show Orange = "o"
--   show Yellow = "y"
--   show Bonus = "b"
--   show Target = "t"


-- loop :: [String] -> (Int, Int) -> Int -> String -> String -> (Int, Int)
-- loop map (x,y) count m1 m2 = if count == 0 then (x,y)
--                                 else loop map (helper map (helper map (x, y) m1) m2) (count-1) m1 m2
--     where 
--         helper :: [String] -> (Int, Int) -> String -> (Int, Int)
--         helper map (x,y) move = movePlayer map (x,y) move 'n' 