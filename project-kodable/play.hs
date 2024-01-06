module Play (
    play,
    playFunc,
    movePlayer,
    updateMap
) where

import Common
import Hint

import Control.Monad.State
import Control.Monad

import Data.List
import Data.Char

import Text.Printf
import System.IO

import System.Directory


updateMap :: [String] -> (Int, Int) -> [String]
updateMap map (x,y) = helperX map 0 (x,y)
    where 
        helperX :: [String] -> Int -> (Int, Int) -> [String]
        helperX (m: ms) count (x,y) = if count == x then ((helperY m 0 y): ms) else (m:(helperX ms (count + 1) (x,y)))
        helperY :: String -> Int -> Int -> String
        helperY (x: xs) count y = if count == y then ('@':xs) else (x:(helperY xs (count+1) y))

playerPos :: Int -> [String] -> (Int, Int)
playerPos x [] = (-1,-1)
playerPos x (rx: rxs) =  if (playerPosHelp 0 rx) > -1 then (x, playerPosHelp 0 rx) else playerPos (x+1) rxs
    where   playerPosHelp :: Int -> String -> Int
            playerPosHelp y [] = -1
            playerPosHelp y (r : rs) =  if r == '@' then y else  playerPosHelp (y+1) rs 

left :: [String] -> Char -> (Int, Int) -> (Int, Int)
left map c (x, y) = if y - 2 >= 0 
                        then if (map !! x) !! (y-2) /= '-' 
                            then if (map !! x) !! (y-2) == 't' 
                                    then (x, y-2)
                                else if (map !! x) !! (y-2) == '*' 
                                    then (x, y)
                                else if c == ((map !! x) !! (y-2)) 
                                    then (x, y-2)
                                else left map c (x, y-2) 
                            else left map c (x, y-2)
                        else (x, y) 

-- please calculate board dimensions
right :: [String] -> Char -> (Int, Int) -> (Int, Int)
right map c (x, y) = if y + 2 <= (length (map !! 0) -1) 
                        then if (map !! x) !! (y+2) /= '-' 
                            then if (map !! x) !! (y+2) == 't'
                                    then (x,y+2) 
                                else if (map !! x) !! (y+2) == '*' 
                                    then (x, y)
                                else if c == ((map !! x) !! (y+2)) 
                                    then (x, y+2)
                                else right map c (x, y+2) 
                            else right map c (x, y+2)
                        else (x, y)

up :: [String] -> Char -> (Int, Int) -> (Int, Int)
up map c (x, y) = if x - 1 >= 0 
                    then if (map !! (x-1)) !! y /= '-' 
                            then if (map !! (x-1)) !! y == 't' 
                                    then (x-1, y)
                            else if (map !! (x-1)) !! y == '*' 
                                    then (x, y)
                            else if c == ((map !! (x-1)) !! y) 
                                    then (x-1, y)
                                else up map c (x-1, y) 
                            else up map c (x-1, y)
                        else (x, y)

-- please calculate board dimensions
down :: [String] -> Char -> (Int, Int) -> (Int, Int)
down map c (x, y) = if x + 1 <= ((length map) -1) 
                        then if (map !! (x+1)) !! y /= '-' 
                            then if (map !! (x+1)) !! y == 't' 
                                    then (x+1, y)
                                else if (map !! (x+1)) !! y == '*' 
                                    then (x, y)
                                else if c == ((map !! (x+1)) !! y) 
                                        then (x+1, y)
                                    else down map c (x+1, y) 
                                else down map c (x+1, y)
                            else (x, y)

movePlayer :: [String] -> (Int, Int) -> String -> Char -> (Int, Int)
movePlayer map position move nextMove
    | take 1 move == "L" = if (move !! 1) /= 'n' then if ((map !! x) !! y == (move !! 1)) then left map nextMove position else position else left map nextMove position
    | take 1 move == "R" = if (move !! 1) /= 'n' then if ((map !! x) !! y == (move !! 1)) then right map nextMove position else position else right map nextMove position
    | take 1 move == "D" = if (move !! 1) /= 'n' then if ((map !! x) !! y == (move !! 1)) then down map nextMove position else position else down map nextMove position
    | take 1 move == "U" = if (move !! 1) /= 'n' then if ((map !! x) !! y == (move !! 1)) then up map nextMove position else position else up map nextMove position
    | otherwise = position
    where 
        (x,y) = position

updateConditional :: String -> String
updateConditional m =
            if (take 1 (drop 5 m)) `elem` ["p","o","y","t"] then 
                ( if take (length (drop 8 m) - 1) (drop 8 m) == "Right" then ('R' : (take 1 (drop 5 m)))
                else if take (length (drop 8 m) - 1) (drop 8 m) == "Left" then ('L' : (take 1 (drop 5 m)))
                else if take (length (drop 8 m) - 1) (drop 8 m) == "Up" then ('U' : (take 1 (drop 5 m)))
                else if take (length (drop 8 m) - 1) (drop 8 m) == "Down" then ('D' : (take 1 (drop 5 m)))
                else  "Invalid" )
            else "Invalid"

extractMoves :: String -> [String]
extractMoves move = helperExtract move ""
    where
        helperExtract :: String -> String -> [String]
        helperExtract [] str = [str]
        helperExtract (m:move) str = if m /= ',' then (helperExtract move (str ++ [m])) else (str : helperExtract move "")

expandLoop :: String -> [String] -> [String]
expandLoop moves funcMoves = expandList (loopHelper moves (read (take 1 (drop 5 moves))::Int)) funcMoves
    where 
        loopHelper :: String -> Int -> [String]
        loopHelper moves 0 = []
        loopHelper moves count = if count > 5 || length (extractMoves (take (length (drop 8 moves)-1) (drop 8 moves))) > 2 
                                    then ["Invalid"]
                                 else (extractMoves (take (length (drop 8 moves)-1) (drop 8 moves))) ++ (loopHelper moves (count-1))

expandList :: [String] -> [String] -> [String] 
expandList [] funcMoves = []
expandList (m:moves) funcMoves
            | take 8 m == "Function" = expandList ( funcMoves ++ moves ) funcMoves
            | take 4 m == "Loop" = (expandLoop m funcMoves) ++ expandList moves funcMoves
            | take 4 m == "Cond" = ((updateConditional m) : expandList moves funcMoves)
            | m == "Left" = ("Ln" : (expandList moves funcMoves))
            | m == "Right" = ("Rn" : (expandList moves funcMoves))
            | m == "Up" = ("Un" : (expandList moves funcMoves))
            | m == "Down" = ("Dn" : (expandList moves funcMoves))
            | otherwise = ["Invalid"]

checkValidInput :: [String] -> Bool
checkValidInput [] = True
checkValidInput (m:moves) = if m == "Invalid" then False else checkValidInput moves 

checkValidMoves :: [(Int, Int)] -> [String] -> (String, Bool)
checkValidMoves [] [] = ("",True)
checkValidMoves (v:[]) (m:[]) = ("",True)
checkValidMoves (v1:v2:[]) (m1:m2:[]) =  if (v1 == v2) then (m2,False) else (checkValidMoves (v2:[]) (m2:[])) 
checkValidMoves (v1:v2:vList) (m1:m2:mList) =  if (v1 == v2) then (m2,False) else (checkValidMoves (v2:vList) (m2:mList))       

findBonus :: [String] -> (Int, Int) -> (Int, Int) -> [String]
findBonus map (cx, cy) (nx, ny) =   if cx == nx then
                                        (if cy > ny then (
                                            if (helperBonusY map (cx, cy) (nx, ny)) /= (-1,-1) 
                                                then (replaceDash map (helperBonusY map (cx, cy) (nx, ny)))
                                            else map )
                                        else if cy < ny then ( 
                                            if (helperBonusY map (nx, ny) (cx, cy)) /= (-1,-1) 
                                                then (replaceDash map (helperBonusY map (nx, ny) (cx, cy)))
                                            else map )
                                        else map)
                                    else if cy == ny then 
                                        (if cx > nx then ( 
                                            if (helperBonusX map (cx, cy) (nx, ny)) /= (-1,-1) 
                                                then (replaceDash map (helperBonusX map (cx, cy) (nx, ny)))
                                            else map )
                                        else if cx < nx then ( 
                                            if (helperBonusX map (nx, ny) (cx, cy)) /= (-1,-1) 
                                                then (replaceDash map (helperBonusX map (nx, ny) (cx, cy)))
                                            else map )
                                        else map)
                                    else map
        where -- point x y and max x y
            helperBonusY :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
            helperBonusY map (px, py) (mx, my) = if ((map !! px) !! py) == 'b' 
                                                    then (px, py) 
                                                else if py > my then
                                                    if ((map !! px) !! (py-1)) == 'b' 
                                                        then (px, py-1) 
                                                    else helperBonusY map (px,py-1) (mx, my)
                                                else (-1, -1)
            helperBonusX :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
            helperBonusX map (px, py) (mx, my) = if ((map !! px) !! py) == 'b' 
                                                    then (px, py) 
                                                else if px > mx then
                                                    if ((map !! (px-1)) !! py) == 'b' 
                                                        then (px-1, py) 
                                                    else helperBonusX map (px-1,py) (mx, my)
                                                else (-1, -1)

printMaps :: [(Int, Int)] -> [String] -> IO() 
printMaps [] map = return ()
printMaps (l:[]) map = do
                        putStrLn "Final Result: "
                        printMap (updateMap (replaceDash map (charPosition 0 '@' map)) l)
printMaps (l:n:list) map = do
                            printMap (updateMap (replaceDash map (charPosition 0 '@' map)) l)
                            let bonusMap = findBonus map l n
                            case bonusMap == map of 
                                True -> putStrLn ""
                                False -> putStrLn "Bonus Collected!"
                            printMaps (n:list) (findBonus map l n)

currentMap :: [(Int, Int)] -> [String] -> [String]
currentMap [] map = map
currentMap (l:[]) map = updateMap (replaceDash map (charPosition 0 '@' map)) l
currentMap (l:n:list) map = currentMap (n:list) (findBonus map l n)

putPositions :: [(Int, Int)] -> IO()
putPositions [] = return ()
putPositions (l:list) = do
                            putPosition l
                            putPositions list

convertToOriginal :: String -> String
convertToOriginal move 
                | take 1 move == "L" && drop 1 move == "n" = "Left"
                | take 1 move == "L" && drop 1 move /= "n" = "Left or block" ++ (drop 1 move) ++ " not there."
                | take 1 move == "R" && drop 1 move == "n" = "Right"
                | take 1 move == "R" && drop 1 move /= "n" = "Right or block" ++ (drop 1 move) ++ " not there."
                | take 1 move == "U" && drop 1 move == "n" = "Up"
                | take 1 move == "U" && drop 1 move /= "n" = "Up or block" ++ (drop 1 move) ++ " not there."
                | take 1 move == "D" && drop 1 move == "n" = "Down"
                | take 1 move == "D" && drop 1 move /= "n" = "Down or block" ++ (drop 1 move) ++ " not there."

play :: [String] -> [String] -> IO()
play map funcMoves = do 
                    putStrLn "Valid Directions: Left, Right, Up, Down"
                    putStrLn "Valid Block: p, o, y"
                    putStrLn "Valid Moves: Direction, Cond{Block}{Direction}, Loop{iterations}{Direction, Direction}, Function"
                    putStrLn ""
                    putStr "First Move: "
                    move <- getLine
                    recursePlay [move]
        where
            recursePlay :: [String] -> IO()
            recursePlay move = do 
                    moves <- playHelp move
                    -- putStr (printMoves moves)
                    let newMovesList = expandList moves funcMoves
                    case checkValidInput newMovesList of
                        False -> putStrLn "Invalid Input" 
                        True -> do
                                let list = (implementMove map (charPosition 0 '@' map) (expandList moves funcMoves))
                                let (moveName, val) = checkValidMoves list (newMovesList)
                                case val of
                                    False -> putStrLn ("Can't make move: " ++ (convertToOriginal moveName))
                                    True -> do
                                        printMaps list map
                                        case (list !! (length list - 1)) == (charPosition 0 't' map) of 
                                            True -> (putStrLn "Congratulations! Map succesfully completed.")
                                            False -> do 
                                                    putStrLn "Sorry! You did not reach the target."
                                                    putStrLn "Do you need a hint? (Yes/No)"
                                                    ans <- getLine
                                                    case ans of
                                                        "Yes"-> do 
                                                                let steps = (hint (currentMap list map))
                                                                putStrLn ("Hint: " ++ (steps !! 0) ++ " " ++ ( if length steps > 1 then (steps !! 1) else ""))
                                                                recursePlay moves
                                                        any -> do 
                                                                putStrLn "Game Over!"
                                                                return ()
            playHelp :: [String] -> IO [String]
            playHelp moveList = do  
                            putStr "Next Move: "
                            move <- getLine
                            case length move of 
                                0 -> return moveList
                                m -> playHelp (moveList ++ [move])

            implementMove :: [String] -> (Int, Int) -> [String] -> [(Int, Int)]
            implementMove map (x,y) [] = []
            implementMove map (x,y) (current:[]) = ((movePlayer map (x,y) (current) 'n') : (implementMove map (movePlayer map (x,y) (current) 'n') []))
            implementMove map (x,y) (current:next:moves) = ((movePlayer map (x,y) (current) ((tail next) !! 0)) : (implementMove map (movePlayer map (x,y) (current) ((tail next) !! 0)) (next:moves)))

extractMovesFunc :: String -> [String]
extractMovesFunc move = helperExtract move ""
    where
        helperExtract :: String -> String -> [String]
        helperExtract [] str = [str]
        helperExtract (m:move) str = if m /= ' ' then (helperExtract move (str ++ [m])) else (str : helperExtract move "")

playFunc :: [String] -> String -> IO()
playFunc map line = do 
                        case (take 5 line) == "play " of
                            True -> do 
                                    let funcMoves = (extractMovesFunc (drop 5 line))
                                    case length funcMoves == 3 && not (substring "Loop" line) of
                                        True -> do 
                                                    play map funcMoves
                                        False -> do
                                                putStrLn "Invalid defination of Play Function! Rules: No Loops & 3 Directions with or without Conditions."      
                            False -> putStrLn "Invalid Choice"