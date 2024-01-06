module Common (
    charPosition,
    substring,
    getMapFile,
    outputMap,
    printMap,
    putPosition,
    printMoves,
    replaceDash,
    bonusLocation,
    printList
) where

import Control.Monad.State
import Control.Monad

import Data.List
import Data.Char

import Text.Printf
import System.IO

printList :: [String] -> String
printList [] = ""
printList (m:moves) = m ++ " " ++ (printList moves)

replaceDash :: [String] -> (Int, Int) -> [String]
replaceDash map (x,y) = helper map (x,y)
    where   helper :: [String] -> (Int, Int) -> [String]
            helper map (x, y) = helperX map 0 (x, y)
                where   helperX :: [String] -> Int -> (Int, Int) -> [String]
                        helperX (m: ms) count (x,y) = if count == x then ((helperY m 0 y): ms) else (m:(helperX ms (count + 1) (x,y)))
                        helperY :: String -> Int -> Int -> String
                        helperY (x: xs) count y = if count == y then ('-':xs) else (x:(helperY xs (count+1) y))

charPosition :: Int -> Char -> [String] -> (Int, Int)
charPosition x c [] = (-1,-1)
charPosition x c (rx: rxs) =  if (charPositionHelp 0 c rx) > -1 then (x, charPositionHelp 0 c rx) else charPosition (x+1) c rxs
    where   charPositionHelp :: Int -> Char -> String -> Int
            charPositionHelp y c [] = -1
            charPositionHelp y c (r : rs) =  if r == c then y else  charPositionHelp (y+1) c rs 

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

getMapFile :: IO String
getMapFile = putStr "Please load a map: " >> getLine

outputMap :: String -> IO ()
outputMap fileContents = mapM_ putStrLn $ lines fileContents

printMap :: [String] -> IO ()
printMap []    = return ()
printMap (m:ms) = do putStrLn m
                     printMap ms

printMoves :: [String] -> String
printMoves [] = ""
printMoves (m:[]) = m
printMoves (m:moves) = m ++ " -> " ++ printMoves moves
                     
putPosition :: (Int, Int) -> IO()
putPosition (x, y) =   do   putStr "("
                            putStr (show x)
                            putStr ","
                            putStr (show (y `div` 2))
                            putStrLn ")"


bonusLocation :: [String] -> [(Int, Int)]
bonusLocation map = if charPosition 0 'b' map /= (-1,-1) 
                        then [(charPosition 0 'b' map)] ++ (bonusLocation (replaceDash map (charPosition 0 'b' map)))
                    else [] 

-- charPosition :: Int -> Char -> [String] -> (Int, Int)
-- charPosition x c [] = (-1,-1)
-- charPosition x c (rx: rxs) =  if (charPositionHelp 0 c rx) > -1 then (x, charPositionHelp 0 c rx) else charPosition (x+1) c rxs
--     where   charPositionHelp :: Int -> Char -> String -> Int
--             charPositionHelp y c [] = -1
--             charPositionHelp y c (r : rs) =  if r == c then y else  charPositionHelp (y+1) c rs 