module Solvable (
        solvable,
        bonusReachable,
        bonusNotReachable,
        bonusToTargetNotReachable
) where

import Common

import Control.Monad.State
import Control.Monad

import Data.List
import Data.Char

import Text.Printf
import System.IO

-- (charPosition 0 '@' map)

solvable :: [String] -> (Int, Int) -> (Int, Int) -> Bool
solvable map current target = findPath map current target [] (-1)

bonusReachable :: [String] -> (Int, Int) -> [(Int, Int)]
bonusReachable map position = helperBonusReachable map position (bonusLocation map)
                where   
                        helperBonusReachable :: [String] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
                        helperBonusReachable map position [] = []
                        helperBonusReachable map position (b:bonusList) =   
                                                if solvable map position b
                                                        then [b] ++ (helperBonusReachable map position bonusList)
                                                else [] ++ (helperBonusReachable map position bonusList)

bonusNotReachable :: [String] -> (Int, Int) -> [(Int, Int)]
bonusNotReachable map position = helperBonusNotReachable map position (bonusLocation map)
                where   
                        helperBonusNotReachable :: [String] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
                        helperBonusNotReachable map position  [] = []
                        helperBonusNotReachable map position (b:bonusList) =   
                                                if solvable map position b
                                                        then [] ++ (helperBonusNotReachable map position bonusList)
                                                else [b] ++ (helperBonusNotReachable map position bonusList)

bonusToTargetNotReachable :: [String] -> (Int, Int) -> [(Int, Int)]
bonusToTargetNotReachable map target = helperBonusToTargetNotReachable map target (bonusLocation map)
        where 
                helperBonusToTargetNotReachable :: [String] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
                helperBonusToTargetNotReachable map target [] = []
                helperBonusToTargetNotReachable map target (b: bonusList) = 
                                                        if solvable map b target 
                                                                then [] ++ (helperBonusToTargetNotReachable map target bonusList)
                                                        else [b] ++ (helperBonusToTargetNotReachable map target bonusList)

findPath :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> Bool
findPath map (x,y) target visited lastDirection
        | (x,y) == target = True 
        | (x,y) /= target && not (True `elem` findWay) = False
        | (x,y) /= target && (True `elem` findWay) = sameDirectionOrAll
        where 
                findWay = possibleMove map (x,y) visited
                sameDirectionOrAll = 
                                if lastDirection /= -1 
                                        then if (findWay !! lastDirection) && not (((map !! x) !! y) `elem` ['p','o','y'])
                                                then findPath map (moveAhead lastDirection (x,y)) target (visited ++ [(x,y)]) lastDirection
                                        else (moveAround findWay)
                                else (moveAround findWay)
                moveAround (l:r:u:d) = (
                                        (l && (findPath map (moveAhead 0 (x,y)) target (visited ++ [(x,y)]) 0)) || 
                                        (r && (findPath map (moveAhead 1 (x,y)) target (visited ++ [(x,y)]) 1)) || 
                                        (u && (findPath map (moveAhead 2 (x,y)) target (visited ++ [(x,y)]) 2)) || 
                                ((d !! 0) && (findPath map (moveAhead 3 (x,y)) target (visited ++ [(x,y)]) 3)) 
                                        )

moveAhead :: Int -> (Int, Int) -> (Int, Int)
moveAhead dir (x,y) =   if dir == 0 then (x, y-2) 
                        else if dir == 1 then (x, y+2)
                        else if dir == 2 then (x-1, y)
                        else (x+1, y)

possibleMove :: [String] -> (Int, Int) -> [(Int, Int)] -> [Bool]
possibleMove map (x,y) visited = checkAllDirections map [moveLeft, moveRight, moveUp, moveDown] (x,y) visited
    where   
        checkAllDirections :: [String] -> [([String] -> [(Int, Int)] -> (Int, Int) -> Bool)] -> (Int, Int) -> [(Int, Int)] -> [Bool]
        checkAllDirections map [] _ _ = [] 
        checkAllDirections map (func:fs) position visited = if func map visited position then [True] ++ (checkAllDirections map fs position visited)
                                                            else [False] ++ (checkAllDirections map fs position visited)

moveUp :: [String] -> [(Int, Int)] -> (Int, Int) -> Bool
moveUp map visited (x,y) 
        | x - 1 >= 0 =  if (map !! (x-1)) !! y == '*' then False
                        else if (x-1,y) `elem` visited then False else True  
        | otherwise = False

moveDown :: [String] -> [(Int, Int)] -> (Int, Int) -> Bool
moveDown map visited (x,y) 
        | x + 1 <= (length map - 1) =  if (map !! (x+1)) !! y == '*' then False
                                        else if (x+1,y) `elem` visited then False else True  
        | otherwise = False 

moveLeft :: [String] -> [(Int, Int)] -> (Int, Int) -> Bool
moveLeft map visited (x,y) 
        | y - 2 >= 0 =  if (map !! x) !! (y-2) == '*' then False
                        else if (x,y-2) `elem` visited then False else True  
        | otherwise = False

moveRight :: [String] -> [(Int, Int)] -> (Int, Int) -> Bool
moveRight map visited (x,y)
        | y + 2 <= (length (map !! 0) - 2) =  if (map !! x) !! (y + 2) == '*' then False
                                                else if (x,y+2) `elem` visited then False else True
        | otherwise = False


-- checkCond :: [String] -> (Int, Int) -> Int -> Bool
-- checkCond map (x,y) dir = if dir == 0 
--                             then if ((map !! x) !! (y-1)) `elem` ['p','o','y'] 
--                                     then True
--                                  else False
--                         else if dir == 1 
--                             then if ((map !! x) !! (y+1)) `elem` ['p','o','y'] 
--                                     then True
--                                  else False
--                         else if dir == 2
--                             then if ((map !! (x-1)) !! y) `elem` ['p','o','y'] 
--                                     then True
--                                  else False
--                         else 
--                                 if ((map !! (x+1)) !! y) `elem` ['p','o','y'] 
--                                     then True
--                                  else False    

-- sameDirection = if (findWay !! lastDirection) && not (checkCond map (x,y) lastDirection)
--                     then findPath map (moveAhead lastDirection (x,y)) target (visited ++ [(x,y)]) lastDirection
--                 else (moveAround findWay)