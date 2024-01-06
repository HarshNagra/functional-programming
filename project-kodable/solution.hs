module Solution (solution, compressSolution) where

import Common

import Control.Monad.State
import Control.Monad

import Data.List
import Data.Char

import Text.Printf
import System.IO
import Data.Ord

compressSolution :: [String] -> [String] 
compressSolution moves = functionMoves ++ (if length function /= 0 then ["with"] else []) ++ function
    where
        (functionMoves, function) = compressFunction (compressLoopSolution moves)

-- Updated list and the function used
compressFunction :: [String] -> ([String], [String])
compressFunction moves = if length moves < 3 then (moves, [])
                            else ((compressFunctionHelp moves), function)
    where    
        compressFunctionHelp :: [String] -> [String]
        compressFunctionHelp moves = if length moves < 3 then moves
                                    else if (take 3 moves == function )
                                        then ["Function"] ++ (compressFunctionHelp (drop 3 moves))
                                    else (take 1 moves) ++ (compressFunctionHelp (drop 1 moves))               
        function = mostFrequent (cleanListThree (generateAllSublist moves))

cleanListThree :: [[String]] -> [[String]]
cleanListThree [] = []
cleanListThree (l : list) = if length l == 3 then [l] ++ (cleanListThree list) else [] ++ (cleanListThree list)

-- below function taken from stackoverflow 
mostFrequent :: [[String]] -> [String]
mostFrequent [] = ["list is empty"]
mostFrequent xs = maxFreqNum
  where 
    sortedGrpBy = fmap (\x -> (head x, length x)) $ groupBy (==) $ reverse $ sort xs
    maxFreq = maximum $ fmap snd sortedGrpBy
    maxFreqNum = fst $ head $ filter (\(a,b) -> b == maxFreq) sortedGrpBy

generateAllSublist :: [String] -> [[String]]
generateAllSublist [] = [[]]
generateAllSublist (l:[]) = [[l]]
generateAllSublist (l:l2:[]) = [ ([l]++[l2]) ]
generateAllSublist (l:l2:l3:list) = if take 4 l == "Loop" || take 4 l2 == "Loop" || take 4 l3 == "Loop" 
                                        then [] ++ (generateAllSublist (l2:l3:list))
                                    else [ ([l] ++ [l2] ++ [l3]) ] ++ (generateAllSublist (l2:l3:list))

compressLoopSolution :: [String]  -> [String]
compressLoopSolution fullSol  = getCompress
    where
        getCompress = if length (getEven) <= length (getEven) then getEven else getOdd
        getOdd = helperCompressLoops ( if (length fullSol) > 3 then (drop 3 fullSol) else fullSol) (if length fullSol > 3 then (take 2 (drop 1 fullSol)) else []) 1 (take 1 fullSol)
        getEven = helperCompressLoops ( if (length fullSol) > 2 then (drop 2 fullSol) else fullSol) (if length fullSol > 2 then (take 2 fullSol) else []) 1 []
        helperCompressLoops :: [String] -> [String] -> Int -> [String] -> [String]
        helperCompressLoops [] loop times newList = ( if times >= 3
                                                        then (take (length newList - 1) newList) ++ [(makeLoopString loop (times))] 
                                                    else if times == 2
                                                        then (take (length newList - 2) newList) ++ [(makeLoopString loop (times))] 
                                                    else newList ++ loop)
        helperCompressLoops (m:[]) loop times newList = ( if times >= 3
                                                            then (take (length newList - 1) newList) ++ [(makeLoopString loop (times))] ++ [m]
                                                        else if times == 2
                                                            then (take (length newList - 2) newList) ++ [(makeLoopString loop (times))]  ++ [m]
                                                        else newList ++ loop ++ [m])
        helperCompressLoops (m1:m2:moves) loop times newList = 
                if loop /= ([m1]++[m2])
                    then (helperCompressLoops moves ([m1]++[m2]) 1 ( if times >= 3
                                                                    then (take (length newList - 1) newList) ++ [(makeLoopString loop (times))] 
                                                                else if times == 2
                                                                    then (take (length newList - 2) newList) ++ [(makeLoopString loop (times))] 
                                                                else newList ++ loop))
                else helperCompressLoops moves loop (times+1) ( if times >= 3
                                                                    then (take (length newList - 1) newList) ++ [(makeLoopString loop (times))] 
                                                                else if times == 2
                                                                    then (take (length newList - 2) newList) ++ [(makeLoopString loop (times))] 
                                                                else newList ++ loop)

-- 



makeLoopString :: [String] -> Int -> String
makeLoopString moves times = "Loop{" ++ (show times) ++ "}{" ++ (moves !! 0) ++ "," ++ (moves !! 1) ++ "}"

solution :: [String] -> (Int, Int) -> Int -> [String]
solution map position totalB =  ( 
                                if length (solutions map position [] [] [(position,0)] totalB) /= 0 
                                    then shortestWay (solutions map position [] [] [(position,0)] totalB)
                                else ["No Solution!"] 
                                )

-- refineResult :: [String] -> [String]
-- refineResult [] = []
-- refineResult (move : [] ) = 

shortestWay :: (Foldable t1, Foldable t2) => t1 (t2 a) -> t2 a
shortestWay results = minimumBy (comparing length) results

-- map -> current position -> current solution -> bonus list -> visited -> total possible bonus
solutions :: [String] -> (Int, Int) ->  [String] -> [(Int, Int)] -> [((Int, Int), Int)] -> Int -> [[String]]
solutions map (x,y) sol bonusList visited totalB
            | ((map !! x) !! y) == 't' && length bonusList /= totalB = []
            | ((map !! x) !! y) == 't' && length bonusList == totalB = [sol]
            | ((map !! x) !! y) `elem` ['p','o','y'] = blockSolution True
            | otherwise = blockSolution False
        where 
            ((lx,ly), lBonusList) = leftVisited map (x,y) bonusList  visited
            ((rx,ry), rBonusList) = rightVisited map (x,y) bonusList  visited
            ((ux,uy), uBonusList) = upVisited map (x,y) bonusList  visited
            ((dx,dy), dBonusList) = downVisited map (x,y) bonusList  visited
            blockLabel = (map !! x) !! y
            blockDirection dir = "Cond{" ++ [blockLabel] ++ "}{" ++ dir ++ "}"
            currentLabel flag dir = if flag
                                        then if (sol !! (length sol -1) /= dir) 
                                            then [(blockDirection dir)]
                                        else []
                                    else [dir]
            blockSolution flag = (   
                if (lx,ly) /= (-1,-1) 
                    then solutions map (lx,ly) (sol ++ currentLabel flag "Left") lBonusList (visited ++ [((lx,ly), length lBonusList)]) totalB
                else []
                ) ++ (
                if (rx,ry) /= (-1,-1) 
                    then solutions map (rx,ry) (sol ++ currentLabel flag "Right") rBonusList (visited ++ [((rx,ry), length rBonusList)]) totalB
                else []
                ) ++ (
                if (ux,uy) /= (-1,-1) 
                    then solutions map (ux,uy) (sol ++ currentLabel flag "Up") uBonusList (visited ++ [((ux,uy), length uBonusList)]) totalB
                else []
                ) ++ (
                if (dx,dy) /= (-1,-1) 
                    then solutions map (dx,dy) (sol ++ currentLabel flag "Down") dBonusList (visited ++ [((dx,dy), length dBonusList)]) totalB
                else []
                )
            
-- returns newPoisiotn and collected bonus list
leftVisited :: [String] -> (Int, Int) -> [(Int, Int)] -> [((Int, Int), Int)] -> ((Int, Int), [(Int, Int)])
leftVisited map (x, y) bonusList visited =
                    if ((lx, ly), length uBonusList) `elem` visited || (x,y) == (lx,ly) 
                        then ((-1,-1), bonusList)
                    else ((lx, ly), uBonusList)
                    where 
                        ((lx, ly), uBonusList) = left map (x, y) bonusList

left :: [String] -> (Int, Int) -> [(Int, Int)] -> ((Int, Int), [(Int, Int)])
left map (x, y) bonusList = if y - 2 >= 0 then  (
                                if (map !! x) !! (y-2) /= '-' then (
                                    if (map !! x) !! (y-2) == 'b' && not ((x,y-2) `elem` bonusList)
                                        then left map (x, y-2) (bonusList ++ [(x,y-2)])
                                    else if (map !! x) !! (y-2) == '*' 
                                        then ((x, y), bonusList)
                                    else if ((map !! x) !! (y-2)) `elem` ['p','o','y','t'] 
                                            then ((x, y-2), bonusList)
                                    else left map (x, y-2) bonusList
                                )
                                else left map (x, y-2) bonusList
                            )   
                            else ((x, y), bonusList) 

rightVisited :: [String] -> (Int, Int) -> [(Int, Int)] -> [((Int, Int), Int)] -> ((Int, Int), [(Int, Int)])
rightVisited map (x, y) bonusList visited =
                    if ((rx, ry), length uBonusList) `elem` visited || (x,y) == (rx,ry) 
                        then ((-1,-1), bonusList)
                    else ((rx, ry), uBonusList)
                    where 
                        ((rx, ry), uBonusList) = right map (x, y) bonusList

right :: [String] -> (Int, Int) -> [(Int, Int)] -> ((Int, Int), [(Int, Int)])
right map (x, y) bonusList = if y + 2 <= (length (map !! 0) - 1) then  (
                                if (map !! x) !! (y+2) /= '-' then (
                                    if (map !! x) !! (y+2) == 'b' && not ((x,y+2) `elem` bonusList)
                                        then right map (x, y+2) (bonusList ++ [(x,y+2)])
                                    else if (map !! x) !! (y+2) == '*' 
                                        then ((x, y), bonusList)
                                    else if ((map !! x) !! (y+2)) `elem` ['p','o','y','t'] 
                                            then ((x, y+2), bonusList)
                                    else right map (x, y+2) bonusList
                                )
                                else right map (x, y+2) bonusList
                            )
                            else ((x, y), bonusList) 
                            
upVisited :: [String] -> (Int, Int) -> [(Int, Int)] -> [((Int, Int), Int)] -> ((Int, Int), [(Int, Int)])
upVisited map (x, y) bonusList visited =
                    if ((ux, uy), length uBonusList) `elem` visited || (x,y) == (ux,uy) 
                        then ((-1,-1), bonusList)
                    else ((ux, uy), uBonusList)
                    where 
                        ((ux, uy), uBonusList) = up map (x, y) bonusList

up :: [String] -> (Int, Int) -> [(Int, Int)] -> ((Int, Int), [(Int, Int)])
up map (x, y) bonusList = if x - 1 >= 0 then  (
                                if (map !! (x-1)) !! y /= '-' then (
                                    if (map !! (x-1)) !! y == 'b' && not ((x-1,y) `elem` bonusList)
                                        then up map (x-1, y) (bonusList ++ [(x-1,y)])
                                    else if (map !! (x-1)) !! y == '*' 
                                        then ((x, y), bonusList)
                                    else if ((map !! (x-1)) !! y) `elem` ['p','o','y','t'] 
                                            then ((x-1, y), bonusList)
                                    else up map (x-1, y) bonusList
                                )
                                else up map (x-1, y) bonusList
                            )  
                            else ((x, y), bonusList) 
                                       
downVisited :: [String] -> (Int, Int) -> [(Int, Int)] -> [((Int, Int), Int)] -> ((Int, Int), [(Int, Int)])
downVisited map (x, y) bonusList visited =
                    if ((dx, dy), length uBonusList) `elem` visited || (x,y) == (dx,dy) 
                        then ((-1,-1), bonusList)
                    else ((dx, dy), uBonusList)
                    where 
                        ((dx, dy), uBonusList) = down map (x, y) bonusList

down :: [String] -> (Int, Int) -> [(Int, Int)] -> ((Int, Int), [(Int, Int)])
down map (x, y) bonusList = if x + 1 <= ((length map) -1) then  (
                                if (map !! (x+1)) !! y /= '-' then (
                                    if (map !! (x+1)) !! y == 'b' && not ((x+1,y) `elem` bonusList)
                                        then down map (x+1, y) (bonusList ++ [(x+1,y)])
                                    else if (map !! (x+1)) !! y == '*' 
                                        then ((x, y), bonusList)
                                    else if ((map !! (x+1)) !! y) `elem` ['p','o','y','t'] 
                                            then ((x+1, y), bonusList)
                                    else down map (x+1, y) bonusList
                                )
                                else down map (x+1, y) bonusList
                            )
                            else ((x, y), bonusList) 