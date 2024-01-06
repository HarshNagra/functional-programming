module TicTacToe (tictactoe) where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.List

import Text.Printf

import System.IO

data Move = O | X

data Piece = Taken Move | Empty

instance Show Move where
    show O = "O"
    show X = "X"

instance Show Piece where
    show (Taken O) = "O"
    show (Taken X) = "X"
    show Empty = " "

instance Eq Piece where
    Taken O == Taken O = True
    Taken X == Taken X = True
    Empty == Empty           = True
    _ == _                   = False

checkBoardCell :: String -> Maybe Int
checkBoardCell "1 1" = Just 0
checkBoardCell "1 2" = Just 1
checkBoardCell "1 3" = Just 2
checkBoardCell "2 1" = Just 3
checkBoardCell "2 2" = Just 4
checkBoardCell "2 3" = Just 5
checkBoardCell "3 1" = Just 6
checkBoardCell "3 2" = Just 7
checkBoardCell "3 3" = Just 8
checkBoardCell _    = Nothing

renderBoard :: [Piece] -> IO ()
renderBoard board = do  putStrLn ".---.---.---."
                        putStrLn $ "| " ++ intercalate " | " (fmap show (take 3 board)) ++ " |"
                        putStrLn ".---.---.---."
                        putStrLn $ "| " ++ intercalate " | " (fmap show (drop 3 . take 6 $ board)) ++ " |"
                        putStrLn ".---.---.---."
                        putStrLn $ "| " ++ intercalate " | " (fmap show (drop 6 board)) ++ " |"
                        putStrLn ".---.---.---."
                        
isThereAWinner :: Move -> [Piece] -> Bool
isThereAWinner move board = 
  (head board == Taken move && board !! 1 == Taken move && board !! 2 == Taken move) ||
  (head board == Taken move && board !! 1 == Taken move && board !! 2 == Taken move) ||
  (head board == Taken move && board !! 3 == Taken move && board !! 6 == Taken move) ||
  (head board == Taken move && board !! 4 == Taken move && board !! 8 == Taken move) ||
  (board !! 1 == Taken move && board !! 4 == Taken move && board !! 7 == Taken move) ||
  (board !! 2 == Taken move && board !! 5 == Taken move && board !! 8 == Taken move) ||
  (board !! 3 == Taken move && board !! 4 == Taken move && board !! 5 == Taken move) ||
  (board !! 6 == Taken move && board !! 7 == Taken move && board !! 8 == Taken move) ||
  (board !! 6 == Taken move && board !! 4 == Taken move && board !! 2 == Taken move)

isDraw :: [Piece] -> Bool
isDraw = foldr (\ piece -> (&&) (piece /= Empty)) True

changeMove :: Move -> Move
changeMove O = X
changeMove X = O

checkString :: String -> String
checkString input = if length input < 3 then "xxx" else help [ x | x <- input, x /= ' ']
    where   help :: String -> String
            help (x:y) = (x : " ") ++ y

isFree ::  [Piece] -> Int -> Maybe Int
isFree board ix = if board !! ix == Empty then Just ix else Nothing

data PositionCheck = Ok [Piece] | Error [Piece] String 

checkPosition :: String -> Move -> [Piece] -> PositionCheck
checkPosition location move board =
  case checkBoardCell location >>= isFree board of
          Nothing -> Error board "INVALID POSITION" 
          Just i -> Ok (take i board ++ [Taken move] ++ drop (i + 1) board)

playRound :: Move  -> [Piece] -> IO ()
playRound move board = do
  renderBoard board
  putStrLn $ show move ++ " MOVE"
  cell <- getLine
  case checkPosition (checkString cell) move board of
    Error board str  -> do
      putStrLn str
      playRound move board
    Ok newBoard -> do
      if isThereAWinner move newBoard then
          do  renderBoard newBoard
              putStrLn (show move ++ " WINS")
              return ()
      else
          if isDraw newBoard then
              do  renderBoard newBoard
                  putStrLn "DRAW"
                  return ()
          else
              playRound (changeMove move) newBoard

tictactoe :: IO ()
tictactoe = playRound O [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]