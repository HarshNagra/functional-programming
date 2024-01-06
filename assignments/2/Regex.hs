module Regex
  ( Regex
  , regexP
  , matcher
  ) where

import Combinators
import Parser

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Prelude

import Data.Char
import Data.Functor
import Data.List
import Data.Monoid

import Text.Printf

{- * Question 6 - 7. (40 pts)

Add definitions of Regex, replace `undefined` with your implementation.
You are encouraged to add auxilliary functions and data types to aid your
implementation.

-}

precedence :: [String]
precedence = ["Pipe", "Concatenate", "Star", "Plus", "QuestionMark"]

data Regex = Epsilon 
  | Single Char 
  | Concatenate Regex Regex
  | Dot
  | Star Regex
  | Plus Regex
  | QuestionMark Regex
  | Pipe Regex Regex
  -- deriving Show

precedenceOrder :: String -> [String] -> [String]
precedenceOrder _ [] = []
precedenceOrder oper (x:xs) = if (x == "Star" || x == "Plus" || x == "Questmark") 
                                then ["Star", "Plus", "Questmark"]
                              else 
                                if (x == oper) 
                                  then xs
                                else 
                                  precedenceOrder oper xs

xxPipe :: String -> Regex -> String
xxPipe save (Pipe a b)  
    | save `elem` precedenceOrder "Pipe" precedence = xx (Single '(') "em" ++ xx a "Pipe" ++ "|" ++ xx b "Pipe" ++ xx (Single ')') "em"
    | otherwise = xx a "Pipe" ++ "|" ++ xx b "Pipe"

xxConcatenate :: String -> Regex -> String
xxConcatenate save (Concatenate a b)
    | save `elem` precedenceOrder "Concatenate" precedence = xx (Single '(') "em"  ++ xx a "Concatenate" ++ xx b "Concatenate" ++ xx (Single ')') "em"
    | otherwise = xx a "Concatenate" ++ xx b "Concatenate"

xxStar :: String -> Regex -> String
xxStar save (Star a) 
    | save `elem` precedenceOrder "Star" precedence = xx (Single '(') "em" ++ xx a "Star" ++ "*" ++ xx (Single ')') "em"
    | otherwise = xx a "Star" ++ "*"

xxPlus :: String -> Regex -> String
xxPlus save (Plus a)
    | save `elem` precedenceOrder "Plus" precedence = xx (Single '(') "em" ++ xx a "Plus" ++ "+" ++ xx (Single ')') "em"
    | otherwise = xx a "Plus" ++ "+"

xxQuestionMark :: String -> Regex -> String
xxQuestionMark save (QuestionMark a) 
    | save `elem` precedenceOrder "QuestionMark" precedence = xx (Single '(') "em" ++ xx a "QuestionMark" ++ "?" ++ xx (Single ')') "em"
    | otherwise = xx a "QuestionMark" ++ "?"

xx :: Regex -> String ->  String
xx Epsilon _        = "[]"
xx (Single a) _   = [a]
xx Dot  _         = "."
xx (Star a) save  = xxStar save (Star a)
xx (Plus a) save  = xxPlus save (Plus a)
xx (QuestionMark a) save  = xxQuestionMark save (QuestionMark a)
xx (Pipe a b) save  = xxPipe save (Pipe a b)
xx (Concatenate a b) save  = xxConcatenate save (Concatenate a b)

instance Show Regex where
  -- show :: Regex -> String
  show regx = xx regx "em"


-- HELPER LISTS

cList :: [Parser Regex]
cList = [cConcat, cPostStar, cPostPlus, cQuestMark, cParen]

cListS:: [Parser Regex]
cListS = [cPostStar, cPostPlus, cQuestMark, cParen]

-- SINGLE

cSingle :: Parser Regex
cSingle = do  d <- checkExpression1 (satisfy isAlpha : [satisfy isDigit])
              return (Single d)

-- DOT

cDot :: Parser Regex
cDot = do char '.'
          return Dot

-- PARENTHESES

cParen :: Parser Regex
cParen = checkExpression1 [do char '('
                              d <- regexHelp
                              char ')'
                              return d, 
                              cDot, cSingle]

-- CONCATENATION

cConcat :: Parser Regex
cConcat = do  x <- checkExpression1 cListS
              y <- checkExpression1 cList
              return (Concatenate x y)

-- POST FIX 

cPostStar :: Parser Regex
cPostStar = do  d <- cParen
                char '*'
                return (Star d)

cPostPlus :: Parser Regex
cPostPlus = do  d <- cParen
                char '+'
                return (Plus d)

cQuestMark :: Parser Regex
cQuestMark = do d <- cParen
                char '?'
                return (QuestionMark d)

-- REGEX Helpers

checkExpressionHelp :: Parser a -> [Parser a]  -> Parser a
checkExpressionHelp x xs = parser $ \inp -> case runParser x inp of
                                        [] -> runParser (checkExpression1 xs) inp
                                        r -> r

checkExpression1 :: [Parser a] -> Parser a
checkExpression1 [] = parser (const [])
checkExpression1 (x:xs) = checkExpressionHelp x xs

regexHelpHelp :: Parser Regex
regexHelpHelp = do  x <- checkExpression1 cList
                    char '|'
                    Pipe x <$> regexHelp
                  
regexHelp :: Parser Regex
regexHelp = checkExpression1 (regexHelpHelp : cList)

regexP :: Parser Regex
regexP = parser $ \inp -> case runParser regexHelp inp of
                    [] -> []
                    [(v, "")] -> [(v, "")]
                    _ -> []

{- * Question 8. (25 pts)

Replace `undefined` with your implementation

-}

matcher :: Regex -> Parser String
matcher = undefined