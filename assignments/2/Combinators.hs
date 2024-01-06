module Combinators
  ( lookAhead
  , optional
  , choose
  , followedBy
  , manyUntil) where

import Parser

import Control.Applicative hiding (optional)
import Control.Monad ()
import Control.Monad.State

import Control.Arrow

import Data.Char
import Data.Functor
import Data.Monoid


{- * Question 1 - 5. (35 pts)

Replace `undefined` with your implementation.
You are free to add auxilliary functions or data types to aid your
implementation.
-}

--- 1

lookAhead :: Parser Char
lookAhead = parser (\inp ->
             case inp of
               ""     -> []
               (c:cs) -> [(c,c:cs)])

--- 2

optional :: Parser a -> Parser (Maybe a)
optional p = parser (\inp -> 
                    case runParser p inp of
                    [] -> [(Nothing, inp)]
                    [(v, out)] -> [(Just v, out)]
                  )

--- 3

choose :: [Parser a] -> Parser a
choose [] = failure
choose (p:ps) = parser $ \inp -> 
                        case runParser p inp of
                          [] -> runParser (choose ps) inp 
                          r -> r ++ runParser (choose ps) inp 

--- 4 

followedBy :: Parser a -> Parser b -> Parser a
followedBy p q = parser $ \inp ->
                          case runParser p inp of
                            [] -> []
                            [(x, xs)] -> case runParser q xs of 
                                    [] -> []
                                    _ -> [(x,xs)]

    
--- 5

manyUntilHelp :: [([a], String)] ->  Bool -> Parser b  -> [([a], String)]
manyUntilHelp [] _ _ = []
manyUntilHelp ((x,xs): all) check second = 
                                    case runParser second xs of
                                            [] -> if not (null all) || check
                                                    then []
                                                    else manyUntilHelp all check second 
                                            [(_,cs)] -> manyUntilHelp all False second ++ [(x,cs)]

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil first second = parser $ \inp -> manyUntilHelp (runParser (many first) inp) True second 
