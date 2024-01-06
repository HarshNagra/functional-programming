module Hint (hint) where

import Common
import Solution
import Solvable

import Control.Monad.State
import Control.Monad

import Data.List
import Data.Char

import Text.Printf
import System.IO

hint :: [String] -> [String]
hint map =  hintHelp (solution map (charPosition 0 '@' map) (length (bonusReachable map (charPosition 0 '@' map)) - length (bonusToTargetNotReachable map (charPosition 0 't' map))))
    where   hintHelp :: [String] -> [String]
            hintHelp steps =    if length steps > 2 
                                    then take 2 steps
                                else steps 