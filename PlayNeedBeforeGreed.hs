module PlayNeedBeforeGreed where


import NeedBeforeGreed
import Roller
import Printer
--import Parser

import Data.List
import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)
import Control.Monad


playNeedBeforeGreed :: [NB4G] -> IO [NB4G]
playNeedBeforeGreed state = do
  putStrLn "Name? (exit with '0')"
  line <- getLine
  if line == "0"
    then do
      if state /= []
        then do
          let resultState = rollResult state
          mapM (printNB4G) resultState
          let winner = winnerNB4G resultState
          let winner_name = name winner
          putStrLn ("Winner is: " ++ winner_name ++ "!")
          return []
      else do
        putStrLn "Need Before Greed has ended"
        return []
    else do
      putStrLn "Pass (1), Greed (2) or Need (3)?"
      line2 <- getLine
      if line2 == "1"
        then do
          putStrLn (line ++ " Selected Pass")
          let newRoller = (NB4G line Pass 0)
          let newState = addNewRoller newRoller state
          playNeedBeforeGreed newState
      else if line2 == "2"
        then do
          putStrLn (line ++ " Selected Greed")
          let newRoller = (NB4G line Greed 0)
          let newState = addNewRoller newRoller state
          playNeedBeforeGreed newState
      else if line2 == "3"
        then do
          putStrLn (line ++ " Selected Need")
          let newRoller = (NB4G line Need 0)
          let newState = addNewRoller newRoller state
          playNeedBeforeGreed newState
      else do
        putStrLn "Could not understand, please try again"
        playNeedBeforeGreed state






rollResult :: [NB4G]-> [NB4G]
rollResult [] = []
rollResult rollers = rollNB4G (rollers)

