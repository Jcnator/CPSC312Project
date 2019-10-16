module PlayNeedBeforeGreed where


import NeedBeforeGreed
import Roller
--import Parser

import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)

playNeedBeforeGreed :: [NB4G] -> IO [NB4G]
playNeedBeforeGreed state = do
  putStrLn "Name? (exit with '0')"
  line <- getLine
  if line == "0"
    then do
      putStrLn "Need Before Greed has ended"
      if state /= []
        then do
          let resultState = rollResult state
          --let winner = winner resultState
          -- let winnerName = name winner
          -- let winnerType = rollType winner
          -- let winnerResult = result winner
          putStrLn ("Winner is: " ++ show (winnerNB4G resultState))
          return resultState
      else
        return []
    else do
      putStrLn "Pass (1), Greed (2) or Need (3)?"
      line2 <- getLine
      if line2 == "1"
        then do
          putStrLn "Selected Pass"
          let newRoller = (NB4G line Pass 0)
          let newState = addNewRoller newRoller state
          playNeedBeforeGreed newState
      else if line2 == "2"
        then do
          putStrLn "Selected Greed"
          let newRoller = (NB4G line Greed 0)
          let newState = addNewRoller newRoller state
          playNeedBeforeGreed newState
      else if line2 == "3"
        then do
          putStrLn "Selected Need"
          let newRoller = (NB4G line Need 0)
          let newState = addNewRoller newRoller state
          playNeedBeforeGreed newState
      else do
        putStrLn "Could not understand, please try again"
        playNeedBeforeGreed state






rollResult :: [NB4G]-> [NB4G]
rollResult [] = []
rollResult rollers = rollNB4G (rollers)
