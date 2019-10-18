module PlayNeedBeforeGreed where


import NeedBeforeGreed
import Roller
import Printer

import Data.List
import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)
import Control.Monad


-- Starts a Need Before greed.
-- Lottery game to decide who gets to keep an item.
-- Players will either Pass meaning they don't play
-- Select Greed, meaining that they will be going against other Greed Rolls
-- Or Select Need if they really need the item. Need always wins over Greed
-- Function will be called with an empty list at the Start
-- The function will ask for the name of the player and their roll type and
-- recursively add them to a new instance of the game which is represented by
-- a [NB4G].
playNeedBeforeGreed :: [NB4G] -> IO ()
playNeedBeforeGreed state = do
  putStrLn "Name? (Finish with '0')"
  line <- getLine
  if line == "0"
    then do
      if state /= [] -- There is at least one player
        then do
          -- a new state resultState is created from the rollResult
          -- function then the printNB4G is mapped to print out the results
          -- the winner is found and the game delcares who won
          let resultState = rollResult state
          mapM (printNB4G) resultState
          let winner = winnerNB4G resultState
          let winner_name = name winner
          putStrLn ("Winner is: " ++ winner_name ++ "!")
      else do
        -- Game ends without any players so no rolls are made
        putStrLn "Need Before Greed has ended"
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
        -- On Rolltype selection the function asks the player to try again
        -- if their input was not valid
        putStrLn "Could not understand, please try again"
        playNeedBeforeGreed state


-- Calls the rollNB4G function and handles the empty list case
rollResult :: [NB4G]-> [NB4G]
rollResult [] = []
rollResult rollers = rollNB4G (rollers)


