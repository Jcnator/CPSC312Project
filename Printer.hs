module Printer where

  import Roller
  import NeedBeforeGreed
  import System.IO
  import Text.Read   (readMaybe)
  import Data.Maybe   (fromJust)
  import System.IO.Unsafe


  -- Prints the contents of a Roll
  printRoll :: Roll->IO ()
  printRoll roll = do
    let tot = total roll
    let dice = rolls roll
    let discarded_dice = discarded roll
    if discarded_dice == []
      then putStrLn("Rolled: " ++ show tot ++ " from " ++ show dice)
      else putStrLn("Rolled: " ++ show tot ++ " from " ++ show dice ++ " discarded " ++ show discarded_dice)

  -- Prints the contents of a NB4G
  printNB4G :: NB4G -> IO ()
  printNB4G player = do
    let n = name player
    let rt = rolltype player
    let r = result player
    putStrLn(show rt ++ " Roll- " ++ show r ++ " by " ++  n)
