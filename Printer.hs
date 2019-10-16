module Printer where

  import Roller
  import NeedBeforeGreed
  import System.IO
  import Text.Read   (readMaybe)
  import Data.Maybe   (fromJust)
  import System.IO.Unsafe


  printRoll :: Roll->IO ()
  printRoll roll = do
    let tot = total roll
    let dice = rolls roll
    putStrLn("Rolled a " ++ show tot ++ "from " ++ show dice)

  printNB4G :: NB4G -> IO NB4G
  printNB4G player = do
    let n = name player
    let rt = rolltype player
    let r = result player
    putStrLn(show rt ++ " Roll- " ++ show r ++ " by " ++  n)
    return player

  -- printNB4GIO :: NB4G -> IO ()
  -- printNB4GIO player = do
  --   let n = name player
  --   let rt = rolltype player
  --   let r = result player
  --   putStrLn(show rt ++ " Roll- " ++ show r ++ " by " ++ show n)
