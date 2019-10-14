module Roller where

import System.Random

data Roll  = Total  Int
            | Dice [Int]

-- rollSum :: Int -> [IO Int] -> Int
-- rollSum total rolls = do
--   n <- rolls !! 1
--   drop 1 rolls
--   rollSum (total+n) rolls



listRoller :: Int -> Int ->[Int] -> IO [Int]
listRoller a b list =
  if a == 0 || b == 0
    then
      return  list
    else do
      n <- getStdRandom (randomR (1,b))
      listRoller (a-1) b (n : list)



roller :: Int -> Int -> Int ->  IO Int
roller a b total  =
   if a == 0 || b == 0
     then do
       --putStrLn("Total: " ++ total)
       return total
     else do
      n <- getStdRandom (randomR (1,b))
      -- n : (Dice roll)
      print n
      roller (a-1) b (n + total)




-- roller a b =
--       -- g <- getStdGen
--       -- n <- fst randomR (1, b) g
--   let n = 1
--   return 0
      --print n
      --roller (a-1) b
