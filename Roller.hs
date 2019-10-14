module Roller where

import System.Random
import Data.List

data Roll  = Total  Int
            | Dice  [Int]

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

listSum :: [Int]->Int
listSum list =
  foldr (+)  0 list


listKstH :: [Int]-> Int ->Int
listKstH list  k =
  (reverse (sort list)) !! (k-1)

listKstL :: [Int]-> Int ->Int
listKstL list  k =
  (sort list) !! (k-1)

listKeepHklist :: [Int] -> Int -> [Int]
listKeepHlist list k =
  fst (splitAt k (reverse (sort list)))

listKeepLklist :: [Int] -> Int -> [Int]
listKeepLlist list k =
    fst (splitAt k (sort list))

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
