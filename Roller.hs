module Roller where

import System.Random
import Data.List
import System.IO.Unsafe

-- Roll is made of a final "result" which is the sum of the individual rolls in [Int]
data Roll  = Roll Int [Int] deriving (Show)

-- Creates a Roll data type
makeRoll :: [Int] -> Roll
makeRoll list =
  Roll (listSum list) list

-- Basic roll: rolls x number of y sided dice
rollxdy :: Int->Int->Roll
rollxdy x y =
  makeRoll (listRoller x y [])

-- Keep kst high: rolls x number of y sided dice and keep kst high
rollxdykH :: Int->Int->Int->Roll
rollxdykH x y k =
  --let list = listRoller x y []
  --listKstH (listRoller x y [])  k
  makeRoll [listKstH (listRoller x y [])  k]

-- Keep kst low: rolls x number of y sided dice and keep kst low
rollxdykL:: Int->Int->Int->Roll
rollxdykL x y k =
  makeRoll [listKstL (listRoller x y [])  k]

-- Keep k number of highest dice
rollxdykHst :: Int->Int->Int->Roll
rollxdykHst x y k =
  makeRoll (listKeepHklist (listRoller x y [])  k)

-- Keep k number of lowest dice
rollxdylHst :: Int->Int->Int->Roll
rollxdylHst x y k =
    makeRoll (listKeepHklist (listRoller x y [])  k)


-- listRoller :: Int -> Int ->[Int] -> IO [Int]
-- listRoller a b list =
--   if a == 0 || b == 0
--     then
--       return  list
--     else do
--       n <- getStdRandom (randomR (1,b))
--       listRoller (a-1) b (n : list)

-- Rolls a list of dice result in the adb form
listRoller :: Int -> Int ->[Int] -> [Int]
listRoller a b list =
  if a == 0 || b == 0
    then
      return list !! 0
    else do
      let n = unsafePerformIO (getStdRandom (randomR (1,b)))
      listRoller (a-1) b (n : list)

-- sums the contents of a list
listSum :: [Int]->Int
listSum list =
  foldr (+)  0 list

-- returns the kst high value of a list
listKstH :: [Int]-> Int ->Int
listKstH list  k =
  (reverse (sort list)) !! (k-1)

-- returns the kst low value of a list
listKstL :: [Int]-> Int ->Int
listKstL list  k =
  (sort list) !! (k-1)

-- keeps only the highest k values of a list
listKeepHklist :: [Int] -> Int -> [Int]
listKeepHklist list k =
  fst (splitAt k (reverse (sort list)))

-- keeps only the lowest k values of a list
listKeepLklist :: [Int] -> Int -> [Int]
listKeepLklist list k =
    fst (splitAt k (sort list))

-- roller :: Int -> Int -> Int ->  IO Int
-- roller a b total  =
--    if a == 0 || b == 0
--      then do
--        --putStrLn("Total: " ++ total)
--        return total
--      else do
--       n <- getStdRandom (randomR (1,b))
--       -- n : (Dice roll)
--       print n
--       roller (a-1) b (n + total)
