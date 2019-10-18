module Roller where

import System.Random
import Data.List
import System.IO.Unsafe

-- Roll is made of a final "result" which is the sum of
-- the individual rolls. Roll also has a discarded field that holds
-- any of the dice rolled that were not used for the total
data Roll  = Roll {total :: Int
                  , rolls :: [Int]
                  , discarded :: [Int]
                  } deriving (Show)

-- Creates a Roll data type, the first member of the input is used rolls
-- The second member of the input tuple is the rolls that are to be discarded
-- input is a tuple to pass the results of the roller functions directly to
-- make roll since it has a random element in it
makeRoll :: ([Int],[Int]) -> Roll
makeRoll input  = Roll (listSum (fst input)) (fst input) (snd input)

-- Basic roll: rolls x number of y sided dice
rollxdy :: Int->Int->Roll
rollxdy x y = makeRoll ((listRoller x y []), [])

-- Keep kst high: rolls x number of y sided dice and keep kst high
rollxdykh :: Int->Int->Int->Roll
rollxdykh x y k =  makeRoll (listKstH (listRoller x y []) k)

-- Keep kst low: rolls x number of y sided dice and keep kst low
rollxdykl:: Int->Int->Int->Roll
rollxdykl x y k = makeRoll (listKstL (listRoller x y []) k)

--Keep k number of highest dice
rollxdykH :: Int->Int->Int->Roll
rollxdykH x y k = makeRoll (listKeepHklist (listRoller x y [])  k)
--
-- Keep k number of lowest dice
rollxdykL :: Int->Int->Int->Roll
rollxdykL x y k = makeRoll (listKeepLklist (listRoller x y [])  k)


-- Rolls a list of dice result in the adb form
listRoller :: Int -> Int ->[Int] -> [Int]
listRoller a b list =
  if a == 0 || b == 0
    then
      return list !! 0
    else do
      let n = unsafePerformIO (getStdRandom (randomR (1,b)))
      listRoller (a-1) b (n : list)

-- Sums the contents of a list of integers
listSum :: [Int]->Int
listSum list = foldr (+)  0 list

-- Returns the kst high value of a list as the first member of the tuple, and the
-- all the other values as the second member
listKstH :: [Int]-> Int ->([Int],[Int])
listKstH list  k =
  ([(reverse (sort list)) !! (k-1)],
  (take (k-1) (reverse (sort list))) ++ (drop (k)  (reverse (sort list))))

-- Returns the kst low value of a list as the first member of the tuple, and the
-- all the other values as the second member
listKstL :: [Int]-> Int ->([Int],[Int])
listKstL list  k =
  ([(sort list) !! (k-1)],
  (take (k-1) (sort list)) ++ (drop (k)   (sort list)))

-- Returns the highest k values of a list as the first member of the tuple, and the
-- all the other values as the second member
listKeepHklist :: [Int] -> Int -> ([Int],[Int])
listKeepHklist list k = (splitAt k (reverse (sort list)))

-- Returns the highest k values of a list as the first member of the tuple, and the
-- all the other values as the second member
listKeepLklist :: [Int] -> Int -> ([Int],[Int])
listKeepLklist list k = (splitAt k (sort list))
