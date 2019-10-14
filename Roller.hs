module Roller where

import System.Random
import Data.List
import System.IO.Unsafe


data Roll  = Roll Int [Int]

makeRoll :: [Int] -> Roll
makeRoll list =
  Roll (listSum list) list

rollxdy :: Int->Int->Roll
rollxdy x y =
  makeRoll (listRoller x y [])





-- listRoller :: Int -> Int ->[Int] -> IO [Int]
-- listRoller a b list =
--   if a == 0 || b == 0
--     then
--       return  list
--     else do
--       n <- getStdRandom (randomR (1,b))
--       listRoller (a-1) b (n : list)


listRoller :: Int -> Int ->[Int] -> [Int]
listRoller a b list =
  if a == 0 || b == 0
    then
      return list !! 0
    else do
      let n = unsafePerformIO (getStdRandom (randomR (1,b)))
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
listKeepHklist list k =
  fst (splitAt k (reverse (sort list)))

listKeepLklist :: [Int] -> Int -> [Int]
listKeepLklist list k =
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
