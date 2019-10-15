module Parser where
  import Data.List
  import Roller
  import NeedBeforeGreed

mainParser :: String->IO
mainParser input =
  let lists_d = makeLists separator_d input
  if  length lists_d /= 2 or lists_d elements are empty
    return error
  else if lists_d elements are integers
    rollToIO (Roll result = rollxdy toInteger(lists[0]) toInteger(lists[1]))
    else
      let lists_k = makeLists separator_k(lists_d !! 1)


 rollToIO :: Roll -> IO
 rollToIO roll = ...



  makeLists :: (Char->Bool)->String -> [String]
  makeLists sep [] = [[]]
  makeLists sep (h:t)
      | sep h = [] : makeLists sep t
      | otherwise = ((h:w):rest)
                  where w:rest = makeLists sep t


  separator_d :: Char -> Bool
  separator_d char = char == 'd'

  separator_k :: Char -> Bool
  separator_k char = char == 'k'

  separator_h :: Char -> Bool
  separator_h char = char == 'h'

  separator_l :: Char -> Bool
  separator_l char = char == 'l'

  separator_H :: Char -> Bool
  separator_H char = char == 'H'

  separator_L :: Char -> Bool
  separator_L char = char == 'L'
