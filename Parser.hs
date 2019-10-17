module Parser where
  import Data.List
  import Roller
  import NeedBeforeGreed
  import Printer
  import Text.Read
  import PlayNeedBeforeGreed

  --{-# LANGUAGE BlockArguments #-}


  mainParser:: String->IO ()
  mainParser input = do
    let key_val = head input
    if key_val == '$'
      then do
        putStrLn "Starting Need Before Greed"
        playNeedBeforeGreed []
        --putStrLn "Need Before Greed Ended"
      else do
        let lists_d = makeLists (separator_d) input
        let lists_k = makeLists (separator_k) input
        let k_string = last(lists_k)
        let k_value = tail(k_string)
        let lists_dnk = makeLists (separator_dnk) input
        let lists_clean_dnk = [(lists_dnk !! 0) , (lists_dnk !! 1) , k_value]
        -- putStrLn (show lists_d)
        -- putStrLn (show lists_k)
        -- putStrLn (show k_string)
        -- putStrLn (show k_value)
        -- putStrLn (show lists_dnk)
        -- putStrLn (show lists_clean_dnk)
        if length lists_dnk == 2
          then
            case (map toInt lists_d) of
                [Just n1,Just n2] ->
                            do
                              if (n1 > 0 && n2 > 0)
                                then printRoll (rollxdy n1 n2)
                                else putStrLn "Invalid Input: Negative value"
                _ -> putStrLn "Invalid Input"
        else if length lists_dnk == 3
              then
                --let lists_dnk = lists_d ++ (tail lists_k)
                case (map toInt lists_clean_dnk) of
                    [Just n1, Just n2, Just n3] ->
                                do
                                  if (n1 > 0 && n2 > 0 && n3 > 0 && ((n1 > n3) || n1 == n3 ))
                                    then
                                      if head(k_string) == 'h'
                                        then printRoll(rollxdykh n1 n2 n3)
                                      else if head(k_string) == 'l'
                                          then printRoll(rollxdykl n1 n2 n3)
                                      else if head(k_string) == 'H'
                                        then printRoll(rollxdykH n1 n2 n3)
                                      else if head(k_string) == 'L'
                                        then printRoll(rollxdykL n1 n2 n3)
                                      else putStrLn "Invalid Input: 'h,l,H, or L' go after k"
                                    else putStrLn "Invalid Input: Negative value or k index too large"
                    _ -> putStrLn "Invalid Input "
              else putStrLn "Invalid Input"




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

  separator_dnk :: Char -> Bool
  separator_dnk char = char == 'd' || char == 'k'

  toInt :: String -> Maybe Int
  toInt = readMaybe
