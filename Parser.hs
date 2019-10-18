module Parser where
  import Data.List
  import Roller
  import NeedBeforeGreed
  import Printer
  import Text.Read
  import PlayNeedBeforeGreed




  -- mainParser: Main Parsing function that parses the command line input string into a set of valid integers
  -- And calls the rolldice and needBeforeGreed functions, essentially initializing the game
  -- If input string from command line is not a valid integer it handles those cases with Invalid Input returns
  -- Provides a list of all commmands and distributes variables to their respective cases depending on the input command
  mainParser:: String->IO ()
  mainParser input = do
    let key_val = head input
    if key_val == '$'
      then do
        putStrLn "Starting Need Before Greed"
        playNeedBeforeGreed []
      else if
        key_val == '?'
        then do
          putStrLn "Roll commands:"
          putStrLn "Roll an X number of dice of Y sides with with the format:"
          putStrLn "  XdY Example: 1d20, 2d100, 3d6"
          putStrLn "Keep the Kst highst or lowest die with:"
          putStrLn "  Example: 3d20kh1 returns the highest die of 3 Twenty Sided Dice"
          putStrLn "  Example: 3d20kl1 returns the lowest die of 3 Twenty Sided Dice"
          putStrLn "  Example: 3d20kh2 returns the second highest roll of 3 Twenty Sided Dice"
          putStrLn "  Example: 3d20kl2 returns the second lowest roll of 3 Twenty Sided Dice"
          putStrLn "Keep the the K higher or lower dice:"
          putStrLn "  Example: 4d6kH3 returns the highest 3 dice of the four rolls"
          putStrLn "  Example: 4d6kL3 returns the lowest 3 dice of the four rolls"
          putStrLn ""
          putStrLn "Play Need Before Greed with the '$' command"


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



-- makeLists: Makes Lists of strings accordingly from the command line input and the separator
-- i.e. "2d10" gets broken into ["2" ,"10"]
-- It takes care of grabbing the integer part of the command line strings in order to make lists which are then parsed
-- the separator is a function that makes sure that the string is separated at the right place, it should always be a "k" or a "d"
  makeLists :: (Char->Bool)->String -> [String]
  makeLists sep [] = [[]]
  makeLists sep (h:t)
      | sep h = [] : makeLists sep t
      | otherwise = ((h:w):rest)
                  where w:rest = makeLists sep t


  -- helper function that gets passed into the makeLists function as the separator input, 
  -- checks for the "d" string in the input
  separator_d :: Char -> Bool
  separator_d char = char == 'd'

  -- helper function that gets passed into the makeLists function as the separator input, 
  -- checks for the "k" string in the input

  separator_k :: Char -> Bool
  separator_k char = char == 'k'

  -- helper function that gets passed into the makeLists function as the separator input, 
  -- checks for the "d" or the "k" string in the input

  separator_dnk :: Char -> Bool
  separator_dnk char = char == 'd' || char == 'k'


  --toInt: reads the String arguments given by the command line and returns either a valid integer or an exception which is handled
  -- in the mainParser function
  toInt :: String -> Maybe Int
  toInt = readMaybe
