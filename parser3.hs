module Parser where
  import Data.List
  import Roller
  import NeedBeforeGreed
  import Printer

 mainParser :: String->IO
 mainParser input =

  putStrLn "Enter a roll"



  let lists_d = makeLists separator_d input
  let lists_dnk = makeLists separator_dnk input
  let lists_k = makeLists separator_k input
   if  length lists_d /= 2 || length lists_k /= 2
     putStrLn "Invalid Input, please write as xdykz" 

   else if lists_dnk == 2 
    do
      let int1 = toInt (lists_d!!0) :: Maybe Int
      case int1 of  
        Just n -> int1 = n
        Nothing -> do 
          putStrLn "Invalid Integer, enter again"
          return
      let int2 = toInt(lists_d!!1) :: Maybe Int
      case int2 of
        Just n -> int2 = n
        Nothing-> do 
          putStrLn "Invalid Integer, enter again"
          return
      
      printRoll (rollxdy int1 int2)  
      --putStrLn (show rollToIo (Roll  result = rollxdy int1 int2))
  --  rollToIO (Roll result = rollxdy toInteger(lists[0]) toInteger(lists[1]))
     -- else if lists_dnk == 3
     --   do
     --    let intk1 = toInt (lists_dnk!!0) :: Maybe Int
     --    case intk1 of
     --      Just n -> intk1 = n
     --      Nothing -> do
     --        putStrLn "Invalid Integer, enter again"
     --        return
     --    let intk2 = toInt (lists_dnk!!1) :: Maybe Int
     --    case intk2 of 
     --      Just n -> intk2 = n
     --      Nothing -> do
     --        putStrLn "Invalid Integer, enter again"
     --        return
     --    let intk3 = toInt (tail(lists_dnk!!2)) :: Maybe Int
     --    case intk3 of
     --      Just n -> intk3 = n
     --      Nothing -> do
     --        putStrLn "Invalid Integer, enter again"
     --        return

     --    if (head (lists_dnk!!2)) == 'h'
     --      then printRoll (rollxdykh intk1 intk2 intk3)
     --    else if (head(lists_dnk!!2)) == 'l'
     --      then printRoll (rollxdykl intk1 intk2 intk3)
     --    else if (head(lists_dnk!!2)) == 'H'
     --      then printRoll (rollxdykH intk1 intk2 intk3)
     --    else if (head(lists_dnk!!2)) == 'L'
     --      then printRoll (rollxdykL intk1 intk2 intk3)
     --    else 
     --      putStrLn ("Invalid k operator. Try kh, kl, kH, KL.")
          



 -- rollToIO :: Roll -> IO
 -- rollToIO roll = ...

 

--  removeChar :: Char->String->String
--  removeChar char input = 
    
      

  makeLists :: (Char->Char->Bool)->String -> [String]
  makeLists sep [] = [[]]
  makeLists sep (h:t)
      | sep h = [] : makeLists sep t
      | otherwise = ((h:w):rest)
                  where w:rest = makeLists sep t

  toInt :: String -> Maybe Int
  toInt = readMaybe 

 -- separator :: Char->Char->Bool
 -- separator sep char = char == sep

 separator_d :: Char -> Bool
 separator_d char = char == 'd'
   
 separator_k :: Char -> Bool
 separator_k char = char == 'k'

 separator_dnk :: Char -> Bool
 separator_dnk char = char == 'd' || char == 'k'

  
 separator_h :: Char -> Bool
 separator_h char = char == 'h'
  --
 separator_l :: Char -> Bool
 separator_l char = char == 'l'
  --
 separator_H :: Char -> Bool
 separator_H char = char == 'H'
  --
 separator_L :: Char -> Bool
 separator_L char = char == 'L'



