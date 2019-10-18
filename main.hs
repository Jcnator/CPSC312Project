module Main where
import Parser

main = do

    
    putStrLn "Enter a command, example: 1d20 "
    line <- getLine
    mainParser line>> main
   


  
    	