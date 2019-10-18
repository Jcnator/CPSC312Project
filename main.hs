module Main where
import Parser

main = do

    
    putStrLn "Enter a command, type '?' for list of commands"
    line <- getLine
    mainParser line>> main
   


  
    	
