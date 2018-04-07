module Main where

import PDP11
import Assembler
import Simulator

main :: IO ()
main = do str <- getContents
          case runPDP11 str of
            Just result -> putStrLn result
            Nothing -> putStrLn "wrong code"
