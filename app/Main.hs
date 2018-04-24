module Main where

import Data.List
import PDP11
import Assembler
import Simulator

main :: IO ()
main = do str <- getContents
          case runPDP11 str of
            Just result -> putStrLn result
            Nothing -> putStrLn "wrong code"
          let form l = l' ++ replicate (32 - length l') ' '
                where
                  l' = reverse . dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t") $ l
              printer l (i, b) = do let head = form (if i == 0 then l else "")
                                    putStrLn $ head ++ show b
          let toBit l = case assemble (l ++ "\n") of
                Right [as] -> mapM_ (printer l) (zip [0 ..] (toBitBlocks as))
                Left mes -> print mes
          mapM_ toBit (lines str)
