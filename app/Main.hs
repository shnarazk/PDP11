module Main where

import Data.List
import PDP11
import Assembler
import Simulator

main :: IO ()
main = do str <- getContents
          let code = assemble str
          case code of
            Right as -> mapM_ putStrLn $ showBinaryCode 100 as
            Left s   -> putStrLn s
          -- print $ (_memory . head . runSimulator 32 initialMachine) <$> assemble str
          case (fromTrace . runSimulator 32 initialMachine) <$> code of
            Right result -> putStrLn result
            Left _ -> putStrLn "wrong code"

showBinaryCode :: Int -> [ASM] -> [String]
showBinaryCode start prg = zipWith merge [start, start + 2 ..] $  concatMap (printer . toBitBlocks) prg
  where printer (oc:ols) = l1 : map show ols
          where l1 = show oc ++ "         # " ++ show (decodeWord (asInt oc) (ols' !! 0, ols' !! 1))
                ols' = map asInt ols ++ [0, 0]
        merge n s = show n ++ "        " ++ s
