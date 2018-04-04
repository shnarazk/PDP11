module Main where

import ASM
import AsmParser
import Simulator

main :: IO ()
main = do
  str <- getContents
  x <- readASM str
  case x of
    Just programs -> do let states = runSimulator' programs
                            instrs = "# Initial state" : map show programs
                        mapM_ (\(n, m) -> do putStrLn n; print m) $ zip instrs states
    Nothing -> return ()
