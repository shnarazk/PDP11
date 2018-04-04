module Main where

import ASM
import AsmParser
import Simulator

main :: IO ()
main = do
  str <- getContents
  x <- readASM str
  case x of
    Just programs -> do let states = reverse $ runSimulator' programs
                        print (head states)
                        mapM_ (\(n, m) -> do print n; print m) $ zip programs (tail states)
    Nothing -> return ()
