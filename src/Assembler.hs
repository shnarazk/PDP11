{-# LANGUAGE TemplateHaskell #-}
module Assembler
    (
      module AssemblerBase
    , assemble
    ) where

import Text.Parsec
import PDP11 hiding (version)
import AssemblerBase

version :: String
version = "0.9.3+"

$(mkParsers codeTable)

assemble :: String -> Either String [ASM]
assemble str = case parse readASMs "ERROR" str of
                Left err -> let l = lines str !! (sourceLine (errorPos err) - 1)
                                i = replicate (sourceColumn (errorPos err) -1) ' ' ++ "^\n"
                            in Left (l ++ "\n" ++ i ++ show err)
                Right x  -> Right x

readASMs :: Parsec String () [ASM]
readASMs = many1 (spaces *> commands <* newline) <* eof
  where commands = choice $(parserNames codeTable)
