module Assembler
    (
      version
    , assemble
    ) where

import Text.Parsec
import Text.Parsec.Char
import PDP11 hiding (version)

version :: String
version = "0.3.0"

assemble :: String -> Maybe [ASM]
assemble str = case parse readASMs "stdin" str of
                Left err -> Nothing
                Right x  -> Just x

readASMs :: Parsec String () [ASM]
readASMs = many1 (spaces *> commands <* newline)
  where
    commands = choice [ asmMOV
                      , asmADD
                      , asmSUB
                      , asmMUL
                      , asmCLR
                      ]

registerId :: Parsec String () RegId
registerId = do
  r <- char 'R' *> many1 digit
  return $ Reg (read r)

addrMode :: Parsec String () AddrMode
addrMode = choice [ addrModeRegister
                  , addrModeImmediate
                  , addrModeIndex
                  , addrModeAutoInc
                  , addrModeAutoDec
                  , addrModeIndirect
                  ]

addrModeRegister :: Parsec String () AddrMode
addrModeRegister = Register <$> registerId

addrModeIndirect :: Parsec String () AddrMode
addrModeIndirect = do
  char '@'
  Indirect <$> addrMode

addrModeIndex :: Parsec String () AddrMode
addrModeIndex = do
  x <- integer <* char '('
  r <- registerId <* char ')'
  return $ Index x r

addrModeAutoInc :: Parsec String () AddrMode
addrModeAutoInc = do
  r <- char '(' *> registerId
  string ")+"
  return $ AutoInc r

addrModeAutoDec :: Parsec String () AddrMode
addrModeAutoDec = do
  r <- string "-(" *> registerId
  char ')'
  return $ AutoDec r

addrModeImmediate :: Parsec String () AddrMode
addrModeImmediate = do
  char '#'
  Immediate <$> integer

oneAddr :: Parsec String () AddrMode
oneAddr = do
  x <- spaces *> addrMode
  return x

twoAddrs :: Parsec String () (AddrMode, AddrMode)
twoAddrs = do
  x <- spaces *> addrMode
  char ','
  y <- spaces *> addrMode
  return $ (x, y)

asmMOV :: Parsec String () ASM
asmMOV = uncurry MOV <$> (string "MOV" *> twoAddrs)

asmADD :: Parsec String () ASM
asmADD = uncurry ADD <$> (string "ADD" *> twoAddrs)

asmSUB :: Parsec String () ASM
asmSUB = uncurry ADD <$> (string "SUB" *> twoAddrs)

asmMUL :: Parsec String () ASM
asmMUL = uncurry ADD <$> (string "MUL" *> twoAddrs)

asmCLR :: Parsec String () ASM
asmCLR = CLR <$> (string "CLR" *> oneAddr)

integer :: Parsec String () Int
integer = read <$> many1 digit

line :: Parsec String () ASM
line = do
  x <- integer
  return $ MOV (Register (Reg x)) (Register (Reg x))
