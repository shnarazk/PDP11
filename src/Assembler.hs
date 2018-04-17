module Assembler
    (
      version
    , assemble
    ) where

import Text.Parsec
import Text.Parsec.Char
import PDP11 hiding (version)

version :: String
version = "0.4.2"

assemble :: String -> Either String [ASM]
assemble str = case parse readASMs "ERROR" str of
                Left err -> let l = lines str !! (sourceLine (errorPos err) - 1)
                                i = replicate (sourceColumn (errorPos err) -1) ' ' ++ "^\n"
                            in Left (l ++ "\n" ++ i ++ show err)
                Right x  -> Right x

readASMs :: Parsec String () [ASM]
readASMs = many1 (spaces *> commands <* newline) <* eof
  where
    commands = choice [ asmMOV
                      , asmADD
                      , asmSUB
                      , asmMUL
                      , asmCLR
                      ]
--               <?> "MOV, ADD, SUB, MUL, or CLR."

registerId :: Parsec String () RegId
registerId = do
  r <-   char 'R' *> integer
  if 0 <= r && r <= 7
    then return $ Reg r
    else fail "Error: the register number should be 0 to 7."

addrMode :: Parsec String () AddrMode
addrMode = choice [ addrModeRegister
                  , addrModeImmediate
                  , addrModeIndex
                  , addrModeAutoInc
                  , addrModeAutoDec
                  , addrModeIndirect
                  ]
           <?> "one of 'Ri', '(Ri)+', '-(Ri)', 'n(Ri)', '@Ri', '@(Ri)+', '@-(Ri)', '@n(Ri), '#n'"

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
asmMOV = uncurry MOV <$> (try (string "MOV ") *> twoAddrs)

asmADD :: Parsec String () ASM
asmADD = uncurry ADD <$> (try (string "ADD ") *> twoAddrs)

asmSUB :: Parsec String () ASM
asmSUB = uncurry ADD <$> (try (string "SUB ") *> twoAddrs)

asmMUL :: Parsec String () ASM
asmMUL = uncurry ADD <$> (try (string "MUL ") *> twoAddrs)

asmCLR :: Parsec String () ASM
asmCLR = CLR <$> (try (string "CLR ") *> oneAddr)

integer :: Parsec String () Int
integer = read <$> many1 digit

line :: Parsec String () ASM
line = do
  x <- integer
  return $ MOV (Register (Reg x)) (Register (Reg x))
