module AsmParser
    (
      readASM
    ) where

import Text.Parsec
import Text.Parsec.Char
import ASM

readASM :: String -> IO (Maybe [ASM])
readASM str = case (parse readASMs "stdin" str) of
                Left err -> do print err; return Nothing
                Right x -> return $ Just x

readASMs :: Parsec String () [ASM]
readASMs = many1 (commands <* newline)
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
                  , addrModeIndirect
                  , addrModeIndexed
                  , addrModeAutoInc
                  , addrModeAutoDec
                  , addrModeImmediate
                  ]

addrModeRegister :: Parsec String () AddrMode
addrModeRegister = Register <$> registerId

addrModeIndirect :: Parsec String () AddrMode
addrModeIndirect = do
  char '@'
  Indirect <$> registerId

addrModeIndexed :: Parsec String () AddrMode
addrModeIndexed = do
  x <- integer <* char '('
  r <- registerId <* char ')'
  return $ Indexed x r

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
