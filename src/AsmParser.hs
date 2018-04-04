module AsmParser
    (
      readASM
    , readASMs
    ) where

import Text.Parsec
import Text.Parsec.Char
import ASM

readASM :: String -> IO (Maybe [ASM])
readASM str = case (parse readASMs "stdin" str) of
                Left err -> do print err; return Nothing
                Right x -> return $ Just x

readASMs :: Parsec String () [ASM]
readASMs = sepBy asmMove newline

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

asmMove :: Parsec String () ASM
asmMove = do
  string "MOV"
  x <- spaces *> addrMode
  y <- spaces *> addrMode
  newline
  return $ MOV x y

integer :: Parsec String () Int
integer = read <$> many1 digit

line :: Parsec String () ASM
line = do
  x <- integer
  return $ MOV (Register (Reg x)) (Register (Reg x))
