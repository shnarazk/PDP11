{-# LANGUAGE TemplateHaskell #-}
module Assembler
    (
      version
    , assemble
    ) where

import Language.Haskell.TH
import Text.Parsec
import Text.Parsec.Char
import PDP11 hiding (version)

version :: String
version = "0.9.3+"

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
                      , asmCMP
                      , asmBIT
                      , asmBIC
                      , asmBIS
                      , asmINC
                      , asmDEC
                      , asmNEG
                      , asmCLR
                      , asmASL
                      , asmASR
                      , asmJMP
                      , asmBR
                      , asmBNE
                      , asmBEQ
                      ]

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
  Immediate <$> pminteger

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

---------------------------------------------------------

asmMOV :: Parsec String () ASM
asmMOV = uncurry (Inst2 MOV) <$> (try (string "MOV ") *> twoAddrs)

asmADD :: Parsec String () ASM
asmADD = uncurry (Inst2 ADD) <$> (try (string "ADD ") *> twoAddrs)

asmSUB :: Parsec String () ASM
asmSUB = uncurry (Inst2 SUB) <$> (try (string "SUB ") *> twoAddrs)

asmCMP :: Parsec String () ASM
asmCMP = uncurry (Inst2 CMP) <$> (try (string "CMP ") *> twoAddrs)

asmBIT :: Parsec String () ASM
asmBIT = uncurry (Inst2 BIT) <$> (try (string "BIT ") *> twoAddrs)

asmBIC :: Parsec String () ASM
asmBIC = uncurry (Inst2 BIC) <$> (try (string "BIC ") *> twoAddrs)

asmBIS :: Parsec String () ASM
asmBIS = uncurry (Inst2 BIS) <$> (try (string "BIS ") *> twoAddrs)

-- asmMUL :: Parsec String () ASM
-- asmMUL = uncurry ADD <$> (try (string "MUL ") *> twoAddrs)

asmINC :: Parsec String () ASM
asmINC = Inst1 INC <$> (try (string "INC ") *> oneAddr)

asmDEC :: Parsec String () ASM
asmDEC = Inst1 DEC <$> (try (string "DEC ") *> oneAddr)

asmNEG :: Parsec String () ASM
asmNEG = Inst1 NEG <$> (try (string "NEG ") *> oneAddr)

asmCLR :: Parsec String () ASM
asmCLR = Inst1 CLR <$> (try (string "CLR ") *> oneAddr)

asmASL :: Parsec String () ASM
asmASL = Inst1 ASL <$> (try (string "ASL ") *> oneAddr)

asmASR :: Parsec String () ASM
asmASR = Inst1 ASR <$> (try (string "ASR ") *> oneAddr)

asmJMP :: Parsec String () ASM
asmJMP = Inst1 JMP <$> (try (string "JMP ") *> oneAddr)

asmBR :: Parsec String () ASM
asmBR  = Inst0 BR <$> (try (string "BR ") *> pminteger)

asmBNE :: Parsec String () ASM
asmBNE = Inst0 BNE <$> (try (string "BNE ") *> pminteger)

asmBEQ :: Parsec String () ASM
asmBEQ = Inst0 BEQ <$> (try (string "BEQ ") *> pminteger)

integer :: Parsec String () Int
integer = read <$> many1 digit

ninteger :: Parsec String () Int
ninteger = char '-' *> (negate <$> integer)

pminteger :: Parsec String () Int
pminteger = choice [ninteger, integer]
