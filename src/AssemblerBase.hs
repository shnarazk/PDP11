{-# LANGUAGE TemplateHaskell #-}
module AssemblerBase
    (
      version
    , mkParsers
    , parserNames
    ) where

import Control.Monad
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsec
import Text.Parsec.Char
import PDP11 hiding (version)

version :: String
version = "0.9.3+"

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

integer :: Parsec String () Int
integer = read <$> many1 digit

ninteger :: Parsec String () Int
ninteger = char '-' *> (negate <$> integer)

pminteger :: Parsec String () Int
pminteger = choice [ninteger, integer]

-- :set -XTemplateHaskell
-- :set -XQuasiQuotes
-- :m +Language.Haskell.TH
-- runQ [d| asmBIS = uncurry (Inst2 BIS) <$> (try (string "BIS ") *> twoAddrs) |]
-- runQ [d| asmDEC = Inst1 DEC <$> (try (string "DEC ") *> oneAddr) |]
-- runQ [d| asmBR  = Inst0 BR <$> (try (string "BR ") *> pminteger) |]

mkParsers :: [(Opcode, [Int], OpFormat)] -> Q [Dec]
mkParsers l = concat <$> mapM mkParser l

mkParser :: (Opcode, [Int], OpFormat) -> DecsQ
mkParser (op, _, ki) = do
  let name = show op
  case ki of
    OFA2 -> [d| $((return . VarP . mkName) ("asm" ++ name)) = uncurry (Inst2 $((return . ConE  . mkName) name)) <$> (try (string (name ++ " ")) *> twoAddrs) |]
    OFA1 -> [d| $((return . VarP . mkName) ("asm" ++ name)) = Inst1 $((return . ConE . mkName) name) <$> (try (string (name ++ " ")) *> oneAddr) |]
    OFI1 -> [d| $((return . VarP . mkName) ("asm" ++ name)) = Inst0 $((return . ConE . mkName) name) <$> (try (string (name ++ " ")) *> pminteger) |]
    _    -> return $ []

parserNames :: [(Opcode, [Int], OpFormat)] -> Q Exp
parserNames l = return . ListE $ map nameOfParser l
nameOfParser :: (Opcode, [Int], OpFormat) -> Exp
nameOfParser (op, _, _) = (VarE . mkName) ("asm" ++ show op)
