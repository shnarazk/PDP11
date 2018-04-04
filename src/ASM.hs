module ASM
    (
      RegId(..)
    , AddrMode(..)
    , ASM(..)
    ) where

data RegId = Reg Int
  deriving (Eq, Ord, Read, Show)  

data AddrMode
  = Register RegId
  | Indirect RegId
  | Indexed Int RegId
--  | PCrelat Int
  | AutoInc RegId
  | AutoDec RegId
  | Immediate Int
  deriving (Eq, Ord, Read, Show)  

data ASM
  = MOV AddrMode AddrMode
  | ADD AddrMode AddrMode
  | SUB AddrMode AddrMode
  deriving (Eq, Ord, Read, Show)
