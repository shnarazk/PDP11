module PDP11
    (
      Machine(..)
    , Locator(..)
    , RegId(..)
    , AddrMode(..)
    , ASM(..)
    , pdp11
    ) where

import Data.Array

data Machine
  = Machine
    {
      _memory   :: Array Int Int
    , _register :: Array Int Int
--  , _psw      :: [Bool]
--  , _insts    :: [ASM]
    }
  deriving (Eq, Ord, Read)

pdp11 :: [ASM] -> Machine -- memory is at left; register is at right.
pdp11 _ = Machine (chunk 16 [0, 10, 0, 20, 0, 40, 1, 255]) (chunk 8 [0, 2, 0, 4, 0, 6])
  where
    chunk :: Int -> [Int] -> Array Int Int
    chunk n l = listArray (0, n-1) (take n (l ++ repeat 0))
    -- psw' = replicate 5 False

instance Show Machine where
  show (Machine m r) = "M: " ++ show (elems m) ++ ", R: " ++ show (elems r)

data Locator
  = AtRegister Int
  | AtMemory Int
  | AsLiteral Int

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
  | MUL AddrMode AddrMode
  | CLR AddrMode
  deriving (Eq, Ord, Read, Show)
