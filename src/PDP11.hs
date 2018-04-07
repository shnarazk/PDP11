module PDP11
    (
      version
    , Machine(..)
    , Locator(..)
    , RegId(..)
    , AddrMode(..)
    , ASM(..)
    , pdp11
    ) where

import Data.Array

version :: String
version = "0.1.0"

{-
- https://programmer209.wordpress.com/2011/08/03/the-pdp-11-assembly-language/
If N is an address in memory then (N) is the data stored at the address N.
Syntax       Mode                          Action
Rn           Register                      Data = Rn
(Rn)+        Autoincrement                 Data = (Rn)
                                           Rn++
-(Rn)        Autodecrement                 Rn–
                                           Data = (Rn)
X(Rn)        Index                         Offset address X = (PC)
                                           PC += 2
                                           Base address = Rn
                                           Data = (Rn + X)
@Rn or (Rn)  Register Deferred             Data = (Rn)
@(Rn)+       Autoincrement Deferred        Data =((Rn))
                                           Rn++
@-(Rn)       Autodecrement Deferred        Rn–
                                           Data =((Rn))
@X(Rn)       Index Deferred                Offset address X = (PC)
                                           PC += 2
                                           Base address = Rn
                                           Data = ((Rn + X))
#n           Immediate                     Data = (PC) = n
@#A          Immediate Deferred (Absolute) Data = ((PC)) = (A)
A or X(PC)   Relative                      Offset address X = (PC)
                                           PC += 2
                                           Data = (PC + X) = (A)
@A or @X(PC) Relative Deferred             Offset address X = (PC)
                                           PC += 2
                                           Data = ((PC + X)) = ((A))
-}

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
  = Register RegId              -- Register
  | Immediate Int               -- Immediate
  | Index Int RegId             -- Index
  | AutoInc RegId               -- Autoincrement
  | AutoDec RegId               -- Autodecrement
  | Indirect AddrMode           -- ... Deffered
--  | PCrelat Int
  deriving (Eq, Ord, Read, Show)  

data ASM
  = MOV AddrMode AddrMode
  | ADD AddrMode AddrMode
  | SUB AddrMode AddrMode
  | MUL AddrMode AddrMode
  | CLR AddrMode
  deriving (Eq, Ord, Read, Show)
