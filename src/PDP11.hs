module PDP11
    (
      version
    , Machine(..)
    , Locator(..)
    , RegId(..)
    , AddrMode(..)
    , ASM(..)
    , pdp11
    , fromASM
--    , (.+.)
--    , fromInt
--    , fromList
--    , fromAddrMode
    ) where

import Data.Array
import Data.Bits

version :: String
version = "0.2.1"

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
  show (Machine m r) = "M:" ++ show (elems m) ++ ", R:" ++ show (elems r)

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

data BitBlock
  = BitBlock
    {
      value :: Int
    , from  :: Int
    , to    :: Int
    }
  deriving (Eq, Ord)

instance Show BitBlock where
  show (BitBlock v f t)
    =  [ if testBit v n then '1' else '0' | n <- [t-f-1, t -f -2 .. 0]]
    ++ replicate f '0'

instance Num BitBlock where
  b1 + b2 = BitBlock (shiftR val from')
                     from'
                     to'
    where
      val = min (shiftL (value b1) (from b1) .|. shiftL (value b2) (from b2)) (shiftL 1 to' - 1)
      from' = min (from b1) (from b2)
      to'   = max (to b1) (to b2)

instance Bits BitBlock where
  a .|. b = a + b

(.+.) :: Int -> Int -> Int
x .+. y = x * 2 + y

fromList :: [Int] -> BitBlock
fromList l = BitBlock (foldl (.+.) 0 l) 0 (length l)

fromInt :: Int -> Int -> BitBlock
fromInt width n = BitBlock n 0 width

shiftBitBlock :: BitBlock -> Int -> BitBlock
shiftBitBlock (BitBlock b f t) i = BitBlock b (f + i) (t + i)
(.<.) = shiftBitBlock

-- toBitBlock width n = BitBlock n width 0

fromAddrMode :: AddrMode -> BitBlock
fromAddrMode (Register (Reg r)) = (fromList [0,0,0] .<. 3) .|. (fromInt 3 r .<. 0)
fromAddrMode (Immediate i)      = (fromList [0,0,1] .<. 3) .|. (fromInt 3 7 .<. 0) -- FIXME
fromAddrMode (Index i (Reg r))  = (fromList [1,1,0] .<. 3) .|. (fromInt 3 r .<. 0) -- FIXME
fromAddrMode (AutoInc (Reg r))  = (fromList [0,1,0] .<. 3) .|. (fromInt 3 r .<. 0)
fromAddrMode (AutoDec (Reg r))  = (fromList [1,0,0] .<. 3) .|. (fromInt 3 r .<. 0)
fromAddrMode (Indirect a )      = (fromList [0,0,1] .<. 3) .|. (fromAddrMode a)

toBitBlock :: ASM -> BitBlock
toBitBlock (CLR a1)    = (fromList [0,0,0,0, 1,0,1,0, 0,0,0] .<. 6)
                         .|. (fromAddrMode a1 .<. 0)
toBitBlock (MOV a1 a2) = (fromList [0,0,0,1] .<. 12)
                         .|. (fromAddrMode a1 .<. 6)
                         .|. (fromAddrMode a2 .<. 0)
toBitBlock (SUB a1 a2) = (fromList [0,1,1,0] .<. 12)
                         .|. (fromAddrMode a1 .<. 6)
                         .|. (fromAddrMode a2 .<. 0)
toBitBlock (ADD a1 a2) = (fromList [1,1,1,0] .<. 12)
                         .|. (fromAddrMode a1 .<. 6)
                         .|. (fromAddrMode a2 .<. 0)

fromASM :: ASM -> String
fromASM a = [ if testBit v n then '1' else '0' | n <- [15,14..0]]
  where (BitBlock v _ _) = toBitBlock a
