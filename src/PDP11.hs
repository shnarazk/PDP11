module PDP11
    (
      version
    , Machine(..)
    , Locator(..)
    , RegId(..)
    , AddrMode(..)
    , ASM(..)
    , toBitBlocks
--    , (.+.)
--    , fromInt
--    , fromList
--    , fromAddrMode
    ) where

import Data.Array
import Data.Bits

version :: String
version = "0.5.0"

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

instance Show Machine where
  -- show (Machine m r) = "M:" ++ show (elems m) ++ ", R:" ++ show (elems r)
  show (Machine m r) = "M(rev):" ++ show (reverse (elems m)) ++ ", R(rev):" ++ show (reverse (elems r))

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
  | BIC AddrMode AddrMode
  | BIS AddrMode AddrMode
  | INC AddrMode
  | DEC AddrMode
--  | MUL AddrMode AddrMode
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
  show (BitBlock v f t) = snd . insertP $ [ padding n | n <- seq ] ++ replicate f '0'
    where
      padding n
        | n == -1     =  '_'
        | testBit v n = '1'
        | otherwise   = '0'
      seq = [t-f-1, t -f -2 .. 0]
      insertP [a] = (1, [a])
      insertP (a:b)
        | mod n 4 == 0 = (n + 1, a : '_' : b')
        | otherwise    = (n + 1, a : b')
        where
          (n, b') = insertP b

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
toBitBlock (CLR a1)    = (fromList [0,0,0,0, 1,0,1,0, 0,0] .<. 6)
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
toBitBlock (BIC a1 a2) = (fromList [0,1,0,0] .<. 12)
                         .|. (fromAddrMode a1 .<. 6)
                         .|. (fromAddrMode a2 .<. 0)
toBitBlock (BIS a1 a2) = (fromList [0,1,0,1] .<. 12)
                         .|. (fromAddrMode a1 .<. 6)
                         .|. (fromAddrMode a2 .<. 0)
toBitBlock (INC a1)    = (fromList [0,0,0,0,1,0,1,0,1,0] .<. 6)
                         .|. (fromAddrMode a1 .<. 0)
toBitBlock (DEC a1)    = (fromList [0,0,0,0,1,0,1,0,1,1] .<. 6)
                         .|. (fromAddrMode a1 .<. 0)

-- fromASM :: ASM -> String
-- fromASM a = [ if testBit (value b) n then '1' else '0' | b <- toBitBlocks a,  n <- [15,14..0] ]

extends :: AddrMode -> Bool
extends (Immediate _) = True
extends (Index i _)   = True
extends _            = False

toExtend :: AddrMode -> BitBlock
toExtend (Immediate i) = fromInt 16 i
toExtend (Index i _)   = fromInt 16 i
toExtend (Indirect a)  = toExtend a
toExtend a             = error $ "toExtend called with an invalid AddrMode " ++ show a

toBitBlocks :: ASM -> [BitBlock]
toBitBlocks m@(CLR a1) = case extends a1 of
                           True -> [toBitBlock m, toExtend a1]
                           False -> [toBitBlock m]
toBitBlocks m@(MOV a1 a2) = case (extends a1, extends a2) of
                              (True, True)   -> [toBitBlock m, toExtend a1, toExtend a2]
                              (True, False)  -> [toBitBlock m, toExtend a1]
                              (False, True)  -> [toBitBlock m, toExtend a2]
                              (False, False) -> [toBitBlock m]
toBitBlocks m@(SUB a1 a2) = case (extends a1, extends a2) of
                              (True, True)   -> [toBitBlock m, toExtend a1, toExtend a2]
                              (True, False)  -> [toBitBlock m, toExtend a1]
                              (False, True)  -> [toBitBlock m, toExtend a2]
                              (False, False) -> [toBitBlock m]
toBitBlocks m@(ADD a1 a2) = case (extends a1, extends a2) of
                              (True, True)   -> [toBitBlock m, toExtend a1, toExtend a2]
                              (True, False)  -> [toBitBlock m, toExtend a1]
                              (False, True)  -> [toBitBlock m, toExtend a2]
                              (False, False) -> [toBitBlock m]
toBitBlocks m@(BIC a1 a2) = case (extends a1, extends a2) of
                              (True, True)   -> [toBitBlock m, toExtend a1, toExtend a2]
                              (True, False)  -> [toBitBlock m, toExtend a1]
                              (False, True)  -> [toBitBlock m, toExtend a2]
                              (False, False) -> [toBitBlock m]
toBitBlocks m@(BIS a1 a2) = case (extends a1, extends a2) of
                              (True, True)   -> [toBitBlock m, toExtend a1, toExtend a2]
                              (True, False)  -> [toBitBlock m, toExtend a1]
                              (False, True)  -> [toBitBlock m, toExtend a2]
                              (False, False) -> [toBitBlock m]
toBitBlocks m@(INC a) = case extends a of
                              True   -> [toBitBlock m, toExtend a]
                              False  -> [toBitBlock m]
toBitBlocks m@(DEC a) = case extends a of
                              True   -> [toBitBlock m, toExtend a]
                              False  -> [toBitBlock m]
