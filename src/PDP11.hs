module PDP11
    (
      version
    , makePDP11
    , initialMachine
    , resetPSW
    , setTrace
    -- * Plumbing
    , MemBlock
    , PSW()
    , Machine(..)
    , _pc
    , dump
    , Locator(..)
    , RegId(..)
    , AddrMode(..)
    , Opcode(..)
    , ASM(..)
    , toBitBlocks
    , asInts
    ) where

import Data.Array
import Data.Bits
import Data.Maybe

version :: String
version = "0.90.0"

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
type MemBlock = Array Int Int

data PSW = PSW { _sN :: Bool
               , _sZ :: Bool
               , _sV :: Bool
               , _sC :: Bool
               }
  deriving (Eq, Ord, Read)

instance Show PSW where
  show (PSW a b c d) = show $ map fromEnum [a, b, c, d]

data Machine
  = Machine
    {
      _memory   :: Array Int Int
    , _register :: Array Int Int
    , _psw      :: PSW
--  , _insts    :: [ASM]
    , _trace    :: (Int, ASM)
    }
  deriving (Eq, Ord, Read)

instance Show Machine where
  show (Machine m r p (c, a)) = "M(rev):" ++ (show . reverse . take 12 . elems $ m)
    ++ ", R(rev):" ++ show (reverse (elems r))
    ++ ", PSW:" ++ show p
    ++ ", Trace:" ++ show a ++ " @" ++ show c
dump :: Machine -> ([Int], [Int], [Int], Int, ASM)
dump (Machine m r (PSW p1 p2 p3 p4) (c, a)) = (elems m, elems r, map fromEnum [p1, p2, p3, p4], c, a)

makePDP11 :: [Int] -> [Int] -> Machine
makePDP11 b1 b2 = Machine (chunk b1) (chunk b2) (PSW False False False False) (-1, NOP)
  where chunk :: [Int] -> MemBlock
        chunk l = listArray (0, n - 1) (take n (l ++ repeat 0))
          where n = length l

initialMachine :: Machine -- memory is at left; register is at right.
initialMachine = makePDP11 [2, 0, 4, 0, 8, 0, 0, 1, 1, 1, 0, 0] [0, 2, 0, 4, 0, 6, 1, 100]

setTrace :: (Int, ASM) -> Machine -> Machine
setTrace p m = m { _trace = p }

resetPSW :: Machine -> Machine
resetPSW (Machine m r _ t) = Machine m' r (_psw initialMachine) t
  where
    m' = accumArray (+) 0 (0, n) $ [(i, m ! i) | i <- [0 .. n]]
    n = snd $ bounds (_memory initialMachine)

-- misc accessors
_pc :: Machine -> Int
_pc m = (_register m) ! 7

data Locator
  = AtRegister Int
  | AtMemory Int
  | AsLiteral Int

data RegId = Reg Int
  deriving (Eq, Ord, Read, Show)

data AddrMode
  = Register RegId              -- 000 Register
  | AutoInc RegId               -- 010 Autoincrement
  | AutoDec RegId               -- 100 Autodecrement
  | Index Int RegId             -- 110 Index
  | Indirect AddrMode           -- **1 ... Deffered
  | Immediate Int               -- 011(R7) Immediate
  deriving (Eq, Ord, Read)

instance Show AddrMode where
  show (Register (Reg n)) = 'R' : show n
  show (Immediate n)      = '#' : show n
  show (Index n (Reg r))  = show n ++ "(R" ++ show r ++ ")"
  show (AutoInc (Reg r))  = "(R" ++ show r ++ ")+"
  show (AutoDec (Reg r))  = "-(R" ++ show r ++ ")"
  show (Indirect x)       = '@' : show x

data OpFormat = OFA1 | OFA2 | OFI1 | OF0

data Opcode
  = MOV
  | ADD
  | SUB
  | CMP
  | BIT
  | BIC
  | BIS
  | INC
  | DEC
  | NEG
  | CLR
  | ASL
  | ASR
  | JMP
  | BR
  | BNE
  | BEQ
  deriving (Eq, Ord, Read)

data ASM
  = Inst2 Opcode AddrMode AddrMode
  | Inst1 Opcode AddrMode
  | Inst0 Opcode Int
  | NOP
  deriving (Eq, Ord, Read)

opcodeAttr :: ASM -> (String, OpFormat, [Int], [AddrMode])
opcodeAttr (Inst2 MOV a1 a2) = ("MOV", OFA2, [0,0,0,1], [a1, a2])
opcodeAttr (Inst2 ADD a1 a2) = ("ADD", OFA2, [1,1,1,0], [a1, a2])
opcodeAttr (Inst2 SUB a1 a2) = ("SUB", OFA2, [0,1,1,0], [a1, a2])
opcodeAttr (Inst2 CMP a1 a2) = ("CMP", OFA2, [0,0,1,0], [a1, a2])
opcodeAttr (Inst2 BIT a1 a2) = ("BIT", OFA2, [0,0,1,1], [a1, a2])
opcodeAttr (Inst2 BIC a1 a2) = ("BIC", OFA2, [0,1,0,0], [a1, a2])
opcodeAttr (Inst2 BIS a1 a2) = ("BIS", OFA2, [0,1,0,1], [a1, a2])
opcodeAttr (Inst1 INC a)     = ("INC", OFA1, [0,0,0,0, 1,0,1,0, 1,0], [a])
opcodeAttr (Inst1 DEC a)     = ("DEC", OFA1, [0,0,0,0, 1,0,1,0, 1,1], [a])
opcodeAttr (Inst1 NEG a)     = ("NEG", OFA1, [0,0,0,0, 1,0,1,1, 0,0], [a])
opcodeAttr (Inst1 CLR a)     = ("CLR", OFA1, [0,0,0,0, 1,0,1,0, 0,0], [a])
opcodeAttr (Inst1 ASL a)     = ("ASL", OFA1, [0,0,0,0, 1,1,0,0, 1,0], [a])
opcodeAttr (Inst1 ASR a)     = ("ASR", OFA1, [0,0,0,0, 1,1,0,0, 1,1], [a])
opcodeAttr (Inst1 JMP a)     = ("JMP", OFA1, [0,0,0,0 ,0,0,0,0, 0,1], [a])
opcodeAttr (Inst0 BR  o)     = ("BR",  OFI1, [0,0,0,0, 0,0,0,1], [Immediate o]) -- bad idea wrapping offset
opcodeAttr (Inst0 BNE o)     = ("BNE", OFI1, [0,0,0,0, 0,0,1,0], [Immediate o])
opcodeAttr (Inst0 BEQ o)     = ("BEQ", OFI1, [0,0,0,0, 0,0,1,1], [Immediate o])
opcodeAttr NOP               = ("NOP", OF0 , [], [])

instance Show ASM where
  show m = case opcodeAttr m of
    (name, OFA2, _, [a, b])        -> name ++ " " ++ show a ++ ", " ++ show b
    (name, OFA1, _, [a])           -> name ++ " " ++ show a
    (name, OFI1, _, [Immediate o]) -> name ++ " " ++ show o
    (name, OF0, _, _)              -> " --- "

-- >> BitBlock 1 0 1 = 1
-- >> BitBlock 10 1 2 = 10 * 2 = 20
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
  (*) =         error "not implemented"
  abs =         error "not implemented"
  signum =      error "not implemented"
  fromInteger = error "not implemented"
  negate =      error "not implemented"

asInts ::BitBlock -> [Int]
asInts (BitBlock v f _) = [mod x 256, div x 256]
  where x = shiftL v f

(.||.) :: BitBlock -> BitBlock -> BitBlock
(.||.)  a b = a + b

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
fromAddrMode (Register (Reg r)) = (fromList [0,0,0] .<. 3) .||. (fromInt 3 r .<. 0)
fromAddrMode (AutoInc (Reg r))  = (fromList [0,1,0] .<. 3) .||. (fromInt 3 r .<. 0)
fromAddrMode (AutoDec (Reg r))  = (fromList [1,0,0] .<. 3) .||. (fromInt 3 r .<. 0)
fromAddrMode (Index i (Reg r))  = (fromList [1,1,0] .<. 3) .||. (fromInt 3 r .<. 0)
fromAddrMode (Indirect a)       = (fromList [0,0,1] .<. 3) .||. (fromAddrMode a)
fromAddrMode (Immediate i)      = (fromList [0,1,1] .<. 3) .||. (fromInt 3 7 .<. 0)

extends :: AddrMode -> Maybe BitBlock
extends (Immediate i) = Just $ fromInt 16 i
extends (Index i _)   = Just $ fromInt 16 i
extends (Indirect a)  = extends a
extends _             = Nothing

toBitBlocks :: ASM -> [BitBlock]
toBitBlocks m = case opcodeAttr m of
  (_, OFA2, b, as@[a1, a2]) ->
    let blk = (fromList b .<. 12) .||. (fromAddrMode a1 .<. 6) .||. (fromAddrMode a2 .<. 0)
    in blk : concatMap maybeToList (map extends as)
  (_, OFA1, b, [a]) ->
    let blk = (fromList b .<. 6) .||. (fromAddrMode a .<. 0)
    in blk : maybeToList (extends a)
  (_, OFI1, b, [Immediate o]) ->
    [(fromList b .<. 8) .||. fromInt 8 (if o < 0 then 256 + o else mod o 128)]
  (_, OF0, _, _)    -> []

{-
fromASM :: ASM -> String
fromASM a = [ if testBit (value b) n then '1' else '0' | b <- toBitBlocks a,  n <- [15,14..0] ]
-}
