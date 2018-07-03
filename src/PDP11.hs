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
    , ASM(..)
    , toBitBlocks
    , asInts
    ) where

import Data.Array
import Data.Bits

version :: String
version = "0.8.0"

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
  -- show (Machine m r) = "M:" ++ show (elems m) ++ ", R:" ++ show (elems r)
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
  = Register RegId              -- Register
  | Immediate Int               -- Immediate
  | Index Int RegId             -- Index
  | AutoInc RegId               -- Autoincrement
  | AutoDec RegId               -- Autodecrement
  | Indirect AddrMode           -- ... Deffered
--  | PCrelat Int
  deriving (Eq, Ord, Read)

instance Show AddrMode where
  show (Register (Reg n)) = 'R' : show n
  show (Immediate n)      = '#' : show n
  show (Index n (Reg r))  = show n ++ "(R" ++ show r ++ ")"
  show (AutoInc (Reg r))  = "(R" ++ show r ++ ")+"
  show (AutoDec (Reg r))  = "-(R" ++ show r ++ ")"
  show (Indirect x)       = '@' : show x

data OpFormat = OFA1 | OFA2 | OFI1 | OF0

data ASM
  = MOV AddrMode AddrMode
  | ADD AddrMode AddrMode
  | SUB AddrMode AddrMode
  | CMP AddrMode AddrMode
  | BIT AddrMode AddrMode
  | BIC AddrMode AddrMode
  | BIS AddrMode AddrMode
--  | MUL AddrMode AddrMode
  | INC AddrMode
  | DEC AddrMode
  | NEG AddrMode
  | CLR AddrMode
  | ASL AddrMode
  | ASR AddrMode
  | JMP AddrMode
  | BR Int
  | BNE Int
  | BEQ Int
  | NOP
  deriving (Eq, Ord, Read)

opcodeFormat :: ASM -> (OpFormat, [Int], [AddrMode])
opcodeFormat (MOV a1 a2) = (OFA2, [0,0,0,1], [a1, a2])
opcodeFormat (ADD a1 a2) = (OFA2, [1,1,1,0], [a1, a2])
opcodeFormat (SUB a1 a2) = (OFA2, [0,1,1,0], [a1, a2])
opcodeFormat (CMP a1 a2) = (OFA2, [0,0,1,0], [a1, a2])
opcodeFormat (BIT a1 a2) = (OFA2, [0,0,1,1], [a1, a2])
opcodeFormat (BIC a1 a2) = (OFA2, [0,1,0,0], [a1, a2])
opcodeFormat (BIS a1 a2) = (OFA2, [0,1,0,1], [a1, a2])
opcodeFormat (INC a) = (OFA1, [0,0,0,0, 1,0,1,0, 1,0], [a])
opcodeFormat (DEC a) = (OFA1, [0,0,0,0, 1,0,1,0, 1,1], [a])
opcodeFormat (NEG a) = (OFA1, [0,0,0,0, 1,0,1,1, 0,0], [a])
opcodeFormat (CLR a) = (OFA1, [0,0,0,0, 1,0,1,0, 0,0], [a])
opcodeFormat (ASL a) = (OFA1, [0,0,0,0, 1,1,0,0, 1,0], [a])
opcodeFormat (ASR a) = (OFA1, [0,0,0,0, 1,1,0,0, 1,1], [a])
opcodeFormat (JMP a) = (OFA1, [0,0,0,0 ,0,0,0,0, 0,1], [a])
opcodeFormat (BR o)  = (OFI1, [0,0,0,0, 0,0,0,1], [Immediate o]) -- bad idea wrapping offset
opcodeFormat (BNE o) = (OFI1, [0,0,0,0, 0,0,1,0], [Immediate o])
opcodeFormat (BEQ o) = (OFI1, [0,0,0,0, 0,0,1,1], [Immediate o])
opcodeFormat NOP     = (OF0 , [], [])

instance Show ASM where
  show (MOV a b) = "MOV " ++ show a ++ ", " ++ show b
  show (ADD a b) = "ADD " ++ show a ++ ", " ++ show b
  show (SUB a b) = "SUB " ++ show a ++ ", " ++ show b
  show (CMP a b) = "CMP " ++ show a ++ ", " ++ show b
  show (BIT a b) = "BIT " ++ show a ++ ", " ++ show b
  show (BIC a b) = "BIC " ++ show a ++ ", " ++ show b
  show (BIS a b) = "BIS " ++ show a ++ ", " ++ show b
  show (INC a)   = "INC " ++ show a
  show (DEC a)   = "DEC " ++ show a
  show (NEG a)   = "NEG " ++ show a
  show (CLR a)   = "CLR " ++ show a
  show (ASL a)   = "ASL " ++ show a
  show (ASR a)   = "ASR " ++ show a
  show (JMP a)   = "JMP " ++ show a
  show (BR a)    = "BR "  ++ show a
  show (BNE a)   = "BNE " ++ show a
  show (BEQ a)   = "BEQ " ++ show a
  show NOP       = " --- "

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
fromAddrMode (Immediate i)      = (fromList [0,0,1] .<. 3) .||. (fromInt 3 7 .<. 0) -- FIXME
fromAddrMode (Index i (Reg r))  = (fromList [1,1,0] .<. 3) .||. (fromInt 3 r .<. 0) -- FIXME
fromAddrMode (AutoInc (Reg r))  = (fromList [0,1,0] .<. 3) .||. (fromInt 3 r .<. 0)
fromAddrMode (AutoDec (Reg r))  = (fromList [1,0,0] .<. 3) .||. (fromInt 3 r .<. 0)
fromAddrMode (Indirect a )      = (fromList [0,0,1] .<. 3) .||. (fromAddrMode a)

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
toBitBlocks m = case opcodeFormat m of
  (OFA2, b, [a1, a2]) ->
    let blk = (fromList b .<. 12) .||. (fromAddrMode a1 .<. 6) .||. (fromAddrMode a2 .<. 0)
    in case (extends a1, extends a2) of
         (True, True)   -> [blk, toExtend a1, toExtend a2]
         (True, False)  -> [blk, toExtend a1]
         (False, True)  -> [blk, toExtend a2]
         (False, False) -> [blk]
  (OFA1, b, [a]) ->
    let blk = (fromList b .<. 6) .||. (fromAddrMode a .<. 0)
    in case extends a of
         True  -> [blk, toExtend a]
         False -> [blk]
  (OFI1, b, [Immediate o]) ->
    [(fromList b .<. 8) .||. fromInt 8 (if o < 0 then 256 + o else mod o 128)]
  (OF0, _, _)    -> []

{-
toBitBlock :: ASM -> [Int] -> BitBlock
toBitBlock m c = case oprandFormat m of
  (OFA2, [a, b]) -> (fromList c .<. 12) .||. (fromAddrMode a .<. 6) .||. (fromAddrMode b .<. 0)
  (OFA1, [a])    -> (fromList c .<. 6)  .||. (fromAddrMode a .<. 0)
  (OFI1, [o])    -> (fromList c .<. 8)  .||. fromInt 8 (if o < 0 then 256 + o else mod o 128)

fromASM :: ASM -> String
fromASM a = [ if testBit (value b) n then '1' else '0' | b <- toBitBlocks a,  n <- [15,14..0] ]
-}
