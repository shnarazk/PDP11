module PDP11
    (
      version
    , makePDP11
    , initialMachine
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
    , asInt
    , encode
    , decodeWord
    ) where

import Data.Array
import Data.Bits
import Data.List
import Data.Maybe

version :: String
version = "0.90.0"

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
    ++ if c == -1 then " -- initial state " else " by " ++ show a ++ " @" ++ show c
dump :: Machine -> ([Int], [Int], [Int], Int, ASM)
dump (Machine m r (PSW p1 p2 p3 p4) (c, a)) = (elems m, elems r, map fromEnum [p1, p2, p3, p4], c, a)

makePDP11 :: [Int] -> [Int] -> Machine
makePDP11 b1 b2 = Machine (chunk b1) (chunk b2) (PSW False False False False) (-1, NOP)
  where chunk :: [Int] -> MemBlock
        chunk l = listArray (0, n - 1) (take n (l ++ repeat 0))
          where n = length l

initialMachine :: Machine -- memory is at left; register is at right.
initialMachine = makePDP11 [2, 0, 4, 0, 8, 0, 0, 1, 1, 1, 0, 0] [0, 2, 0, 4, 0, 6, 1, 100]

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
  deriving (Eq, Ord, Read, Show)

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
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data ASM
  = Inst2 Opcode AddrMode AddrMode
  | Inst1 Opcode AddrMode
  | Inst0 Opcode Int
  | NOP
  deriving (Eq, Ord, Read)

codeTable :: [(Opcode, [Int], OpFormat)]
codeTable =
  [ (MOV, [0,0,0,1], OFA2)
  , (ADD, [1,1,1,0], OFA2)
  , (SUB, [0,1,1,0], OFA2)
  , (CMP, [0,0,1,0], OFA2)
  , (BIT, [0,0,1,1], OFA2)
  , (BIC, [0,1,0,0], OFA2)
  , (BIS, [0,1,0,1], OFA2)
  , (INC, [0,0,0,0, 1,0,1,0, 1,0], OFA1)
  , (DEC, [0,0,0,0, 1,0,1,0, 1,1], OFA1)
  , (NEG, [0,0,0,0, 1,0,1,1, 0,0], OFA1)
  , (CLR, [0,0,0,0, 1,0,1,0, 0,0], OFA1)
  , (ASL, [0,0,0,0, 1,1,0,0, 1,0], OFA1)
  , (ASR, [0,0,0,0, 1,1,0,0, 1,1], OFA1)
  , (JMP, [0,0,0,0 ,0,0,0,0, 0,1], OFA1)
  , (BR , [0,0,0,0, 0,0,0,1], OFI1)
  , (BNE, [0,0,0,0, 0,0,1,0], OFI1)
  , (BEQ, [0,0,0,0, 0,0,1,1], OFI1)
  ]

asBinaryCode :: Opcode -> [Int]
asBinaryCode op = maybe [] (\(_, b, _) -> b) $ find (\(o, b, t) -> o == op) codeTable

opcodeAttr :: ASM -> (String, OpFormat, [Int], [AddrMode])
opcodeAttr (Inst2 op a1 a2) = (show op, OFA2, (asBinaryCode op), [a1, a2])
opcodeAttr (Inst1 op a)     = (show op, OFA1, (asBinaryCode op), [a])
opcodeAttr (Inst0 op o)     = (show op, OFI1, (asBinaryCode op), [Immediate o]) -- wrapping offset
opcodeAttr NOP              = ("NOP", OF0 , [], [])

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

asInt ::BitBlock -> Int
asInt (BitBlock v f _) = shiftL v f

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
fromAddrMode (Immediate i)      = (fromList [0,1,0] .<. 3) .||. (fromInt 3 7 .<. 0)

extends :: AddrMode -> Maybe BitBlock
extends (Immediate i) = Just $ fromInt 16 i
extends (Index i _)   = Just $ fromInt 16 i
extends (Indirect a)  = extends a
extends _             = Nothing

injectOffset :: AddrMode -> Int -> AddrMode
injectOffset (Index _ r) x  = Index x r
injectOffset (Indirect a) x = Indirect (injectOffset a x)
injectOffset a _ = a

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

fromBinaryList :: [Int] -> Int
fromBinaryList l = foldr (\a b -> a + 2 * b) 0 $ reverse l

asBinaryList :: Int -> [Int]
asBinaryList = reverse . take 16 . map (`mod` 2) . iterate (`div` 2)

decodeWord :: Int -> (Int, Int) -> ASM
decodeWord x (x1, x2)
  | Just (op, _, t) <- find (\(_, b, _) -> b `isPrefixOf` bits) codeTable =
      case t of
        OFA2 -> let a1 = am 11 6
                    a2 = am 5 0
                in case (extends a1, extends a2) of
                     (Just _, Just _)   -> Inst2 op (injectOffset a1 x1) (injectOffset a2 x2)
                     (Just _, Nothing)  -> Inst2 op (injectOffset a1 x1) a2
                     (Nothing, Just _)  -> Inst2 op a1 (injectOffset a2 x2)
                     (Nothing, Nothing) -> Inst2 op a1 a2
        OFA1 -> let a = am 5 0
                in case extends a of
                     Just _  -> Inst1 op (injectOffset a x1)
                     Nothing -> Inst1 op a
        OFI1 -> Inst0 op (fromBinaryList (drop 8 bits))
  | otherwise = error $ "invalid code: " ++ show x
  where
    bits = asBinaryList x
    am l r = decodeAddrMode $ drop (15 - l) $ take (16 - r) bits

decodeAddrMode :: [Int] -> AddrMode
decodeAddrMode l = case take 3 l of
  [0,0,0] -> Register (Reg i)
  [0,1,0] -> AutoInc (Reg i)
  [1,0,0] -> AutoDec (Reg i)
  [1,1,0] -> Index 0 (Reg i)
  [0,0,1] -> Indirect (Register (Reg i))
  [0,1,1] -> Indirect (AutoInc (Reg i))
  [1,0,1] -> Indirect (AutoDec (Reg i))
  [1,1,1] -> Indirect (Index 0 (Reg i))
  where
    i = fromBinaryList (drop 3 l)

encode :: [ASM] -> [Int]
encode = concatMap asInts . concatMap toBitBlocks
