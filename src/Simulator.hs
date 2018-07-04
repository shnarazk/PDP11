{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , TemplateHaskell
  , ViewPatterns
  #-}

module Simulator
    (
      version
    , runSimulator
    , fromTrace
    ) where

import Control.Lens hiding ((<.))
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.List
import Data.Maybe
import PDP11 hiding (version)
import Assembler (assemble)

version :: String
version = "0.90.0"

-- * m ^. register ^? iix 2       	    to access R2 maybe
-- * m ^. register & iix 2 .~ 300 	    to update R2 = 300
-- * m & register . iix 2 .~ 300 	    to update R2 in m to 300
-- * m & register %~ (// [(1,10), (2,20)])  to update R1 and R2 in m
-- Note: (register %~ (// ...)) :: Machine -> Machine
makeLenses ''Machine

-- pdp .^ (psw . s[NZVC])               to access N, Zero, V, Clear
-- pdp & (psw . s[NZVC]) .~ True        to set N, Z, V, C
makeLenses ''PSW

type CodeMap = [(Int, ASM)]

newtype PDPState a = PDPState (State Machine a)
  deriving (Functor, Applicative, Monad, MonadState Machine)

codemap :: Int -> [ASM] -> CodeMap
codemap addr l = zip (scanl (\a c -> a + (2 * length (toBitBlocks c))) addr l) l

injectCode :: Machine -> [ASM] -> Machine
injectCode pdp c = pdp & memory .~ m'
  where
    adr = _pc pdp
    n = length b
    m' = accumArray (+) 0 (0, adr + n) $ assocs (pdp ^. memory) ++ zip [adr ..] b
    b = encode c

fetchInst :: Machine -> (ASM, Machine)
fetchInst m = (c, m')
  where
    m' = m { _trace = (p, c),
             _register = (_register m) // [(7, p + d)]
           }
    p = _pc m
    i0 = (_memory m) !.. p
    i1 = (_memory m) !.. (p + 2)
    i2 = (_memory m) !.. (p + 4)
    (d, c) = decodeWord i0 (i1, i2)

resetPSW :: Machine -> Machine
resetPSW (Machine m r _ t) = Machine m' r (_psw initialMachine) t
  where
    m' = accumArray (+) 0 (0, n) $ [(i, m ! i) | i <- [0 .. n]]
    n = snd $ bounds (_memory initialMachine)

runSimulator :: Int -> Machine -> [ASM] -> [Machine]
runSimulator n initM' prg = take n $ initM : runI initM
  where imap = codemap (_pc initM') prg
        initM = injectCode initM' prg
        runI :: Machine -> [Machine]
        runI m
          | Just _ <- lookup (_pc m) imap =
              let m'' = execState execute m'
                  (a, m') = fetchInst m
                  (PDPState execute) = code a
              in m'' : runI m''
          | otherwise = []

fromTrace :: [Machine] -> String
fromTrace states = unlines $ zipWith combine [0 :: Int .. ] states
  where combine n c = "#" ++ show n ++ "\t" ++ show c

runPDP11 :: Int -> Machine -> [ASM] -> String
runPDP11 n pdp11 program = fromTrace $ runSimulator n pdp11 program

--------------------------------------------------------------------------------

-- successive memory access
(!..) :: MemBlock -> Int -> Int
v !.. i = (v ! (i + 1)) * 256 + (v ! i)
infixr 9 !..

-- updater of byte array index for an Int
(<..) :: Int -> Int -> (MemBlock -> MemBlock)
i <.. x = (// [(i, mod x 256), (i + 1, div x 256)])
infixl 9 <..

-- updater of Int array index for an Int
(<.) :: Int -> Int -> (MemBlock -> MemBlock)
i <. x = (// [(i, x)])
infixl 9 <.

accessI :: Getting MemBlock Machine MemBlock -> PDPState MemBlock
accessI block = (^. block) <$> get

updateI :: ASetter Machine Machine MemBlock MemBlock -> (MemBlock -> MemBlock) -> PDPState ()
updateI block updates = do s <- get; put $ s & block %~ updates

readPSW :: ((Bool -> Const Bool Bool) -> PSW -> Const Bool PSW) -> PDPState Bool
readPSW acs = (^. (psw . acs)) <$> get

updatePSW :: ((Bool -> Identity Bool) -> PSW -> Identity PSW) -> Bool -> PDPState ()
updatePSW acs val = do s <- get; put $ s & (psw . acs) .~ val

incrementPC :: PDPState ()
incrementPC = do reg <- accessI register
                 updateI register (7 <. (reg ! 7 + 2))

fetchI :: AddrMode -> PDPState (Locator, Int)
fetchI (Register (Reg i)) = do reg <- accessI register
                               return (AtRegister i, reg ! i)
fetchI (Immediate n)      = do incrementPC
                               return (AsLiteral n, n)
fetchI (Index o (Reg i))  = do incrementPC
                               reg <- accessI register
                               mem <- accessI memory
                               let n = reg ! i + o
                               return (AtMemory n, mem !.. n)
fetchI (AutoInc (Reg j))  = do reg <- accessI register
                               mem <- accessI memory
                               let n = reg ! j
                               updateI register (j <. (n + 2))
                               return (AtMemory n, mem !.. n)
fetchI (AutoDec (Reg j))  = do reg <- accessI register
                               mem <- accessI memory
                               let n = reg ! j - 2
                               updateI register (j <. n)
                               return (AtMemory n, mem !.. n)
fetchI (Indirect a)       = do (l, v) <- fetchI a
                               mem <- accessI memory
                               case l of
                                 AtRegister _ -> return (AtMemory v, mem !.. v)
                                 AtMemory _   -> return (AtMemory v, mem !.. v)
                                 AsLiteral i  -> return (AtMemory (mem !.. i), mem !.. mem !.. i)

storeI :: Locator -> Int -> PDPState ()
storeI (AtMemory i)   x = updateI memory (i <.. x)
storeI (AtRegister i) x = updateI register (i <. x)
storeI (AsLiteral i)  x = updateI memory (i <.. x)

--------------------------------------------------------------------------------
code :: ASM -> PDPState ()
code (Inst2 MOV s d) = do incrementPC
                          (_, x) <- fetchI s
                          (p, _) <- fetchI d
                          storeI p x
                          updatePSW sV False
                          updatePSW sC False

code (Inst2 ADD s d) = do incrementPC
                          (_, x) <- fetchI s
                          (p, y) <- fetchI d
                          let x' = (y + x)
                          storeI p x'
                          updatePSW sN (x' < 0)
                          updatePSW sZ (x' == 0)
                          updatePSW sV (2 ^ 15 < x')

code (Inst2 SUB s d) = do incrementPC
                          (_, x) <- fetchI s
                          (p, y) <- fetchI d
                          let x' = (y - x)
                          storeI p x'
                          updatePSW sN (x' < 0)
                          updatePSW sZ (x' == 0)
                          updatePSW sV (2 ^ 15 < x')

code (Inst2 CMP s d) = do incrementPC
                          (_, x) <- fetchI s
                          (p, y) <- fetchI d
                          let z = x - y
                          updatePSW sN (z < 0)
                          updatePSW sZ (z == 0)

code (Inst2 BIT s d) = do incrementPC
                          (_, x) <- fetchI s
                          (p, y) <- fetchI d
                          let z = y .&. x
                          updatePSW sN (z < 0)
                          updatePSW sZ (z == 0)
                          updatePSW sV False

code (Inst2 BIC s d) = do incrementPC
                          (_, x) <- fetchI s
                          (p, y) <- fetchI d
                          storeI p (y .&. x)
                          updatePSW sZ (y .&. x == 0)
                          updatePSW sV False

code (Inst2 BIS s d) = do incrementPC
                          (_, x) <- fetchI s
                          (p, y) <- fetchI d
                          storeI p (y .|. x)
                          updatePSW sZ (y .&. x == 0)
                          updatePSW sV False

-- code (Inst2 MUL s d) = do (_, x) <- fetchI s
--                     (p, y) <- fetchI d
--                     storeI p (y * x)

code (Inst1 INC s)   = do incrementPC
                          (p, x) <- fetchI s
                          let x' = x + 1
                          storeI p x'
                          updatePSW sN (x' < 0)
                          updatePSW sZ (x' == 0)
                          updatePSW sV (2 ^ 15 < x')

code (Inst1 DEC s)   = do incrementPC
                          (p, x) <- fetchI s
                          let x' = x - 1
                          storeI p x'
                          updatePSW sN (x' < 0)
                          updatePSW sZ (x' == 0)
                          updatePSW sV (2 ^ 15 < x')

code (Inst1 NEG s)   = do incrementPC
                          (p, x) <- fetchI s
                          let x' = negate x
                          storeI p x'
                          updatePSW sN (x' < 0)
                          updatePSW sZ (x' == 0)
                          updatePSW sV (2 ^ 15 < x')

code (Inst1 CLR d)   = do incrementPC
                          (p, _) <- fetchI d
                          storeI p 0
                          updatePSW sN False
                          updatePSW sZ True
                          updatePSW sV False
                          updatePSW sC False

code (Inst1 ASL d)   = do incrementPC
                          (p, x) <- fetchI d
                          let x' = shiftL x 1
                          storeI p x'
                          updatePSW sN (x' < 0)
                          updatePSW sZ (x' == 0)
                          updatePSW sV (2 ^ 15 < x')
                          updatePSW sC False

code (Inst1 ASR d)   = do incrementPC
                          (p, x) <- fetchI d
                          let x' = shiftR x 1
                          storeI p x'
                          updatePSW sN (x' < 0)
                          updatePSW sZ (x' == 0)
                          updatePSW sV (2 ^ 15 < x')
                          updatePSW sC False

code (Inst1 JMP s)   = do incrementPC
                          (_, x) <- fetchI s
                          storeI (AtRegister 7) x

code (Inst0 BR o)    = do incrementPC
                          (p, x) <- fetchI (Register (Reg 7))
                          storeI p (x + 2 * o)

code (Inst0 BNE o)   = do incrementPC
                          (p, x) <- fetchI (Register (Reg 7))
                          y <- readPSW sZ
                          when (not y) $ storeI p (x + 2 * o)

code (Inst0 BEQ o)   = do incrementPC
                          (p, x) <- fetchI (Register (Reg 7))
                          y <- readPSW sZ
                          when y $ storeI p (x + 2 * o)
