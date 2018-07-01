{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , TemplateHaskell
  , ViewPatterns
  #-}

module Simulator
    (
      version
    , PSW(..)
    , Machine(..)
    , makePDP11'
    , runPDP11
    , runSimulator
    , runSimulator'
    -- * for debug in ghci
    , psw, sN, sZ, sV, sC
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
version = "0.10.0"

-- * m ^. register ^? iix 2       	    to access R2 maybe
-- * m ^. register & iix 2 .~ 300 	    to update R2 = 300
-- * m & register . iix 2 .~ 300 	    to update R2 in m to 300
-- * m & register %~ (// [(1,10), (2,20)])  to update R1 and R2 in m
-- Note: (register %~ (// ...)) :: Machine -> Machine
makeLenses ''Machine

-- pdp .^ (psw . s[NZVC])               to access N, Zero, V, Clear
-- pdp & (psw . s[NZVC]) .~ True        to set N, Z, V, C
makeLenses ''PSW

updatePSW :: ((PSW -> Identity Bool) -> PSW -> Identity PSW) -> Bool -> Machine -> Machine
updatePSW acs val m = m & (psw . acs) .~ val

type CodeMap = [(Int, ASM)]

newtype PDPState a = PDPState (State Machine a)
  deriving (Functor, Applicative, Monad, MonadState Machine)

codemap :: Int -> [ASM] -> CodeMap
codemap addr l = zip (scanl (\a c -> a + (2 * length (toBitBlocks c))) addr l) l

makePDP11' :: Int -> [Int] -> [Int] -> Machine
makePDP11' n b1 b2 = makePDP11 (take n (b1 ++ repeat 0)) (take 8 (b2 ++ repeat 0))

runSimulator :: Machine -> CodeMap -> [Machine]
runSimulator m is = take 16 $ runI m
  where runI :: Machine -> [Machine]
        runI m
          | Just a <- lookup (_pc m) is =
              let m' = execState execute m
                  (PDPState execute) = code a
              in m' : runI m'
          | otherwise = [] -- error $ show ((_register m ! 7), is)

runSimulator' :: [ASM] -> [Machine]
runSimulator' asm = runSimulator initialMachine (codemap (_pc initialMachine) asm)

runPDP11 :: String -> Maybe String
runPDP11 str@(assemble -> result) =
  case result of
    Right program -> Just . unlines $ zipWith (++) instrs (map show states)
      where instrs = "#0\tInitial state\n" : zipWith3 combine [1 :: Int .. ] intAddrs instrs'
            combine n a b = "#" ++ show n ++ "\t" ++ show b ++ "\t@ " ++ show a ++ "\n"
            codes = codemap (_pc initialMachine) program
            states = initialMachine : runSimulator initialMachine codes
            intAddrs = map _pc states
            instrs' = map (fromJust . flip lookup codes) intAddrs
    Left message  -> Just message

code :: ASM -> PDPState ()
code (MOV s d) = do incrementPC
                    (_, x) <- fetchI s
                    (p, _) <- fetchI d
                    storeI p x

code (ADD s d) = do incrementPC
                    (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y + x)

code (SUB s d) = do incrementPC
                    (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y - x)

-- code (MUL s d) = do (_, x) <- fetchI s
--                     (p, y) <- fetchI d
--                     storeI p (y * x)

code (CLR d)   = do incrementPC
                    (p, _) <- fetchI d
                    storeI p 0

code (BIC s d) = do incrementPC
                    (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y .&. x)

code (BIS s d) = do incrementPC
                    (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y .|. x)

code (INC s)   = do incrementPC
                    (p, x) <- fetchI s
                    storeI p (x + 1)

code (DEC s)   = do incrementPC
                    (p, x) <- fetchI s
                    storeI p (x - 1)

code (JMP o)   = do incrementPC
                    (p, x) <- fetchI (Register (Reg 7))
                    storeI p (x + o)

code (BNE o)   = do incrementPC
                    (p, x) <- fetchI (Register (Reg 7))
                    when (False) $ storeI p (x + o)  -- FIXME

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

pswI :: ((Bool -> Identity Bool) -> PSW -> Identity PSW) -> Bool -> PDPState ()
pswI acs val = do s <- get; put $ s & (psw . acs) .~ val

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
