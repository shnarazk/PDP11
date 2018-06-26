{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , TemplateHaskell
  , ViewPatterns
  #-}

module Simulator
    (
      version
    , Machine(..)
    , makePDP11
    , makePDP11'
    , runPDP11
    , initialMachine
    , runSimulator
    , runSimulator'
    ) where

import Control.Lens hiding ((<.))
import Control.Monad.State
import Data.Array
import Data.Bits
import PDP11 hiding (version)
import Assembler (assemble)

version :: String
version = "0.8.1"

-- * m ^. register ^? iix 2       	    to access R2 maybe
-- * m ^. register & iix 2 .~ 300 	    to update R2 = 300
-- * m & register . iix 2 .~ 300 	    to update R2 in m to 300
-- * m & register %~ (// [(1,10), (2,20)])  to update R1 and R2 in m
-- Note: (register %~ (// ...)) :: Machine -> Machine
makeLenses ''Machine

type MemBlock = Array Int Int

newtype PDPState a = PDPState (State Machine a)
  deriving (Functor, Applicative, Monad, MonadState Machine)

makePDP11' :: (Int, Int) -> [Int] -> [Int] -> Machine
makePDP11' (m, r) b1 b2 = Machine (chunk m b1) (chunk r b2)
  where chunk :: Int -> [Int] -> MemBlock
        chunk n l = listArray (0, n-1) (take n (l ++ repeat 0))

makePDP11 :: [Int] -> [Int] -> Machine
makePDP11 b1 b2 = makePDP11' (length b1, length b2) b1 b2

initialMachine :: Machine -- memory is at left; register is at right.
initialMachine = makePDP11'
  (16,8)
  [2, 0, 4, 0, 8, 0, 0, 1, 1, 1, 6, 0] -- a sequence of (lower 8 bits, upper 8 bits)
  [0, 2, 0, 4, 0, 6, 1, 200]

runSimulator :: Machine -> [ASM] -> [Machine]
runSimulator m l = scanl runI m l
  where runI :: Machine -> ASM -> Machine
        runI m a = execState execute m
          where (PDPState execute) = do incrementPC; code a

runSimulator' :: [ASM] -> [Machine]
runSimulator' l = runSimulator initialMachine l

runPDP11 :: String -> Maybe String
runPDP11 str@(assemble -> result) =
  case result of
    Right program -> Just . unlines $ zipWith (++) instrs states
      where instrs = "#0 Initial state\n" : zipWith3 combine [1 :: Int .. ] mnems program
            combine n a b = "#" ++ show n ++ " " ++ a ++ "\t; " ++ show b ++ "\n"
            mnems = map (dropWhile (`elem` " \t")) $ lines str
            states = map show $ runSimulator' program
    Left message  -> Just message

code :: ASM -> PDPState ()
code (MOV s d) = do (_, x) <- fetchI s
                    (p, _) <- fetchI d
                    storeI p x

code (ADD s d) = do (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y + x)

code (SUB s d) = do (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y - x)

-- code (MUL s d) = do (_, x) <- fetchI s
--                     (p, y) <- fetchI d
--                     storeI p (y * x)

code (CLR d)   = do (p, _) <- fetchI d
                    storeI p 0

code (BIC s d) = do (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y .&. x)

code (BIS s d) = do (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y .|. x)

code (INC s)   = do (p, x) <- fetchI s
                    storeI p (x + 1)

code (DEC s)   = do (p, x) <- fetchI s
                    storeI p (x - 1)

--------------------------------------------------------------------------------

-- successive memory access
(!..) :: MemBlock -> Int -> Int
v !.. i = (v ! i) * 256 + (v ! (i + 1))
infixr 9 !..

-- updater of byte array index for an Int
(<..) :: Int -> Int -> (MemBlock -> MemBlock)
i <.. x = (// [(i, div x 256), (i + 1, mod x 256)])
infixl 9 <..

-- updater of Int array index for an Int
(<.) :: Int -> Int -> (MemBlock -> MemBlock)
i <. x = (// [(i, x)])
infixl 9 <.

accessI :: Getting MemBlock Machine MemBlock -> PDPState MemBlock
accessI block = (^. block) <$> get

updateI :: ASetter Machine Machine MemBlock MemBlock -> (MemBlock -> MemBlock) -> PDPState ()
updateI block updates = do s <- get; put $ s & block %~ updates

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
