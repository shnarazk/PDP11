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
    , runPDP11
    , initialMachine
    , runSimulator
    , runSimulator'
    ) where

import Control.Lens hiding ((<.))
import Control.Monad.State
import Data.Array
import PDP11 hiding (version)
import Assembler (assemble)

version :: String
version = "0.4.1"

-- * m ^. register ^? iix 2       	    to access R2 maybe
-- * m ^. register & iix 2 .~ 300 	    to update R2 = 300
-- * m & register . iix 2 .~ 300 	    to update R2 in m to 300
-- * m & register %~ (// [(1,10), (2,20)])  to update R1 and R2 in m
-- Note: (register %~ (// ...)) :: Machine -> Machine
makeLenses ''Machine

newtype PDPState a = PDPState (State Machine a)
  deriving (Functor, Applicative, Monad, MonadState Machine)

initialMachine :: Machine -- memory is at left; register is at right.
initialMachine = Machine (chunk 16 [0, 2, 0, 4, 0, 8, 1, 255, 0, 8, 0, 10]) (chunk 8 [0, 2, 0, 4, 0, 6])
  where
    chunk :: Int -> [Int] -> Array Int Int
    chunk n l = listArray (0, n-1) (take n (l ++ repeat 0))

runSimulator :: Machine -> [ASM] -> [Machine]
runSimulator m l = scanl runI m l

runSimulator' :: [ASM] -> [Machine]
runSimulator' l = runSimulator initialMachine l

runPDP11 :: String -> Maybe String
runPDP11 str@(assemble -> Just program) = Just . unlines $ zipWith (++) instrs states
  where instrs = "#0 Initial state\n" : zipWith3 combine [1 :: Int .. ] mnems program
        combine n a b = "#" ++ show n ++ " " ++ a ++ "\t; " ++ show b ++ "\n"
        mnems = map (dropWhile (`elem` " \t")) $ lines str
        states = map show $ runSimulator' program
runPDP11 _ = Nothing

runI :: Machine -> ASM -> Machine
runI m a = execState execute m
  where (PDPState execute) = code a

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

code (MUL s d) = do (_, x) <- fetchI s
                    (p, y) <- fetchI d
                    storeI p (y * x)

code (CLR d)   = do (p, _) <- fetchI d
                    storeI p 0

-- successive memory access
(!..) :: Array Int Int -> Int -> Int
v !.. i = (v ! i) * 256 + (v ! (i + 1))

-- updater of byte array index for an Int
(<..) :: Int -> Int -> (Array Int Int -> Array Int Int)
i <.. x = (// [(i, div x 256), (i + 1, mod x 256)])

-- updater of Int array index for an Int
(<.) :: Int -> Int -> (Array Int Int -> Array Int Int)
i <. x = (// [(i, x)])

fetchI :: AddrMode -> PDPState (Locator, Int)
fetchI (Register (Reg i)) = do Machine{ _register = reg } <- get
                               return (AtRegister i, reg ! i)
fetchI (Immediate n)      = do return (AsLiteral n, n)
fetchI (Index o (Reg i))  = do Machine{ _memory = mem } <- get
                               return (AtMemory (i + o), mem !.. (i + o))
fetchI (AutoInc (Reg j))  = do s@Machine{ _register = reg } <- get
                               let i = reg ! j
                               put $ s & register %~ (j <. (i + 2))
                               return (AtMemory i, (s ^. memory) !.. i)
fetchI (AutoDec (Reg j))  = do s@Machine{ _register = reg } <- get
                               let i = (reg ! j) - 2
                               put $ s & register %~ (j <. i)
                               return (AtMemory i, (s ^. memory) !.. i)
fetchI (Indirect a)       = do (l, v) <- fetchI a
                               Machine{ _memory = mem } <- get
                               case l of
                                 AtRegister _ -> return (AtMemory v, mem !.. v)
                                 AtMemory _   -> return (AtMemory v, mem !.. v)
                                 AsLiteral i  -> return (AtMemory (mem !.. i), mem !.. (mem !.. i))

storeI :: Locator -> Int -> PDPState ()
storeI (AtMemory i)   x = do m <- get ; put (m & memory   %~ (i <.. x))
storeI (AtRegister i) x = do m <- get ; put (m & register %~ (i <.  x))
storeI (AsLiteral i)  x = do m <- get ; put (m & memory   %~ (i <.. x))
