{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TemplateHaskell
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
import Control.Monad.Identity
import Control.Monad.State
import Data.Array
import PDP11 hiding (version)
import Assembler hiding (version)

version :: String
version = "0.3.0"

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
runPDP11 str = run <$> readASM str
  where run program = unlines $ concatMap (\(n, m) -> [n, show m]) $ zip instrs states
          where states = runSimulator' program
                instrs = "#0 Initial state" : zipWith3 combine [1 :: Int .. ] mnems program
                combine n a b = "#" ++ show n ++ " " ++ a ++ "\t; " ++ show b
        mnems = map (dropWhile (`elem` " \t")) $ lines str

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

-- returns a pair of left-hand value and right-hand value
fetchI :: AddrMode -> PDPState (Locator, Int)
fetchI a = do s <- get
              let (b, s') = fetchLR a s
              put s'
              return b

fetchLR :: AddrMode -> Machine -> ((Locator, Int), Machine)
fetchLR (Register (Reg i)) s = ((AtRegister i, (s ^. register) ! i), s)
fetchLR (Immediate n) s      = ((AsLiteral n, n), s)
fetchLR (Index o (Reg i)) s  = ((AtMemory (i + o), (s ^. memory) !.. (i + o)), s)
fetchLR (AutoInc (Reg j)) s  = ((AtMemory i, (s ^. memory) !.. i), s')
  where i = (s ^. register) ! j
        s' = s & register %~ (j <. (i + 2))
fetchLR (AutoDec (Reg j)) s  = ((AtMemory i, (s ^. memory) !.. i), s')
  where i = ((s ^. register) ! j) - 2
        s' = s & register %~ (j <. i)
fetchLR (Indirect a) s       =
  case l of
    AtRegister _ -> ((AtMemory v, m !.. v)                , s')
    AtMemory _   -> ((AtMemory v, m !.. v)                , s')
    AsLiteral i  -> ((AtMemory (m !.. i), m !.. (m !.. i)), s')
  where ((l, v), s') = fetchLR a s
        m = s' ^. memory

storeI :: Locator -> Int -> PDPState ()
storeI (AtMemory i)   x = do m <- get ; put (m & memory   %~ (i <.. x))
storeI (AtRegister i) x = do m <- get ; put (m & register %~ (i <.  x))
storeI (AsLiteral i)  x = do m <- get ; put (m & memory   %~ (i <.. x))
