module Simulator
    (
      Machine(..)
    , runSimulator
    , runSimulator'
    ) where

import Data.Array
import ASM

data Machine
  = Machine
    {
      memory :: Array Int Int
    , register :: Array Int Int
    }
  deriving (Eq, Ord, Read)

instance Show Machine where
  show (Machine m r) = "M: " ++ show (elems m) ++ ", R: " ++ show (elems r)

initialMachine :: Machine -- memory is at left; register is at right.
initialMachine = Machine (chunk 20 [0, 10, 20]) (chunk 8 [0, 1, 0, 2])
  where
    chunk :: Int -> [Int] -> Array Int Int
    chunk n l = listArray (0, n-1) (take n (l ++ repeat 0))

runSimulator :: Machine -> [ASM] -> [Machine]
runSimulator m l = scanr execute m l

runSimulator' :: [ASM] -> [Machine]
runSimulator' l = runSimulator initialMachine l

execute :: ASM -> Machine -> Machine
execute (MOV s d) m = accessStore d m' x
  where
    (x, m') = accessGet s m

execute (ADD s d) m = accessStore d m'' (y + x)
  where
    (x, m') = accessGet s m
    (y, m'') = accessGet d m'

execute (SUB s d) m = accessStore d m'' (y - x)
  where
    (x, m') = accessGet s m
    (y, m'') = accessGet d m'

execute (MUL s d) m = accessStore d m'' (y * x)
  where
    (x, m') = accessGet s m
    (y, m'') = accessGet d m'

execute (CLR d) m = accessStore d m 0

-- data Modifier = Dec Machine | Inc Machine | None

effectiveAddr :: AddrMode -> Machine -> Either Int Int
effectiveAddr (Immediate n) _ = Left n
effectiveAddr (Register (Reg r)) _ = Right r
effectiveAddr (Indirect (Reg i)) (Machine _ r)
  | i < 0 =    error "a wrong register"
  | a < 0 =    error "a wrong address"
  | otherwise = Left a
  where a = r ! i
effectiveAddr (AutoInc (Reg i)) (Machine _ r)
  | i < 0 =    error "a wrong register"
  | a < 0 =    error "a wrong address"
  | otherwise = Left a
  where a = r ! i
effectiveAddr (AutoDec (Reg i)) (Machine _ r)
  | i < 0 =    error "a wrong register"
  | a < 0 =    error "a wrong address"
  | otherwise = Left a
  where a = r ! i

accessGet :: AddrMode -> Machine -> (Int, Machine)
accessGet a@(AutoInc (Reg j)) mr@(Machine m r) =
  case effectiveAddr a mr of
    Left  i -> (m ! i, update)
    Right i -> (r ! i, update)
  where
    update = Machine m (r // [(j, r ! j + 1)])

accessGet a@(AutoDec (Reg j)) (Machine m' r') =
  case effectiveAddr a mr of
    Left  i -> (m ! i, mr)
    Right i -> (r ! i, mr)
  where
    mr@(Machine m r) = Machine m' (r' //[(j, r' ! j - 1)])

accessGet a mr@(Machine m r) =
  case effectiveAddr a mr of
    Left  i -> (m ! i, mr)
    Right i -> (r ! i, mr)

accessStore :: AddrMode -> Machine -> Int -> Machine
accessStore a@(AutoInc (Reg j)) mr@(Machine m r) x =
  case effectiveAddr a mr of
    Left  i -> Machine (m // [(i, x)]) (r // [(j, r ! j + 1)])
    Right i -> Machine m               (r // [(i, x), (j, r ! j + 1)])

accessStore a@(AutoDec (Reg j)) (Machine m' r') x =
  case effectiveAddr a mr of
    Left  i -> Machine (m // [(i, x)]) r
    Right i -> Machine m               (r // [(i, x)])
  where
    mr@(Machine m r) = Machine m' (r' //[(j, r' ! j - 1)])

accessStore a mr@(Machine m r) x =
  case effectiveAddr a mr of
    Left  i -> Machine (m // [(i, x)]) r
    Right i -> Machine m               (r // [(i, x)])
