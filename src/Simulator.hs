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
initialMachine = Machine (chunk 20 [0, 10, 0, 20, 0, 40, 1, 255]) (chunk 8 [0, 2, 0, 4, 0, 6])
  where
    chunk :: Int -> [Int] -> Array Int Int
    chunk n l = listArray (0, n-1) (take n (l ++ repeat 0))

runSimulator :: Machine -> [ASM] -> [Machine]
runSimulator m l = scanl (flip execute) m l

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

data DataHolder
  = AtRegister Int
  | AtMemory Int
  | AsLiteral Int

effectiveAddr :: AddrMode -> Machine -> DataHolder
effectiveAddr (Immediate n) _ = AsLiteral n
effectiveAddr (Register (Reg r)) _ = AtRegister r
effectiveAddr (Indirect (Reg i)) (Machine _ r)
  | i < 0 =    error "a wrong register"
  | a < 0 =    error "a wrong address"
  | otherwise = AtMemory a
  where a = r ! i
effectiveAddr (AutoInc (Reg i)) (Machine _ r)
  | i < 0 =    error "a wrong register"
  | a < 0 =    error "a wrong address"
  | otherwise = AtMemory a
  where a = r ! i
effectiveAddr (AutoDec (Reg i)) (Machine _ r)
  | i < 0 =    error "a wrong register"
  | a < 0 =    error "a wrong address"
  | otherwise = AtMemory a
  where a = r ! i

accessGet :: AddrMode -> Machine -> (Int, Machine)
accessGet a@(AutoInc (Reg j)) mr@(Machine m r) =
  case effectiveAddr a mr of
    AtMemory   i -> (m ! i * 256 + m ! (i+1), update)
    AtRegister i -> (r ! i, update)
    AsLiteral  _ -> error "strange situation"
  where
    update = Machine m (r // [(j, (r ! j) + 2)])

accessGet a@(AutoDec (Reg j)) (Machine m' r') =
  case effectiveAddr a mr of
    AtMemory   i -> (m ! i * 256 + m ! (i+1), mr)
    AtRegister i -> (r ! i, mr)
    AsLiteral  _ -> error "strange situation"
  where
    mr@(Machine m r) = Machine m' (r' //[(j, (r' ! j) - 2)])

accessGet a mr@(Machine m r) =
  case effectiveAddr a mr of
    AtMemory   i -> (m ! i * 256 + m ! (i+1), mr)
    AtRegister i -> (r ! i, mr)
    AsLiteral  n -> (n,     mr)

accessStore :: AddrMode -> Machine -> Int -> Machine
accessStore a@(AutoInc (Reg j)) mr@(Machine m r) x =
  case effectiveAddr a mr of
    AtMemory   i -> Machine (m // update i x) (r // [(j, (r ! j) + 2)])
    AtRegister i -> Machine m                 (r // [(i, x), (j, (r ! j) + 2)])
    AsLiteral  _ -> error "strange situation"
  where
    update :: Int -> Int -> [(Int, Int)]
    update i w = [(i, div w 256), (i + 1, mod w 256)]

accessStore a@(AutoDec (Reg j)) (Machine m' r') x =
  case effectiveAddr a mr of
    AtMemory   i -> Machine (m // update i x) r
    AtRegister i -> Machine m                 (r // [(i, x)])
    AsLiteral  _ -> error "strange situation"
  where
    mr@(Machine m r) = Machine m' (r' //[(j, (r' ! j) - 2)])
    update :: Int -> Int -> [(Int, Int)]
    update i w = [(i, div w 256), (i + 1, mod w 256)]

accessStore a mr@(Machine m r) x =
  case effectiveAddr a mr of
    AtMemory   i -> Machine (m // update i x) r
    AtRegister i -> Machine m                 (r // [(i, x)])
    AsLiteral  n -> Machine (m // update n x) r
  where
    update :: Int -> Int -> [(Int, Int)]
    update i w = [(i, div w 256), (i + 1, mod w 256)]
