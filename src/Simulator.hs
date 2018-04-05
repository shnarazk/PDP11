{-# LANGUAGE
    TemplateHaskell
  #-}

module Simulator
    (
      Machine(..)
    , initialMachine
    , runSimulator
    , runSimulator'
    ) where

import Control.Lens
import Data.Array
import PDP11

makeLenses ''Machine

newtype State a = State ((->) Machine (Machine, a))

instance Functor State where
  fmap g (State f) = State $ \m -> let (m', x) = f m in (m', g x)

instance Applicative State where
  pure v = State $ \m -> (m, v)
  (State g) <*> (State f) = State $ \m -> let (m', x') = f m
                                              (m'', g') = g m'
                                          in (m'', g' x')

instance Monad State where
  (State f) >> (State g) = State $ \x -> let (x', _) = f x in g x'
  (State f) >>= g = State $ \m -> let (m', x') = f m
                                      (State h) = g x'
                                  in h m'

initialMachine :: Machine -- memory is at left; register is at right.
initialMachine = Machine (chunk 20 [0, 10, 0, 20, 0, 40, 1, 255]) (chunk 8 [0, 2, 0, 4, 0, 6])
  where
    chunk :: Int -> [Int] -> Array Int Int
    chunk n l = listArray (0, n-1) (take n (l ++ repeat 0))

runSimulator :: Machine -> [ASM] -> [Machine]
runSimulator m l = scanl (flip execute) m l

runSimulator' :: [ASM] -> [Machine]
runSimulator' l = runSimulator initialMachine l

runI :: State a -> Machine -> Machine
runI (State s) m = fst $ s m

execute :: ASM -> Machine -> Machine
execute (MOV s d) = runI $ do x <- getI s
                              setI d x

execute (ADD s d) = runI $ do x <- getI s
                              y <- getI d
                              setI d (y + x)

execute (SUB s d) = runI $ do x <- getI s
                              y <- getI d
                              setI d (y - x)

execute (MUL s d) = runI $ do x <- getI s
                              y <- getI d
                              setI d (y + x)

execute (CLR d) = runI $ setI d 0

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

getI :: AddrMode -> State Int
getI = State . accessGet

accessGet :: AddrMode -> Machine -> (Machine, Int) -- AddrMode -> State Int
accessGet a@(AutoInc (Reg j)) mr@(Machine m r) =
  case effectiveAddr a mr of
    AtMemory   i -> (update, m ! i * 256 + m ! (i+1))
    AtRegister i -> (update, r ! i)
    AsLiteral  _ -> error "strange situation"
  where
    update = Machine m (r // [(j, (r ! j) + 2)])

accessGet a@(AutoDec (Reg j)) (Machine m' r') =
  case effectiveAddr a mr of
    AtMemory   i -> (mr, m ! i * 256 + m ! (i+1))
    AtRegister i -> (mr, r ! i)
    AsLiteral  _ -> error "strange situation"
  where
    mr@(Machine m r) = Machine m' (r' //[(j, (r' ! j) - 2)])

accessGet a mr@(Machine m r) =
  case effectiveAddr a mr of
    AtMemory   i -> (mr, m ! i * 256 + m ! (i+1))
    AtRegister i -> (mr, r ! i)
    AsLiteral  n -> (mr, n)

setI :: AddrMode -> Int -> State ()
setI a d = State $ accessStore a d

accessStore :: AddrMode -> Int -> Machine -> (Machine, ())
accessStore a@(AutoInc (Reg j)) x mr@(Machine m r) =
  case effectiveAddr a mr of
    AtMemory   i -> (Machine (m // update i x) (r // [(j, (r ! j) + 2)]), ())
    AtRegister i -> (Machine m                 (r // [(i, x), (j, (r ! j) + 2)]), ())
    AsLiteral  _ -> error "strange situation"
  where
    update :: Int -> Int -> [(Int, Int)]
    update i w = [(i, div w 256), (i + 1, mod w 256)]

accessStore a@(AutoDec (Reg j)) x (Machine m' r') =
  case effectiveAddr a mr of
    AtMemory   i -> (Machine (m // update i x) r, ())
    AtRegister i -> (Machine m                 (r // [(i, x)]), ())
    AsLiteral  _ -> error "strange situation"
  where
    mr@(Machine m r) = Machine m' (r' //[(j, (r' ! j) - 2)])
    update :: Int -> Int -> [(Int, Int)]
    update i w = [(i, div w 256), (i + 1, mod w 256)]

accessStore a x mr@(Machine m r) =
  case effectiveAddr a mr of
    AtMemory   i -> (Machine (m // update i x) r, ())
    AtRegister i -> (Machine m                 (r // [(i, x)]), ())
    AsLiteral  n -> (Machine (m // update n x) r, ())
  where
    update :: Int -> Int -> [(Int, Int)]
    update i w = [(i, div w 256), (i + 1, mod w 256)]
