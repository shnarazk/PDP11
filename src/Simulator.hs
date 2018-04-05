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
initialMachine = Machine (chunk 16 [0, 10, 0, 20, 0, 40, 1, 255]) (chunk 8 [0, 2, 0, 4, 0, 6])
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
execute (MOV s' d') = runI $ do s <- getL s'
                                d <- getL d'
                                x <- fetchL s
                                storeL d x

execute (ADD s' d') = runI $ do s <- getL s'
                                d <- getL d'
                                x <- fetchL s
                                y <- fetchL d
                                storeL d (y + x)

execute (SUB s' d') = runI $ do s <- getL s'
                                d <- getL d'
                                x <- fetchL s
                                y <- fetchL d
                                storeL d (y - x)

execute (MUL s' d') = runI $ do s <- getL s'
                                d <- getL d'
                                x <- fetchL s
                                y <- fetchL d
                                storeL d (y * x)

execute (CLR d') = runI $ do d <- getL d'
                             storeL d 0

effectiveAddr :: AddrMode -> Machine -> Locator
effectiveAddr (Immediate n) _ = AsLiteral n
effectiveAddr (Register (Reg r)) _ = AtRegister r
effectiveAddr (Indirect (Reg i)) (Machine _ r)
  | i < 0 =     error "a wrong register"
  | a < 0 =     error "a wrong address"
  | otherwise = AtMemory a
  where a = r ! i
effectiveAddr (AutoInc (Reg i)) (Machine _ r)
  | i < 0 =     error "a wrong register"
  | a < 0 =     error "a wrong address"
  | otherwise = AtMemory a
  where a = r ! i
effectiveAddr (AutoDec (Reg i)) (Machine _ r)
  | i < 0 =     error "a wrong register"
  | a < 0 =     error "a wrong address"
  | otherwise = AtMemory a
  where a = r ! i

getL :: AddrMode -> State Locator
getL = State . asLocator

asLocator :: AddrMode -> Machine -> (Machine, Locator) -- AddrMode -> State Int
asLocator a@(AutoInc (Reg j)) mr@(Machine m r) =
  case effectiveAddr a mr of
    AtMemory   i -> (update, AtMemory i)
    AtRegister i -> (update, AtRegister i)
    AsLiteral  _ -> error "strange situation"
  where
    update = Machine m (r // [(j, (r ! j) + 2)])

asLocator a@(AutoDec (Reg j)) (Machine m' r') =
  case effectiveAddr a mr of
    AtMemory   i -> (mr, AtMemory i)
    AtRegister i -> (mr, AtRegister i)
    AsLiteral  _ -> error "strange situation"
  where
    mr@(Machine m r) = Machine m' (r' //[(j, (r' ! j) - 2)])

asLocator a mr = (mr, effectiveAddr a mr)

fetchL :: Locator -> State Int
fetchL (AtMemory i)   = State $ \s@(Machine m _) -> (s, (m ! i * 256 + m ! (i+1)))
fetchL (AtRegister i) = State $ \s@(Machine _ r) -> (s, (r ! i))
fetchL (AsLiteral x)  = State $ \s -> (s, x)

storeL :: Locator -> Int -> State ()
storeL (AtMemory i)   x = State $ \(Machine m r) -> (Machine (m // [(i, div x 256), (i + 1, mod x 256)]) r, ())
storeL (AtRegister i) x = State $ \(Machine m r) -> (Machine m (r // [(i, x)]), ())
