{-# LANGUAGE
    TemplateHaskell
  #-}

module Simulator
    (
      Machine(..)
    , runPDP11
    , initialMachine
    , runSimulator
    , runSimulator'
    ) where

import Control.Lens
import Data.Array
import PDP11
import Assembler

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
initialMachine = Machine (chunk 16 [0, 2, 0, 4, 0, 8, 1, 255]) (chunk 8 [0, 2, 0, 4, 0, 6])
  where
    chunk :: Int -> [Int] -> Array Int Int
    chunk n l = listArray (0, n-1) (take n (l ++ repeat 0))

runSimulator :: Machine -> [ASM] -> [Machine]
runSimulator m l = scanl (flip execute) m l

runSimulator' :: [ASM] -> [Machine]
runSimulator' l = runSimulator initialMachine l

runPDP11 :: String -> Maybe String
runPDP11 str = run <$> readASM str
  where run program = unlines $ concatMap (\(n, m) -> [n, show m]) $ zip instrs states
          where states = runSimulator' program
                instrs = "#0 Initial state" : zipWith3 combine [1.. ] mnems program
                combine n a b = "#" ++ show n ++ " " ++ a ++ "\t; " ++ show b
        mnems = lines str

runI :: State a -> Machine -> Machine
runI (State s) m = fst $ s m

execute :: ASM -> Machine -> Machine
execute (MOV s d) = runI $ do (_, x) <- fetchI s
                              (p, _) <- fetchI d
                              storeI p x

execute (ADD s d) = runI $ do (_, x) <- fetchI s
                              (p, y) <- fetchI d
                              storeI p (y + x)

execute (SUB s d) = runI $ do (_, x) <- fetchI s
                              (p, y) <- fetchI d
                              storeI p (y - x)

execute (MUL s d) = runI $ do (_, x) <- fetchI s
                              (p, y) <- fetchI d
                              storeI p (y * x)

execute (CLR d) = runI $ do (p, _) <- fetchI d
                            storeI p 0

-- successive memory access
(!..) :: Array Int Int -> Int -> Int
v !.. i = (v ! i) * 256 + (v ! (i + 1))

-- byte array index for an int
(<..) :: Int -> Int -> [(Int, Int)]
i <.. x = [(i, div x 256), (i + 1, mod x 256)]

-- returns a pair of left-hand value and right-hand value
fetchI :: AddrMode -> State (Locator, Int)
fetchI = State . fetchLR

fetchLR :: AddrMode -> Machine -> (Machine, (Locator, Int))
fetchLR (Immediate n) s = (s, (AsLiteral n, n))
fetchLR (Register (Reg i)) s@(Machine _ r) = (s, (AtRegister i, r ! i))
fetchLR (AutoInc (Reg j)) (Machine m r) = (s', (AtMemory i, m !.. i))
  where i = r ! j
        s' = Machine m (r // [(j, i + 2)])
fetchLR (AutoDec (Reg j)) (Machine m' r') = (s, (AtMemory i, m !.. i))
  where i = r ! j
        s@(Machine m r) = Machine m' (r' //[(j, (r' ! j) - 2)])
fetchLR (Indirect a) s =
  case l of
    AtRegister _ -> (s', (AtMemory v, m !.. v))
    AtMemory _   -> (s', (AtMemory v, m !.. v))
    AsLiteral i  -> (s', (AtMemory (m !.. i), m !.. (m !.. i)))
  where (s'@(Machine m _), (l, v)) = fetchLR a s

storeI :: Locator -> Int -> State ()
storeI (AtMemory i)   x = State $ \(Machine m r) -> (Machine (m // (i <.. x)) r, ())
storeI (AtRegister i) x = State $ \(Machine m r) -> (Machine m (r // [(i, x)]), ())
storeI (AsLiteral i)  x = State $ \(Machine m r) -> (Machine (m // (i <.. x)) r, ())
