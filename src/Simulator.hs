{-# LANGUAGE
    TemplateHaskell
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
import Data.Array
import PDP11 hiding (version)
import Assembler hiding (version)

version :: String
version = "0.2.1"

-- * m ^. register ^? iix 2       	    to access R2 maybe
-- * m ^. register & iix 2 .~ 300 	    to update R2 = 300
-- * m & register . iix 2 .~ 300 	    to update R2 in m to 300
-- * m & register %~ (// [(1,10), (2,20)])  to update R1 and R2 in m
-- Note: (register %~ (// ...)) :: Machine -> Machine
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
initialMachine = Machine (chunk 16 [0, 2, 0, 4, 0, 8, 1, 255, 0, 8, 0, 10]) (chunk 8 [0, 2, 0, 4, 0, 6])
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
        mnems = map (dropWhile (`elem` " \t")) $ lines str

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

-- updater of byte array index for an Int
(<..) :: Int -> Int -> (Array Int Int -> Array Int Int)
i <.. x = (// [(i, div x 256), (i + 1, mod x 256)])

-- updater of Int array index for an Int
(<.) :: Int -> Int -> (Array Int Int -> Array Int Int)
i <. x = (// [(i, x)])

-- returns a pair of left-hand value and right-hand value
fetchI :: AddrMode -> State (Locator, Int)
fetchI = State . fetchLR

fetchLR :: AddrMode -> Machine -> (Machine, (Locator, Int))
fetchLR (Register (Reg i)) s = (s, (AtRegister i, (s ^. register) ! i))
fetchLR (Immediate n) s      = (s, (AsLiteral n, n))
fetchLR (Index o (Reg i)) s  = (s, (AtMemory (i + o), (s ^. memory) !.. (i + o)))
fetchLR (AutoInc (Reg j)) s  = (s', (AtMemory i, (s ^. memory) !.. i))
  where i = (s ^. register) ! j
        s' = s & register %~ (j <. (i + 2))
fetchLR (AutoDec (Reg j)) s  = (s', (AtMemory i, (s ^. memory) !.. i))
  where i = ((s ^. register) ! j) - 2
        s' = s & register %~ (j <. i)
fetchLR (Indirect a) s       =
  case l of
    AtRegister _ -> (s', (AtMemory v, m !.. v))
    AtMemory _   -> (s', (AtMemory v, m !.. v))
    AsLiteral i  -> (s', (AtMemory (m !.. i), m !.. (m !.. i)))
  where (s', (l, v)) = fetchLR a s
        m = s' ^. memory

storeI :: Locator -> Int -> State ()
storeI (AtMemory i)   x = State $ \m -> (m & memory   %~ (i <.. x), ())
storeI (AtRegister i) x = State $ \m -> (m & register %~ (i <.  x), ())
storeI (AsLiteral i)  x = State $ \m -> (m & memory   %~ (i <.. x), ())
