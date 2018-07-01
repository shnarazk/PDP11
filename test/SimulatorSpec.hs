module SimulatorSpec (spec) where

import Test.Hspec
import PDP11
import Assembler
import Simulator

instances :: [(FilePath, [Int], [Int])]
instances =
  [ ("Q7-5.asm" , [2,0,4,0,8,0,0,1,1,1,0,0], [0,2,2,4,0,6,1,102])
  , ("Q7-9.asm" , [0,0,0,0,0,0,0,0,0,0,0,0], [10,2,0,4,0,6,1,112])
  , ("Q7-10.asm", [2,0,4,0,8,0,0,1,1,1,0,0], [12,527,0,4,0,6,1,116]) -- 512 + 2 + 4 + 8 + 1
  , ("Q7-11.asm", [2,0,4,0,8,0,0,1,1,1,0,0], [10,2,0,4,0,6,1,118])
  , ("Q7-12.asm", [2,0,4,0,8,0,0,1,1,1,0,0], [16,2,0,4,0,6,1,114])
  , ("Q7-13.asm", [0,0,0,0,0,0,0,0,1,1,0,0], [0,0,0,4,0,6,1,112])
  , ("Q7-14.asm", [0,0,0,0,8,0,0,1,0,0,0,0], [0,6,0,4,0,6,1,110])
  ]

testRun :: (FilePath, [Int], [Int]) -> Spec
testRun (target, mem, reg) = do
  (Right code) <- assemble <$> runIO (readFile target)
  let s = makePDP11 mem reg
  describe target $ do
    it ("ends in " ++ show s) $ resetPSW (last (runSimulator' code)) `shouldBe` s

spec :: Spec
spec = mapM_ testRun instances
