module SimulatorSpec (spec) where

import Test.Hspec
import Assembler
import Simulator

instances :: [(FilePath, [Int], [Int])]
instances =
  [ ("Q7-9.asm" , [0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0],   [10,2,0,4,0,6,1,28])
  , ("Q7-10.asm", [0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], [12,543,0,4,0,6,1,32])
  , ("Q7-11.asm", [0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], [10,2,0,4,0,6,1,34])
  , ("Q7-12.asm", [0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], [16,2,0,4,0,6,1,30])
  , ("Q7-13.asm", [0,0,0,0,0,0,0,0,0,8,0,10,0,0,0,0],   [0,0,0,4,0,6,1,28])
  , ("Q7-14.asm", [0,0,0,0,0,8,1,255,0,0,0,10,0,0,0,0], [0,6,0,4,0,6,1,26])
  ]

testRun :: (FilePath, [Int], [Int]) -> Spec
testRun (target, mem, reg) = do
  (Just code) <- assemble <$> runIO (readFile target)
  let s = makePDP11 mem reg
  describe target $ do
    it ("ends in " ++ show s) $ last (runSimulator' code) `shouldBe` s

spec :: Spec
spec = mapM_ testRun instances
