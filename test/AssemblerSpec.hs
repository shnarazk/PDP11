module AssemblerSpec (spec) where

import Test.Hspec
import PDP11
import Assembler
-- import Simulator

makeString :: (Opcode, [Int], OpFormat)  -> String
makeString (op, _, OFA2) = show op ++ " R1, R2"
makeString (op, _, OFA1) = show op ++ " R1"
makeString (op, _, OFI1) = show op ++ " 100"

testRun :: String -> Spec
testRun str = do
  let asm = assemble (str ++ "\n")
  describe str $ do
    case asm of
      Left err  -> it str $ err `shouldBe` str
      Right asm -> it (show asm)$ (show . decodeList . encodeWord) asm  `shouldBe` str

spec :: Spec
spec = mapM_ (testRun . makeString) codeTable

decodeList :: [Int] -> ASM
decodeList [x]       = decodeWord x (0,0)
decodeList [x, a]    = decodeWord x (a, 0)
decodeList (x:a:b:_) = decodeWord x (a,b)
