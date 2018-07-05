module AssemblerSpec (spec) where

import Test.Hspec
import Data.List
import PDP11
import Assembler
-- import Simulator

makeString :: (Opcode, [Int], OpFormat)  -> String
makeString (op, _, OFA2) = show op ++ " R1, R2"
makeString (op, _, OFA1) = show op ++ " R1"
makeString (op, _, OFI1) = show op ++ " 100"

movAttr :: (Opcode, [Int], OpFormat)
movAttr = head codeTable

decodeEncode :: String -> Spec
decodeEncode str = do
    case assemble (str ++ "\n") of
      Left err  -> it str $ err `shouldBe` str
      Right asm -> it (show asm)$ (show . decodeList . encodeWord) asm  `shouldBe` str

addrModes :: [(String, [Int])]
addrModes = [ ("R7",     [0,0,0,1,1,1])
            , ("@R7",    [0,0,1,1,1,1])
            , ("(R7)+",  [0,1,0,1,1,1])
            , ("@(R7)+", [0,1,1,1,1,1])
            , ("-(R7)",  [1,0,0,1,1,1])
            , ("@-(R7)", [1,0,1,1,1,1])
            , ("1(R7)",  [1,1,0,1,1,1])
            , ("@1(R7)", [1,1,1,1,1,1])
            ]

encodeAddrMode :: String -> (String, [Int]) -> Spec
encodeAddrMode opc (am, bits) = do
  case assemble (opc ++ " " ++ am ++ "," ++ am ++ "\n") of
    Left err     -> it am $ err `shouldBe` show bits
    Right (as:_) -> it am $ (drop 10 . asBinaryList . asInt . head . toBitBlocks) as `shouldBe` bits

spec :: Spec
spec = do
  describe "(decode . encode)" $ do
    mapM_ (decodeEncode . makeString) codeTable
  describe "Address modes" $ do
    mapM_ (encodeAddrMode "MOV") addrModes

decodeList :: [Int] -> ASM
decodeList [x]       = decodeWord x (0,0)
decodeList [x, a]    = decodeWord x (a, 0)
decodeList (x:a:b:_) = decodeWord x (a,b)
