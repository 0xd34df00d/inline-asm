{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString(ByteString)
import Foreign.Ptr
import GHC.Word
import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck

import Language.Asm.Inline
import Language.Asm.Inline.QQ
import Language.Asm.Inline.Util

defineAsmFun "timesTwoInt" [t| Int -> Int |] "add %rbx, %rbx"
defineAsmFun "plusInt" [t| Int -> Int -> Int |] "add %r14, %rbx"
defineAsmFun "swapInts" [t| Int -> Int -> (Int, Int) |] "xchg %rbx, %r14"


defineAsmFun "timesTwoIntQQ"
  [asmTy| (a : Int) | (_ : Int) |]
  [asm| add ${a}, ${a} |]

defineAsmFun "plusIntQQ"
  [asmTy| (a : Int) (b : Int) | (_ : Int) |]
  [asm| add ${b}, ${a} |]

defineAsmFun "plus3IntQQ"
  [asmTy| (a : Int) (b : Int) (c : Int) | (_ : Int) |]
  [asm|
  add ${b}, ${a}
  add ${c}, ${a}
  |]

defineAsmFun "swap2p1QQ"
  [asmTy| (a : Int) (b : Int) | (a : Int) (b : Int) |]
  [asm|
  xchg ${a}, ${b}
  add $1, ${b}
  |]


defineAsmFun "timesTwoFloatQQ"
  [asmTy| (a : Float) | (_ : Float) |]
  [asm| addss ${a}, ${a} |]

defineAsmFun "plusFloatQQ"
  [asmTy| (a : Float) (b : Float) | (_ : Float) |]
  [asm| addss ${b}, ${a} |]


defineAsmFun "timesTwoDoubleQQ"
  [asmTy| (a : Double) | (_ : Double) |]
  [asm| addsd ${a}, ${a} |]

defineAsmFun "plusDoubleQQ"
  [asmTy| (a : Double) (b : Double) | (_ : Double) |]
  [asm| addsd ${b}, ${a} |]


defineAsmFun "timesTwoEverything"
  [asmTy| (d : Double) (n : Int) (f : Float) (w : Word) | (_ : Double) (_ : Int) (_ : Float) (_ : Word) |]
  [asm|
  addsd ${d}, ${d}
  addss ${f}, ${f}
  add ${n}, ${n}
  add ${w}, ${w}
  |]


defineAsmFun "addPtr"
  [asmTy| (ptr : Ptr Int) (shift : Int) | (_ : Ptr Int) |]
  [asm|
  add ${shift}, ${ptr}
  |]

defineAsmFun "swapPtrs"
  [asmTy| (a : Ptr Int) (b : Ptr Int) | (_ : Ptr Int) (_ : Ptr Int) |]
  [asm|
  xchg ${a}, ${b}
  |]


defineAsmFun "lastChar"
  [asmTy| (bs : ByteString) (def : Word) | (w : Word) |]
  [asm|
  test ${bs:len}, ${bs:len}
  jz is_zero
  movzbq -1(${bs:ptr},${bs:len}), ${w}
  RET_HASK
is_zero:
  mov ${def}, ${w}
  |]

defineAsmFun "countCharsSSE42"
  [asmTy| (ch : Word8) (ptr : Ptr Word8) (len : Int) | (cnt : Int) |] $
  unroll "i" [12..15]
  [asm|
  push %r${i}|] <> [asm|
  vmovd ${ch}, %xmm15
  vpxor %xmm0, %xmm0, %xmm0
  vpshufb %xmm0, %xmm15, %xmm15

  shr $7, ${len}

  mov $16, %eax
  mov $16, %edx

  xor ${cnt}, ${cnt}

  ${move ptr rdi}
loop: |] <> unrolls "i" [1..8] [
  [asm|
  vmovdqa ${(i - 1) * 0x10}(${ptr}), %xmm${i}
  |], [asm|
  vpcmpestrm $10, %xmm15, %xmm${i}
  vmovdqa %xmm0, %xmm${i}
  |], [asm|
  vmovq %xmm${i}, %r${i + 7}
  |], [asm|
  popcnt %r${i + 7}, %r${i + 7}
  |], [asm|
  add %r${i + 7}, ${cnt}
  |]
  ] <>
  [asm|
  add $128, ${ptr}
  dec ${len}
  jnz loop|] <> unroll "i" [15,14..12] [asm|
  pop %r${i} |]

countChars :: Word8 -> BS.ByteString -> Int
countChars ch bs | BS.length bs <= 256 = BS.count ch bs
                 | otherwise = BS.count ch (substr 0 startLen bs)
                             + countCharsSSE42 ch (castPtr alignedPtr) alignedLen
                             + BS.count ch (substr endPos endLen bs)
  where
    basePtr = getBSAddr bs
    alignedPtr = alignPtr basePtr alignment
    startLen = alignedPtr `minusPtr` basePtr
    (alignedLen, endLen) = let remLen = BS.length bs - startLen
                               remainder = remLen `rem` alignment
                            in (remLen - remainder, remainder)
    endPos = startLen + alignedLen
    alignment = 128

asBS :: ASCIIString -> BS.ByteString
asBS (ASCIIString str) = BS8.pack str

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000) $ do
  describe "Works with Ints (the non-QQ version)" $ do
    it "timesTwo" $ property $ \num -> timesTwoInt num `shouldBe` num * 2
    it "plusInt" $ property $ \n1 n2 -> plusInt n1 n2 `shouldBe` n1 + n2
    it "swap returns a tuple properly" $ property $ \n1 n2 -> swapInts n1 n2 `shouldBe` (n2, n1)
  describe "Works on Ints" $ do
    it "timesTwo" $ property $ \num -> timesTwoIntQQ num `shouldBe` num * 2
    it "plusInt" $ property $ \n1 n2 -> plusIntQQ n1 n2 `shouldBe` n1 + n2
    it "plus3Int" $ property $ \n1 n2 n3 -> plus3IntQQ n1 n2 n3 `shouldBe` n1 + n2 + n3
    it "swap returns a tuple properly" $ property $ \n1 n2 -> swap2p1QQ n1 n2 `shouldBe` (n2, n1 + 1)
  describe "Works on Floats" $ do
    it "timesTwo" $ property $ \num -> timesTwoFloatQQ num `shouldBe` num * 2
    it "plusFloat" $ property $ \n1 n2 -> plusFloatQQ n1 n2 `shouldBe` n1 + n2
  describe "Works on Doubles" $ do
    it "timesTwo" $ property $ \num -> timesTwoDoubleQQ num `shouldBe` num * 2
    it "plusDouble" $ property $ \n1 n2 -> plusDoubleQQ n1 n2 `shouldBe` n1 + n2
  describe "Works on Ptrs" $ do
    it "addPtr" $ property $ \int num -> let ptr = intToPtr int
                                          in addPtr ptr num `shouldBe` (ptr `plusPtr` num)
    it "swap returns a tuple properly" $ property $ \n1 n2 -> let p1 = intToPtr n1
                                                                  p2 = intToPtr n2
                                                               in swapPtrs p1 p2 `shouldBe` (p2, p1)
  describe "Works on mixed types" $
    it "timesTwoEverything" $ property $ \d n f w -> timesTwoEverything d n f w `shouldBe` (d + d, n * 2, f + f, w * 2)
  describe "Works on ByteString" $
    it "lastChar" $ property $ \(asBS -> bs) def -> lastChar bs def `shouldBe` if BS.null bs
                                                                               then def
                                                                               else fromIntegral $ fromEnum $ BS.last bs
  describe "More examples" $
    it "counting chars" $ property $ \(InfiniteList infList _) len needle ->
        let bs = BS.pack $ take (len * 100) infList
         in countChars needle bs `shouldBe` BS.count needle bs

intToPtr :: Int -> Ptr a
intToPtr = intPtrToPtr . IntPtr

substr :: Int -> Int -> BS.ByteString -> BS.ByteString
substr start len = BS.take len . BS.drop start
