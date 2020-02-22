{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

import Test.Hspec
import Test.QuickCheck

import Language.Asm.Inline
import Language.Asm.Inline.QQ

defineAsmFun "timesTwoInt" [t| Int -> Int |] "add %rbx, %rbx"
defineAsmFun "plusInt" [t| Int -> Int -> Int |] "add %r14, %rbx"
defineAsmFun "swapInts" [t| Int -> Int -> (Int, Int) |] "xchg %rbx, %r14"

defineAsmFun "timesTwoIntQQ"
  [asmTy| (a : Int) | (_ : Int) |]
  [asm| add ${a}, ${a} |]

defineAsmFun "plusIntQQ"
  [asmTy| (a : Int) (b : Int) | (_ : Int) |]
  [asm| add ${b}, ${a} |]

defineAsmFun "swap2p1QQ"
  [asmTy| (a : Int) (b : Int) | (a : Int) (b : Int) |]
  [asm|
  xchg ${a}, ${b}
  add $1, ${b}
  |]

main :: IO ()
main = hspec $ do
  describe "Works with Ints (the non-QQ version)" $ do
    it "timesTwo" $ property $ \num -> timesTwoInt num `shouldBe` num * 2
    it "plusWord" $ property $ \n1 n2 -> plusInt n1 n2 `shouldBe` n1 + n2
    it "swap returns a tuple properly" $ property $ \n1 n2 -> swapInts n1 n2 `shouldBe` (n2, n1)
  describe "Works on Ints" $ do
    it "timesTwo" $ property $ \num -> timesTwoIntQQ num `shouldBe` num * 2
    it "plusWord" $ property $ \n1 n2 -> plusIntQQ n1 n2 `shouldBe` n1 + n2
    it "swap returns a tuple properly" $ property $ \n1 n2 -> swap2p1QQ n1 n2 `shouldBe` (n2, n1 + 1)
