{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

import Test.Hspec
import Test.QuickCheck

import Language.Asm.Inline

defineAsmFun "timesTwoInt" [t| Int -> Int |] "add %rbx, %rbx"
defineAsmFun "plusInt" [t| Int -> Int -> Int |] "add %r14, %rbx"
defineAsmFun "swapInts" [t| Int -> Int -> (Int, Int) |] "xchg %rbx, %r14"

main :: IO ()
main = hspec $
  describe "defineAsmFun (without QQ, with Ints)" $ do
    it "timesTwo" $ property $ \num -> timesTwoInt num `shouldBe` num * 2
    it "plusWord" $ property $ \n1 n2 -> plusInt n1 n2 `shouldBe` n1 + n2
    it "swap returns a tuple properly" $ property $ \n1 n2 -> swapInts n1 n2 `shouldBe` (n2, n1)
