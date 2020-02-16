{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

import Test.Hspec
import Test.QuickCheck

import Language.Asm.Inline

defineAsmFun "timesTwo" [t| Int -> Int |] "add %rbx, %rbx"
defineAsmFun "plusWord" [t| Int -> Int -> Int |] "add %r14, %rbx"
defineAsmFun "swap" [t| Int -> Int -> (Int, Int) |] "xchg %rbx, %r14"

main :: IO ()
main = hspec $ do
  describe "Basic functions" $ do
    it "timesTwo" $ property $ \num -> timesTwo num `shouldBe` num * 2
    it "plusWord" $ property $ \n1 n2 -> plusWord n1 n2 `shouldBe` n1 + n2
  describe "Returning tuples" $
    it "swap" $ property $ \n1 n2 -> swap n1 n2 `shouldBe` (n2, n1)
