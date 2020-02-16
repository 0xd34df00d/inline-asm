{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes #-}

import Test.Hspec
import Test.QuickCheck

import Language.Asm.Inline

defineAsmFun "timesTwo" [t| Int -> Int |] "add %rbx, %rbx"
defineAsmFun "plusWord" [t| Int -> Int -> Int |] "add %r14, %rbx"

main :: IO ()
main = hspec $
  describe "Basic functions" $ do
    it "timesTwo" $ property $ \num -> timesTwo num `shouldBe` num * 2
    it "plusWord" $ property $ \n1 n2 -> plusWord n1 n2 `shouldBe` n1 + n2
