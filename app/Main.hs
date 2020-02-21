{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

module Main where

import Language.Asm.Inline
import Language.Asm.Inline.QQ

defineAsmFun "swap" [t| Int -> Int -> (Int, Int) |] "xchg %rbx, %r14"

defineAsmFun "swap2p1"
  [t| Int -> Int -> (Int, Int) |]
  [asm| a b |
  xchg ${a}, ${b}
  add $1, ${b}
  |]

defineAsmFun "testInt" [t| Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int |] "int $3"
defineAsmFun "testDouble" [t| Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double |] "int $3"

main :: IO ()
main = do
  print $ testInt 0 1 2 3 4 5 6
  print $ testDouble 1 1 0 0 0 0 1
