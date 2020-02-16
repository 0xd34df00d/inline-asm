{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

module Main where

import Language.Asm.Inline

defineAsmFun "swap" [t| Int -> Int -> (Int, Int) |] "xchg %rbx, %r14"

main :: IO ()
main = print $ swap 2 3
