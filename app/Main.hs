{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes #-}

module Main where

import Language.Asm.Inline

defineAsmFun "foobar" [t| Word -> Word |] "add %rbx, %rbx"

main :: IO ()
main = print $ foobar 21
