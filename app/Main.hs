{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

module Main where

import GHC.Prim
import GHC.Word

import Language.Asm.Inline

defineAsmFun "foobar" [t| Word -> Word |] "\tadd %rbx, %rbx"

foreign import prim "foobar_unlifted" foobar_unlifted :: Word# -> Word#

foobar :: Word -> Word
foobar (W# w) = W# (foobar_unlifted w)

main :: IO ()
main = print $ foobar 21
