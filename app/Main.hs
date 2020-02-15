{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

module Main where

import GHC.Prim
import GHC.Word

import Language.Asm.Inline

inlineAsm ".global foobar\nfoobar:\tadd %rbx, %rbx"

foreign import prim "foobar" foobar :: Word# -> Word#

foobar' :: Word -> Word
foobar' (W# w) = W# (foobar w)

main :: IO ()
main = print $ foobar' 21
