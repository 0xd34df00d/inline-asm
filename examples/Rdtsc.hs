{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

module Main where

import GHC.Word

import Language.Asm.Inline
import Language.Asm.Inline.QQ

defineAsmFunM "rdtsc"
  [asmTy| | (out : Word64) |]
  [asm|
  rdtsc
  mov %rdx, {out}
  shl $32, {out}
  add %rax, {out}
  |]

defineAsmFunM "rdtscP"
  [asmTy| | (hi : Word32) (lo : Word32) |]
  [asm|
  rdtsc
  mov %rdx, {hi}
  mov %rax, {lo}
  |]

defineAsmFunM "rdtsc2"
  [asmTy| | (out1 : Word64) (out2 : Word64) |]
  [asm|
  rdtsc
  mov %rdx, {out1}
  shl $32, {out1}
  add %rax, {out1}

  rdtsc
  mov %rdx, {out2}
  shl $32, {out2}
  add %rax, {out2}
  |]

main :: IO ()
main = do
  v1 <- rdtsc
  v2 <- rdtsc
  print v1
  print v2
  print $ v2 - v1

  p1 <- rdtscP
  p2 <- rdtscP
  print p1
  print p2

  (o1, o2) <- rdtsc2
  print $ o2 - o1
