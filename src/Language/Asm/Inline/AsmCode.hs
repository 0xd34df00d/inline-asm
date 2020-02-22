{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

module Language.Asm.Inline.AsmCode where

import Language.Haskell.TH.Syntax

class AsmCode tyAnn code | code -> tyAnn, tyAnn -> code where
  codeToString :: tyAnn -> code -> String
  toTypeQ :: tyAnn -> Q Type

instance AsmCode (Q Type) String where
  codeToString _ = id
  toTypeQ = id
