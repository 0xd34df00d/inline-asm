{-# LANGUAGE FlexibleInstances #-}

module Language.Asm.Inline.AsmCode where

import Language.Haskell.TH.Syntax

class AsmCode c where
  codeToString :: c -> String
  validateCode :: Type -> c -> Either String ()

instance AsmCode String where
  codeToString = id
  validateCode _ _ = pure ()
