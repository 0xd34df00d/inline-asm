{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}

module Language.Asm.Inline where

import GHC.Prim
import GHC.Types hiding (Type)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type family ArgRep a :: RuntimeRep where
  ArgRep Int = 'IntRep
  ArgRep Word = 'WordRep
  ArgRep Float = 'FloatRep
  ArgRep Double = 'DoubleRep

class AsmArg a where
  type UnboxedType a :: TYPE (ArgRep a)
  unbox :: a -> UnboxedType a
  rebox :: UnboxedType a -> a

instance AsmArg Int where
  type UnboxedType Int = Int#
  unbox (I# w) = w
  rebox = I#

instance AsmArg Word where
  type UnboxedType Word = Word#
  unbox (W# w) = w
  rebox = W#

type family UnboxedFunTyRep a :: RuntimeRep where
  UnboxedFunTyRep (_ -> _) = 'LiftedRep
  UnboxedFunTyRep a = ArgRep a

type family UnboxedFunTy a :: TYPE (UnboxedFunTyRep a) where
  UnboxedFunTy (a -> b) = UnboxedFunTy a -> UnboxedFunTy b
  UnboxedFunTy a = UnboxedType a

inlineAsm :: String -> Q [Dec]
inlineAsm asmCode = do
  addForeignSource LangAsm $ unlines [ asmCode
                                     , "\tjmp *(%rbp)"
                                     ]
  pure []
