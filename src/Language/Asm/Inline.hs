{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Asm.Inline where

import Control.Monad
import Data.Generics.Uniplate.Data
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

defineAsmFun :: String -> Q Type -> String -> Q [Dec]
defineAsmFun name funTyQ asmCode = do
  addForeignSource LangAsm $ unlines [ ".global " <> asmName
                                     , asmName <> ":"
                                     , asmCode
                                     , "\tjmp *(%rbp)"
                                     ]
  funTy <- funTyQ
  let importedName = mkName asmName
  funD <- mkFunD name importedName funTy
  pure
    [ ForeignD $ ImportF Prim Safe asmName importedName $ unliftType funTy
    , SigD (mkName name) funTy
    , funD
    ]
  where
    asmName = name <> "_unlifted"

mkFunD :: String -> Name -> Type -> Q Dec
mkFunD funName importedName funTy = do
  argNames <- replicateM (countArgs funTy) $ newName "arg"
  funAppE <- foldM f (VarE importedName) argNames
  body <- [e| rebox ( $(pure funAppE) ) |]
  pure $ FunD (mkName funName) [Clause (VarP <$> argNames) (NormalB body) []]
  where
    f acc argName = [e| $(pure acc) (unbox $(pure $ VarE argName)) |]

unliftType :: Type -> Type
unliftType = transformBi f
  where
    f x | x == ''Word = ''Word#
        | x == ''Int = ''Int#
        | otherwise = x

countArgs :: Type -> Int
countArgs ty = length [ () | ConT _ <- universeBi ty ] - 1
