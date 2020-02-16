{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Asm.Inline where

import Control.Monad
import Data.Generics.Uniplate.Data
import GHC.Prim
import GHC.Types hiding (Type)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class AsmArg a (rep :: RuntimeRep) (uty :: TYPE rep) | a -> rep, a -> uty where
  unbox :: a -> uty
  rebox :: uty -> a

instance AsmArg Int 'IntRep Int# where
  unbox (I# w) = w
  rebox = I#

instance AsmArg Word 'WordRep Word# where
  unbox (W# w) = w
  rebox = W#

defineAsmFun :: String -> Q Type -> String -> Q [Dec]
defineAsmFun name funTyQ asmCode = do
  addForeignSource LangAsm $ unlines [ ".global " <> asmName
                                     , asmName <> ":"
                                     , asmCode
                                     , "jmp *(%rbp)"
                                     ]
  funTy <- funTyQ
  let importedName = mkName asmName
  wrapperFunD <- mkFunD name importedName funTy
  pure
    [ ForeignD $ ImportF Prim Safe asmName importedName $ unliftType funTy
    , SigD (mkName name) funTy
    , wrapperFunD
    ]
  where
    asmName = name <> "_unlifted"

mkFunD :: String -> Name -> Type -> Q Dec
mkFunD funName importedName funTy = do
  argNames <- replicateM (countArgs funTy) $ newName "arg"
  funAppE <- foldM f (VarE importedName) argNames
  body <- [e| rebox $(pure funAppE) |]
  pure $ FunD (mkName funName) [Clause (VarP <$> argNames) (NormalB body) []]
  where
    f acc argName = [e| $(pure acc) (unbox $(pure $ VarE argName)) |]

unliftType :: Type -> Type
unliftType = transformBi unliftTuple . transformBi unliftBaseTy
  where
    unliftBaseTy x | x == ''Word = ''Word#
                   | x == ''Int = ''Int#
                   | otherwise = x
    unliftTuple (TupleT n) = UnboxedTupleT n
    unliftTuple x = x

countArgs :: Type -> Int
countArgs ty = length $ filter (== ArrowT) $ universeBi ty
