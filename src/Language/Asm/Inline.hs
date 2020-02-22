{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Asm.Inline(defineAsmFun) where

import Control.Monad
import Data.Generics.Uniplate.Data
import GHC.Prim
import GHC.Types hiding (Type)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Asm.Inline.AsmCode
import Language.Asm.Inline.Util

class AsmArg a (rep :: RuntimeRep) (unboxedTy :: TYPE rep) | a -> rep, a -> unboxedTy where
  unbox :: a -> unboxedTy
  rebox :: unboxedTy -> a

instance AsmArg Int 'IntRep Int# where
  unbox (I# w) = w
  rebox = I#

instance AsmArg Word 'WordRep Word# where
  unbox (W# w) = w
  rebox = W#

instance AsmArg Double 'DoubleRep Double# where
  unbox (D# d) = d
  rebox = D#

instance AsmArg Float 'FloatRep Float# where
  unbox (F# f) = f
  rebox = F#

defineAsmFun :: AsmCode tyAnn code => String -> tyAnn -> code -> Q [Dec]
defineAsmFun name tyAnn asmCode = do
  addForeignSource LangAsm $ unlines [ ".global " <> asmName
                                     , asmName <> ":"
                                     , codeToString tyAnn asmCode
                                     , "jmp *(%rbp)"
                                     ]
  funTy <- toTypeQ tyAnn
  let importedName = mkName asmName
  wrapperFunD <- mkFunD name importedName funTy
  pure
    [ ForeignD $ ImportF Prim Safe asmName importedName $ unliftType funTy
    , SigD name' funTy
    , wrapperFunD
    , PragmaD $ InlineP name' Inline ConLike AllPhases
    ]
  where
    name' = mkName name
    asmName = name <> "_unlifted"

mkFunD :: String -> Name -> Type -> Q Dec
mkFunD funName importedName funTy = do
  argNames <- replicateM (countArgs funTy) $ newName "arg"
  funAppE <- foldM f (VarE importedName) argNames
  body <- case detectRetTuple funTy of
               Nothing -> [e| rebox $(pure funAppE) |]
               Just n -> do
                  retNames <- replicateM n $ newName "ret"
                  boxing <- forM retNames $ \name -> [e| rebox $(pure $ VarE name) |]
                  [e| case $(pure funAppE) of
                           $(pure $ UnboxedTupP $ VarP <$> retNames) -> $(pure $ TupE boxing)
                    |]
  pure $ FunD (mkName funName) [Clause (VarP <$> argNames) (NormalB body) []]
  where
    f acc argName = [e| $(pure acc) (unbox $(pure $ VarE argName)) |]

unliftType :: Type -> Type
unliftType = transformBi unliftTuple . transformBi unliftBaseTy
  where
    unliftBaseTy x | x == ''Word = ''Word#
                   | x == ''Int = ''Int#
                   | x == ''Double = ''Double#
                   | x == ''Float = ''Float#
                   | otherwise = x
    unliftTuple (TupleT n) = UnboxedTupleT n
    unliftTuple x = x

-- This doesn't check if this is indeed a return type,
-- but since we are not going to support argument tuples (and we'll add a check about that later),
-- it should be fine.
detectRetTuple :: Type -> Maybe Int
detectRetTuple ty | [TupleT n] <- tuples = Just n
                  | otherwise = Nothing
  where
    tuples = [ t | t@(TupleT _) <- universeBi ty]
