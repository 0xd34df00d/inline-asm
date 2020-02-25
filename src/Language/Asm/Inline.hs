{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Asm.Inline(defineAsmFun) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Control.Monad
import Data.Generics.Uniplate.Data
import Foreign.Ptr
import Foreign.ForeignPtr.Unsafe
import GHC.Prim
import GHC.Ptr
import GHC.Types hiding (Type)
import GHC.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

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

instance AsmArg (Ptr a) 'AddrRep Addr# where
  unbox (Ptr p) = p
  rebox = Ptr

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

getBSAddr :: BS.ByteString -> Ptr Word8
getBSAddr bs = unsafeForeignPtrToPtr ptr `plusPtr` offset
  where
    (ptr, offset, _) = BS.toForeignPtr bs

mkFunD :: String -> Name -> Type -> Q Dec
mkFunD funName importedName funTy = do
  argNames <- replicateM (countArgs funTy) $ newName "arg"
  funAppE <- foldM f (VarE importedName) $ zip (VarE <$> argNames) (getArgs funTy)
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
    f acc (argName, argType) | argType == ConT ''BS.ByteString = [e| $(pure acc)
                                                                            (unbox $ getBSAddr $(pure argName))
                                                                            (unbox $ BS.length $(pure argName))
                                                                   |]
                             | otherwise = [e| $(pure acc) (unbox $(pure argName)) |]

unliftType :: Type -> Type
unliftType = transformBi unliftTuple
           . transformBi unliftBaseTy
           . transformBi unliftPtrs
           . transformBi unliftBS
  where
    unliftBaseTy x | x == ''Word = ''Word#
                   | x == ''Int = ''Int#
                   | x == ''Double = ''Double#
                   | x == ''Float = ''Float#
                   | otherwise = x

    unliftPtrs (AppT (ConT name) _) | name == ''Ptr = ConT ''Addr#
    unliftPtrs x = x

    unliftBS (AppT (AppT ArrowT (ConT bs)) rhs) | bs == ''BS.ByteString = unsafePerformIO $ runQ [t| Addr# -> Int# -> $(pure rhs) |]
    unliftBS x = x

    unliftTuple (TupleT n) = UnboxedTupleT n
    unliftTuple x = x

detectRetTuple :: Type -> Maybe Int
detectRetTuple (AppT (AppT ArrowT _) rhs) = detectRetTuple rhs
detectRetTuple (AppT lhs _) = detectRetTuple lhs
detectRetTuple (TupleT n) = Just n
detectRetTuple _ = Nothing

getArgs :: Type -> [Type]
getArgs ty = [ argTy | AppT ArrowT argTy <- universeBi ty ]
