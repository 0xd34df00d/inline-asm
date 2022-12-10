{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

#include "MachDeps.h"

module Language.Asm.Inline
( defineAsmFun
, defineAsmFunM
, Unit(..)
) where

import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Primitive
import Data.Generics.Uniplate.Data
import Data.List
import Foreign.Ptr
import GHC.Int
import GHC.Prim
import GHC.Ptr
import GHC.Types hiding (Type)
import GHC.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

import Language.Asm.Inline.AsmCode
import Language.Asm.Inline.Util

class AsmArg a (unboxedTy :: TYPE rep) | a -> unboxedTy where
  unbox :: a -> unboxedTy
  rebox :: unboxedTy -> a

data Unit = Unit

#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
type Int8Rep# = Int8#
type Int16Rep# = Int16#
type Int32Rep# = Int32#
type Word8Rep# = Word8#
type Word16Rep# = Word16#
type Word32Rep# = Word32#
#else
type Int8Rep# = Int#
type Int16Rep# = Int#
type Int32Rep# = Int#
type Int64Rep# = Int#
type Word8Rep# = Word#
type Word16Rep# = Word#
type Word32Rep# = Word#
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
type Int64Rep# = Int64#
type Word64Rep# = Word64#
#else
type Int64Rep# = Int#
type Word64Rep# = Word#
#endif

instance AsmArg Unit Int# where
  unbox _ = 0#
  rebox _ = Unit

instance AsmArg Int Int# where
  unbox (I# w) = w
  rebox = I#

instance AsmArg Int8 Int8Rep# where
  unbox (I8# w) = w
  rebox = I8#

instance AsmArg Int16 Int16Rep# where
  unbox (I16# w) = w
  rebox = I16#

instance AsmArg Int32 Int32Rep# where
  unbox (I32# w) = w
  rebox = I32#

#if WORD_SIZE_IN_BITS > 32
instance AsmArg Int64 Int64Rep# where
#else
instance AsmArg Int64 Int64# where
#endif
  unbox (I64# w) = w
  rebox = I64#

instance AsmArg Word Word# where
  unbox (W# w) = w
  rebox = W#

instance AsmArg Word8 Word8Rep# where
  unbox (W8# w) = w
  rebox = W8#

instance AsmArg Word16 Word16Rep# where
  unbox (W16# w) = w
  rebox = W16#

instance AsmArg Word32 Word32Rep# where
  unbox (W32# w) = w
  rebox = W32#

#if WORD_SIZE_IN_BITS > 32
instance AsmArg Word64 Word64Rep# where
#else
instance AsmArg Word64 Word64# where
#endif
  unbox (W64# w) = w
  rebox = W64#

instance AsmArg Double Double# where
  unbox (D# d) = d
  rebox = D#

instance AsmArg Float Float# where
  unbox (F# f) = f
  rebox = F#

instance AsmArg (Ptr a) Addr# where
  unbox (Ptr p) = p
  rebox = Ptr

replace :: String -> String -> String -> String
replace what with = go
  where
    go [] = []
    go str@(s:ss) | what `isPrefixOf` str = with <> go (drop (length what) str)
                  | otherwise = s : go ss

data FunKind = Pure | Monadic

defineAsmFunImpl :: AsmCode tyAnn code => FunKind -> String -> tyAnn -> code -> Q [Dec]
defineAsmFunImpl kind name tyAnn asmCode = do
  addForeignSource LangAsm $ unlines [ ".global " <> asmName
                                     , asmName <> ":"
                                     , replace "RET_HASK" retToHask $ codeToString tyAnn asmCode
                                     , retToHask
                                     ]
  funTy <- toTypeQ tyAnn
  (importedTy, sigTy) <- case kind of
                              Pure -> pure (funTy, funTy)
                              Monadic -> (,) <$> stateifyUnlifted funTy <*> stateifyLifted funTy
  let importedName = mkName asmName
  wrapperFunD <- mkFunD kind name importedName funTy
  pure
    [ ForeignD $ ImportF Prim Safe asmName importedName $ unliftType importedTy
    , SigD name' sigTy
    , wrapperFunD
    , PragmaD $ InlineP name' Inline FunLike AllPhases
    ]
  where
    name' = mkName name
    asmName = name <> "_unlifted"
    retToHask = "jmp *(%rbp)"

defineAsmFun :: AsmCode tyAnn code => String -> tyAnn -> code -> Q [Dec]
defineAsmFun = defineAsmFunImpl Pure

defineAsmFunM :: AsmCode tyAnn code => String -> tyAnn -> code -> Q [Dec]
defineAsmFunM = defineAsmFunImpl Monadic

#if MIN_VERSION_template_haskell(2, 18, 0)
mkPlainTV :: Name -> TyVarBndr Specificity
mkPlainTV n = PlainTV n SpecifiedSpec
#else
mkPlainTV :: Name -> TyVarBndr
mkPlainTV = PlainTV
#endif

-- |Converts the wrapped function type to live in a 'PrimMonad':
-- given 'Ty1 -> Ty2 -> Ret' it produces
-- 'forall m. PrimMonad m => Ty1 -> Ty2 -> m Ret'.
stateifyLifted :: Type -> Q Type
stateifyLifted ty = do
  m <- newName "m"
  ForallT [mkPlainTV m] [AppT (ConT ''PrimMonad) (VarT m)] <$> go m ty
  where
    go m (AppT (AppT ArrowT lhs) rhs) = AppT (AppT ArrowT lhs) <$> go m rhs
    go m rhs = [t| $(pure $ VarT m) $(pure rhs) |]

-- |Converts the unwrapped/unlifted function type to be a 'primitive' action:
-- given 'Ty1# -> Ty2# -> Ret#' it produces
-- 'forall s. Ty1# -> Ty2# -> State# s -> (# State# s, Ret# #)'.
stateifyUnlifted :: Type -> Q Type
stateifyUnlifted ty = do
  s <- newName "s"
  ForallT [mkPlainTV s] [] <$> go s ty
  where
    go s (AppT (AppT ArrowT lhs) rhs) = AppT (AppT ArrowT lhs) <$> go s rhs
    go s rhs = [t| State# $(pure $ VarT s) -> (# State# $(pure $ VarT s), $(pure rhs) #) |]

mkFunD :: FunKind -> String -> Name -> Type -> Q Dec
mkFunD kind funName importedName funTy = do
  token <- newName "token"
  argNames <- replicateM (countArgs funTy) $ newName "arg"
  funAppE <- foldM f (VarE importedName) $ zip (VarE <$> argNames) (getArgs funTy)
  fullFunAppE <- case kind of
                      Pure -> pure funAppE
                      Monadic -> [e| $(pure funAppE) $(pure $ VarE token) |]

  body <- case detectRetTuple funTy of
               Nothing ->
                 case kind of
                      Pure ->
                        [e| rebox $(pure fullFunAppE) |]
                      Monadic ->
                        [e| case $(pure fullFunAppE) of
                                 (# token', res #) -> (# token', rebox res #)
                          |]
               Just n -> do
                  retNames <- replicateM n $ newName "ret"
                  boxing <- forM retNames $ \name -> Just <$> [e| rebox $(pure $ VarE name) |]
                  case kind of
                       Pure ->
                          [e| case $(pure fullFunAppE) of
                                   $(pure $ UnboxedTupP $ VarP <$> retNames) -> $(pure $ TupE boxing)
                            |]
                       Monadic ->
                          [e| case $(pure fullFunAppE) of
                                   (# token', $(pure $ UnboxedTupP $ VarP <$> retNames) #) -> (# token', $(pure $ TupE boxing) #)
                            |]

  body' <- case kind of
                Pure -> pure body
                Monadic -> [e| primitive (\ $(pure $ VarP token) -> $(pure body)) |]
  pure $ FunD (mkName funName) [Clause (VarP <$> argNames) (NormalB body') []]
  where
    f acc (argName, argType) | argType == ConT ''BS.ByteString = [e| $(pure acc)
                                                                            (unbox $ getBSAddr $(pure argName))
                                                                            (unbox $ BS.length $(pure argName))
                                                                   |]
                             | otherwise = [e| $(pure acc) (unbox $(pure argName)) |]

{-# NOINLINE unliftType #-}
unliftType :: Type -> Type
unliftType = transformBi unliftTuple
           . transformBi unliftBaseTy
           . transformBi unliftPtrs
           . transformBi unliftBS
  where
    unliftBaseTy x
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
      | x == ''Word || x == ''Word64 = ''Word#
      | x == ''Word8 = ''Word8#
      | x == ''Word16 = ''Word16#
      | x == ''Word32 = ''Word32#

      | x == ''Int || x == ''Int64 = ''Int#
      | x == ''Int8 = ''Int8#
      | x == ''Int16 = ''Int16#
      | x == ''Int32 = ''Int32#
#else
      | x `elem` [ ''Word, ''Word8, ''Word16, ''Word32, ''Word64 ] = ''Word#
      | x `elem` [ ''Int, ''Int8, ''Int16, ''Int32, ''Int64 ] = ''Int#
#endif
      | x == ''Double = ''Double#
      | x == ''Float = ''Float#
      | x == ''Unit = ''Int#
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

countArgs :: Type -> Int
countArgs ty = length $ filter (== ArrowT) $ universeBi ty
