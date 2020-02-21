{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Language.Asm.Inline
( defineAsmFun
, asm
) where

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Either.Combinators
import Data.Generics.Uniplate.Data
import Data.List
import GHC.Prim
import GHC.Types hiding (Type)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

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

class AsmCode c where
  codeToString :: c -> String
  validateCode :: Type -> c -> Either String ()

instance AsmCode String where
  codeToString = id
  validateCode _ _ = pure ()

instance AsmCode AsmQQParsed where
  codeToString = asmBody
  validateCode ty code =
    check "arguments count mismatch" $ argsCount code == countArgs ty
    where
      check _ True = pure ()
      check str False = throwError $ "Type error: " <> str

defineAsmFun :: AsmCode c => String -> Q Type -> c -> Q [Dec]
defineAsmFun name funTyQ asmCode = do
  addForeignSource LangAsm $ unlines [ ".global " <> asmName
                                     , asmName <> ":"
                                     , codeToString asmCode
                                     , "jmp *(%rbp)"
                                     ]
  funTy <- funTyQ
  case validateCode funTy asmCode of
       Right () -> pure ()
       Left err -> error err
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

asm :: QuasiQuoter
asm = QuasiQuoter { quoteExp = asmQE, quotePat = unsupported, quoteType = unsupported, quoteDec = unsupported }
  where
    unsupported = const $ error "Unsupported quasiquotation type"

asmQE :: String -> Q Exp
asmQE p = case parseAsmQQ p of
               Left err -> error err
               Right parsed -> [e| parsed |]

data AsmQQParsed = AsmQQParsed
  { argsCount :: Int
  , asmBody :: String
  } deriving (Show, Lift)

parseAsmQQ :: String -> Either String AsmQQParsed
parseAsmQQ = findSplitter
         >=> (pure . first words)
         >=> substituteArgs
  where
    findSplitter p = case break (== '|') p of
                          (vars, '|' : body) -> pure (vars, body)
                          _ -> throwError "Unable to find variable section separator"

substituteArgs :: ([String], String) -> Either String AsmQQParsed
substituteArgs (args, contents) = AsmQQParsed (length args) <$> go contents
  where
    go ('$' : '{' : rest)
      | (arg, '}' : rest') <- break (== '}') rest = do
        idx <- if retPrefix `isPrefixOf` arg
                  then pure $ read (drop (length retPrefix) arg)
                  else maybeToRight ("Unknown argument: `" <> trim arg <> "`") $ elemIndex (trim arg) args
        reg <- argIdxToReg idx
        (('%' : reg) <>) <$> go rest'
      | otherwise = throwError $ "Unable to parse argument: " <> take 20 rest <> "..."
    go (x : xs) = (x :) <$> go xs
    go [] = pure []

    retPrefix = "ret"

argIdxToReg :: Int -> Either String String
argIdxToReg 0 = pure "rbx"
argIdxToReg 1 = pure "r14"
argIdxToReg 2 = pure "rsi"
argIdxToReg 3 = pure "rdi"
argIdxToReg 4 = pure "r8"
argIdxToReg 5 = pure "r9"
argIdxToReg n = throwError $ "Unsupported register index: " <> show n

trim :: String -> String
trim = pass . pass
  where
    pass = reverse . dropWhile (== ' ')

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

countArgs :: Type -> Int
countArgs ty = length $ filter (== ArrowT) $ universeBi ty

-- This doesn't check if this is indeed a return type,
-- but since we are not going to support argument tuples (and we'll add a check about that later),
-- it should be fine.
detectRetTuple :: Type -> Maybe Int
detectRetTuple ty | [TupleT n] <- tuples = Just n
                  | otherwise = Nothing
  where
    tuples = [ t | t@(TupleT _) <- universeBi ty]
