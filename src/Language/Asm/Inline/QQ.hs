{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Asm.Inline.QQ(asm, asmTy) where

import qualified Data.Map as M
import Control.Monad.Except
import Data.Bifunctor
import Data.Char
import Data.Either.Combinators
import Data.Foldable
import Data.String
import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as ML

import Language.Asm.Inline.AsmCode

instance AsmCode AsmQQType AsmQQCode where
  codeToString ty code = case substituteArgs ty code of
                              Left e -> error e
                              Right s -> s
  toTypeQ = unreflectTy

asm :: QuasiQuoter
asm = expQQ asmQE

asmQE :: String -> Q Exp
asmQE p = [e| AsmQQCode p |]

newtype AsmQQCode = AsmQQCode { asmCode :: String }

substituteArgs :: AsmQQType -> AsmQQCode -> Either String String
substituteArgs AsmQQType { .. } AsmQQCode { .. } = do
  argRegs <- computeRegisters args
  retRegs <- computeRegisters rets
  go' argRegs retRegs asmCode
  where
    go' argRegs retRegs = go
      where
        go ('$' : '{' : rest)
          | (argStr, '}' : rest') <- break (== '}') rest
          , not $ null argStr = do
            let arg = AsmVarName $ trim argStr
            RegName reg <- maybeToRight ("Unknown argument: `" <> show arg <> "`") $ msum [lookup arg argRegs, lookup arg retRegs]
            (('%' : reg) <>) <$> go rest'
          | otherwise = throwError $ "Unable to parse argument: " <> take 20 rest <> "..."
        go (x : xs) = (x :) <$> go xs
        go [] = pure []

newtype RegName = RegName { regName :: String } deriving (Show, IsString)

computeRegisters :: [(AsmVarName, AsmVarType)] -> Either String [(AsmVarName, RegName)]
computeRegisters vars = fst <$> foldM f ([], mempty) vars
  where
    f (regNames, regCounts) (name, ty) = do
      cat <- categorize ty
      let idx = M.findWithDefault 0 cat regCounts
      reg <- argIdxToReg cat idx
      pure ((name, reg) : regNames, M.insert cat (idx + 1) regCounts)

data VarTyCat = Integer | Other deriving (Eq, Ord, Show, Enum, Bounded)

categorize :: AsmVarType -> Either String VarTyCat
categorize (AsmVarType "Int") = pure Integer
categorize (AsmVarType "Word") = pure Integer
categorize (AsmVarType "Float") = pure Other
categorize (AsmVarType "Double") = pure Other
categorize (AsmVarType s) = throwError $ "Unknown register type: " <> s

argIdxToReg :: VarTyCat -> Int -> Either String RegName
argIdxToReg Integer 0 = pure "rbx"
argIdxToReg Integer 1 = pure "r14"
argIdxToReg Integer 2 = pure "rsi"
argIdxToReg Integer 3 = pure "rdi"
argIdxToReg Integer 4 = pure "r8"
argIdxToReg Integer 5 = pure "r9"
argIdxToReg Other n | n >= 0 && n <= 6 = pure $ RegName $ "xmm" <> show (n + 1)
argIdxToReg _ n = throwError $ "Unsupported register index: " <> show n

trim :: String -> String
trim = pass . pass
  where
    pass = reverse . dropWhile (== ' ')

findSplitter :: String -> Either String (String, String)
findSplitter p = case break (== '|') p of
                      (vars, '|' : body) -> pure (vars, body)
                      _ -> throwError "Unable to find variable section separator"

expQQ :: (String -> Q Exp) -> QuasiQuoter
expQQ qq = QuasiQuoter { quoteExp = qq, quotePat = unsupported, quoteType = unsupported, quoteDec = unsupported }
  where
    unsupported = const $ error "Unsupported quasiquotation type"

asmTy :: QuasiQuoter
asmTy = expQQ asmTyQE

asmTyQE :: String -> Q Exp
asmTyQE str = case parseAsmTyQQ str of
                   Left err -> error err
                   Right parsed -> [e| parsed |]

newtype AsmVarName = AsmVarName { varName :: String } deriving (Show, Eq, Lift)
newtype AsmVarType = AsmVarType { varType :: String } deriving (Show, Eq, Lift)

data AsmQQType = AsmQQType
 { args :: [(AsmVarName, AsmVarType)]
 , rets :: [(AsmVarName, AsmVarType)]
 } deriving (Show, Lift)

parseAsmTyQQ :: String -> Either String AsmQQType
parseAsmTyQQ str = do
  (inputStr, outputStr) <- findSplitter str
  args <- first showParseError $ runParser (parseInTypes <* eof) "" inputStr
  rets <- first showParseError $ runParser (parseInTypes <* eof) "" outputStr
  pure AsmQQType { .. }
  where
    showParseError = errorBundlePretty :: ParseErrorBundle String Void -> String

parseInTypes :: forall m e. MonadParsec e String m => m [(AsmVarName, AsmVarType)]
parseInTypes = space *> many parseType
  where
    parseType = do
      void $ lexeme $ string "("
      name <- lexeme $ parseWFirst letterChar <|> string "_"
      void $ lexeme $ string ":"
      ty <- lexeme $ parseWFirst upperChar
      void $ lexeme $ string ")"
      pure (AsmVarName name, AsmVarType ty)

    parseWFirst :: m Char -> m String
    parseWFirst p = do
      firstLetter <- p
      rest <- takeWhileP (Just "variable") isAlphaNum
      pure $ firstLetter : rest

    lexeme = ML.lexeme $ ML.space space1 empty empty

unreflectTy :: AsmQQType -> Q Type
unreflectTy AsmQQType { .. } = do
  retTy <- unreflectRetTy rets
  maybeArgTyNames <- lookupTyNames args
  case maybeArgTyNames of
       Left err -> error err
       Right argTyNames -> foldrM argFolder retTy argTyNames
  where
    argFolder argName funAcc = [t| $(pure $ ConT argName) -> $(pure funAcc) |]

unreflectRetTy :: [(AsmVarName, AsmVarType)] -> Q Type
unreflectRetTy [] = [t| () |]
unreflectRetTy rets = do
  maybeRetTyNames <- lookupTyNames rets
  case maybeRetTyNames of
       Left err -> error err
       Right [tyName] -> pure $ ConT tyName
       Right retNames -> pure $ foldl retFolder (TupleT $ length retNames) retNames
  where
    retFolder tupAcc ret = tupAcc `AppT` ConT ret

lookupTyNames :: [(AsmVarName, AsmVarType)] -> Q (Either String [Name])
lookupTyNames = fmap sequence . mapM f
  where
    f (name, ty) = maybeToRight ("Unable to lookup type " <> show ty <> " for var " <> show name) <$> lookupTypeName (varType ty)
