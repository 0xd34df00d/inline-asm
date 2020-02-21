{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Asm.Inline.QQ(asm, asmTy) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Char
import Data.Either.Combinators
import Data.List
import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as ML

import Language.Asm.Inline.AsmCode
import Language.Asm.Inline.Util

instance AsmCode AsmQQParsed where
  codeToString = asmBody
  validateCode ty code =
    check "arguments count mismatch" $ argsCount code == countArgs ty
    where
      check _ True = pure ()
      check str False = throwError $ "Type error: " <> str

asm :: QuasiQuoter
asm = expQQ asmQE

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

newtype AsmVarName = AsmVarName { varName :: String } deriving (Show, Lift)
newtype AsmVarType = AsmVarType { varType :: String } deriving (Show, Lift)

data AsmQQType = AsmQQType
 { args :: [(AsmVarName, AsmVarType)]
 , rets :: [AsmVarType]
 } deriving (Show, Lift)

parseAsmTyQQ :: String -> Either String AsmQQType
parseAsmTyQQ str = do
  (inputStr, outputStr) <- findSplitter str
  let rets = AsmVarType <$> words outputStr
  args <- first showParseError $ runParser (parseInTypes <* eof) "" inputStr
  pure AsmQQType { .. }
  where
    showParseError = errorBundlePretty :: ParseErrorBundle String Void -> String

parseInTypes :: forall m e. MonadParsec e String m => m [(AsmVarName, AsmVarType)]
parseInTypes = space *> many parseType
  where
    parseType = do
      void $ lexeme $ string "("
      name <- lexeme $ parseWFirst letterChar
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
