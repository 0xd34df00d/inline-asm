{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Asm.Inline.QQ(asm) where

import Control.Arrow
import Control.Monad.Except
import Data.Either.Combinators
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

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
