{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, MultiWayIf, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Asm.Inline.QQ
( asm
, asmTy

, substitute
, unroll
, unrolls
) where

import qualified Data.Map as M
import Control.Applicative(ZipList(..))
import Control.Monad.Combinators.Expr as CE
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Either.Combinators
import Data.Foldable
import Data.Functor
import Data.List
import Data.String
import Data.Void
import Foreign.Ptr
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
                              Right s -> asmCode s
  toTypeQ = unreflectTy

asm :: QuasiQuoter
asm = expQQ asmQE

asmQE :: String -> Q Exp
asmQE p = [e| AsmQQCode p |]

newtype AsmQQCode = AsmQQCode { asmCode :: String }

instance Semigroup AsmQQCode where
  c1 <> c2 = AsmQQCode $ asmCode c1 <> "\n" <> asmCode c2

instance Monoid AsmQQCode where
  mempty = AsmQQCode ""


parseExpr :: MonadError String m => String -> Int -> String -> m Int
parseExpr var num inputStr = liftEither $ first showParseError $ runParser (expr <* eof) "" inputStr
  where
    expr = makeExprParser term table <?> "expr"
    term = parens expr <|> ML.signed lexSpace (string "0x" *> ML.hexadecimal <|> ML.decimal) <|> (lexeme (string var) $> num) <?> "term"
    table = [ [ binary "*" (*) ]
            , [ binary "+" (+)
              , binary "-" (-)
              ]
            ]
    binary name fun = CE.InfixL $ symbol name $> fun
    symbol = ML.symbol lexSpace
    parens = between (symbol "(") (symbol ")")
    lexeme = ML.lexeme lexSpace
    lexSpace = ML.space space1 empty empty

unroll :: String -> [Int] -> AsmQQCode -> AsmQQCode
unroll var ints code = case substitute sub code of
                            Left err -> error err
                            Right codes -> mconcat $ getZipList codes
  where
    sub str = case traverse (\n -> parseExpr var n str) ints of
                   Right results -> show <$> ZipList results
                   Left _ -> pure $ "${" <> str <> "}"

unrolls :: String -> [Int] -> [AsmQQCode] -> AsmQQCode
unrolls var ints = foldMap $ unroll var ints

substitute :: Applicative f => (String -> f String) -> AsmQQCode -> Either String (f AsmQQCode)
substitute subst AsmQQCode { .. } = fmap AsmQQCode <$> go asmCode
  where
    go ('$' : '{' : rest)
      | (argStr, '}' : rest') <- break (== '}') rest
      , not $ null argStr = ((<>) <$> subst (trim argStr) <*>) <$> go rest'
      | otherwise = Left $ "Unable to parse argument: " <> take 20 rest <> "..."
    go (x : xs) = fmap (x :) <$> go xs
    go [] = pure $ pure []

substituteArgs :: AsmQQType -> AsmQQCode -> Either String AsmQQCode
substituteArgs AsmQQType { .. } asmCode = do
  argRegs <- computeRegisters args
  retRegs <- computeRegisters rets
  res <- substitute subst asmCode
  evalStateT res $ M.fromList $ retRegs <> argRegs
  where
    subst arg | "move" `isPrefixOf` arg = moveReg arg
              | otherwise = do
        let var = AsmVarName arg
        maybeReg <- gets $ \regMap -> M.lookup var regMap
        RegName reg <- liftEither $ maybeToRight ("Unknown argument: `" <> show var <> "`") maybeReg
        pure $ '%' : reg

    moveReg (words -> ["move", regName, reg]) = do
      oldReg <- subst regName
      let mov = "mov " <> oldReg <> ", %" <> reg
      modify' $ M.insert (AsmVarName regName) (RegName reg)
      pure mov
    moveReg s = throwError $ "Unable to parse move command `" <> s <> "`"

newtype RegName = RegName { regName :: String } deriving (Show, IsString)

computeRegisters :: [(AsmVarName, AsmVarType)] -> Either String [(AsmVarName, RegName)]
computeRegisters vars = fst <$> foldM handleType ([], mempty) vars
  where
    handleType (regNames, regCounts) (name, ty) = do
      cats <- categorize name ty
      foldM handleCats (regNames, regCounts) cats

    handleCats (regNames, regCounts) (name, cat) = do
      reg <- argIdxToReg cat idx
      pure ((name, reg) : regNames, M.insert cat (idx + 1) regCounts)
      where
        idx = M.findWithDefault 0 cat regCounts

data VarTyCat = Integer | Other deriving (Eq, Ord, Show, Enum, Bounded)

categorize :: AsmVarName -> AsmVarType -> Either String [(AsmVarName, VarTyCat)]
categorize name (AsmVarType "Int") = pure [(name, Integer)]
categorize name (AsmVarType "Word") = pure [(name, Integer)]
categorize name (AsmVarType "Word8") = pure [(name, Integer)]
categorize name (AsmVarType "Ptr") = pure [(name, Integer)]
categorize name (AsmVarType "Float") = pure [(name, Other)]
categorize name (AsmVarType "Double") = pure [(name, Other)]
categorize name (AsmVarType "ByteString") = pure [(name <> ":ptr", Integer), (name <> ":len", Integer)]
categorize _ (AsmVarType s) = throwError $ "Unknown register type: " <> s

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

newtype AsmVarName = AsmVarName { varName :: String } deriving (Show, Eq, Ord, Lift, Semigroup, IsString)
newtype AsmVarType = AsmVarType { varType :: String } deriving (Show, Eq, Ord, Lift)

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

showParseError :: ParseErrorBundle String Void -> String
showParseError = errorBundlePretty

parseInTypes :: forall m e. MonadParsec e String m => m [(AsmVarName, AsmVarType)]
parseInTypes = space *> many parseType
  where
    parseType = do
      void $ lexeme $ string "("
      name <- lexeme $ parseWFirst letterChar <|> string "_"
      void $ lexeme $ string ":"
      ty <- lexeme $ parseWFirst upperChar
      void $ takeWhileP Nothing (/= ')')
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
    argFolder argName funAcc | argName == ''Ptr = [t| Ptr () -> $(pure funAcc) |]
                             | otherwise = [t| $(pure $ ConT argName) -> $(pure funAcc) |]

unreflectRetTy :: [(AsmVarName, AsmVarType)] -> Q Type
unreflectRetTy [] = [t| () |]
unreflectRetTy rets = do
  maybeRetTyNames <- lookupTyNames rets
  case maybeRetTyNames of
       Left err -> error err
       Right [tyName] -> if | tyName == ''Ptr -> [t| Ptr () |]
                            | otherwise -> pure $ ConT tyName
       Right retNames -> pure $ foldl retFolder (TupleT $ length retNames) retNames
  where
    retFolder tupAcc ret | ret == ''Ptr = tupAcc `AppT` (ConT ret `AppT` TupleT 0)
                         | otherwise = tupAcc `AppT` ConT ret

lookupTyNames :: [(AsmVarName, AsmVarType)] -> Q (Either String [Name])
lookupTyNames = fmap sequence . mapM f
  where
    f (name, ty) = maybeToRight ("Unable to lookup type " <> show ty <> " for var " <> show name) <$> lookupTypeName (varType ty)
