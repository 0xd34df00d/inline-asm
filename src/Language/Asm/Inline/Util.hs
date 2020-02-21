module Language.Asm.Inline.Util where

import Data.Generics.Uniplate.Data
import Language.Haskell.TH.Syntax

countArgs :: Type -> Int
countArgs ty = length $ filter (== ArrowT) $ universeBi ty
