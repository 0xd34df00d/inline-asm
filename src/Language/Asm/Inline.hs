module Language.Asm.Inline where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

inlineAsm :: String -> Q [Dec]
inlineAsm asmCode = do
  addForeignSource LangAsm $ unlines [ asmCode
                                     , "\tjmp *(%rbp)"
                                     ]
  pure []
