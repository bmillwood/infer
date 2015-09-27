module Lambda.Pretty where

import Lambda.Untyped

prettysPrecExp :: (Show n) => Integer -> Exp n -> ShowS
prettysPrecExp p e = case e of
  Var n -> shows n
  App f x -> parensIf (p > 1) $
    prettysPrecExp 1 f . str " " . prettysPrecExp 2 x
  l@(Lam _ _) -> parensIf (p > 0) $
    str "λ " . lam l
 where
  str = showString
  parensIf True f = str "(" . f . str ")"
  parensIf False f = f
  lam (Lam n e) = shows n . str " " . lam e
  lam e = str "→ " . prettysPrecExp 0 e

prettyExp :: Show n => Exp n -> String
prettyExp e = prettysPrecExp 0 e ""
