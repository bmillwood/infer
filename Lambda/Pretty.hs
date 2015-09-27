module Lambda.Pretty where

import Lambda.Untyped

prettysPrecExp
  :: (l -> ShowS) -> (n -> ShowS)
  -> Integer -> Exp l n -> ShowS
prettysPrecExp showL showN p e = case e of
  Var n -> showN n
  Lit l -> showL l
  App f x -> parensIf (p > 1) $ rec 1 f . str " " . rec 2 x
  l@(Lam _ _) -> parensIf (p > 0) $
    str "λ " . lam l
 where
  rec = prettysPrecExp showL showN
  str = showString
  parensIf True f = str "(" . f . str ")"
  parensIf False f = f
  lam (Lam n e) = showN n . str " " . lam e
  lam e = str "→ " . rec 0 e

prettyExp
  :: (l -> String) -> (n -> String)
  -> Exp l n -> String
prettyExp showL showN e =
  prettysPrecExp (showString . showL) (showString . showN) 0 e ""
