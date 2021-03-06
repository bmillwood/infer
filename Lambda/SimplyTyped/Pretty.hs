module Lambda.SimplyTyped.Pretty where

import Lambda.SimplyTyped

parensIf True f = showChar '(' . f . showChar ')'
parensIf False f = f

parens = parensIf True

prettysPrecType :: (t -> ShowS) -> Integer -> Type t -> ShowS
prettysPrecType showsN p e = case e of
  Base n -> showsN n
  Arrow t1 t2 -> parensIf (p > 0) $
    prettysPrecType showsN 1 t1 . showString " → " . prettysPrecType showsN 0 t2

prettyType :: (t -> String) -> Type t -> String
prettyType showN t = prettysPrecType (showString . showN) 0 t ""

prettysPrecExp
  :: (l -> ShowS) -> (t -> ShowS) -> (n -> ShowS)
  -> Integer -> TExp l t n -> ShowS
prettysPrecExp showsL showsT showsN p e = go p e
  where
    ppt t = showChar ':' . prettysPrecType showsT 0 t
    go _ (TVar t n) = parens $ showsN n . ppt t
    go _ (TLit t l) = parens $ showsL l . ppt t
    go p (TApp t f x) = parens $ go p f . showChar ' ' . go p x . ppt t
    go p (TLam t n nt x) = parens $
      showString "λ " . parens (showsN n . ppt nt) . showString ". " . go p x . ppt t

prettyTExp
  :: (l -> String) -> (t -> String) -> (n -> String)
  -> TExp l t n -> String
prettyTExp showL showT showN e =
  prettysPrecExp (showString . showL) (showString . showT) (showString . showN)
    0 e ""
