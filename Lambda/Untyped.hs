{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.Untyped where

data Exp l n
  = Var n
  | Lit l
  | App (Exp l n) (Exp l n)
  | Lam n (Exp l n)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

foldExp
  :: (n -> z)
  -> (l -> z)
  -> (z -> z -> z)
  -> (n -> z -> z)
  -> Exp l n -> z
foldExp var lit app lam = go
  where
    go (Var n) = var n
    go (Lit l) = lit l
    go (App f x) = app (go f) (go x)
    go (Lam n e) = lam n (go e)
