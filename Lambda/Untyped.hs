{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.Untyped where

-- | Type representing expressions in the untyped lambda calculus, parametrised 
-- by type of names.
data Exp n
  = Var n
  | App (Exp n) (Exp n)
  | Lam n (Exp n)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

foldExp
  :: (n -> z)
  -> (z -> z -> z)
  -> (n -> z -> z)
  -> Exp n -> z
foldExp v a l = go
  where
    go (Var n) = v n
    go (App f x) = a (go f) (go x)
    go (Lam n e) = l n (go e)
