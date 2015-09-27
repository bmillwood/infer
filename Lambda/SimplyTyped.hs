{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.SimplyTyped where

import Lambda.Untyped

-- | Types for STLC over a "base type"
data Type t
  = Base t
  | Arrow (Type t) (Type t)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Applicative Type where
  pure = Base

  Base f <*> tx = fmap f tx
  tf <*> Base x = fmap ($ x) tf
  Arrow tf1 tf2 <*> Arrow tx1 tx2 = Arrow (tf1 <*> tx1) (tf2 <*> tx2)

instance Monad Type where
  return = Base

  Base x >>= f = f x
  Arrow t1 t2 >>= f = Arrow (t1 >>= f) (t2 >>= f)

foldType
  :: (t -> z)
  -> (z -> z -> z)
  -> Type t -> z
foldType b a = go
  where
    go (Base t) = b t
    go (Arrow t1 t2) = a (go t1) (go t2)

-- | AST annotated with types
data TExp t n
  = TVar (Type t) n
  | TApp (Type t) (TExp t n) (TExp t n)
  | TLam (Type t) n (Type t) (TExp t n)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

foldTExp
  :: (Type t -> n -> z)
  -> (Type t -> z -> z -> z)
  -> (Type t -> n -> Type t -> z -> z)
  -> TExp t n -> z
foldTExp v a l = go
  where
    go (TVar t n) = v t n
    go (TApp t e1 e2) = a t (go e1) (go e2)
    go (TLam t n nt e) = l t n nt (go e)

forget :: TExp t n -> Exp n
forget = foldTExp
  (\_ n -> Var n)
  (\_ e1 e2 -> App e1 e2)
  (\_ n _ e -> Lam n e)

getType :: TExp t n -> Type t
getType (TVar t _) = t
getType (TApp t _ _) = t
getType (TLam t _ _ _) = t

onTypes :: (Type t -> Type t) -> TExp t n -> TExp t n
onTypes f = foldTExp
  (\t n -> TVar (f t) n)
  (\t g x -> TApp (f t) g x)
  (\t n nt e -> TLam (f t) n (f nt) e)

check :: (Eq n, Eq t) => (n -> Type t) -> TExp t n -> Bool
check typeOf (TVar t n) = typeOf n == t
check typeOf (TApp t f x) =
  case getType f of
    Base _ -> False
    Arrow argTy resultTy ->
      and
        [ t == resultTy
        , getType x == argTy
        , check typeOf f
        , check typeOf x
        ]
check typeOf (TLam t n nt e) =
  and
    [ getType e == t
    , check (\n' -> if n' == n then nt else typeOf n') e
    ]
