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
data TExp l t n
  = TVar (Type t) n
  | TLit (Type t) l
  | TApp (Type t) (TExp l t n) (TExp l t n)
  | TLam (Type t) n (Type t) (TExp l t n)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

foldTExp
  :: (Type t -> n -> z)
  -> (Type t -> l -> z)
  -> (Type t -> z -> z -> z)
  -> (Type t -> n -> Type t -> z -> z)
  -> TExp l t n -> z
foldTExp var lit app lam = go
  where
    go (TVar t n) = var t n
    go (TLit t l) = lit t l
    go (TApp t e1 e2) = app t (go e1) (go e2)
    go (TLam t n nt e) = lam t n nt (go e)

forget :: TExp l t n -> Exp l n
forget = foldTExp
  (\_ n -> Var n)
  (\_ l -> Lit l)
  (\_ e1 e2 -> App e1 e2)
  (\_ n _ e -> Lam n e)

getType :: TExp l t n -> Type t
getType (TVar t _) = t
getType (TLit t _) = t
getType (TApp t _ _) = t
getType (TLam t _ _ _) = t

onTypes :: (Type t -> Type t) -> TExp l t n -> TExp l t n
onTypes f = foldTExp
  (\t n -> TVar (f t) n)
  (\t l -> TLit (f t) l)
  (\t g x -> TApp (f t) g x)
  (\t n nt e -> TLam (f t) n (f nt) e)

check :: (Eq n, Eq t) => (n -> Type t) -> (l -> Type t) -> TExp l t n -> Bool
check varTy _ (TVar t n) = varTy n == t
check _ litTy (TLit t l) = litTy l == t
check varTy litTy (TApp t f x) =
  case getType f of
    Base _ -> False
    Arrow argTy resultTy ->
      and
        [ t == resultTy
        , getType x == argTy
        , check varTy litTy f
        , check varTy litTy x
        ]
check varTy litTy (TLam t n nt e) =
  and
    [ getType e == t
    , check (\n' -> if n' == n then nt else varTy n') litTy e
    ]
